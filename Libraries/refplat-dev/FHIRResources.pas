  {@Class TFhirResourceFactory : TFHIRBaseFactory
     FHIR factory: class constructors and general useful builders
  }
{!Wrapper uses FHIRBase, FHIRBase_Wrapper, FHIRTypes, FHIRTypes_Wrapper, FHIRComponents, FHIRComponents_Wrapper, DateAndTime, DateAndTime_Wrapper}

unit FHIRResources;

{
  Copyright (c) 2011-2014, HL7, Inc.
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

// FHIR v0.3.0 generated Tue, Aug 26, 2014 04:54+1000

uses
  SysUtils, Classes, StringSupport, DecimalSupport, AdvBuffers, DateAndTime, FHIRBase, FHIRTypes, FHIRComponents;

Type
  {@Enum TFhirResourceType
    Enumeration of known resource types
  }
  TFhirResourceType = (
    frtNull, {@enum.value Resource type not known / not Specified }
    frtAdverseReaction, {@enum.value Records an unexpected reaction suspected to be related to the exposure of the reaction subject to a substance. }
    frtAlert, {@enum.value Prospective warnings of potential issues when providing care to the patient. }
    frtAllergyIntolerance, {@enum.value Indicates the patient has a susceptibility to an adverse reaction upon exposure to a specified substance. }
    frtAppointment, {@enum.value (informative) A scheduled appointment for a patient and/or practitioner(s) where a service may take place. }
    frtAppointmentResponse, {@enum.value A reply to an appointment request for a patient and/or practitioner(s), such as a confirmation or rejection. }
    frtAvailability, {@enum.value (informative) A container for slot(s) of time that may be available for booking appointments. }
    frtCarePlan, {@enum.value Describes the intention of how one or more practitioners intend to deliver care for a particular patient for a period of time, possibly limited to care for a specific condition or set of conditions. }
    frtComposition, {@enum.value A set of healthcare-related information that is assembled together into a single logical document that provides a single coherent statement of meaning, establishes its own context and that has clinical attestation with regard to who is making the statement. }
    frtConceptMap, {@enum.value A statement of relationships from one set of concepts to one or more other concepts - either code systems or data elements, or classes in class models. }
    frtCondition, {@enum.value Use to record detailed information about conditions, problems or diagnoses recognized by a clinician. There are many uses including: recording a Diagnosis during an Encounter; populating a problem List or a Summary Statement, such as a Discharge Summary. }
    frtConformance, {@enum.value A conformance statement is a set of requirements for a desired implementation or a description of how a target application fulfills those requirements in a particular implementation. }
    frtContraindication, {@enum.value Indicates an actual or potential clinical issue with or between one or more active or proposed clinical actions for a patient.  E.g. Drug-drug interaction, Ineffective treatment frequency, Procedure-condition conflict, etc. }
    frtDataElement, {@enum.value The formal description of a single piece of information that can be gathered and reported. }
    frtDevice, {@enum.value This resource identifies an instance of a manufactured thing that is used in the provision of healthcare without being substantially changed through that activity. The device may be a machine, an insert, a computer, an application, etc. This includes durable (reusable) medical equipment as well as disposable equipment used for diagnostic, treatment, and research for healthcare and public health. }
    frtDeviceObservationReport, {@enum.value Describes the data produced by a device at a point in time. }
    frtDiagnosticOrder, {@enum.value A request for a diagnostic investigation service to be performed. }
    frtDiagnosticReport, {@enum.value The findings and interpretation of diagnostic  tests performed on patients, groups of patients, devices, and locations, and/or specimens derived from these. The report includes clinical context such as requesting and provider information, and some mix of atomic results, images, textual and coded interpretation, and formatted representation of diagnostic reports. }
    frtDocumentManifest, {@enum.value A manifest that defines a set of documents. }
    frtDocumentReference, {@enum.value A reference to a document. }
    frtEncounter, {@enum.value An interaction between a patient and healthcare provider(s) for the purpose of providing healthcare service(s) or assessing the health status of a patient. }
    frtFamilyHistory, {@enum.value Significant health events and conditions for people related to the subject relevant in the context of care for the subject. }
    frtGroup, {@enum.value Represents a defined collection of entities that may be discussed or acted upon collectively but which are not expected to act collectively and are not formally or legally recognized.  I.e. A collection of entities that isn't an Organization. }
    frtImagingStudy, {@enum.value Manifest of a set of images produced in study. The set of images may include every image in the study, or it may be an incomplete sample, such as a list of key images. }
    frtImmunization, {@enum.value Immunization event information. }
    frtImmunizationRecommendation, {@enum.value A patient's point-of-time immunization status and recommendation with optional supporting justification. }
    frtList, {@enum.value A set of information summarized from a list of other resources. }
    frtLocation, {@enum.value Details and position information for a physical place where services are provided  and resources and participants may be stored, found, contained or accommodated. }
    frtMedia, {@enum.value A photo, video, or audio recording acquired or used in healthcare. The actual content may be inline or provided by direct reference. }
    frtMedication, {@enum.value Primarily used for identification and definition of Medication, but also covers ingredients and packaging. }
    frtMedicationAdministration, {@enum.value Describes the event of a patient being given a dose of a medication.  This may be as simple as swallowing a tablet or it may be a long running infusion.

Related resources tie this event to the authorizing prescription, and the specific encounter between patient and health care practitioner. }
    frtMedicationDispense, {@enum.value Dispensing a medication to a named patient.  This includes a description of the supply provided and the instructions for administering the medication. }
    frtMedicationPrescription, {@enum.value An order for both supply of the medication and the instructions for administration of the medicine to a patient. }
    frtMedicationStatement, {@enum.value A record of medication being taken by a patient, or that the medication has been given to a patient where the record is the result of a report from the patient or another clinician. }
    frtMessageHeader, {@enum.value The header for a message exchange that is either requesting or responding to an action.  The resource(s) that are the subject of the action as well as other Information related to the action are typically transmitted in a bundle in which the MessageHeader resource instance is the first resource in the bundle. }
    frtNamespace, {@enum.value A curated namespace that issues unique symbols within that namespace for the identification of concepts, people, devices, etc.  Represents a "System" used within the Identifier and Coding data types. }
    frtObservation, {@enum.value Measurements and simple assertions made about a patient, device or other subject. }
    frtOperationDefinition, {@enum.value A formal computable definition of an operation (on the RESTful interface) or a named query (using the search interaction). }
    frtOperationOutcome, {@enum.value A collection of error, warning or information messages that result from a system action. }
    frtOrder, {@enum.value A request to perform an action. }
    frtOrderResponse, {@enum.value A response to an order. }
    frtOrganization, {@enum.value A formally or informally recognized grouping of people or organizations formed for the purpose of achieving some form of collective action.  Includes companies, institutions, corporations, departments, community groups, healthcare practice groups, etc. }
    frtOther, {@enum.value Other is a conformant for handling resource concepts not yet defined for FHIR or outside HL7's scope of interest. }
    frtPatient, {@enum.value Demographics and other administrative information about a person or animal receiving care or other health-related services. }
    frtPractitioner, {@enum.value A person who is directly or indirectly involved in the provisioning of healthcare. }
    frtProcedure, {@enum.value An action that is performed on a patient. This can be a physical 'thing' like an operation, or less invasive like counseling or hypnotherapy. }
    frtProfile, {@enum.value A Resource Profile - a statement of use of one or more FHIR Resources.  It may include constraints on Resources and Data Types, Terminology Binding Statements and Extension Definitions. }
    frtProvenance, {@enum.value Provenance information that describes the activity that led to the creation of a set of resources. This information can be used to help determine their reliability or trace where the information in them came from. The focus of the provenance resource is record keeping, audit and traceability, and not explicit statements of clinical significance. }
    frtQuery, {@enum.value A description of a query with a set of parameters. }
    frtQuestionnaire, {@enum.value A structured set of questions intended to guide the collection of answers. The questions are ordered and grouped into coherent subsets, corresponding to the structure of the grouping of the underlying questions. }
    frtQuestionnaireAnswers, {@enum.value A structured set of questions and their answers. The questions are ordered and grouped into coherent subsets, corresponding to the structure of the grouping of the underlying questions. }
    frtReferralRequest, {@enum.value Used to record and send details about a request for referral service or transfer of a patient to the care of another provider or provider organisation. }
    frtRelatedPerson, {@enum.value Information about a person that is involved in the care for a patient, but who is not the target of healthcare, nor has a formal responsibility in the care process. }
    frtRiskAssessment, {@enum.value An assessment of the likely outcome(s) for a patient or other subject as well as the likelihood of each outcome. }
    frtSecurityEvent, {@enum.value A record of an event made for purposes of maintaining a security log. Typical uses include detection of intrusion attempts and monitoring for inappropriate usage. }
    frtSlot, {@enum.value (informative) A slot of time on a schedule that may be available for booking appointments. }
    frtSpecimen, {@enum.value Sample for analysis. }
    frtSubscription, {@enum.value Todo. }
    frtSubstance, {@enum.value A homogeneous material with a definite composition. }
    frtSupply, {@enum.value A supply - a  request for something, and provision of what is supplied. }
    frtValueSet, {@enum.value A value set specifies a set of codes drawn from one or more code systems. }
    frtBinary); {@enum.value Binary Resource }

  TFhirResourceTypeSet = set of TFhirResourceType;
  {@Enum TSearchParamsAdverseReaction
    Search Parameters for AdverseReaction
  }
  TSearchParamsAdverseReaction = (
    spAdverseReaction__id, {@enum.value spAdverseReaction__id The logical resource id associated with the resource (must be supported by all servers) }
    spAdverseReaction__language, {@enum.value spAdverseReaction__language The stated language of the resource }
    spAdverseReaction_Date, {@enum.value spAdverseReaction_Date The date of the reaction }
    spAdverseReaction_Subject, {@enum.value spAdverseReaction_Subject The subject that the sensitivity is about }
    spAdverseReaction_Substance, {@enum.value spAdverseReaction_Substance The name or code of the substance that produces the sensitivity }
    spAdverseReaction_Symptom); {@enum.value spAdverseReaction_Symptom One of the symptoms of the reaction }

  {@Enum TSearchParamsAlert
    Search Parameters for Alert
  }
  TSearchParamsAlert = (
    spAlert__id, {@enum.value spAlert__id The logical resource id associated with the resource (must be supported by all servers) }
    spAlert__language, {@enum.value spAlert__language The stated language of the resource }
    spAlert_Subject); {@enum.value spAlert_Subject The identity of a subject to list alerts for }

  {@Enum TSearchParamsAllergyIntolerance
    Search Parameters for AllergyIntolerance
  }
  TSearchParamsAllergyIntolerance = (
    spAllergyIntolerance__id, {@enum.value spAllergyIntolerance__id The logical resource id associated with the resource (must be supported by all servers) }
    spAllergyIntolerance__language, {@enum.value spAllergyIntolerance__language The stated language of the resource }
    spAllergyIntolerance_Date, {@enum.value spAllergyIntolerance_Date Recorded date/time. }
    spAllergyIntolerance_Recorder, {@enum.value spAllergyIntolerance_Recorder Who recorded the sensitivity }
    spAllergyIntolerance_Status, {@enum.value spAllergyIntolerance_Status The status of the sensitivity }
    spAllergyIntolerance_Subject, {@enum.value spAllergyIntolerance_Subject The subject that the sensitivity is about }
    spAllergyIntolerance_Substance, {@enum.value spAllergyIntolerance_Substance The name or code of the substance that produces the sensitivity }
    spAllergyIntolerance_Type); {@enum.value spAllergyIntolerance_Type The type of sensitivity }

  {@Enum TSearchParamsAppointment
    Search Parameters for Appointment
  }
  TSearchParamsAppointment = (
    spAppointment__id, {@enum.value spAppointment__id The logical resource id associated with the resource (must be supported by all servers) }
    spAppointment__language, {@enum.value spAppointment__language The stated language of the resource }
    spAppointment_Date, {@enum.value spAppointment_Date Appointment date/time. }
    spAppointment_Individual, {@enum.value spAppointment_Individual Any one of the individuals participating in the appointment }
    spAppointment_Partstatus, {@enum.value spAppointment_Partstatus The Participation status of the subject, or other participant on the appointment }
    spAppointment_Status); {@enum.value spAppointment_Status The overall status of the appointment }

  {@Enum TSearchParamsAppointmentResponse
    Search Parameters for AppointmentResponse
  }
  TSearchParamsAppointmentResponse = (
    spAppointmentResponse__id, {@enum.value spAppointmentResponse__id The logical resource id associated with the resource (must be supported by all servers) }
    spAppointmentResponse__language, {@enum.value spAppointmentResponse__language The stated language of the resource }
    spAppointmentResponse_Appointment, {@enum.value spAppointmentResponse_Appointment The appointment that the response is attached to }
    spAppointmentResponse_Partstatus, {@enum.value spAppointmentResponse_Partstatus The overall status of the appointment }
    spAppointmentResponse_Subject); {@enum.value spAppointmentResponse_Subject The subject that the appointment response replies for }

  {@Enum TSearchParamsAvailability
    Search Parameters for Availability
  }
  TSearchParamsAvailability = (
    spAvailability__id, {@enum.value spAvailability__id The logical resource id associated with the resource (must be supported by all servers) }
    spAvailability__language, {@enum.value spAvailability__language The stated language of the resource }
    spAvailability_Actor, {@enum.value spAvailability_Actor The individual(HealthcareService, Practitioner, Location, ...) to find an availability for }
    spAvailability_Date, {@enum.value spAvailability_Date Search for availability resources that have a period that contains this date specified }
    spAvailability_Type); {@enum.value spAvailability_Type The type of appointments that can be booked into associated slot(s) }

  {@Enum TSearchParamsCarePlan
    Search Parameters for CarePlan
  }
  TSearchParamsCarePlan = (
    spCarePlan__id, {@enum.value spCarePlan__id The logical resource id associated with the resource (must be supported by all servers) }
    spCarePlan__language, {@enum.value spCarePlan__language The stated language of the resource }
    spCarePlan_Activitycode, {@enum.value spCarePlan_Activitycode Detail type of activity }
    spCarePlan_Activitydate, {@enum.value spCarePlan_Activitydate Specified date occurs within period specified by CarePlan.activity.timingSchedule }
    spCarePlan_Activitydetail, {@enum.value spCarePlan_Activitydetail Activity details defined in specific resource }
    spCarePlan_Condition, {@enum.value spCarePlan_Condition Health issues this plan addresses }
    spCarePlan_Date, {@enum.value spCarePlan_Date Time period plan covers }
    spCarePlan_Participant, {@enum.value spCarePlan_Participant Who is involved }
    spCarePlan_Patient); {@enum.value spCarePlan_Patient Who care plan is for }

  {@Enum TSearchParamsComposition
    Search Parameters for Composition
  }
  TSearchParamsComposition = (
    spComposition__id, {@enum.value spComposition__id The logical resource id associated with the resource (must be supported by all servers) }
    spComposition__language, {@enum.value spComposition__language The stated language of the resource }
    spComposition_Attester, {@enum.value spComposition_Attester Who attested the composition }
    spComposition_Author, {@enum.value spComposition_Author Who and/or what authored the composition }
    spComposition_Class, {@enum.value spComposition_Class Categorization of Composition }
    spComposition_Context, {@enum.value spComposition_Context Code(s) that apply to the event being documented }
    spComposition_Date, {@enum.value spComposition_Date Composition editing time }
    spComposition_Identifier, {@enum.value spComposition_Identifier Logical identifier of composition (version-independent) }
    spComposition_Section_content, {@enum.value spComposition_Section_content The actual data for the section }
    spComposition_Section_type, {@enum.value spComposition_Section_type Classification of section (recommended) }
    spComposition_Subject, {@enum.value spComposition_Subject Who and/or what the composition is about }
    spComposition_Type); {@enum.value spComposition_Type Kind of composition (LOINC if possible) }

  {@Enum TSearchParamsConceptMap
    Search Parameters for ConceptMap
  }
  TSearchParamsConceptMap = (
    spConceptMap__id, {@enum.value spConceptMap__id The logical resource id associated with the resource (must be supported by all servers) }
    spConceptMap__language, {@enum.value spConceptMap__language The stated language of the resource }
    spConceptMap_Date, {@enum.value spConceptMap_Date The concept map publication date }
    spConceptMap_Dependson, {@enum.value spConceptMap_Dependson Reference to element/field/valueset mapping depends on }
    spConceptMap_Description, {@enum.value spConceptMap_Description Text search in the description of the concept map }
    spConceptMap_Identifier, {@enum.value spConceptMap_Identifier The identifier of the concept map }
    spConceptMap_Name, {@enum.value spConceptMap_Name Name of the concept map }
    spConceptMap_Product, {@enum.value spConceptMap_Product Reference to element/field/valueset mapping depends on }
    spConceptMap_Publisher, {@enum.value spConceptMap_Publisher Name of the publisher of the concept map }
    spConceptMap_Source, {@enum.value spConceptMap_Source The system for any concepts mapped by this concept map }
    spConceptMap_Status, {@enum.value spConceptMap_Status Status of the concept map }
    spConceptMap_System, {@enum.value spConceptMap_System The system for any destination concepts mapped by this map }
    spConceptMap_Target, {@enum.value spConceptMap_Target Provides context to the mappings }
    spConceptMap_Version); {@enum.value spConceptMap_Version The version identifier of the concept map }

  {@Enum TSearchParamsCondition
    Search Parameters for Condition
  }
  TSearchParamsCondition = (
    spCondition__id, {@enum.value spCondition__id The logical resource id associated with the resource (must be supported by all servers) }
    spCondition__language, {@enum.value spCondition__language The stated language of the resource }
    spCondition_Asserter, {@enum.value spCondition_Asserter Person who asserts this condition }
    spCondition_Category, {@enum.value spCondition_Category The category of the condition }
    spCondition_Code, {@enum.value spCondition_Code Code for the condition }
    spCondition_Date_asserted, {@enum.value spCondition_Date_asserted When first detected/suspected/entered }
    spCondition_Encounter, {@enum.value spCondition_Encounter Encounter when condition first asserted }
    spCondition_Evidence, {@enum.value spCondition_Evidence Manifestation/symptom }
    spCondition_Location, {@enum.value spCondition_Location Location - may include laterality }
    spCondition_Onset, {@enum.value spCondition_Onset When the Condition started (if started on a date) }
    spCondition_Related_code, {@enum.value spCondition_Related_code Relationship target by means of a predefined code }
    spCondition_Related_item, {@enum.value spCondition_Related_item Relationship target resource }
    spCondition_Severity, {@enum.value spCondition_Severity The severity of the condition }
    spCondition_Stage, {@enum.value spCondition_Stage Simple summary (disease specific) }
    spCondition_Status, {@enum.value spCondition_Status The status of the condition }
    spCondition_Subject); {@enum.value spCondition_Subject Who has the condition? }

  {@Enum TSearchParamsConformance
    Search Parameters for Conformance
  }
  TSearchParamsConformance = (
    spConformance__id, {@enum.value spConformance__id The logical resource id associated with the resource (must be supported by all servers) }
    spConformance__language, {@enum.value spConformance__language The stated language of the resource }
    spConformance_Date, {@enum.value spConformance_Date The conformance statement publication date }
    spConformance_Description, {@enum.value spConformance_Description Text search in the description of the conformance statement }
    spConformance_Event, {@enum.value spConformance_Event Event code in a conformance statement }
    spConformance_Fhirversion, {@enum.value spConformance_Fhirversion The version of FHIR }
    spConformance_Format, {@enum.value spConformance_Format formats supported (xml | json | mime type) }
    spConformance_Identifier, {@enum.value spConformance_Identifier The identifier of the conformance statement }
    spConformance_Mode, {@enum.value spConformance_Mode Mode - restful (server/client) or messaging (sender/receiver) }
    spConformance_Name, {@enum.value spConformance_Name Name of the conformance statement }
    spConformance_Profile, {@enum.value spConformance_Profile A profile id invoked in a conformance statement }
    spConformance_Publisher, {@enum.value spConformance_Publisher Name of the publisher of the conformance statement }
    spConformance_Resource, {@enum.value spConformance_Resource Name of a resource mentioned in a conformance statement }
    spConformance_Security, {@enum.value spConformance_Security Information about security of implementation }
    spConformance_Software, {@enum.value spConformance_Software Part of a the name of a software application }
    spConformance_Status, {@enum.value spConformance_Status The current status of the conformance statement }
    spConformance_Supported_profile, {@enum.value spConformance_Supported_profile Profiles supported by the system }
    spConformance_Version); {@enum.value spConformance_Version The version identifier of the conformance statement }

  {@Enum TSearchParamsContraindication
    Search Parameters for Contraindication
  }
  TSearchParamsContraindication = (
    spContraindication__id, {@enum.value spContraindication__id The logical resource id associated with the resource (must be supported by all servers) }
    spContraindication__language, {@enum.value spContraindication__language The stated language of the resource }
    spContraindication_Category, {@enum.value spContraindication_Category E.g. Drug-drug, duplicate therapy, etc. }
    spContraindication_Date, {@enum.value spContraindication_Date When identified }
    spContraindication_Identifier, {@enum.value spContraindication_Identifier Unique id for the contraindication }
    spContraindication_Implicated, {@enum.value spContraindication_Implicated Problem resource }
    spContraindication_Patient); {@enum.value spContraindication_Patient Associated patient }

  {@Enum TSearchParamsDataElement
    Search Parameters for DataElement
  }
  TSearchParamsDataElement = (
    spDataElement__id, {@enum.value spDataElement__id The logical resource id associated with the resource (must be supported by all servers) }
    spDataElement__language, {@enum.value spDataElement__language The stated language of the resource }
    spDataElement_Category, {@enum.value spDataElement_Category A category assigned to the data element (server may choose to do subsumption) }
    spDataElement_Code, {@enum.value spDataElement_Code A code for the data element (server may choose to do subsumption) }
    spDataElement_Date, {@enum.value spDataElement_Date The data element publication date }
    spDataElement_Description, {@enum.value spDataElement_Description Text search in the description of the data element }
    spDataElement_Identifier, {@enum.value spDataElement_Identifier The identifier of the data element }
    spDataElement_Name, {@enum.value spDataElement_Name Name of the data element }
    spDataElement_Publisher, {@enum.value spDataElement_Publisher Name of the publisher of the data element }
    spDataElement_Status, {@enum.value spDataElement_Status The current status of the data element }
    spDataElement_Version); {@enum.value spDataElement_Version The version identifier of the data element }

  {@Enum TSearchParamsDevice
    Search Parameters for Device
  }
  TSearchParamsDevice = (
    spDevice__id, {@enum.value spDevice__id The logical resource id associated with the resource (must be supported by all servers) }
    spDevice__language, {@enum.value spDevice__language The stated language of the resource }
    spDevice_Identifier, {@enum.value spDevice_Identifier Instance id from manufacturer, owner and others }
    spDevice_Location, {@enum.value spDevice_Location A location, where the resource is found }
    spDevice_Manufacturer, {@enum.value spDevice_Manufacturer The manufacturer of the device }
    spDevice_Model, {@enum.value spDevice_Model The model of the device }
    spDevice_Organization, {@enum.value spDevice_Organization The organization responsible for the device }
    spDevice_Patient, {@enum.value spDevice_Patient Patient information, if the resource is affixed to a person }
    spDevice_Type, {@enum.value spDevice_Type The type of the device }
    spDevice_Udi); {@enum.value spDevice_Udi FDA Mandated Unique Device Identifier }

  {@Enum TSearchParamsDeviceObservationReport
    Search Parameters for DeviceObservationReport
  }
  TSearchParamsDeviceObservationReport = (
    spDeviceObservationReport__id, {@enum.value spDeviceObservationReport__id The logical resource id associated with the resource (must be supported by all servers) }
    spDeviceObservationReport__language, {@enum.value spDeviceObservationReport__language The stated language of the resource }
    spDeviceObservationReport_Channel, {@enum.value spDeviceObservationReport_Channel The channel code }
    spDeviceObservationReport_Code, {@enum.value spDeviceObservationReport_Code The compatment code }
    spDeviceObservationReport_Observation, {@enum.value spDeviceObservationReport_Observation The data for the metric }
    spDeviceObservationReport_Source, {@enum.value spDeviceObservationReport_Source Identifies/describes where the data came from }
    spDeviceObservationReport_Subject); {@enum.value spDeviceObservationReport_Subject Subject of the measurement }

  {@Enum TSearchParamsDiagnosticOrder
    Search Parameters for DiagnosticOrder
  }
  TSearchParamsDiagnosticOrder = (
    spDiagnosticOrder__id, {@enum.value spDiagnosticOrder__id The logical resource id associated with the resource (must be supported by all servers) }
    spDiagnosticOrder__language, {@enum.value spDiagnosticOrder__language The stated language of the resource }
    spDiagnosticOrder_Actor, {@enum.value spDiagnosticOrder_Actor Who recorded or did this }
    spDiagnosticOrder_Bodysite, {@enum.value spDiagnosticOrder_Bodysite Location of requested test (if applicable) }
    spDiagnosticOrder_Code, {@enum.value spDiagnosticOrder_Code Code to indicate the item (test or panel) being ordered }
    spDiagnosticOrder_Encounter, {@enum.value spDiagnosticOrder_Encounter The encounter that this diagnostic order is associated with }
    spDiagnosticOrder_Event_date, {@enum.value spDiagnosticOrder_Event_date The date at which the event happened }
    spDiagnosticOrder_Event_status, {@enum.value spDiagnosticOrder_Event_status requested | received | accepted | in progress | review | completed | suspended | rejected | failed }
    spDiagnosticOrder_Event_status_date, {@enum.value spDiagnosticOrder_Event_status_date A combination of past-status and date }
    spDiagnosticOrder_Identifier, {@enum.value spDiagnosticOrder_Identifier Identifiers assigned to this order }
    spDiagnosticOrder_Item_date, {@enum.value spDiagnosticOrder_Item_date The date at which the event happened }
    spDiagnosticOrder_Item_past_status, {@enum.value spDiagnosticOrder_Item_past_status requested | received | accepted | in progress | review | completed | suspended | rejected | failed }
    spDiagnosticOrder_Item_status, {@enum.value spDiagnosticOrder_Item_status requested | received | accepted | in progress | review | completed | suspended | rejected | failed }
    spDiagnosticOrder_Item_status_date, {@enum.value spDiagnosticOrder_Item_status_date A combination of item-past-status and item-date }
    spDiagnosticOrder_Orderer, {@enum.value spDiagnosticOrder_Orderer Who ordered the test }
    spDiagnosticOrder_Specimen, {@enum.value spDiagnosticOrder_Specimen If the whole order relates to specific specimens }
    spDiagnosticOrder_Status, {@enum.value spDiagnosticOrder_Status requested | received | accepted | in progress | review | completed | suspended | rejected | failed }
    spDiagnosticOrder_Subject); {@enum.value spDiagnosticOrder_Subject Who and/or what test is about }

  {@Enum TSearchParamsDiagnosticReport
    Search Parameters for DiagnosticReport
  }
  TSearchParamsDiagnosticReport = (
    spDiagnosticReport__id, {@enum.value spDiagnosticReport__id The logical resource id associated with the resource (must be supported by all servers) }
    spDiagnosticReport__language, {@enum.value spDiagnosticReport__language The stated language of the resource }
    spDiagnosticReport_Date, {@enum.value spDiagnosticReport_Date The clinically relevant time of the report }
    spDiagnosticReport_Diagnosis, {@enum.value spDiagnosticReport_Diagnosis A coded diagnosis on the report }
    spDiagnosticReport_Identifier, {@enum.value spDiagnosticReport_Identifier An identifier for the report }
    spDiagnosticReport_Image, {@enum.value spDiagnosticReport_Image Reference to the image source }
    spDiagnosticReport_Issued, {@enum.value spDiagnosticReport_Issued When the report was issued }
    spDiagnosticReport_Name, {@enum.value spDiagnosticReport_Name The name of the report (e.g. the code for the report as a whole, as opposed to codes for the atomic results, which are the names on the observation resource referred to from the result) }
    spDiagnosticReport_Performer, {@enum.value spDiagnosticReport_Performer Who was the source of the report (organization) }
    spDiagnosticReport_Request, {@enum.value spDiagnosticReport_Request What was requested }
    spDiagnosticReport_Result, {@enum.value spDiagnosticReport_Result Link to an atomic result (observation resource) }
    spDiagnosticReport_Service, {@enum.value spDiagnosticReport_Service Which diagnostic discipline/department created the report }
    spDiagnosticReport_Specimen, {@enum.value spDiagnosticReport_Specimen The specimen details }
    spDiagnosticReport_Status, {@enum.value spDiagnosticReport_Status The status of the report }
    spDiagnosticReport_Subject); {@enum.value spDiagnosticReport_Subject The subject of the report }

  {@Enum TSearchParamsDocumentManifest
    Search Parameters for DocumentManifest
  }
  TSearchParamsDocumentManifest = (
    spDocumentManifest__id, {@enum.value spDocumentManifest__id The logical resource id associated with the resource (must be supported by all servers) }
    spDocumentManifest__language, {@enum.value spDocumentManifest__language The stated language of the resource }
    spDocumentManifest_Author, {@enum.value spDocumentManifest_Author Who and/or what authored the document }
    spDocumentManifest_Confidentiality, {@enum.value spDocumentManifest_Confidentiality Sensitivity of set of documents }
    spDocumentManifest_Content, {@enum.value spDocumentManifest_Content Contents of this set of documents }
    spDocumentManifest_Created, {@enum.value spDocumentManifest_Created When this document manifest created }
    spDocumentManifest_Description, {@enum.value spDocumentManifest_Description Human-readable description (title) }
    spDocumentManifest_Identifier, {@enum.value spDocumentManifest_Identifier Unique Identifier for the set of documents }
    spDocumentManifest_Recipient, {@enum.value spDocumentManifest_Recipient Intended to get notified about this set of documents }
    spDocumentManifest_Status, {@enum.value spDocumentManifest_Status current | superceded | entered in error }
    spDocumentManifest_Subject, {@enum.value spDocumentManifest_Subject The subject of the set of documents }
    spDocumentManifest_Supersedes, {@enum.value spDocumentManifest_Supersedes If this document manifest replaces another }
    spDocumentManifest_Type); {@enum.value spDocumentManifest_Type What kind of document set this is }

  {@Enum TSearchParamsDocumentReference
    Search Parameters for DocumentReference
  }
  TSearchParamsDocumentReference = (
    spDocumentReference__id, {@enum.value spDocumentReference__id The logical resource id associated with the resource (must be supported by all servers) }
    spDocumentReference__language, {@enum.value spDocumentReference__language The stated language of the resource }
    spDocumentReference_Authenticator, {@enum.value spDocumentReference_Authenticator Who/What authenticated the document }
    spDocumentReference_Author, {@enum.value spDocumentReference_Author Who and/or what authored the document }
    spDocumentReference_Class, {@enum.value spDocumentReference_Class Categorization of Document }
    spDocumentReference_Confidentiality, {@enum.value spDocumentReference_Confidentiality Sensitivity of source document }
    spDocumentReference_Created, {@enum.value spDocumentReference_Created Document creation time }
    spDocumentReference_Custodian, {@enum.value spDocumentReference_Custodian Org which maintains the document }
    spDocumentReference_Description, {@enum.value spDocumentReference_Description Human-readable description (title) }
    spDocumentReference_Event, {@enum.value spDocumentReference_Event Main Clinical Acts Documented }
    spDocumentReference_Facility, {@enum.value spDocumentReference_Facility Kind of facility where patient was seen }
    spDocumentReference_Format, {@enum.value spDocumentReference_Format Format/content rules for the document }
    spDocumentReference_Identifier, {@enum.value spDocumentReference_Identifier Master Version Specific Identifier }
    spDocumentReference_Indexed, {@enum.value spDocumentReference_Indexed When this document reference created }
    spDocumentReference_Language, {@enum.value spDocumentReference_Language The marked primary language for the document }
    spDocumentReference_Location, {@enum.value spDocumentReference_Location Where to access the document }
    spDocumentReference_Period, {@enum.value spDocumentReference_Period Time of service that is being documented }
    spDocumentReference_Relatesto, {@enum.value spDocumentReference_Relatesto Target of the relationship }
    spDocumentReference_Relation, {@enum.value spDocumentReference_Relation replaces | transforms | signs | appends }
    spDocumentReference_Relationship, {@enum.value spDocumentReference_Relationship Combination of relation and relatesTo }
    spDocumentReference_Size, {@enum.value spDocumentReference_Size Size of the document in bytes }
    spDocumentReference_Status, {@enum.value spDocumentReference_Status current | superceded | entered in error }
    spDocumentReference_Subject, {@enum.value spDocumentReference_Subject Who|what is the subject of the document }
    spDocumentReference_Type); {@enum.value spDocumentReference_Type What kind of document this is (LOINC if possible) }

  {@Enum TSearchParamsEncounter
    Search Parameters for Encounter
  }
  TSearchParamsEncounter = (
    spEncounter__id, {@enum.value spEncounter__id The logical resource id associated with the resource (must be supported by all servers) }
    spEncounter__language, {@enum.value spEncounter__language The stated language of the resource }
    spEncounter_Date, {@enum.value spEncounter_Date A date within the period the Encounter lasted }
    spEncounter_Identifier, {@enum.value spEncounter_Identifier Identifier(s) by which this encounter is known }
    spEncounter_Indication, {@enum.value spEncounter_Indication Reason the encounter takes place (resource) }
    spEncounter_Length, {@enum.value spEncounter_Length Length of encounter in days }
    spEncounter_Location, {@enum.value spEncounter_Location Location the encounter takes place }
    spEncounter_Location_period, {@enum.value spEncounter_Location_period Time period during which the patient was present at the location }
    spEncounter_Status, {@enum.value spEncounter_Status planned | in progress | onleave | finished | cancelled }
    spEncounter_Subject); {@enum.value spEncounter_Subject The patient present at the encounter }

  {@Enum TSearchParamsFamilyHistory
    Search Parameters for FamilyHistory
  }
  TSearchParamsFamilyHistory = (
    spFamilyHistory__id, {@enum.value spFamilyHistory__id The logical resource id associated with the resource (must be supported by all servers) }
    spFamilyHistory__language, {@enum.value spFamilyHistory__language The stated language of the resource }
    spFamilyHistory_Date, {@enum.value spFamilyHistory_Date When history was captured/updated }
    spFamilyHistory_Subject); {@enum.value spFamilyHistory_Subject The identity of a subject to list family history items for }

  {@Enum TSearchParamsGroup
    Search Parameters for Group
  }
  TSearchParamsGroup = (
    spGroup__id, {@enum.value spGroup__id The logical resource id associated with the resource (must be supported by all servers) }
    spGroup__language, {@enum.value spGroup__language The stated language of the resource }
    spGroup_Actual, {@enum.value spGroup_Actual Descriptive or actual }
    spGroup_Characteristic, {@enum.value spGroup_Characteristic Kind of characteristic }
    spGroup_Characteristic_value, {@enum.value spGroup_Characteristic_value A composite of both characteristic and value }
    spGroup_Code, {@enum.value spGroup_Code The kind of resources contained }
    spGroup_Exclude, {@enum.value spGroup_Exclude Group includes or excludes }
    spGroup_Identifier, {@enum.value spGroup_Identifier Unique id }
    spGroup_Member, {@enum.value spGroup_Member Who is in group }
    spGroup_Type, {@enum.value spGroup_Type The type of resources the group contains }
    spGroup_Value); {@enum.value spGroup_Value Value held by characteristic }

  {@Enum TSearchParamsImagingStudy
    Search Parameters for ImagingStudy
  }
  TSearchParamsImagingStudy = (
    spImagingStudy__id, {@enum.value spImagingStudy__id The logical resource id associated with the resource (must be supported by all servers) }
    spImagingStudy__language, {@enum.value spImagingStudy__language The stated language of the resource }
    spImagingStudy_Accession, {@enum.value spImagingStudy_Accession The accession id for the image }
    spImagingStudy_Bodysite, {@enum.value spImagingStudy_Bodysite Body part examined (Map from 0018,0015) }
    spImagingStudy_Date, {@enum.value spImagingStudy_Date The date the study was done was taken }
    spImagingStudy_Dicom_class, {@enum.value spImagingStudy_Dicom_class DICOM class type (0008,0016) }
    spImagingStudy_Modality, {@enum.value spImagingStudy_Modality The modality of the image }
    spImagingStudy_Series, {@enum.value spImagingStudy_Series The series id for the image }
    spImagingStudy_Size, {@enum.value spImagingStudy_Size The size of the image in MB - may include > or < in the value }
    spImagingStudy_Study, {@enum.value spImagingStudy_Study The study id for the image }
    spImagingStudy_Subject, {@enum.value spImagingStudy_Subject Who the study is about }
    spImagingStudy_Uid); {@enum.value spImagingStudy_Uid Formal identifier for this instance (0008,0018) }

  {@Enum TSearchParamsImmunization
    Search Parameters for Immunization
  }
  TSearchParamsImmunization = (
    spImmunization__id, {@enum.value spImmunization__id The logical resource id associated with the resource (must be supported by all servers) }
    spImmunization__language, {@enum.value spImmunization__language The stated language of the resource }
    spImmunization_Date, {@enum.value spImmunization_Date Vaccination  Administration / Refusal Date }
    spImmunization_Dose_sequence, {@enum.value spImmunization_Dose_sequence What dose number within series? }
    spImmunization_Identifier, {@enum.value spImmunization_Identifier Business identifier }
    spImmunization_Location, {@enum.value spImmunization_Location The service delivery location or facility in which the vaccine was / was to be administered }
    spImmunization_Lot_number, {@enum.value spImmunization_Lot_number Vaccine Lot Number }
    spImmunization_Manufacturer, {@enum.value spImmunization_Manufacturer Vaccine Manufacturer }
    spImmunization_Performer, {@enum.value spImmunization_Performer The practitioner who administered the vaccination }
    spImmunization_Reaction, {@enum.value spImmunization_Reaction Additional information on reaction }
    spImmunization_Reaction_date, {@enum.value spImmunization_Reaction_date When did reaction start? }
    spImmunization_Reason, {@enum.value spImmunization_Reason Why immunization occurred }
    spImmunization_Refusal_reason, {@enum.value spImmunization_Refusal_reason Explanation of refusal / exemption }
    spImmunization_Refused, {@enum.value spImmunization_Refused Was immunization refused? }
    spImmunization_Requester, {@enum.value spImmunization_Requester The practitioner who ordered the vaccination }
    spImmunization_Subject, {@enum.value spImmunization_Subject The subject of the vaccination event / refusal }
    spImmunization_Vaccine_type); {@enum.value spImmunization_Vaccine_type Vaccine Product Type Administered }

  {@Enum TSearchParamsImmunizationRecommendation
    Search Parameters for ImmunizationRecommendation
  }
  TSearchParamsImmunizationRecommendation = (
    spImmunizationRecommendation__id, {@enum.value spImmunizationRecommendation__id The logical resource id associated with the resource (must be supported by all servers) }
    spImmunizationRecommendation__language, {@enum.value spImmunizationRecommendation__language The stated language of the resource }
    spImmunizationRecommendation_Date, {@enum.value spImmunizationRecommendation_Date Date recommendation created }
    spImmunizationRecommendation_Dose_number, {@enum.value spImmunizationRecommendation_Dose_number Recommended dose number }
    spImmunizationRecommendation_Dose_sequence, {@enum.value spImmunizationRecommendation_Dose_sequence Number of dose within sequence }
    spImmunizationRecommendation_Identifier, {@enum.value spImmunizationRecommendation_Identifier Business identifier }
    spImmunizationRecommendation_Information, {@enum.value spImmunizationRecommendation_Information Patient observations supporting recommendation }
    spImmunizationRecommendation_Status, {@enum.value spImmunizationRecommendation_Status Vaccine administration status }
    spImmunizationRecommendation_Subject, {@enum.value spImmunizationRecommendation_Subject Who this profile is for }
    spImmunizationRecommendation_Support, {@enum.value spImmunizationRecommendation_Support Past immunizations supporting recommendation }
    spImmunizationRecommendation_Vaccine_type); {@enum.value spImmunizationRecommendation_Vaccine_type Vaccine recommendation applies to }

  {@Enum TSearchParamsList
    Search Parameters for List
  }
  TSearchParamsList = (
    spList__id, {@enum.value spList__id The logical resource id associated with the resource (must be supported by all servers) }
    spList__language, {@enum.value spList__language The stated language of the resource }
    spList_Code, {@enum.value spList_Code What the purpose of this list is }
    spList_Date, {@enum.value spList_Date When the list was prepared }
    spList_Empty_reason, {@enum.value spList_Empty_reason Why list is empty }
    spList_Item, {@enum.value spList_Item Actual entry }
    spList_Source, {@enum.value spList_Source Who and/or what defined the list contents }
    spList_Subject); {@enum.value spList_Subject If all resources have the same subject }

  {@Enum TSearchParamsLocation
    Search Parameters for Location
  }
  TSearchParamsLocation = (
    spLocation__id, {@enum.value spLocation__id The logical resource id associated with the resource (must be supported by all servers) }
    spLocation__language, {@enum.value spLocation__language The stated language of the resource }
    spLocation_Address, {@enum.value spLocation_Address A (part of the) address of the location }
    spLocation_Identifier, {@enum.value spLocation_Identifier Unique code or number identifying the location to its users }
    spLocation_Name, {@enum.value spLocation_Name A (portion of the) name of the location }
    spLocation_Near, {@enum.value spLocation_Near The coordinates expressed as [lat],[long] (using KML, see notes) to find locations near to (servers may search using a square rather than a circle for efficiency) }
    spLocation_Near_distance, {@enum.value spLocation_Near_distance A distance quantity to limit the near search to locations within a specific distance }
    spLocation_Partof, {@enum.value spLocation_Partof The location of which this location is a part }
    spLocation_Status, {@enum.value spLocation_Status Searches for locations with a specific kind of status }
    spLocation_Type); {@enum.value spLocation_Type A code for the type of location }

  {@Enum TSearchParamsMedia
    Search Parameters for Media
  }
  TSearchParamsMedia = (
    spMedia__id, {@enum.value spMedia__id The logical resource id associated with the resource (must be supported by all servers) }
    spMedia__language, {@enum.value spMedia__language The stated language of the resource }
    spMedia_Date, {@enum.value spMedia_Date When the media was taken/recorded (end) }
    spMedia_Identifier, {@enum.value spMedia_Identifier Identifier(s) for the image }
    spMedia_Operator, {@enum.value spMedia_Operator The person who generated the image }
    spMedia_Subject, {@enum.value spMedia_Subject Who/What this Media is a record of }
    spMedia_Subtype, {@enum.value spMedia_Subtype The type of acquisition equipment/process }
    spMedia_Type, {@enum.value spMedia_Type photo | video | audio }
    spMedia_View); {@enum.value spMedia_View Imaging view e.g Lateral or Antero-posterior }

  {@Enum TSearchParamsMedication
    Search Parameters for Medication
  }
  TSearchParamsMedication = (
    spMedication__id, {@enum.value spMedication__id The logical resource id associated with the resource (must be supported by all servers) }
    spMedication__language, {@enum.value spMedication__language The stated language of the resource }
    spMedication_Code, {@enum.value spMedication_Code Codes that identify this medication }
    spMedication_Container, {@enum.value spMedication_Container E.g. box, vial, blister-pack }
    spMedication_Content, {@enum.value spMedication_Content A product in the package }
    spMedication_Form, {@enum.value spMedication_Form powder | tablets | carton + }
    spMedication_Ingredient, {@enum.value spMedication_Ingredient The product contained }
    spMedication_Manufacturer, {@enum.value spMedication_Manufacturer Manufacturer of the item }
    spMedication_Name); {@enum.value spMedication_Name Common / Commercial name }

  {@Enum TSearchParamsMedicationAdministration
    Search Parameters for MedicationAdministration
  }
  TSearchParamsMedicationAdministration = (
    spMedicationAdministration__id, {@enum.value spMedicationAdministration__id The logical resource id associated with the resource (must be supported by all servers) }
    spMedicationAdministration__language, {@enum.value spMedicationAdministration__language The stated language of the resource }
    spMedicationAdministration_Device, {@enum.value spMedicationAdministration_Device Return administrations with this administration device identity }
    spMedicationAdministration_Encounter, {@enum.value spMedicationAdministration_Encounter Return administrations that share this encounter }
    spMedicationAdministration_Identifier, {@enum.value spMedicationAdministration_Identifier Return administrations with this external identity }
    spMedicationAdministration_Medication, {@enum.value spMedicationAdministration_Medication Return administrations of this medication }
    spMedicationAdministration_Notgiven, {@enum.value spMedicationAdministration_Notgiven Administrations that were not made }
    spMedicationAdministration_Patient, {@enum.value spMedicationAdministration_Patient The identity of a patient to list administrations  for }
    spMedicationAdministration_Prescription, {@enum.value spMedicationAdministration_Prescription The identity of a prescription to list administrations from }
    spMedicationAdministration_Status, {@enum.value spMedicationAdministration_Status MedicationAdministration event status (for example one of active/paused/completed/nullified) }
    spMedicationAdministration_Whengiven); {@enum.value spMedicationAdministration_Whengiven Date of administration }

  {@Enum TSearchParamsMedicationDispense
    Search Parameters for MedicationDispense
  }
  TSearchParamsMedicationDispense = (
    spMedicationDispense__id, {@enum.value spMedicationDispense__id The logical resource id associated with the resource (must be supported by all servers) }
    spMedicationDispense__language, {@enum.value spMedicationDispense__language The stated language of the resource }
    spMedicationDispense_Destination, {@enum.value spMedicationDispense_Destination Return dispenses that should be sent to a secific destination }
    spMedicationDispense_Dispenser, {@enum.value spMedicationDispense_Dispenser Return all dispenses performed by a specific indiividual }
    spMedicationDispense_Identifier, {@enum.value spMedicationDispense_Identifier Return dispenses with this external identity }
    spMedicationDispense_Medication, {@enum.value spMedicationDispense_Medication Returns dispenses of this medicine }
    spMedicationDispense_Patient, {@enum.value spMedicationDispense_Patient The identity of a patient to list dispenses  for }
    spMedicationDispense_Prescription, {@enum.value spMedicationDispense_Prescription The identity of a prescription to list dispenses from }
    spMedicationDispense_Responsibleparty, {@enum.value spMedicationDispense_Responsibleparty Return all dispenses with the specified responsible party }
    spMedicationDispense_Status, {@enum.value spMedicationDispense_Status Status of the dispense }
    spMedicationDispense_Type, {@enum.value spMedicationDispense_Type Return all dispenses of a specific type }
    spMedicationDispense_Whenhandedover, {@enum.value spMedicationDispense_Whenhandedover Date when medication handed over to patient (outpatient setting), or supplied to ward or clinic (inpatient setting) }
    spMedicationDispense_Whenprepared); {@enum.value spMedicationDispense_Whenprepared Date when medication prepared }

  {@Enum TSearchParamsMedicationPrescription
    Search Parameters for MedicationPrescription
  }
  TSearchParamsMedicationPrescription = (
    spMedicationPrescription__id, {@enum.value spMedicationPrescription__id The logical resource id associated with the resource (must be supported by all servers) }
    spMedicationPrescription__language, {@enum.value spMedicationPrescription__language The stated language of the resource }
    spMedicationPrescription_Datewritten, {@enum.value spMedicationPrescription_Datewritten Return prescriptions written on this date }
    spMedicationPrescription_Encounter, {@enum.value spMedicationPrescription_Encounter Return prescriptions with this encounter identity }
    spMedicationPrescription_Identifier, {@enum.value spMedicationPrescription_Identifier Return prescriptions with this external identity }
    spMedicationPrescription_Medication, {@enum.value spMedicationPrescription_Medication Code for medicine or text in medicine name }
    spMedicationPrescription_Patient, {@enum.value spMedicationPrescription_Patient The identity of a patient to list dispenses  for }
    spMedicationPrescription_Status); {@enum.value spMedicationPrescription_Status Status of the prescription }

  {@Enum TSearchParamsMedicationStatement
    Search Parameters for MedicationStatement
  }
  TSearchParamsMedicationStatement = (
    spMedicationStatement__id, {@enum.value spMedicationStatement__id The logical resource id associated with the resource (must be supported by all servers) }
    spMedicationStatement__language, {@enum.value spMedicationStatement__language The stated language of the resource }
    spMedicationStatement_Device, {@enum.value spMedicationStatement_Device Return administrations with this administration device identity }
    spMedicationStatement_Identifier, {@enum.value spMedicationStatement_Identifier Return administrations with this external identity }
    spMedicationStatement_Medication, {@enum.value spMedicationStatement_Medication Code for medicine or text in medicine name }
    spMedicationStatement_Patient, {@enum.value spMedicationStatement_Patient The identity of a patient to list administrations  for }
    spMedicationStatement_When_given); {@enum.value spMedicationStatement_When_given Date of administration }

  {@Enum TSearchParamsMessageHeader
    Search Parameters for MessageHeader
  }
  TSearchParamsMessageHeader = (
    spMessageHeader__id, {@enum.value spMessageHeader__id The logical resource id associated with the resource (must be supported by all servers) }
    spMessageHeader__language); {@enum.value spMessageHeader__language The stated language of the resource }

  {@Enum TSearchParamsNamespace
    Search Parameters for Namespace
  }
  TSearchParamsNamespace = (
    spNamespace__id, {@enum.value spNamespace__id The logical resource id associated with the resource (must be supported by all servers) }
    spNamespace__language); {@enum.value spNamespace__language The stated language of the resource }

  {@Enum TSearchParamsObservation
    Search Parameters for Observation
  }
  TSearchParamsObservation = (
    spObservation__id, {@enum.value spObservation__id The logical resource id associated with the resource (must be supported by all servers) }
    spObservation__language, {@enum.value spObservation__language The stated language of the resource }
    spObservation_Date, {@enum.value spObservation_Date Obtained date/time. If the obtained element is a period, a date that falls in the period }
    spObservation_Encounter, {@enum.value spObservation_Encounter Healthcare event related to the observation }
    spObservation_Name, {@enum.value spObservation_Name The name of the observation type }
    spObservation_Name_value_x, {@enum.value spObservation_Name_value_[x] Both name and one of the value parameters }
    spObservation_Performer, {@enum.value spObservation_Performer Who and/or what performed the observation }
    spObservation_Related, {@enum.value spObservation_Related Related Observations - search on related-type and related-target together }
    spObservation_Related_target, {@enum.value spObservation_Related_target Observation that is related to this one }
    spObservation_Related_type, {@enum.value spObservation_Related_type has-component | has-member | derived-from | sequel-to | replaces | qualified-by | interfered-by }
    spObservation_Reliability, {@enum.value spObservation_Reliability The reliability of the observation }
    spObservation_Specimen, {@enum.value spObservation_Specimen Specimen used for this observation }
    spObservation_Status, {@enum.value spObservation_Status The status of the observation }
    spObservation_Subject, {@enum.value spObservation_Subject The subject that the observation is about }
    spObservation_Value_concept, {@enum.value spObservation_Value_concept The value of the observation, if the value is a CodeableConcept }
    spObservation_Value_date, {@enum.value spObservation_Value_date The value of the observation, if the value is a Period }
    spObservation_Value_quantity, {@enum.value spObservation_Value_quantity The value of the observation, if the value is a Quantity, or a SampledData (just search on the bounds of the values in sampled data) }
    spObservation_Value_string); {@enum.value spObservation_Value_string The value of the observation, if the value is a string, and also searches in CodeableConcept.text }

  {@Enum TSearchParamsOperationDefinition
    Search Parameters for OperationDefinition
  }
  TSearchParamsOperationDefinition = (
    spOperationDefinition__id, {@enum.value spOperationDefinition__id The logical resource id associated with the resource (must be supported by all servers) }
    spOperationDefinition__language, {@enum.value spOperationDefinition__language The stated language of the resource }
    spOperationDefinition_Base, {@enum.value spOperationDefinition_Base Marks this as a profile of the base }
    spOperationDefinition_Code, {@enum.value spOperationDefinition_Code Assist with indexing and finding }
    spOperationDefinition_Date, {@enum.value spOperationDefinition_Date Date for this version of the operation definition }
    spOperationDefinition_Identifier, {@enum.value spOperationDefinition_Identifier Logical id to reference this operation definition }
    spOperationDefinition_Instance, {@enum.value spOperationDefinition_Instance Invoke on an instance? }
    spOperationDefinition_Kind, {@enum.value spOperationDefinition_Kind operation | query }
    spOperationDefinition_Name, {@enum.value spOperationDefinition_Name Name used to invoke the operation }
    spOperationDefinition_Profile, {@enum.value spOperationDefinition_Profile Profile on the type }
    spOperationDefinition_Publisher, {@enum.value spOperationDefinition_Publisher Name of the publisher (Organization or individual) }
    spOperationDefinition_Status, {@enum.value spOperationDefinition_Status draft | active | retired }
    spOperationDefinition_System, {@enum.value spOperationDefinition_System Invoke at the system level? }
    spOperationDefinition_Title, {@enum.value spOperationDefinition_Title Informal name for this profile }
    spOperationDefinition_Type, {@enum.value spOperationDefinition_Type Invoke at resource level for these type }
    spOperationDefinition_Version); {@enum.value spOperationDefinition_Version Logical id for this version of the operation definition }

  {@Enum TSearchParamsOperationOutcome
    Search Parameters for OperationOutcome
  }
  TSearchParamsOperationOutcome = (
    spOperationOutcome__id, {@enum.value spOperationOutcome__id The logical resource id associated with the resource (must be supported by all servers) }
    spOperationOutcome__language); {@enum.value spOperationOutcome__language The stated language of the resource }

  {@Enum TSearchParamsOrder
    Search Parameters for Order
  }
  TSearchParamsOrder = (
    spOrder__id, {@enum.value spOrder__id The logical resource id associated with the resource (must be supported by all servers) }
    spOrder__language, {@enum.value spOrder__language The stated language of the resource }
    spOrder_Authority, {@enum.value spOrder_Authority If required by policy }
    spOrder_Date, {@enum.value spOrder_Date When the order was made }
    spOrder_Detail, {@enum.value spOrder_Detail What action is being ordered }
    spOrder_Source, {@enum.value spOrder_Source Who initiated the order }
    spOrder_Subject, {@enum.value spOrder_Subject Patient this order is about }
    spOrder_Target, {@enum.value spOrder_Target Who is intended to fulfill the order }
    spOrder_When, {@enum.value spOrder_When A formal schedule }
    spOrder_When_code); {@enum.value spOrder_When_code Code specifies when request should be done. The code may simply be a priority code }

  {@Enum TSearchParamsOrderResponse
    Search Parameters for OrderResponse
  }
  TSearchParamsOrderResponse = (
    spOrderResponse__id, {@enum.value spOrderResponse__id The logical resource id associated with the resource (must be supported by all servers) }
    spOrderResponse__language, {@enum.value spOrderResponse__language The stated language of the resource }
    spOrderResponse_Code, {@enum.value spOrderResponse_Code pending | review | rejected | error | accepted | cancelled | replaced | aborted | complete }
    spOrderResponse_Date, {@enum.value spOrderResponse_Date When the response was made }
    spOrderResponse_Fulfillment, {@enum.value spOrderResponse_Fulfillment Details of the outcome of performing the order }
    spOrderResponse_Request, {@enum.value spOrderResponse_Request The order that this is a response to }
    spOrderResponse_Who); {@enum.value spOrderResponse_Who Who made the response }

  {@Enum TSearchParamsOrganization
    Search Parameters for Organization
  }
  TSearchParamsOrganization = (
    spOrganization__id, {@enum.value spOrganization__id The logical resource id associated with the resource (must be supported by all servers) }
    spOrganization__language, {@enum.value spOrganization__language The stated language of the resource }
    spOrganization_Active, {@enum.value spOrganization_Active Whether the organization's record is active }
    spOrganization_Identifier, {@enum.value spOrganization_Identifier Any identifier for the organization (not the accreditation issuer's identifier) }
    spOrganization_Name, {@enum.value spOrganization_Name A portion of the organization's name }
    spOrganization_Partof, {@enum.value spOrganization_Partof Search all organizations that are part of the given organization }
    spOrganization_Phonetic, {@enum.value spOrganization_Phonetic A portion of the organization's name using some kind of phonetic matching algorithm }
    spOrganization_Type); {@enum.value spOrganization_Type A code for the type of organization }

  {@Enum TSearchParamsOther
    Search Parameters for Other
  }
  TSearchParamsOther = (
    spOther__id, {@enum.value spOther__id The logical resource id associated with the resource (must be supported by all servers) }
    spOther__language, {@enum.value spOther__language The stated language of the resource }
    spOther_Code, {@enum.value spOther_Code Kind of Resource }
    spOther_Created, {@enum.value spOther_Created When created }
    spOther_Subject); {@enum.value spOther_Subject Identifies the }

  {@Enum TSearchParamsPatient
    Search Parameters for Patient
  }
  TSearchParamsPatient = (
    spPatient__id, {@enum.value spPatient__id The logical resource id associated with the resource (must be supported by all servers) }
    spPatient__language, {@enum.value spPatient__language The stated language of the resource }
    spPatient_Active, {@enum.value spPatient_Active Whether the patient record is active }
    spPatient_Address, {@enum.value spPatient_Address An address in any kind of address/part of the patient }
    spPatient_Animal_breed, {@enum.value spPatient_Animal_breed The breed for animal patients }
    spPatient_Animal_species, {@enum.value spPatient_Animal_species The species for animal patients }
    spPatient_Birthdate, {@enum.value spPatient_Birthdate The patient's date of birth }
    spPatient_Family, {@enum.value spPatient_Family A portion of the family name of the patient }
    spPatient_Gender, {@enum.value spPatient_Gender Gender of the patient }
    spPatient_Given, {@enum.value spPatient_Given A portion of the given name of the patient }
    spPatient_Identifier, {@enum.value spPatient_Identifier A patient identifier }
    spPatient_Language, {@enum.value spPatient_Language Language code (irrespective of use value) }
    spPatient_Link, {@enum.value spPatient_Link All patients linked to the given patient }
    spPatient_Name, {@enum.value spPatient_Name A portion of either family or given name of the patient }
    spPatient_Phonetic, {@enum.value spPatient_Phonetic A portion of either family or given name using some kind of phonetic matching algorithm }
    spPatient_Provider, {@enum.value spPatient_Provider The organization at which this person is a patient }
    spPatient_Telecom); {@enum.value spPatient_Telecom The value in any kind of telecom details of the patient }

  {@Enum TSearchParamsPractitioner
    Search Parameters for Practitioner
  }
  TSearchParamsPractitioner = (
    spPractitioner__id, {@enum.value spPractitioner__id The logical resource id associated with the resource (must be supported by all servers) }
    spPractitioner__language, {@enum.value spPractitioner__language The stated language of the resource }
    spPractitioner_Address, {@enum.value spPractitioner_Address An address in any kind of address/part }
    spPractitioner_Communication, {@enum.value spPractitioner_Communication One of the languages that the practitioner can communicate with }
    spPractitioner_Family, {@enum.value spPractitioner_Family A portion of the family name }
    spPractitioner_Gender, {@enum.value spPractitioner_Gender Gender of the practitioner }
    spPractitioner_Given, {@enum.value spPractitioner_Given A portion of the given name }
    spPractitioner_Identifier, {@enum.value spPractitioner_Identifier A practitioner's Identifier }
    spPractitioner_Name, {@enum.value spPractitioner_Name A portion of either family or given name }
    spPractitioner_Organization, {@enum.value spPractitioner_Organization The identity of the organization the practitioner represents / acts on behalf of }
    spPractitioner_Phonetic, {@enum.value spPractitioner_Phonetic A portion of either family or given name using some kind of phonetic matching algorithm }
    spPractitioner_Telecom); {@enum.value spPractitioner_Telecom The value in any kind of contact }

  {@Enum TSearchParamsProcedure
    Search Parameters for Procedure
  }
  TSearchParamsProcedure = (
    spProcedure__id, {@enum.value spProcedure__id The logical resource id associated with the resource (must be supported by all servers) }
    spProcedure__language, {@enum.value spProcedure__language The stated language of the resource }
    spProcedure_Date, {@enum.value spProcedure_Date The date the procedure was performed on }
    spProcedure_Subject, {@enum.value spProcedure_Subject The identity of a patient to list procedures  for }
    spProcedure_Type); {@enum.value spProcedure_Type Type of procedure }

  {@Enum TSearchParamsProfile
    Search Parameters for Profile
  }
  TSearchParamsProfile = (
    spProfile__id, {@enum.value spProfile__id The logical resource id associated with the resource (must be supported by all servers) }
    spProfile__language, {@enum.value spProfile__language The stated language of the resource }
    spProfile_Code, {@enum.value spProfile_Code A code for the profile in the format uri::code (server may choose to do subsumption) }
    spProfile_Date, {@enum.value spProfile_Date The profile publication date }
    spProfile_Description, {@enum.value spProfile_Description Text search in the description of the profile }
    spProfile_Extension, {@enum.value spProfile_Extension An extension code (use or definition) }
    spProfile_Identifier, {@enum.value spProfile_Identifier The identifier of the profile }
    spProfile_Name, {@enum.value spProfile_Name Name of the profile }
    spProfile_Publisher, {@enum.value spProfile_Publisher Name of the publisher of the profile }
    spProfile_Status, {@enum.value spProfile_Status The current status of the profile }
    spProfile_Type, {@enum.value spProfile_Type Type of resource that is constrained in the profile }
    spProfile_Url, {@enum.value spProfile_Url Literal URL used to reference this profile }
    spProfile_Valueset, {@enum.value spProfile_Valueset A vocabulary binding code }
    spProfile_Version); {@enum.value spProfile_Version The version identifier of the profile }

  {@Enum TSearchParamsProvenance
    Search Parameters for Provenance
  }
  TSearchParamsProvenance = (
    spProvenance__id, {@enum.value spProvenance__id The logical resource id associated with the resource (must be supported by all servers) }
    spProvenance__language, {@enum.value spProvenance__language The stated language of the resource }
    spProvenance_End, {@enum.value spProvenance_End End time with inclusive boundary, if not ongoing }
    spProvenance_Location, {@enum.value spProvenance_Location Where the activity occurred, if relevant }
    spProvenance_Party, {@enum.value spProvenance_Party Identity of agent (urn or url) }
    spProvenance_Partytype, {@enum.value spProvenance_Partytype e.g. Resource | Person | Application | Record | Document + }
    spProvenance_Start, {@enum.value spProvenance_Start Starting time with inclusive boundary }
    spProvenance_Target); {@enum.value spProvenance_Target Target resource(s) (usually version specific) }

  {@Enum TSearchParamsQuery
    Search Parameters for Query
  }
  TSearchParamsQuery = (
    spQuery__id, {@enum.value spQuery__id The logical resource id associated with the resource (must be supported by all servers) }
    spQuery__language, {@enum.value spQuery__language The stated language of the resource }
    spQuery_Identifier, {@enum.value spQuery_Identifier Links query and its response(s) }
    spQuery_Response); {@enum.value spQuery_Response Links response to source query }

  {@Enum TSearchParamsQuestionnaire
    Search Parameters for Questionnaire
  }
  TSearchParamsQuestionnaire = (
    spQuestionnaire__id, {@enum.value spQuestionnaire__id The logical resource id associated with the resource (must be supported by all servers) }
    spQuestionnaire__language, {@enum.value spQuestionnaire__language The stated language of the resource }
    spQuestionnaire_Code, {@enum.value spQuestionnaire_Code A code that corresponds to the questionnaire or one of its groups }
    spQuestionnaire_Date, {@enum.value spQuestionnaire_Date When the questionnaire was last changed }
    spQuestionnaire_Identifier, {@enum.value spQuestionnaire_Identifier An identifier for the questionnaire }
    spQuestionnaire_Publisher, {@enum.value spQuestionnaire_Publisher The author of the questionnaire }
    spQuestionnaire_Status, {@enum.value spQuestionnaire_Status The status of the questionnaire }
    spQuestionnaire_Title, {@enum.value spQuestionnaire_Title All or part of the name of the questionnaire (title for the root group of the questionnaire) }
    spQuestionnaire_Version); {@enum.value spQuestionnaire_Version The business version of the questionnaire }

  {@Enum TSearchParamsQuestionnaireAnswers
    Search Parameters for QuestionnaireAnswers
  }
  TSearchParamsQuestionnaireAnswers = (
    spQuestionnaireAnswers__id, {@enum.value spQuestionnaireAnswers__id The logical resource id associated with the resource (must be supported by all servers) }
    spQuestionnaireAnswers__language, {@enum.value spQuestionnaireAnswers__language The stated language of the resource }
    spQuestionnaireAnswers_Author, {@enum.value spQuestionnaireAnswers_Author The author of the questionnaire }
    spQuestionnaireAnswers_Authored, {@enum.value spQuestionnaireAnswers_Authored When the questionnaire was authored }
    spQuestionnaireAnswers_Encounter, {@enum.value spQuestionnaireAnswers_Encounter Encounter during which questionnaire was authored }
    spQuestionnaireAnswers_Questionnaire, {@enum.value spQuestionnaireAnswers_Questionnaire The questionnaire the answers are provided for }
    spQuestionnaireAnswers_Status, {@enum.value spQuestionnaireAnswers_Status The status of the questionnaire answers }
    spQuestionnaireAnswers_Subject); {@enum.value spQuestionnaireAnswers_Subject The subject of the questionnaire }

  {@Enum TSearchParamsReferralRequest
    Search Parameters for ReferralRequest
  }
  TSearchParamsReferralRequest = (
    spReferralRequest__id, {@enum.value spReferralRequest__id The logical resource id associated with the resource (must be supported by all servers) }
    spReferralRequest__language, {@enum.value spReferralRequest__language The stated language of the resource }
    spReferralRequest_Priority, {@enum.value spReferralRequest_Priority The priority assigned to the referral }
    spReferralRequest_Recipient, {@enum.value spReferralRequest_Recipient The person that the referral was sent to }
    spReferralRequest_Specialty, {@enum.value spReferralRequest_Specialty The specialty that the referral is for }
    spReferralRequest_Status, {@enum.value spReferralRequest_Status The status of the referral }
    spReferralRequest_Subject, {@enum.value spReferralRequest_Subject Who the referral is about }
    spReferralRequest_Type); {@enum.value spReferralRequest_Type The type of the referral }

  {@Enum TSearchParamsRelatedPerson
    Search Parameters for RelatedPerson
  }
  TSearchParamsRelatedPerson = (
    spRelatedPerson__id, {@enum.value spRelatedPerson__id The logical resource id associated with the resource (must be supported by all servers) }
    spRelatedPerson__language, {@enum.value spRelatedPerson__language The stated language of the resource }
    spRelatedPerson_Address, {@enum.value spRelatedPerson_Address An address in any kind of address/part }
    spRelatedPerson_Gender, {@enum.value spRelatedPerson_Gender Gender of the person }
    spRelatedPerson_Identifier, {@enum.value spRelatedPerson_Identifier A patient Identifier }
    spRelatedPerson_Name, {@enum.value spRelatedPerson_Name A portion of name in any name part }
    spRelatedPerson_Patient, {@enum.value spRelatedPerson_Patient The patient this person is related to }
    spRelatedPerson_Phonetic, {@enum.value spRelatedPerson_Phonetic A portion of name using some kind of phonetic matching algorithm }
    spRelatedPerson_Telecom); {@enum.value spRelatedPerson_Telecom The value in any kind of contact }

  {@Enum TSearchParamsRiskAssessment
    Search Parameters for RiskAssessment
  }
  TSearchParamsRiskAssessment = (
    spRiskAssessment__id, {@enum.value spRiskAssessment__id The logical resource id associated with the resource (must be supported by all servers) }
    spRiskAssessment__language, {@enum.value spRiskAssessment__language The stated language of the resource }
    spRiskAssessment_Condition, {@enum.value spRiskAssessment_Condition Condition assessed }
    spRiskAssessment_Date, {@enum.value spRiskAssessment_Date When was assessment made? }
    spRiskAssessment_Identifier, {@enum.value spRiskAssessment_Identifier Unique identifier for the assessment }
    spRiskAssessment_Method, {@enum.value spRiskAssessment_Method Evaluation mechanism }
    spRiskAssessment_Performer, {@enum.value spRiskAssessment_Performer Who did assessment? }
    spRiskAssessment_Subject); {@enum.value spRiskAssessment_Subject Who/what does assessment apply to? }

  {@Enum TSearchParamsSecurityEvent
    Search Parameters for SecurityEvent
  }
  TSearchParamsSecurityEvent = (
    spSecurityEvent__id, {@enum.value spSecurityEvent__id The logical resource id associated with the resource (must be supported by all servers) }
    spSecurityEvent__language, {@enum.value spSecurityEvent__language The stated language of the resource }
    spSecurityEvent_Action, {@enum.value spSecurityEvent_Action Type of action performed during the event }
    spSecurityEvent_Address, {@enum.value spSecurityEvent_Address Identifier for the network access point of the user device }
    spSecurityEvent_Altid, {@enum.value spSecurityEvent_Altid Alternative User id e.g. authentication }
    spSecurityEvent_Date, {@enum.value spSecurityEvent_Date Time when the event occurred on source }
    spSecurityEvent_Desc, {@enum.value spSecurityEvent_Desc Instance-specific descriptor for Object }
    spSecurityEvent_Identity, {@enum.value spSecurityEvent_Identity Specific instance of object (e.g. versioned) }
    spSecurityEvent_Name, {@enum.value spSecurityEvent_Name Human-meaningful name for the user }
    spSecurityEvent_Object_type, {@enum.value spSecurityEvent_Object_type Object type being audited }
    spSecurityEvent_Patientid, {@enum.value spSecurityEvent_Patientid The id of the patient (one of multiple kinds of participations) }
    spSecurityEvent_Reference, {@enum.value spSecurityEvent_Reference Specific instance of resource (e.g. versioned) }
    spSecurityEvent_Site, {@enum.value spSecurityEvent_Site Logical source location within the enterprise }
    spSecurityEvent_Source, {@enum.value spSecurityEvent_Source The id of source where event originated }
    spSecurityEvent_Subtype, {@enum.value spSecurityEvent_Subtype More specific type/id for the event }
    spSecurityEvent_Type, {@enum.value spSecurityEvent_Type Type/identifier of event }
    spSecurityEvent_User); {@enum.value spSecurityEvent_User Unique identifier for the user }

  {@Enum TSearchParamsSlot
    Search Parameters for Slot
  }
  TSearchParamsSlot = (
    spSlot__id, {@enum.value spSlot__id The logical resource id associated with the resource (must be supported by all servers) }
    spSlot__language, {@enum.value spSlot__language The stated language of the resource }
    spSlot_Availability, {@enum.value spSlot_Availability The Availability Resource that we are seeking a slot within }
    spSlot_Fbtype, {@enum.value spSlot_Fbtype The free/busy status of the appointment }
    spSlot_Slottype, {@enum.value spSlot_Slottype The type of appointments that can be booked into the slot }
    spSlot_Start); {@enum.value spSlot_Start Appointment date/time. }

  {@Enum TSearchParamsSpecimen
    Search Parameters for Specimen
  }
  TSearchParamsSpecimen = (
    spSpecimen__id, {@enum.value spSpecimen__id The logical resource id associated with the resource (must be supported by all servers) }
    spSpecimen__language, {@enum.value spSpecimen__language The stated language of the resource }
    spSpecimen_Subject); {@enum.value spSpecimen_Subject The subject of the specimen }

  {@Enum TSearchParamsSubscription
    Search Parameters for Subscription
  }
  TSearchParamsSubscription = (
    spSubscription__id, {@enum.value spSubscription__id The logical resource id associated with the resource (must be supported by all servers) }
    spSubscription__language, {@enum.value spSubscription__language The stated language of the resource }
    spSubscription_Contact, {@enum.value spSubscription_Contact Contact details for source (e.g. troubleshooting) }
    spSubscription_Criteria, {@enum.value spSubscription_Criteria Rule for server push criteria }
    spSubscription_Payload, {@enum.value spSubscription_Payload Mimetype to send, or blank for no payload }
    spSubscription_Status, {@enum.value spSubscription_Status requested | active | error | off }
    spSubscription_Tag, {@enum.value spSubscription_Tag The term that identifies the tag }
    spSubscription_Type, {@enum.value spSubscription_Type rest-hook | websocket | email | sms | message }
    spSubscription_Url); {@enum.value spSubscription_Url Where the channel points to }

  {@Enum TSearchParamsSubstance
    Search Parameters for Substance
  }
  TSearchParamsSubstance = (
    spSubstance__id, {@enum.value spSubstance__id The logical resource id associated with the resource (must be supported by all servers) }
    spSubstance__language, {@enum.value spSubstance__language The stated language of the resource }
    spSubstance_Expiry, {@enum.value spSubstance_Expiry When no longer valid to use }
    spSubstance_Identifier, {@enum.value spSubstance_Identifier Identifier of the package/container }
    spSubstance_Quantity, {@enum.value spSubstance_Quantity Amount of substance in the package }
    spSubstance_Substance, {@enum.value spSubstance_Substance A component of the substance }
    spSubstance_Type); {@enum.value spSubstance_Type The type of the substance }

  {@Enum TSearchParamsSupply
    Search Parameters for Supply
  }
  TSearchParamsSupply = (
    spSupply__id, {@enum.value spSupply__id The logical resource id associated with the resource (must be supported by all servers) }
    spSupply__language, {@enum.value spSupply__language The stated language of the resource }
    spSupply_Dispenseid, {@enum.value spSupply_Dispenseid External identifier }
    spSupply_Dispensestatus, {@enum.value spSupply_Dispensestatus in progress | dispensed | abandoned }
    spSupply_Identifier, {@enum.value spSupply_Identifier Unique identifier }
    spSupply_Kind, {@enum.value spSupply_Kind The kind of supply (central, non-stock, etc) }
    spSupply_Patient, {@enum.value spSupply_Patient Patient for whom the item is supplied }
    spSupply_Status, {@enum.value spSupply_Status requested | dispensed | received | failed | cancelled }
    spSupply_Supplier); {@enum.value spSupply_Supplier Dispenser }

  {@Enum TSearchParamsValueSet
    Search Parameters for ValueSet
  }
  TSearchParamsValueSet = (
    spValueSet__id, {@enum.value spValueSet__id The logical resource id associated with the resource (must be supported by all servers) }
    spValueSet__language, {@enum.value spValueSet__language The stated language of the resource }
    spValueSet_Code, {@enum.value spValueSet_Code A code defined in the value set }
    spValueSet_Date, {@enum.value spValueSet_Date The value set publication date }
    spValueSet_Description, {@enum.value spValueSet_Description Text search in the description of the value set }
    spValueSet_Identifier, {@enum.value spValueSet_Identifier The identifier of the value set }
    spValueSet_Name, {@enum.value spValueSet_Name The name of the value set }
    spValueSet_Publisher, {@enum.value spValueSet_Publisher Name of the publisher of the value set }
    spValueSet_Reference, {@enum.value spValueSet_Reference A code system included or excluded in the value set or an imported value set }
    spValueSet_Status, {@enum.value spValueSet_Status The status of the value set }
    spValueSet_System, {@enum.value spValueSet_System The system for any codes defined by this value set }
    spValueSet_Version); {@enum.value spValueSet_Version The version identifier of the value set }

Type
  TFhirResource = class;
  TFhirResourceList = class;
  TFhirAdverseReaction = class;
  TFhirAlert = class;
  TFhirAllergyIntolerance = class;
  TFhirAppointment = class;
  TFhirAppointmentResponse = class;
  TFhirAvailability = class;
  TFhirCarePlan = class;
  TFhirComposition = class;
  TFhirConceptMap = class;
  TFhirCondition = class;
  TFhirConformance = class;
  TFhirContraindication = class;
  TFhirDataElement = class;
  TFhirDevice = class;
  TFhirDeviceObservationReport = class;
  TFhirDiagnosticOrder = class;
  TFhirDiagnosticReport = class;
  TFhirDocumentManifest = class;
  TFhirDocumentReference = class;
  TFhirEncounter = class;
  TFhirFamilyHistory = class;
  TFhirGroup = class;
  TFhirImagingStudy = class;
  TFhirImmunization = class;
  TFhirImmunizationRecommendation = class;
  TFhirList = class;
  TFhirLocation = class;
  TFhirMedia = class;
  TFhirMedication = class;
  TFhirMedicationAdministration = class;
  TFhirMedicationDispense = class;
  TFhirMedicationPrescription = class;
  TFhirMedicationStatement = class;
  TFhirMessageHeader = class;
  TFhirNamespace = class;
  TFhirObservation = class;
  TFhirOperationDefinition = class;
  TFhirOperationOutcome = class;
  TFhirOrder = class;
  TFhirOrderResponse = class;
  TFhirOrganization = class;
  TFhirOther = class;
  TFhirPatient = class;
  TFhirPractitioner = class;
  TFhirProcedure = class;
  TFhirProfile = class;
  TFhirProvenance = class;
  TFhirQuery = class;
  TFhirQuestionnaire = class;
  TFhirQuestionnaireAnswers = class;
  TFhirReferralRequest = class;
  TFhirRelatedPerson = class;
  TFhirRiskAssessment = class;
  TFhirSecurityEvent = class;
  TFhirSlot = class;
  TFhirSpecimen = class;
  TFhirSubscription = class;
  TFhirSubstance = class;
  TFhirSupply = class;
  TFhirValueSet = class;

  {@Class TFhirResource : TFhirElement
    Base Resource Definition - extensions, narrative, contained resources
  }
  {!.Net HL7Connect.Fhir.Resource}
  TFhirResource = {abstract} class (TFhirBackboneElement)
  private
    FText : TFhirNarrative;
    FLanguage : TFhirCode;
    FFormat : TFHIRFormat;
    FContainedList : TFhirResourceList;
    procedure SetText(value : TFhirNarrative);
    procedure SetLanguage(value : TFhirCode);
  protected
    function GetResourceType : TFhirResourceType; virtual; abstract;
    function GetHasASummary : Boolean; virtual; abstract;
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
  public
    constructor Create; override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirResource; overload;
    function Clone : TFhirResource; overload;
    {!script show}
  published
    Property ResourceType : TFhirResourceType read GetResourceType;

    Property HasASummary : Boolean read GetHasASummary;

    {@member language
      The base language of the resource
    }
    property language : TFhirCode read FLanguage write SetLanguage;
    {@member text
      Text summary of resource content, for human interpretation
    }
    property text : TFhirNarrative read FText write SetText;
    {@member containedList
      Text summary of resource content, for human interpretation
    }
    property containedList : TFhirResourceList read FContainedList;
    {@member _source_format
      Whether the resource was first represented in XML or JSON
    }
    property _source_format : TFHIRFormat read FFormat write FFormat;
  end;
  
  TFhirResourceClass = class of TFhirResource;
  
  
  {@Class TFhirBinary : TFhirResource
    Special Binary Resource
  }
  {!.Net HL7Connect.Fhir.Binary}
  TFhirBinary = class (TFhirResource)
  private
    FContent : TAdvBuffer;
    FContentType : string;
  protected
    function GetResourceType : TFhirResourceType; override;
    function GetHasASummary : Boolean; override;
  public
    Constructor Create; Overload; Override;
    Destructor Destroy; Override;
  published
    Property Content : TAdvBuffer read FContent;
    Property ContentType : string read FContentType write FContentType;
  end;
  


  {@Class TFhirResourceList
    A list of FhirResource
  }
  {!.Net HL7Connect.Fhir.ResourceList}
  TFhirResourceList = class (TFHIRObjectList)
  private
    function GetItemN(index : Integer) : TFhirResource;
    procedure SetItemN(index : Integer; value : TFhirResource);
  public
    {!script hide}
    function Link : TFhirResourceList; Overload;
    function Clone : TFhirResourceList; Overload;
    {!script show}
    

    
    {@member AddItem
      Add an already existing FhirResource to the end of the list.
    }
    procedure AddItem(value : TFhirResource);
    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    
    {@member IndexOf
      See if an item is already in the list. returns -1 if not in the list
    }
    function IndexOf(value : TFhirResource) : Integer;
    

    {@member InsertItem
       Insert an existing FhirResource before the designated index (0 = first item)
    }
    procedure InsertItem(index : Integer; value : TFhirResource);
    
    {@member Item
       Get the iIndexth FhirResource. (0 = first item)
    }
    
    {@member Item
       Get the iIndexth FhirResource. (0 = first item)
    }
    procedure SetItemByIndex(index : Integer; value : TFhirResource);
    
    {@member Count
      The number of items in the collection
    }
    function Item(index : Integer) : TFhirResource;
    
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
    
    Property FhirResources[index : Integer] : TFhirResource read GetItemN write SetItemN; default;
  End;


  {@Class TFhirAdverseReaction : TFhirResource
    Records an unexpected reaction suspected to be related to the exposure of the reaction subject to a substance.
  }
  {!.Net HL7Connect.Fhir.AdverseReaction}
  TFhirAdverseReaction = class (TFhirResource)
  private
    FidentifierList : TFhirIdentifierList;
    FDate : TFhirDateTime;
    FSubject : TFhirResourceReference{TFhirPatient};
    FDidNotOccurFlag : TFhirBoolean;
    FRecorder : TFhirResourceReference{Resource};
    FsymptomList : TFhirAdverseReactionSymptomList;
    FexposureList : TFhirAdverseReactionExposureList;
    Procedure SetDate(value : TFhirDateTime);
    Function GetDateST : TDateAndTime;
    Procedure SetDateST(value : TDateAndTime);
    Procedure SetSubject(value : TFhirResourceReference{TFhirPatient});
    Procedure SetDidNotOccurFlag(value : TFhirBoolean);
    Function GetDidNotOccurFlagST : Boolean;
    Procedure SetDidNotOccurFlagST(value : Boolean);
    Procedure SetRecorder(value : TFhirResourceReference{Resource});
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    Function GetHasASummary : Boolean; Override;
    function GetResourceType : TFhirResourceType; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirAdverseReaction; overload;
    function Clone : TFhirAdverseReaction; overload;
    {!script show}
  published
    {@member identifierList
      This records identifiers associated with this reaction that are defined by business processed and/ or used to refer to it when a direct URL reference to the resource itself is not appropriate (e.g. in CDA documents, or in written / printed documentation).
    }
    property identifierList : TFhirIdentifierList read FIdentifierList;

    {@member date
      The date (and possibly time) when the reaction began.
    }
    property date : TFhirDateTime read FDate write SetDate;
    {@member dateST
      Typed access to The date (and possibly time) when the reaction began.
    }
    property dateST : TDateAndTime read GetDateST write SetDateST;

    {@member subject
      The subject of the adverse reaction.
    }
    property subject : TFhirResourceReference{TFhirPatient} read FSubject write SetSubject;

    {@member didNotOccurFlag
      If true, indicates that no reaction occurred.
    }
    property didNotOccurFlag : TFhirBoolean read FDidNotOccurFlag write SetDidNotOccurFlag;
    {@member didNotOccurFlagST
      Typed access to If true, indicates that no reaction occurred.
    }
    property didNotOccurFlagST : Boolean read GetDidNotOccurFlagST write SetDidNotOccurFlagST;

    {@member recorder
      Identifies the individual responsible for the information in the reaction record.
    }
    property recorder : TFhirResourceReference{Resource} read FRecorder write SetRecorder;

    {@member symptomList
      The signs and symptoms that were observed as part of the reaction.
    }
    property symptomList : TFhirAdverseReactionSymptomList read FSymptomList;

    {@member exposureList
      An exposure to a substance that preceded a reaction occurrence.
    }
    property exposureList : TFhirAdverseReactionExposureList read FExposureList;

  end;


  {@Class TFhirAlert : TFhirResource
    Prospective warnings of potential issues when providing care to the patient.
  }
  {!.Net HL7Connect.Fhir.Alert}
  TFhirAlert = class (TFhirResource)
  private
    FidentifierList : TFhirIdentifierList;
    FCategory : TFhirCodeableConcept;
    FStatus : TFhirEnum;
    FSubject : TFhirResourceReference{TFhirPatient};
    FAuthor : TFhirResourceReference{Resource};
    FNote : TFhirString;
    Procedure SetCategory(value : TFhirCodeableConcept);
    Procedure SetStatus(value : TFhirEnum);
    Function GetStatusST : TFhirAlertStatus;
    Procedure SetStatusST(value : TFhirAlertStatus);
    Procedure SetSubject(value : TFhirResourceReference{TFhirPatient});
    Procedure SetAuthor(value : TFhirResourceReference{Resource});
    Procedure SetNote(value : TFhirString);
    Function GetNoteST : String;
    Procedure SetNoteST(value : String);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    Function GetHasASummary : Boolean; Override;
    function GetResourceType : TFhirResourceType; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirAlert; overload;
    function Clone : TFhirAlert; overload;
    {!script show}
  published
    {@member identifierList
      Identifier assigned to the alert for external use (outside the FHIR environment).
    }
    property identifierList : TFhirIdentifierList read FIdentifierList;

    {@member category
      Allows an alert to be divided into different categories like clinical, administrative etc.
    }
    property category : TFhirCodeableConcept read FCategory write SetCategory;

    {@member status
      Supports basic workflow.
    }
    property status : TFhirEnum read FStatus write SetStatus;
    {@member statusST
      Typed access to Supports basic workflow.
    }
    property statusST : TFhirAlertStatus read GetStatusST write SetStatusST;

    {@member subject
      The person who this alert concerns.
    }
    property subject : TFhirResourceReference{TFhirPatient} read FSubject write SetSubject;

    {@member author
      The person or device that created the alert.
    }
    property author : TFhirResourceReference{Resource} read FAuthor write SetAuthor;

    {@member note
      The textual component of the alert to display to the user.
    }
    property note : TFhirString read FNote write SetNote;
    {@member noteST
      Typed access to The textual component of the alert to display to the user.
    }
    property noteST : String read GetNoteST write SetNoteST;

  end;


  {@Class TFhirAllergyIntolerance : TFhirResource
    Indicates the patient has a susceptibility to an adverse reaction upon exposure to a specified substance.
  }
  {!.Net HL7Connect.Fhir.AllergyIntolerance}
  TFhirAllergyIntolerance = class (TFhirResource)
  private
    FidentifierList : TFhirIdentifierList;
    FCriticality : TFhirEnum;
    FSensitivityType : TFhirEnum;
    FRecordedDate : TFhirDateTime;
    FStatus : TFhirEnum;
    FSubject : TFhirResourceReference{TFhirPatient};
    FRecorder : TFhirResourceReference{Resource};
    FSubstance : TFhirResourceReference{TFhirSubstance};
    FreactionList : TFhirResourceReferenceList{TFhirAdverseReaction};
    FsensitivityTestList : TFhirResourceReferenceList{TFhirObservation};
    Procedure SetCriticality(value : TFhirEnum);
    Function GetCriticalityST : TFhirCriticality;
    Procedure SetCriticalityST(value : TFhirCriticality);
    Procedure SetSensitivityType(value : TFhirEnum);
    Function GetSensitivityTypeST : TFhirSensitivitytype;
    Procedure SetSensitivityTypeST(value : TFhirSensitivitytype);
    Procedure SetRecordedDate(value : TFhirDateTime);
    Function GetRecordedDateST : TDateAndTime;
    Procedure SetRecordedDateST(value : TDateAndTime);
    Procedure SetStatus(value : TFhirEnum);
    Function GetStatusST : TFhirSensitivitystatus;
    Procedure SetStatusST(value : TFhirSensitivitystatus);
    Procedure SetSubject(value : TFhirResourceReference{TFhirPatient});
    Procedure SetRecorder(value : TFhirResourceReference{Resource});
    Procedure SetSubstance(value : TFhirResourceReference{TFhirSubstance});
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    Function GetHasASummary : Boolean; Override;
    function GetResourceType : TFhirResourceType; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirAllergyIntolerance; overload;
    function Clone : TFhirAllergyIntolerance; overload;
    {!script show}
  published
    {@member identifierList
      This records identifiers associated with this allergy/intolerance concern that are defined by business processed and/ or used to refer to it when a direct URL reference to the resource itself is not appropriate (e.g. in CDA documents, or in written / printed documentation).
    }
    property identifierList : TFhirIdentifierList read FIdentifierList;

    {@member criticality
      Criticality of the sensitivity.
    }
    property criticality : TFhirEnum read FCriticality write SetCriticality;
    {@member criticalityST
      Typed access to Criticality of the sensitivity.
    }
    property criticalityST : TFhirCriticality read GetCriticalityST write SetCriticalityST;

    {@member sensitivityType
      Type of the sensitivity.
    }
    property sensitivityType : TFhirEnum read FSensitivityType write SetSensitivityType;
    {@member sensitivityTypeST
      Typed access to Type of the sensitivity.
    }
    property sensitivityTypeST : TFhirSensitivitytype read GetSensitivityTypeST write SetSensitivityTypeST;

    {@member recordedDate
      Date when the sensitivity was recorded.
    }
    property recordedDate : TFhirDateTime read FRecordedDate write SetRecordedDate;
    {@member recordedDateST
      Typed access to Date when the sensitivity was recorded.
    }
    property recordedDateST : TDateAndTime read GetRecordedDateST write SetRecordedDateST;

    {@member status
      Status of the sensitivity.
    }
    property status : TFhirEnum read FStatus write SetStatus;
    {@member statusST
      Typed access to Status of the sensitivity.
    }
    property statusST : TFhirSensitivitystatus read GetStatusST write SetStatusST;

    {@member subject
      The patient who has the allergy or intolerance.
    }
    property subject : TFhirResourceReference{TFhirPatient} read FSubject write SetSubject;

    {@member recorder
      Indicates who has responsibility for the record.
    }
    property recorder : TFhirResourceReference{Resource} read FRecorder write SetRecorder;

    {@member substance
      The substance that causes the sensitivity.
    }
    property substance : TFhirResourceReference{TFhirSubstance} read FSubstance write SetSubstance;

    {@member reactionList
      Reactions associated with the sensitivity.
    }
    property reactionList : TFhirResourceReferenceList{TFhirAdverseReaction} read FReactionList;

    {@member sensitivityTestList
      Observations that confirm or refute the sensitivity.
    }
    property sensitivityTestList : TFhirResourceReferenceList{TFhirObservation} read FSensitivityTestList;

  end;


  {@Class TFhirAppointment : TFhirResource
    (informative) A scheduled appointment for a patient and/or practitioner(s) where a service may take place.
  }
  {!.Net HL7Connect.Fhir.Appointment}
  TFhirAppointment = class (TFhirResource)
  private
    FidentifierList : TFhirIdentifierList;
    FPriority : TFhirInteger;
    FStatus : TFhirCode;
    FType_ : TFhirCodeableConcept;
    FReason : TFhirCodeableConcept;
    FDescription : TFhirString;
    FStart : TFhirInstant;
    FEnd_ : TFhirInstant;
    FslotList : TFhirResourceReferenceList{TFhirSlot};
    FLocation : TFhirResourceReference{TFhirLocation};
    FComment : TFhirString;
    FOrder : TFhirResourceReference{TFhirOrder};
    FparticipantList : TFhirAppointmentParticipantList;
    FLastModifiedBy : TFhirResourceReference{Resource};
    FLastModified : TFhirDateTime;
    Procedure SetPriority(value : TFhirInteger);
    Function GetPriorityST : String;
    Procedure SetPriorityST(value : String);
    Procedure SetStatus(value : TFhirCode);
    Function GetStatusST : String;
    Procedure SetStatusST(value : String);
    Procedure SetType_(value : TFhirCodeableConcept);
    Procedure SetReason(value : TFhirCodeableConcept);
    Procedure SetDescription(value : TFhirString);
    Function GetDescriptionST : String;
    Procedure SetDescriptionST(value : String);
    Procedure SetStart(value : TFhirInstant);
    Function GetStartST : TDateAndTime;
    Procedure SetStartST(value : TDateAndTime);
    Procedure SetEnd_(value : TFhirInstant);
    Function GetEnd_ST : TDateAndTime;
    Procedure SetEnd_ST(value : TDateAndTime);
    Procedure SetLocation(value : TFhirResourceReference{TFhirLocation});
    Procedure SetComment(value : TFhirString);
    Function GetCommentST : String;
    Procedure SetCommentST(value : String);
    Procedure SetOrder(value : TFhirResourceReference{TFhirOrder});
    Procedure SetLastModifiedBy(value : TFhirResourceReference{Resource});
    Procedure SetLastModified(value : TFhirDateTime);
    Function GetLastModifiedST : TDateAndTime;
    Procedure SetLastModifiedST(value : TDateAndTime);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    Function GetHasASummary : Boolean; Override;
    function GetResourceType : TFhirResourceType; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirAppointment; overload;
    function Clone : TFhirAppointment; overload;
    {!script show}
  published
    {@member identifierList
      This records identifiers associated with this appointment concern that are defined by business processed and/ or used to refer to it when a direct URL reference to the resource itself is not appropriate (e.g. in CDA documents, or in written / printed documentation).
    }
    property identifierList : TFhirIdentifierList read FIdentifierList;

    {@member priority
      The priority of the appointment. Can be used to make informed decisions if needing to re-prioritize appointments. (The iCal Standard specifies 0 as undefined, 1 as highest, 9 as lowest priority) (Need to change back to CodeableConcept).
    }
    property priority : TFhirInteger read FPriority write SetPriority;
    {@member priorityST
      Typed access to The priority of the appointment. Can be used to make informed decisions if needing to re-prioritize appointments. (The iCal Standard specifies 0 as undefined, 1 as highest, 9 as lowest priority) (Need to change back to CodeableConcept).
    }
    property priorityST : String read GetPriorityST write SetPriorityST;

    {@member status
      Each of the participants has their own participation status which indicates their involvement in the process, however this status indicates the shared status.
    }
    property status : TFhirCode read FStatus write SetStatus;
    {@member statusST
      Typed access to Each of the participants has their own participation status which indicates their involvement in the process, however this status indicates the shared status.
    }
    property statusST : String read GetStatusST write SetStatusST;

    {@member type_
      The type of appointments that is being booked (ideally this would be an identifiable service - which is at a location, rather than the location itself).
    }
    property type_ : TFhirCodeableConcept read FType_ write SetType_;

    {@member reason
      The reason that this appointment is being scheduled, this is more clinical than administrative.
    }
    property reason : TFhirCodeableConcept read FReason write SetReason;

    {@member description
      The brief description of the appointment as would be shown on a subject line in a meeting request, or appointment list. Detailed or expanded information should be put in the comment field.
    }
    property description : TFhirString read FDescription write SetDescription;
    {@member descriptionST
      Typed access to The brief description of the appointment as would be shown on a subject line in a meeting request, or appointment list. Detailed or expanded information should be put in the comment field.
    }
    property descriptionST : String read GetDescriptionST write SetDescriptionST;

    {@member start
      Date/Time that the appointment is to take place.
    }
    property start : TFhirInstant read FStart write SetStart;
    {@member startST
      Typed access to Date/Time that the appointment is to take place.
    }
    property startST : TDateAndTime read GetStartST write SetStartST;

    {@member end_
      Date/Time that the appointment is to conclude.
    }
    property end_ : TFhirInstant read FEnd_ write SetEnd_;
    {@member end_ST
      Typed access to Date/Time that the appointment is to conclude.
    }
    property end_ST : TDateAndTime read GetEnd_ST write SetEnd_ST;

    {@member slotList
      The slot that this appointment is filling. If provided then the schedule will not be provided as slots are not recursive, and the start/end values MUST be the same as from the slot.
    }
    property slotList : TFhirResourceReferenceList{TFhirSlot} read FSlotList;

    {@member location
      The primary location that this appointment is to take place.
    }
    property location : TFhirResourceReference{TFhirLocation} read FLocation write SetLocation;

    {@member comment
      Additional comments about the appointment.
    }
    property comment : TFhirString read FComment write SetComment;
    {@member commentST
      Typed access to Additional comments about the appointment.
    }
    property commentST : String read GetCommentST write SetCommentST;

    {@member order
      An Order that lead to the creation of this appointment.
    }
    property order : TFhirResourceReference{TFhirOrder} read FOrder write SetOrder;

    {@member participantList
      List of participants involved in the appointment.
    }
    property participantList : TFhirAppointmentParticipantList read FParticipantList;

    {@member lastModifiedBy
      Who recorded the appointment.
    }
    property lastModifiedBy : TFhirResourceReference{Resource} read FLastModifiedBy write SetLastModifiedBy;

    {@member lastModified
      Date when the appointment was recorded.
    }
    property lastModified : TFhirDateTime read FLastModified write SetLastModified;
    {@member lastModifiedST
      Typed access to Date when the appointment was recorded.
    }
    property lastModifiedST : TDateAndTime read GetLastModifiedST write SetLastModifiedST;

  end;


  {@Class TFhirAppointmentResponse : TFhirResource
    A reply to an appointment request for a patient and/or practitioner(s), such as a confirmation or rejection.
  }
  {!.Net HL7Connect.Fhir.AppointmentResponse}
  TFhirAppointmentResponse = class (TFhirResource)
  private
    FidentifierList : TFhirIdentifierList;
    FAppointment : TFhirResourceReference{TFhirAppointment};
    FparticipantTypeList : TFhirCodeableConceptList;
    FindividualList : TFhirResourceReferenceList{Resource};
    FParticipantStatus : TFhirEnum;
    FComment : TFhirString;
    FStart : TFhirInstant;
    FEnd_ : TFhirInstant;
    FLastModifiedBy : TFhirResourceReference{Resource};
    FLastModified : TFhirDateTime;
    Procedure SetAppointment(value : TFhirResourceReference{TFhirAppointment});
    Procedure SetParticipantStatus(value : TFhirEnum);
    Function GetParticipantStatusST : TFhirParticipantstatus;
    Procedure SetParticipantStatusST(value : TFhirParticipantstatus);
    Procedure SetComment(value : TFhirString);
    Function GetCommentST : String;
    Procedure SetCommentST(value : String);
    Procedure SetStart(value : TFhirInstant);
    Function GetStartST : TDateAndTime;
    Procedure SetStartST(value : TDateAndTime);
    Procedure SetEnd_(value : TFhirInstant);
    Function GetEnd_ST : TDateAndTime;
    Procedure SetEnd_ST(value : TDateAndTime);
    Procedure SetLastModifiedBy(value : TFhirResourceReference{Resource});
    Procedure SetLastModified(value : TFhirDateTime);
    Function GetLastModifiedST : TDateAndTime;
    Procedure SetLastModifiedST(value : TDateAndTime);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    Function GetHasASummary : Boolean; Override;
    function GetResourceType : TFhirResourceType; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirAppointmentResponse; overload;
    function Clone : TFhirAppointmentResponse; overload;
    {!script show}
  published
    {@member identifierList
      This records identifiers associated with this appointment concern that are defined by business processed and/ or used to refer to it when a direct URL reference to the resource itself is not appropriate (e.g. in CDA documents, or in written / printed documentation).
    }
    property identifierList : TFhirIdentifierList read FIdentifierList;

    {@member appointment
      Parent appointment that this response is replying to.
    }
    property appointment : TFhirResourceReference{TFhirAppointment} read FAppointment write SetAppointment;

    {@member participantTypeList
      Role of participant in the appointment.
    }
    property participantTypeList : TFhirCodeableConceptList read FParticipantTypeList;

    {@member individualList
      A Person of device that is participating in the appointment, usually Practitioner, Patient, RelatedPerson or Device.
    }
    property individualList : TFhirResourceReferenceList{Resource} read FIndividualList;

    {@member participantStatus
      Participation status of the Patient.
    }
    property participantStatus : TFhirEnum read FParticipantStatus write SetParticipantStatus;
    {@member participantStatusST
      Typed access to Participation status of the Patient.
    }
    property participantStatusST : TFhirParticipantstatus read GetParticipantStatusST write SetParticipantStatusST;

    {@member comment
      Additional comments about the appointment.
    }
    property comment : TFhirString read FComment write SetComment;
    {@member commentST
      Typed access to Additional comments about the appointment.
    }
    property commentST : String read GetCommentST write SetCommentST;

    {@member start
      Date/Time that the appointment is to take place.
    }
    property start : TFhirInstant read FStart write SetStart;
    {@member startST
      Typed access to Date/Time that the appointment is to take place.
    }
    property startST : TDateAndTime read GetStartST write SetStartST;

    {@member end_
      Date/Time that the appointment is to conclude.
    }
    property end_ : TFhirInstant read FEnd_ write SetEnd_;
    {@member end_ST
      Typed access to Date/Time that the appointment is to conclude.
    }
    property end_ST : TDateAndTime read GetEnd_ST write SetEnd_ST;

    {@member lastModifiedBy
      Who recorded the appointment response.
    }
    property lastModifiedBy : TFhirResourceReference{Resource} read FLastModifiedBy write SetLastModifiedBy;

    {@member lastModified
      Date when the response was recorded or last updated.
    }
    property lastModified : TFhirDateTime read FLastModified write SetLastModified;
    {@member lastModifiedST
      Typed access to Date when the response was recorded or last updated.
    }
    property lastModifiedST : TDateAndTime read GetLastModifiedST write SetLastModifiedST;

  end;


  {@Class TFhirAvailability : TFhirResource
    (informative) A container for slot(s) of time that may be available for booking appointments.
  }
  {!.Net HL7Connect.Fhir.Availability}
  TFhirAvailability = class (TFhirResource)
  private
    FidentifierList : TFhirIdentifierList;
    Ftype_List : TFhirCodeableConceptList;
    FActor : TFhirResourceReference{Resource};
    FPlanningHorizon : TFhirPeriod;
    FComment : TFhirString;
    FLastModified : TFhirDateTime;
    Procedure SetActor(value : TFhirResourceReference{Resource});
    Procedure SetPlanningHorizon(value : TFhirPeriod);
    Procedure SetComment(value : TFhirString);
    Function GetCommentST : String;
    Procedure SetCommentST(value : String);
    Procedure SetLastModified(value : TFhirDateTime);
    Function GetLastModifiedST : TDateAndTime;
    Procedure SetLastModifiedST(value : TDateAndTime);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    Function GetHasASummary : Boolean; Override;
    function GetResourceType : TFhirResourceType; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirAvailability; overload;
    function Clone : TFhirAvailability; overload;
    {!script show}
  published
    {@member identifierList
      External Ids for this item.
    }
    property identifierList : TFhirIdentifierList read FIdentifierList;

    {@member type_List
      The schedule type can be used for the categorization of healthcare services or other appointment types.
    }
    property type_List : TFhirCodeableConceptList read FType_List;

    {@member actor
      The resource this availability resource is providing availability information for. These are expected to usually be one of HealthcareService, Location, Practitioner, Device, Patient or RelatedPerson.
    }
    property actor : TFhirResourceReference{Resource} read FActor write SetActor;

    {@member planningHorizon
      The period of time that the slots that are attached to this availability resource cover (even if none exist). These  cover the amount of time that an organization's planning horizon; the interval for which they are currently accepting appointments. This does not define a "template" for planning outside these dates.
    }
    property planningHorizon : TFhirPeriod read FPlanningHorizon write SetPlanningHorizon;

    {@member comment
      Comments on the availability to describe any extended information. Such as custom constraints on the slot(s) that may be associated.
    }
    property comment : TFhirString read FComment write SetComment;
    {@member commentST
      Typed access to Comments on the availability to describe any extended information. Such as custom constraints on the slot(s) that may be associated.
    }
    property commentST : String read GetCommentST write SetCommentST;

    {@member lastModified
      When this availability was created, or last revised.
    }
    property lastModified : TFhirDateTime read FLastModified write SetLastModified;
    {@member lastModifiedST
      Typed access to When this availability was created, or last revised.
    }
    property lastModifiedST : TDateAndTime read GetLastModifiedST write SetLastModifiedST;

  end;


  {@Class TFhirCarePlan : TFhirResource
    Describes the intention of how one or more practitioners intend to deliver care for a particular patient for a period of time, possibly limited to care for a specific condition or set of conditions.
  }
  {!.Net HL7Connect.Fhir.CarePlan}
  TFhirCarePlan = class (TFhirResource)
  private
    FidentifierList : TFhirIdentifierList;
    FPatient : TFhirResourceReference{TFhirPatient};
    FStatus : TFhirEnum;
    FPeriod : TFhirPeriod;
    FModified : TFhirDateTime;
    FconcernList : TFhirResourceReferenceList{TFhirCondition};
    FparticipantList : TFhirCarePlanParticipantList;
    FgoalList : TFhirCarePlanGoalList;
    FactivityList : TFhirCarePlanActivityList;
    FNotes : TFhirString;
    Procedure SetPatient(value : TFhirResourceReference{TFhirPatient});
    Procedure SetStatus(value : TFhirEnum);
    Function GetStatusST : TFhirCarePlanStatus;
    Procedure SetStatusST(value : TFhirCarePlanStatus);
    Procedure SetPeriod(value : TFhirPeriod);
    Procedure SetModified(value : TFhirDateTime);
    Function GetModifiedST : TDateAndTime;
    Procedure SetModifiedST(value : TDateAndTime);
    Procedure SetNotes(value : TFhirString);
    Function GetNotesST : String;
    Procedure SetNotesST(value : String);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    Function GetHasASummary : Boolean; Override;
    function GetResourceType : TFhirResourceType; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirCarePlan; overload;
    function Clone : TFhirCarePlan; overload;
    {!script show}
  published
    {@member identifierList
      This records identifiers associated with this care plan that are defined by business processed and/ or used to refer to it when a direct URL reference to the resource itself is not appropriate (e.g. in CDA documents, or in written / printed documentation).
    }
    property identifierList : TFhirIdentifierList read FIdentifierList;

    {@member patient
      Identifies the patient/subject whose intended care is described by the plan.
    }
    property patient : TFhirResourceReference{TFhirPatient} read FPatient write SetPatient;

    {@member status
      Indicates whether the plan is currently being acted upon, represents future intentions or is now just historical record.
    }
    property status : TFhirEnum read FStatus write SetStatus;
    {@member statusST
      Typed access to Indicates whether the plan is currently being acted upon, represents future intentions or is now just historical record.
    }
    property statusST : TFhirCarePlanStatus read GetStatusST write SetStatusST;

    {@member period
      Indicates when the plan did (or is intended to) come into effect and end.
    }
    property period : TFhirPeriod read FPeriod write SetPeriod;

    {@member modified
      Identifies the most recent date on which the plan has been revised.
    }
    property modified : TFhirDateTime read FModified write SetModified;
    {@member modifiedST
      Typed access to Identifies the most recent date on which the plan has been revised.
    }
    property modifiedST : TDateAndTime read GetModifiedST write SetModifiedST;

    {@member concernList
      Identifies the conditions/problems/concerns/diagnoses/etc. whose management and/or mitigation are handled by this plan.
    }
    property concernList : TFhirResourceReferenceList{TFhirCondition} read FConcernList;

    {@member participantList
      Identifies all people and organizations who are expected to be involved in the care envisioned by this plan.
    }
    property participantList : TFhirCarePlanParticipantList read FParticipantList;

    {@member goalList
      Describes the intended objective(s) of carrying out the Care Plan.
    }
    property goalList : TFhirCarePlanGoalList read FGoalList;

    {@member activityList
      Identifies a planned action to occur as part of the plan.  For example, a medication to be used, lab tests to perform, self-monitoring, education, etc.
    }
    property activityList : TFhirCarePlanActivityList read FActivityList;

    {@member notes
      General notes about the care plan not covered elsewhere.
    }
    property notes : TFhirString read FNotes write SetNotes;
    {@member notesST
      Typed access to General notes about the care plan not covered elsewhere.
    }
    property notesST : String read GetNotesST write SetNotesST;

  end;


  {@Class TFhirComposition : TFhirResource
    A set of healthcare-related information that is assembled together into a single logical document that provides a single coherent statement of meaning, establishes its own context and that has clinical attestation with regard to who is making the statement.
  }
  {!.Net HL7Connect.Fhir.Composition}
  TFhirComposition = class (TFhirResource)
  private
    FIdentifier : TFhirIdentifier;
    FDate : TFhirDateTime;
    FType_ : TFhirCodeableConcept;
    FClass_ : TFhirCodeableConcept;
    FTitle : TFhirString;
    FStatus : TFhirEnum;
    FConfidentiality : TFhirCoding;
    FSubject : TFhirResourceReference{Resource};
    FauthorList : TFhirResourceReferenceList{Resource};
    FattesterList : TFhirCompositionAttesterList;
    FCustodian : TFhirResourceReference{TFhirOrganization};
    FEvent : TFhirCompositionEvent;
    FEncounter : TFhirResourceReference{TFhirEncounter};
    FsectionList : TFhirCompositionSectionList;
    Procedure SetIdentifier(value : TFhirIdentifier);
    Procedure SetDate(value : TFhirDateTime);
    Function GetDateST : TDateAndTime;
    Procedure SetDateST(value : TDateAndTime);
    Procedure SetType_(value : TFhirCodeableConcept);
    Procedure SetClass_(value : TFhirCodeableConcept);
    Procedure SetTitle(value : TFhirString);
    Function GetTitleST : String;
    Procedure SetTitleST(value : String);
    Procedure SetStatus(value : TFhirEnum);
    Function GetStatusST : TFhirCompositionStatus;
    Procedure SetStatusST(value : TFhirCompositionStatus);
    Procedure SetConfidentiality(value : TFhirCoding);
    Procedure SetSubject(value : TFhirResourceReference{Resource});
    Procedure SetCustodian(value : TFhirResourceReference{TFhirOrganization});
    Procedure SetEvent(value : TFhirCompositionEvent);
    Procedure SetEncounter(value : TFhirResourceReference{TFhirEncounter});
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    Function GetHasASummary : Boolean; Override;
    function GetResourceType : TFhirResourceType; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirComposition; overload;
    function Clone : TFhirComposition; overload;
    {!script show}
  published
    {@member identifier
      Logical Identifier for the composition, assigned when created. This identifier stays constant as the composition is changed over time.
    }
    property identifier : TFhirIdentifier read FIdentifier write SetIdentifier;

    {@member date
      The composition editing time, when the composition was last logically changed by the author.
    }
    property date : TFhirDateTime read FDate write SetDate;
    {@member dateST
      Typed access to The composition editing time, when the composition was last logically changed by the author.
    }
    property dateST : TDateAndTime read GetDateST write SetDateST;

    {@member type_
      Specifies the particular kind of composition (e.g. History and Physical, Discharge Summary, Progress Note). This usually equates to the purpose of making the composition.
    }
    property type_ : TFhirCodeableConcept read FType_ write SetType_;

    {@member class_
      A categorization for the type of the composition. This may be implied by or derived from the code specified in the Composition Type.
    }
    property class_ : TFhirCodeableConcept read FClass_ write SetClass_;

    {@member title
      Official human-readable label for the composition.
    }
    property title : TFhirString read FTitle write SetTitle;
    {@member titleST
      Typed access to Official human-readable label for the composition.
    }
    property titleST : String read GetTitleST write SetTitleST;

    {@member status
      The workflow/clinical status of this composition. The status is a marker for the clinical standing of the document.
    }
    property status : TFhirEnum read FStatus write SetStatus;
    {@member statusST
      Typed access to The workflow/clinical status of this composition. The status is a marker for the clinical standing of the document.
    }
    property statusST : TFhirCompositionStatus read GetStatusST write SetStatusST;

    {@member confidentiality
      The code specifying the level of confidentiality of the Composition.
    }
    property confidentiality : TFhirCoding read FConfidentiality write SetConfidentiality;

    {@member subject
      Who or what the composition is about. The composition can be about a person, (patient or healthcare practitioner), a device (I.e. machine) or even a group of subjects (such as a document about a herd of livestock, or a set of patients that share a common exposure).
    }
    property subject : TFhirResourceReference{Resource} read FSubject write SetSubject;

    {@member authorList
      Identifies who is responsible for the information in the composition.  (Not necessarily who typed it in.).
    }
    property authorList : TFhirResourceReferenceList{Resource} read FAuthorList;

    {@member attesterList
      A participant who has attested to the accuracy of the composition/document.
    }
    property attesterList : TFhirCompositionAttesterList read FAttesterList;

    {@member custodian
      Identifies the organization or group who is responsible for ongoing maintenance of and access to the composition/document information.
    }
    property custodian : TFhirResourceReference{TFhirOrganization} read FCustodian write SetCustodian;

    {@member event
      The main event/act/item, such as a colonoscopy or an appendectomy, being documented.
    }
    property event : TFhirCompositionEvent read FEvent write SetEvent;

    {@member encounter
      Describes the clinical encounter or type of care this documentation is associated with.
    }
    property encounter : TFhirResourceReference{TFhirEncounter} read FEncounter write SetEncounter;

    {@member sectionList
      The root of the sections that make up the composition.
    }
    property sectionList : TFhirCompositionSectionList read FSectionList;

  end;


  {@Class TFhirConceptMap : TFhirResource
    A statement of relationships from one set of concepts to one or more other concepts - either code systems or data elements, or classes in class models.
  }
  {!.Net HL7Connect.Fhir.ConceptMap}
  TFhirConceptMap = class (TFhirResource)
  private
    FIdentifier : TFhirString;
    FVersion : TFhirString;
    FName : TFhirString;
    FPublisher : TFhirString;
    FtelecomList : TFhirContactList;
    FDescription : TFhirString;
    FCopyright : TFhirString;
    FStatus : TFhirEnum;
    FExperimental : TFhirBoolean;
    FDate : TFhirDateTime;
    FSource : TFhirType;
    FTarget : TFhirType;
    FelementList : TFhirConceptMapElementList;
    Procedure SetIdentifier(value : TFhirString);
    Function GetIdentifierST : String;
    Procedure SetIdentifierST(value : String);
    Procedure SetVersion(value : TFhirString);
    Function GetVersionST : String;
    Procedure SetVersionST(value : String);
    Procedure SetName(value : TFhirString);
    Function GetNameST : String;
    Procedure SetNameST(value : String);
    Procedure SetPublisher(value : TFhirString);
    Function GetPublisherST : String;
    Procedure SetPublisherST(value : String);
    Procedure SetDescription(value : TFhirString);
    Function GetDescriptionST : String;
    Procedure SetDescriptionST(value : String);
    Procedure SetCopyright(value : TFhirString);
    Function GetCopyrightST : String;
    Procedure SetCopyrightST(value : String);
    Procedure SetStatus(value : TFhirEnum);
    Function GetStatusST : TFhirValuesetStatus;
    Procedure SetStatusST(value : TFhirValuesetStatus);
    Procedure SetExperimental(value : TFhirBoolean);
    Function GetExperimentalST : Boolean;
    Procedure SetExperimentalST(value : Boolean);
    Procedure SetDate(value : TFhirDateTime);
    Function GetDateST : TDateAndTime;
    Procedure SetDateST(value : TDateAndTime);
    Procedure SetSource(value : TFhirType);
    Procedure SetTarget(value : TFhirType);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    Function GetHasASummary : Boolean; Override;
    function GetResourceType : TFhirResourceType; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirConceptMap; overload;
    function Clone : TFhirConceptMap; overload;
    {!script show}
  published
    {@member identifier
      The identifier that is used to identify this concept map when it is referenced in a specification, model, design or an instance (should be globally unique OID, UUID, or URI).
    }
    property identifier : TFhirString read FIdentifier write SetIdentifier;
    {@member identifierST
      Typed access to The identifier that is used to identify this concept map when it is referenced in a specification, model, design or an instance (should be globally unique OID, UUID, or URI).
    }
    property identifierST : String read GetIdentifierST write SetIdentifierST;

    {@member version
      The identifier that is used to identify this version of the concept map when it is referenced in a specification, model, design or instance. This is an arbitrary value managed by the profile author manually and the value should be a timestamp.
    }
    property version : TFhirString read FVersion write SetVersion;
    {@member versionST
      Typed access to The identifier that is used to identify this version of the concept map when it is referenced in a specification, model, design or instance. This is an arbitrary value managed by the profile author manually and the value should be a timestamp.
    }
    property versionST : String read GetVersionST write SetVersionST;

    {@member name
      A free text natural language name describing the concept map.
    }
    property name : TFhirString read FName write SetName;
    {@member nameST
      Typed access to A free text natural language name describing the concept map.
    }
    property nameST : String read GetNameST write SetNameST;

    {@member publisher
      The name of the individual or organization that published the concept map.
    }
    property publisher : TFhirString read FPublisher write SetPublisher;
    {@member publisherST
      Typed access to The name of the individual or organization that published the concept map.
    }
    property publisherST : String read GetPublisherST write SetPublisherST;

    {@member telecomList
      Contacts of the publisher to assist a user in finding and communicating with the publisher.
    }
    property telecomList : TFhirContactList read FTelecomList;

    {@member description
      A free text natural language description of the use of the concept map - reason for definition, conditions of use, etc.
    }
    property description : TFhirString read FDescription write SetDescription;
    {@member descriptionST
      Typed access to A free text natural language description of the use of the concept map - reason for definition, conditions of use, etc.
    }
    property descriptionST : String read GetDescriptionST write SetDescriptionST;

    {@member copyright
      A copyright statement relating to the concept map and/or its contents.
    }
    property copyright : TFhirString read FCopyright write SetCopyright;
    {@member copyrightST
      Typed access to A copyright statement relating to the concept map and/or its contents.
    }
    property copyrightST : String read GetCopyrightST write SetCopyrightST;

    {@member status
      The status of the concept map.
    }
    property status : TFhirEnum read FStatus write SetStatus;
    {@member statusST
      Typed access to The status of the concept map.
    }
    property statusST : TFhirValuesetStatus read GetStatusST write SetStatusST;

    {@member experimental
      This ConceptMap was authored for testing purposes (or education/evaluation/marketing), and is not intended to be used for genuine usage.
    }
    property experimental : TFhirBoolean read FExperimental write SetExperimental;
    {@member experimentalST
      Typed access to This ConceptMap was authored for testing purposes (or education/evaluation/marketing), and is not intended to be used for genuine usage.
    }
    property experimentalST : Boolean read GetExperimentalST write SetExperimentalST;

    {@member date
      The date that the concept map status was last changed.
    }
    property date : TFhirDateTime read FDate write SetDate;
    {@member dateST
      Typed access to The date that the concept map status was last changed.
    }
    property dateST : TDateAndTime read GetDateST write SetDateST;

    {@member source
      The source value set that specifies the concepts that are being mapped.
    }
    property source : TFhirType read FSource write SetSource;

    {@member target
      The target value set provides context to the mappings. Note that the mapping is made between concepts, not between value sets, but the value set provides important context about how the concept mapping choices are made.
    }
    property target : TFhirType read FTarget write SetTarget;

    {@member elementList
      Mappings for a concept from the source set.
    }
    property elementList : TFhirConceptMapElementList read FElementList;

  end;


  {@Class TFhirCondition : TFhirResource
    Use to record detailed information about conditions, problems or diagnoses recognized by a clinician. There are many uses including: recording a Diagnosis during an Encounter; populating a problem List or a Summary Statement, such as a Discharge Summary.
  }
  {!.Net HL7Connect.Fhir.Condition}
  TFhirCondition = class (TFhirResource)
  private
    FidentifierList : TFhirIdentifierList;
    FSubject : TFhirResourceReference{TFhirPatient};
    FEncounter : TFhirResourceReference{TFhirEncounter};
    FAsserter : TFhirResourceReference{Resource};
    FDateAsserted : TFhirDate;
    FCode : TFhirCodeableConcept;
    FCategory : TFhirCodeableConcept;
    FStatus : TFhirEnum;
    FCertainty : TFhirCodeableConcept;
    FSeverity : TFhirCodeableConcept;
    FOnset : TFhirType;
    FAbatement : TFhirType;
    FStage : TFhirConditionStage;
    FevidenceList : TFhirConditionEvidenceList;
    FlocationList : TFhirConditionLocationList;
    FrelatedItemList : TFhirConditionRelatedItemList;
    FNotes : TFhirString;
    Procedure SetSubject(value : TFhirResourceReference{TFhirPatient});
    Procedure SetEncounter(value : TFhirResourceReference{TFhirEncounter});
    Procedure SetAsserter(value : TFhirResourceReference{Resource});
    Procedure SetDateAsserted(value : TFhirDate);
    Function GetDateAssertedST : TDateAndTime;
    Procedure SetDateAssertedST(value : TDateAndTime);
    Procedure SetCode(value : TFhirCodeableConcept);
    Procedure SetCategory(value : TFhirCodeableConcept);
    Procedure SetStatus(value : TFhirEnum);
    Function GetStatusST : TFhirConditionStatus;
    Procedure SetStatusST(value : TFhirConditionStatus);
    Procedure SetCertainty(value : TFhirCodeableConcept);
    Procedure SetSeverity(value : TFhirCodeableConcept);
    Procedure SetOnset(value : TFhirType);
    Procedure SetAbatement(value : TFhirType);
    Procedure SetStage(value : TFhirConditionStage);
    Procedure SetNotes(value : TFhirString);
    Function GetNotesST : String;
    Procedure SetNotesST(value : String);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    Function GetHasASummary : Boolean; Override;
    function GetResourceType : TFhirResourceType; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirCondition; overload;
    function Clone : TFhirCondition; overload;
    {!script show}
  published
    {@member identifierList
      This records identifiers associated with this condition that are defined by business processed and/ or used to refer to it when a direct URL reference to the resource itself is not appropriate (e.g. in CDA documents, or in written / printed documentation).
    }
    property identifierList : TFhirIdentifierList read FIdentifierList;

    {@member subject
      Indicates the patient who the condition record is associated with.
    }
    property subject : TFhirResourceReference{TFhirPatient} read FSubject write SetSubject;

    {@member encounter
      Encounter during which the condition was first asserted.
    }
    property encounter : TFhirResourceReference{TFhirEncounter} read FEncounter write SetEncounter;

    {@member asserter
      Person who takes responsibility for asserting the existence of the condition as part of the electronic record.
    }
    property asserter : TFhirResourceReference{Resource} read FAsserter write SetAsserter;

    {@member dateAsserted
      Estimated or actual date the condition/problem/diagnosis was first detected/suspected.
    }
    property dateAsserted : TFhirDate read FDateAsserted write SetDateAsserted;
    {@member dateAssertedST
      Typed access to Estimated or actual date the condition/problem/diagnosis was first detected/suspected.
    }
    property dateAssertedST : TDateAndTime read GetDateAssertedST write SetDateAssertedST;

    {@member code
      Identification of the condition, problem or diagnosis.
    }
    property code : TFhirCodeableConcept read FCode write SetCode;

    {@member category
      A category assigned to the condition. E.g. complaint | symptom | finding | diagnosis.
    }
    property category : TFhirCodeableConcept read FCategory write SetCategory;

    {@member status
      The clinical status of the condition.
    }
    property status : TFhirEnum read FStatus write SetStatus;
    {@member statusST
      Typed access to The clinical status of the condition.
    }
    property statusST : TFhirConditionStatus read GetStatusST write SetStatusST;

    {@member certainty
      The degree of confidence that this condition is correct.
    }
    property certainty : TFhirCodeableConcept read FCertainty write SetCertainty;

    {@member severity
      A subjective assessment of the severity of the condition as evaluated by the clinician.
    }
    property severity : TFhirCodeableConcept read FSeverity write SetSeverity;

    {@member onset
      Estimated or actual date the condition began, in the opinion of the clinician.
    }
    property onset : TFhirType read FOnset write SetOnset;

    {@member abatement
      The date or estimated date that the condition resolved or went into remission. This is called "abatement" because of the many overloaded connotations associated with "remission" or "resolution" - Conditions are never really resolved, but they can abate.
    }
    property abatement : TFhirType read FAbatement write SetAbatement;

    {@member stage
      Clinical stage or grade of a condition. May include formal severity assessments.
    }
    property stage : TFhirConditionStage read FStage write SetStage;

    {@member evidenceList
      Supporting Evidence / manifestations that are the basis on which this condition is suspected or confirmed.
    }
    property evidenceList : TFhirConditionEvidenceList read FEvidenceList;

    {@member locationList
      The anatomical location where this condition manifests itself.
    }
    property locationList : TFhirConditionLocationList read FLocationList;

    {@member relatedItemList
      Further conditions, problems, diagnoses, procedures or events that are related in some way to this condition, or the substance that caused/triggered this Condition.
    }
    property relatedItemList : TFhirConditionRelatedItemList read FRelatedItemList;

    {@member notes
      Additional information about the Condition. This is a general notes/comments entry  for description of the Condition, its diagnosis and prognosis.
    }
    property notes : TFhirString read FNotes write SetNotes;
    {@member notesST
      Typed access to Additional information about the Condition. This is a general notes/comments entry  for description of the Condition, its diagnosis and prognosis.
    }
    property notesST : String read GetNotesST write SetNotesST;

  end;


  {@Class TFhirConformance : TFhirResource
    A conformance statement is a set of requirements for a desired implementation or a description of how a target application fulfills those requirements in a particular implementation.
  }
  {!.Net HL7Connect.Fhir.Conformance}
  TFhirConformance = class (TFhirResource)
  private
    FIdentifier : TFhirString;
    FVersion : TFhirString;
    FName : TFhirString;
    FPublisher : TFhirString;
    FtelecomList : TFhirContactList;
    FDescription : TFhirString;
    FStatus : TFhirEnum;
    FExperimental : TFhirBoolean;
    FDate : TFhirDateTime;
    FSoftware : TFhirConformanceSoftware;
    FImplementation_ : TFhirConformanceImplementation;
    FFhirVersion : TFhirId;
    FAcceptUnknown : TFhirBoolean;
    FformatList : TFhirCodeList;
    FprofileList : TFhirResourceReferenceList{TFhirProfile};
    FrestList : TFhirConformanceRestList;
    FmessagingList : TFhirConformanceMessagingList;
    FdocumentList : TFhirConformanceDocumentList;
    Procedure SetIdentifier(value : TFhirString);
    Function GetIdentifierST : String;
    Procedure SetIdentifierST(value : String);
    Procedure SetVersion(value : TFhirString);
    Function GetVersionST : String;
    Procedure SetVersionST(value : String);
    Procedure SetName(value : TFhirString);
    Function GetNameST : String;
    Procedure SetNameST(value : String);
    Procedure SetPublisher(value : TFhirString);
    Function GetPublisherST : String;
    Procedure SetPublisherST(value : String);
    Procedure SetDescription(value : TFhirString);
    Function GetDescriptionST : String;
    Procedure SetDescriptionST(value : String);
    Procedure SetStatus(value : TFhirEnum);
    Function GetStatusST : TFhirConformanceStatementStatus;
    Procedure SetStatusST(value : TFhirConformanceStatementStatus);
    Procedure SetExperimental(value : TFhirBoolean);
    Function GetExperimentalST : Boolean;
    Procedure SetExperimentalST(value : Boolean);
    Procedure SetDate(value : TFhirDateTime);
    Function GetDateST : TDateAndTime;
    Procedure SetDateST(value : TDateAndTime);
    Procedure SetSoftware(value : TFhirConformanceSoftware);
    Procedure SetImplementation_(value : TFhirConformanceImplementation);
    Procedure SetFhirVersion(value : TFhirId);
    Function GetFhirVersionST : String;
    Procedure SetFhirVersionST(value : String);
    Procedure SetAcceptUnknown(value : TFhirBoolean);
    Function GetAcceptUnknownST : Boolean;
    Procedure SetAcceptUnknownST(value : Boolean);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    Function GetHasASummary : Boolean; Override;
    function GetResourceType : TFhirResourceType; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirConformance; overload;
    function Clone : TFhirConformance; overload;
    {!script show}
  published
    {@member identifier
      The identifier that is used to identify this conformance statement when it is referenced in a specification, model, design or an instance (should be globally unique OID, UUID, or URI).
    }
    property identifier : TFhirString read FIdentifier write SetIdentifier;
    {@member identifierST
      Typed access to The identifier that is used to identify this conformance statement when it is referenced in a specification, model, design or an instance (should be globally unique OID, UUID, or URI).
    }
    property identifierST : String read GetIdentifierST write SetIdentifierST;

    {@member version
      The identifier that is used to identify this version of the conformance statement when it is referenced in a specification, model, design or instance. This is an arbitrary value managed by the profile author manually and the value should be a timestamp.
    }
    property version : TFhirString read FVersion write SetVersion;
    {@member versionST
      Typed access to The identifier that is used to identify this version of the conformance statement when it is referenced in a specification, model, design or instance. This is an arbitrary value managed by the profile author manually and the value should be a timestamp.
    }
    property versionST : String read GetVersionST write SetVersionST;

    {@member name
      A free text natural language name identifying the conformance statement.
    }
    property name : TFhirString read FName write SetName;
    {@member nameST
      Typed access to A free text natural language name identifying the conformance statement.
    }
    property nameST : String read GetNameST write SetNameST;

    {@member publisher
      Name of Organization publishing this conformance statement.
    }
    property publisher : TFhirString read FPublisher write SetPublisher;
    {@member publisherST
      Typed access to Name of Organization publishing this conformance statement.
    }
    property publisherST : String read GetPublisherST write SetPublisherST;

    {@member telecomList
      Contacts for Organization relevant to this conformance statement.  The contacts may be a website, email, phone numbers, etc.
    }
    property telecomList : TFhirContactList read FTelecomList;

    {@member description
      A free text natural language description of the conformance statement and its use. Typically, this is used when the profile describes a desired rather than an actual solution, for example as a formal expression of requirements as part of an RFP.
    }
    property description : TFhirString read FDescription write SetDescription;
    {@member descriptionST
      Typed access to A free text natural language description of the conformance statement and its use. Typically, this is used when the profile describes a desired rather than an actual solution, for example as a formal expression of requirements as part of an RFP.
    }
    property descriptionST : String read GetDescriptionST write SetDescriptionST;

    {@member status
      The status of this conformance statement.
    }
    property status : TFhirEnum read FStatus write SetStatus;
    {@member statusST
      Typed access to The status of this conformance statement.
    }
    property statusST : TFhirConformanceStatementStatus read GetStatusST write SetStatusST;

    {@member experimental
      A flag to indicate that this conformance statement is authored for testing purposes (or education/evaluation/marketing), and is not intended to be used for genuine usage.
    }
    property experimental : TFhirBoolean read FExperimental write SetExperimental;
    {@member experimentalST
      Typed access to A flag to indicate that this conformance statement is authored for testing purposes (or education/evaluation/marketing), and is not intended to be used for genuine usage.
    }
    property experimentalST : Boolean read GetExperimentalST write SetExperimentalST;

    {@member date
      The date when the conformance statement was published.
    }
    property date : TFhirDateTime read FDate write SetDate;
    {@member dateST
      Typed access to The date when the conformance statement was published.
    }
    property dateST : TDateAndTime read GetDateST write SetDateST;

    {@member software
      Software that is covered by this conformance statement.  It is used when the profile describes the capabilities of a particular software version, independent of an installation.
    }
    property software : TFhirConformanceSoftware read FSoftware write SetSoftware;

    {@member implementation_
      Identifies a specific implementation instance that is described by the conformance statement - i.e. a particular installation, rather than the capabilities of a software program.
    }
    property implementation_ : TFhirConformanceImplementation read FImplementation_ write SetImplementation_;

    {@member fhirVersion
      The version of the FHIR specification on which this conformance statement is based.
    }
    property fhirVersion : TFhirId read FFhirVersion write SetFhirVersion;
    {@member fhirVersionST
      Typed access to The version of the FHIR specification on which this conformance statement is based.
    }
    property fhirVersionST : String read GetFhirVersionST write SetFhirVersionST;

    {@member acceptUnknown
      A flag that indicates whether the application accepts unknown elements as part of a resource.
    }
    property acceptUnknown : TFhirBoolean read FAcceptUnknown write SetAcceptUnknown;
    {@member acceptUnknownST
      Typed access to A flag that indicates whether the application accepts unknown elements as part of a resource.
    }
    property acceptUnknownST : Boolean read GetAcceptUnknownST write SetAcceptUnknownST;

    {@member formatList
      A list of the formats supported by this implementation.
    }
    property formatList : TFhirCodeList read FFormatList;

    {@member profileList
      A list of profiles supported by the system. For a server, "supported by the system" means the system hosts/produces a set of recourses, conformant to a particular profile, and allows its clients to search using this profile and to find appropriate data. For a client, it means the system will search by this profile and process data according to the guidance implicit in the profile.
    }
    property profileList : TFhirResourceReferenceList{TFhirProfile} read FProfileList;

    {@member restList
      A definition of the restful capabilities of the solution, if any.
    }
    property restList : TFhirConformanceRestList read FRestList;

    {@member messagingList
      A description of the messaging capabilities of the solution.
    }
    property messagingList : TFhirConformanceMessagingList read FMessagingList;

    {@member documentList
      A document definition.
    }
    property documentList : TFhirConformanceDocumentList read FDocumentList;

  end;


  {@Class TFhirContraindication : TFhirResource
    Indicates an actual or potential clinical issue with or between one or more active or proposed clinical actions for a patient.  E.g. Drug-drug interaction, Ineffective treatment frequency, Procedure-condition conflict, etc.
  }
  {!.Net HL7Connect.Fhir.Contraindication}
  TFhirContraindication = class (TFhirResource)
  private
    FPatient : TFhirResourceReference{TFhirPatient};
    FCategory : TFhirCodeableConcept;
    FSeverity : TFhirCode;
    FimplicatedList : TFhirResourceReferenceList{Resource};
    FDetail : TFhirString;
    FDate : TFhirDateTime;
    FAuthor : TFhirResourceReference{Resource};
    FIdentifier : TFhirIdentifier;
    FReference : TFhirUri;
    FmitigationList : TFhirContraindicationMitigationList;
    Procedure SetPatient(value : TFhirResourceReference{TFhirPatient});
    Procedure SetCategory(value : TFhirCodeableConcept);
    Procedure SetSeverity(value : TFhirCode);
    Function GetSeverityST : String;
    Procedure SetSeverityST(value : String);
    Procedure SetDetail(value : TFhirString);
    Function GetDetailST : String;
    Procedure SetDetailST(value : String);
    Procedure SetDate(value : TFhirDateTime);
    Function GetDateST : TDateAndTime;
    Procedure SetDateST(value : TDateAndTime);
    Procedure SetAuthor(value : TFhirResourceReference{Resource});
    Procedure SetIdentifier(value : TFhirIdentifier);
    Procedure SetReference(value : TFhirUri);
    Function GetReferenceST : String;
    Procedure SetReferenceST(value : String);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    Function GetHasASummary : Boolean; Override;
    function GetResourceType : TFhirResourceType; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirContraindication; overload;
    function Clone : TFhirContraindication; overload;
    {!script show}
  published
    {@member patient
      Indicates the patient whose record the contraindication is associated with.
    }
    property patient : TFhirResourceReference{TFhirPatient} read FPatient write SetPatient;

    {@member category
      Identifies the general type of issue identified.
    }
    property category : TFhirCodeableConcept read FCategory write SetCategory;

    {@member severity
      Indicates the degree of importance associated with the identified issue based on the potential impact on the patient.
    }
    property severity : TFhirCode read FSeverity write SetSeverity;
    {@member severityST
      Typed access to Indicates the degree of importance associated with the identified issue based on the potential impact on the patient.
    }
    property severityST : String read GetSeverityST write SetSeverityST;

    {@member implicatedList
      Indicates the resource representing the current activity or proposed activity that.
    }
    property implicatedList : TFhirResourceReferenceList{Resource} read FImplicatedList;

    {@member detail
      A textual explanation of the contraindication.
    }
    property detail : TFhirString read FDetail write SetDetail;
    {@member detailST
      Typed access to A textual explanation of the contraindication.
    }
    property detailST : String read GetDetailST write SetDetailST;

    {@member date
      The date or date-time when the contraindication was initially identified.
    }
    property date : TFhirDateTime read FDate write SetDate;
    {@member dateST
      Typed access to The date or date-time when the contraindication was initially identified.
    }
    property dateST : TDateAndTime read GetDateST write SetDateST;

    {@member author
      Identifies the provider or software that identified the.
    }
    property author : TFhirResourceReference{Resource} read FAuthor write SetAuthor;

    {@member identifier
      Business identifier associated with the contraindication record.
    }
    property identifier : TFhirIdentifier read FIdentifier write SetIdentifier;

    {@member reference
      The literature, knowledge-base or similar reference that describes the propensity for the contraindication identified.
    }
    property reference : TFhirUri read FReference write SetReference;
    {@member referenceST
      Typed access to The literature, knowledge-base or similar reference that describes the propensity for the contraindication identified.
    }
    property referenceST : String read GetReferenceST write SetReferenceST;

    {@member mitigationList
      Indicates an action that has been taken or is committed to to reduce or eliminate the likelihood of the risk identified by the contraindicaiton from manifesting.  Can also reflect an observation of known mitigating factors that may reduce/eliminate the need for any action.
    }
    property mitigationList : TFhirContraindicationMitigationList read FMitigationList;

  end;


  {@Class TFhirDataElement : TFhirResource
    The formal description of a single piece of information that can be gathered and reported.
  }
  {!.Net HL7Connect.Fhir.DataElement}
  TFhirDataElement = class (TFhirResource)
  private
    FIdentifier : TFhirIdentifier;
    FVersion : TFhirString;
    FPublisher : TFhirString;
    FtelecomList : TFhirContactList;
    FStatus : TFhirEnum;
    FDate : TFhirDateTime;
    FName : TFhirString;
    FcategoryList : TFhirCodeableConceptList;
    FcodeList : TFhirCodingList;
    FQuestion : TFhirString;
    FDefinition : TFhirString;
    FComments : TFhirString;
    FRequirements : TFhirString;
    FsynonymList : TFhirStringList;
    FType_ : TFhirCode;
    FExample : TFhirType;
    FMaxLength : TFhirInteger;
    FUnits : TFhirCodeableConcept;
    FBinding : TFhirDataElementBinding;
    FmappingList : TFhirDataElementMappingList;
    Procedure SetIdentifier(value : TFhirIdentifier);
    Procedure SetVersion(value : TFhirString);
    Function GetVersionST : String;
    Procedure SetVersionST(value : String);
    Procedure SetPublisher(value : TFhirString);
    Function GetPublisherST : String;
    Procedure SetPublisherST(value : String);
    Procedure SetStatus(value : TFhirEnum);
    Function GetStatusST : TFhirResourceObservationDefStatus;
    Procedure SetStatusST(value : TFhirResourceObservationDefStatus);
    Procedure SetDate(value : TFhirDateTime);
    Function GetDateST : TDateAndTime;
    Procedure SetDateST(value : TDateAndTime);
    Procedure SetName(value : TFhirString);
    Function GetNameST : String;
    Procedure SetNameST(value : String);
    Procedure SetQuestion(value : TFhirString);
    Function GetQuestionST : String;
    Procedure SetQuestionST(value : String);
    Procedure SetDefinition(value : TFhirString);
    Function GetDefinitionST : String;
    Procedure SetDefinitionST(value : String);
    Procedure SetComments(value : TFhirString);
    Function GetCommentsST : String;
    Procedure SetCommentsST(value : String);
    Procedure SetRequirements(value : TFhirString);
    Function GetRequirementsST : String;
    Procedure SetRequirementsST(value : String);
    Procedure SetType_(value : TFhirCode);
    Function GetType_ST : String;
    Procedure SetType_ST(value : String);
    Procedure SetExample(value : TFhirType);
    Procedure SetMaxLength(value : TFhirInteger);
    Function GetMaxLengthST : String;
    Procedure SetMaxLengthST(value : String);
    Procedure SetUnits(value : TFhirCodeableConcept);
    Procedure SetBinding(value : TFhirDataElementBinding);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    Function GetHasASummary : Boolean; Override;
    function GetResourceType : TFhirResourceType; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirDataElement; overload;
    function Clone : TFhirDataElement; overload;
    {!script show}
  published
    {@member identifier
      The identifier that is used to identify this data element when it is referenced in a Profile, Questionnaire or an instance.
    }
    property identifier : TFhirIdentifier read FIdentifier write SetIdentifier;

    {@member version
      The identifier that is used to identify this version of the data element when it is referenced in a Profile, Questionnaire or instance. This is an arbitrary value managed by the definition author manually.
    }
    property version : TFhirString read FVersion write SetVersion;
    {@member versionST
      Typed access to The identifier that is used to identify this version of the data element when it is referenced in a Profile, Questionnaire or instance. This is an arbitrary value managed by the definition author manually.
    }
    property versionST : String read GetVersionST write SetVersionST;

    {@member publisher
      Details of the individual or organization who accepts responsibility for publishing the data element.
    }
    property publisher : TFhirString read FPublisher write SetPublisher;
    {@member publisherST
      Typed access to Details of the individual or organization who accepts responsibility for publishing the data element.
    }
    property publisherST : String read GetPublisherST write SetPublisherST;

    {@member telecomList
      Contact details to assist a user in finding and communicating with the publisher.
    }
    property telecomList : TFhirContactList read FTelecomList;

    {@member status
      The status of the data element.
    }
    property status : TFhirEnum read FStatus write SetStatus;
    {@member statusST
      Typed access to The status of the data element.
    }
    property statusST : TFhirResourceObservationDefStatus read GetStatusST write SetStatusST;

    {@member date
      The date that this version of the data element was published.
    }
    property date : TFhirDateTime read FDate write SetDate;
    {@member dateST
      Typed access to The date that this version of the data element was published.
    }
    property dateST : TDateAndTime read GetDateST write SetDateST;

    {@member name
      The term used by humans to refer to the data element.  Should ideally be unique within the context in which the data element is expected to be used.
    }
    property name : TFhirString read FName write SetName;
    {@member nameST
      Typed access to The term used by humans to refer to the data element.  Should ideally be unique within the context in which the data element is expected to be used.
    }
    property nameST : String read GetNameST write SetNameST;

    {@member categoryList
      A set of terms from external terminologies that may be used to assist with indexing and searching of data element definitions.
    }
    property categoryList : TFhirCodeableConceptList read FCategoryList;

    {@member codeList
      A code that provides the meaning for a data element according to a particular terminology.
    }
    property codeList : TFhirCodingList read FCodeList;

    {@member question
      The default/suggested phrasing to use when prompting a human to capture the data element.
    }
    property question : TFhirString read FQuestion write SetQuestion;
    {@member questionST
      Typed access to The default/suggested phrasing to use when prompting a human to capture the data element.
    }
    property questionST : String read GetQuestionST write SetQuestionST;

    {@member definition
      Provides a complete explanation of the meaning of the data element for human readability.
    }
    property definition : TFhirString read FDefinition write SetDefinition;
    {@member definitionST
      Typed access to Provides a complete explanation of the meaning of the data element for human readability.
    }
    property definitionST : String read GetDefinitionST write SetDefinitionST;

    {@member comments
      Comments about the use of the element, including notes about how to use the data properly, exceptions to proper use, etc.
    }
    property comments : TFhirString read FComments write SetComments;
    {@member commentsST
      Typed access to Comments about the use of the element, including notes about how to use the data properly, exceptions to proper use, etc.
    }
    property commentsST : String read GetCommentsST write SetCommentsST;

    {@member requirements
      Explains why this element is needed and why it's been constrained as it has.
    }
    property requirements : TFhirString read FRequirements write SetRequirements;
    {@member requirementsST
      Typed access to Explains why this element is needed and why it's been constrained as it has.
    }
    property requirementsST : String read GetRequirementsST write SetRequirementsST;

    {@member synonymList
      Identifies additional names by which this element might also be known.
    }
    property synonymList : TFhirStringList read FSynonymList;

    {@member type_
      The FHIR data type that is the type for this element.
    }
    property type_ : TFhirCode read FType_ write SetType_;
    {@member type_ST
      Typed access to The FHIR data type that is the type for this element.
    }
    property type_ST : String read GetType_ST write SetType_ST;

    {@member example
      An example value for this element.
    }
    property example : TFhirType read FExample write SetExample;

    {@member maxLength
      Indicates the shortest length that SHALL be supported by conformant instances without truncation.
    }
    property maxLength : TFhirInteger read FMaxLength write SetMaxLength;
    {@member maxLengthST
      Typed access to Indicates the shortest length that SHALL be supported by conformant instances without truncation.
    }
    property maxLengthST : String read GetMaxLengthST write SetMaxLengthST;

    {@member units
      Identifies the units of measure in which the data element should be captured or expressed.
    }
    property units : TFhirCodeableConcept read FUnits write SetUnits;

    {@member binding
      Binds to a value set if this element is coded (code, Coding, CodeableConcept).
    }
    property binding : TFhirDataElementBinding read FBinding write SetBinding;

    {@member mappingList
      Identifies a concept from an external specification that roughly corresponds to this element.
    }
    property mappingList : TFhirDataElementMappingList read FMappingList;

  end;


  {@Class TFhirDevice : TFhirResource
    This resource identifies an instance of a manufactured thing that is used in the provision of healthcare without being substantially changed through that activity. The device may be a machine, an insert, a computer, an application, etc. This includes durable (reusable) medical equipment as well as disposable equipment used for diagnostic, treatment, and research for healthcare and public health.
  }
  {!.Net HL7Connect.Fhir.Device}
  TFhirDevice = class (TFhirResource)
  private
    FidentifierList : TFhirIdentifierList;
    FType_ : TFhirCodeableConcept;
    FManufacturer : TFhirString;
    FModel : TFhirString;
    FVersion : TFhirString;
    FExpiry : TFhirDate;
    FUdi : TFhirString;
    FLotNumber : TFhirString;
    FOwner : TFhirResourceReference{TFhirOrganization};
    FLocation : TFhirResourceReference{TFhirLocation};
    FPatient : TFhirResourceReference{TFhirPatient};
    FcontactList : TFhirContactList;
    FUrl : TFhirUri;
    Procedure SetType_(value : TFhirCodeableConcept);
    Procedure SetManufacturer(value : TFhirString);
    Function GetManufacturerST : String;
    Procedure SetManufacturerST(value : String);
    Procedure SetModel(value : TFhirString);
    Function GetModelST : String;
    Procedure SetModelST(value : String);
    Procedure SetVersion(value : TFhirString);
    Function GetVersionST : String;
    Procedure SetVersionST(value : String);
    Procedure SetExpiry(value : TFhirDate);
    Function GetExpiryST : TDateAndTime;
    Procedure SetExpiryST(value : TDateAndTime);
    Procedure SetUdi(value : TFhirString);
    Function GetUdiST : String;
    Procedure SetUdiST(value : String);
    Procedure SetLotNumber(value : TFhirString);
    Function GetLotNumberST : String;
    Procedure SetLotNumberST(value : String);
    Procedure SetOwner(value : TFhirResourceReference{TFhirOrganization});
    Procedure SetLocation(value : TFhirResourceReference{TFhirLocation});
    Procedure SetPatient(value : TFhirResourceReference{TFhirPatient});
    Procedure SetUrl(value : TFhirUri);
    Function GetUrlST : String;
    Procedure SetUrlST(value : String);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    Function GetHasASummary : Boolean; Override;
    function GetResourceType : TFhirResourceType; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirDevice; overload;
    function Clone : TFhirDevice; overload;
    {!script show}
  published
    {@member identifierList
      Identifiers assigned to this device by various organizations. The most likely organizations to assign identifiers are the manufacturer and the owner, though regulatory agencies may also assign an identifier. The identifiers identify the particular device, not the kind of device.
    }
    property identifierList : TFhirIdentifierList read FIdentifierList;

    {@member type_
      A kind of this device.
    }
    property type_ : TFhirCodeableConcept read FType_ write SetType_;

    {@member manufacturer
      A name of the manufacturer.
    }
    property manufacturer : TFhirString read FManufacturer write SetManufacturer;
    {@member manufacturerST
      Typed access to A name of the manufacturer.
    }
    property manufacturerST : String read GetManufacturerST write SetManufacturerST;

    {@member model
      The "model" - an identifier assigned by the manufacturer to identify the product by its type. This number is shared by the all devices sold as the same type.
    }
    property model : TFhirString read FModel write SetModel;
    {@member modelST
      Typed access to The "model" - an identifier assigned by the manufacturer to identify the product by its type. This number is shared by the all devices sold as the same type.
    }
    property modelST : String read GetModelST write SetModelST;

    {@member version
      The version of the device, if the device has multiple releases under the same model, or if the device is software or carries firmware.
    }
    property version : TFhirString read FVersion write SetVersion;
    {@member versionST
      Typed access to The version of the device, if the device has multiple releases under the same model, or if the device is software or carries firmware.
    }
    property versionST : String read GetVersionST write SetVersionST;

    {@member expiry
      Date of expiry of this device (if applicable).
    }
    property expiry : TFhirDate read FExpiry write SetExpiry;
    {@member expiryST
      Typed access to Date of expiry of this device (if applicable).
    }
    property expiryST : TDateAndTime read GetExpiryST write SetExpiryST;

    {@member udi
      FDA Mandated Unique Device Identifier. Use the human readable information (the content that the user sees, which is sometimes different to the exact syntax represented in the barcode)  - see http://www.fda.gov/MedicalDevices/DeviceRegulationandGuidance/UniqueDeviceIdentification/default.htm.
    }
    property udi : TFhirString read FUdi write SetUdi;
    {@member udiST
      Typed access to FDA Mandated Unique Device Identifier. Use the human readable information (the content that the user sees, which is sometimes different to the exact syntax represented in the barcode)  - see http://www.fda.gov/MedicalDevices/DeviceRegulationandGuidance/UniqueDeviceIdentification/default.htm.
    }
    property udiST : String read GetUdiST write SetUdiST;

    {@member lotNumber
      Lot number assigned by the manufacturer.
    }
    property lotNumber : TFhirString read FLotNumber write SetLotNumber;
    {@member lotNumberST
      Typed access to Lot number assigned by the manufacturer.
    }
    property lotNumberST : String read GetLotNumberST write SetLotNumberST;

    {@member owner
      An organization that is responsible for the provision and ongoing maintenance of the device.
    }
    property owner : TFhirResourceReference{TFhirOrganization} read FOwner write SetOwner;

    {@member location
      The resource may be found in a literal location (i.e. GPS coordinates), a logical place (i.e. "in/with the patient"), or a coded location.
    }
    property location : TFhirResourceReference{TFhirLocation} read FLocation write SetLocation;

    {@member patient
      Patient information, if the resource is affixed to a person.
    }
    property patient : TFhirResourceReference{TFhirPatient} read FPatient write SetPatient;

    {@member contactList
      Contact details for an organization or a particular human that is responsible for the device.
    }
    property contactList : TFhirContactList read FContactList;

    {@member url
      A network address on which the device may be contacted directly.
    }
    property url : TFhirUri read FUrl write SetUrl;
    {@member urlST
      Typed access to A network address on which the device may be contacted directly.
    }
    property urlST : String read GetUrlST write SetUrlST;

  end;


  {@Class TFhirDeviceObservationReport : TFhirResource
    Describes the data produced by a device at a point in time.
  }
  {!.Net HL7Connect.Fhir.DeviceObservationReport}
  TFhirDeviceObservationReport = class (TFhirResource)
  private
    FInstant : TFhirInstant;
    FIdentifier : TFhirIdentifier;
    FSource : TFhirResourceReference{TFhirDevice};
    FSubject : TFhirResourceReference{Resource};
    FvirtualDeviceList : TFhirDeviceObservationReportVirtualDeviceList;
    Procedure SetInstant(value : TFhirInstant);
    Function GetInstantST : TDateAndTime;
    Procedure SetInstantST(value : TDateAndTime);
    Procedure SetIdentifier(value : TFhirIdentifier);
    Procedure SetSource(value : TFhirResourceReference{TFhirDevice});
    Procedure SetSubject(value : TFhirResourceReference{Resource});
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    Function GetHasASummary : Boolean; Override;
    function GetResourceType : TFhirResourceType; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirDeviceObservationReport; overload;
    function Clone : TFhirDeviceObservationReport; overload;
    {!script show}
  published
    {@member instant
      The point in time that the values are reported.
    }
    property instant : TFhirInstant read FInstant write SetInstant;
    {@member instantST
      Typed access to The point in time that the values are reported.
    }
    property instantST : TDateAndTime read GetInstantST write SetInstantST;

    {@member identifier
      An identifier assigned to this observation bu the source device that made the observation.
    }
    property identifier : TFhirIdentifier read FIdentifier write SetIdentifier;

    {@member source
      Identification information for the device that is the source of the data.
    }
    property source : TFhirResourceReference{TFhirDevice} read FSource write SetSource;

    {@member subject
      The subject of the measurement.
    }
    property subject : TFhirResourceReference{Resource} read FSubject write SetSubject;

    {@member virtualDeviceList
      A medical-related subsystem of a medical device.
    }
    property virtualDeviceList : TFhirDeviceObservationReportVirtualDeviceList read FVirtualDeviceList;

  end;


  {@Class TFhirDiagnosticOrder : TFhirResource
    A request for a diagnostic investigation service to be performed.
  }
  {!.Net HL7Connect.Fhir.DiagnosticOrder}
  TFhirDiagnosticOrder = class (TFhirResource)
  private
    FSubject : TFhirResourceReference{Resource};
    FOrderer : TFhirResourceReference{TFhirPractitioner};
    FidentifierList : TFhirIdentifierList;
    FEncounter : TFhirResourceReference{TFhirEncounter};
    FClinicalNotes : TFhirString;
    FsupportingInformationList : TFhirResourceReferenceList{Resource};
    FspecimenList : TFhirResourceReferenceList{TFhirSpecimen};
    FStatus : TFhirEnum;
    FPriority : TFhirEnum;
    FeventList : TFhirDiagnosticOrderEventList;
    FitemList : TFhirDiagnosticOrderItemList;
    Procedure SetSubject(value : TFhirResourceReference{Resource});
    Procedure SetOrderer(value : TFhirResourceReference{TFhirPractitioner});
    Procedure SetEncounter(value : TFhirResourceReference{TFhirEncounter});
    Procedure SetClinicalNotes(value : TFhirString);
    Function GetClinicalNotesST : String;
    Procedure SetClinicalNotesST(value : String);
    Procedure SetStatus(value : TFhirEnum);
    Function GetStatusST : TFhirDiagnosticOrderStatus;
    Procedure SetStatusST(value : TFhirDiagnosticOrderStatus);
    Procedure SetPriority(value : TFhirEnum);
    Function GetPriorityST : TFhirDiagnosticOrderPriority;
    Procedure SetPriorityST(value : TFhirDiagnosticOrderPriority);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    Function GetHasASummary : Boolean; Override;
    function GetResourceType : TFhirResourceType; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirDiagnosticOrder; overload;
    function Clone : TFhirDiagnosticOrder; overload;
    {!script show}
  published
    {@member subject
      Who or what the investigation is to be performed on. This is usually a human patient, but diagnostic tests can also be requested on animals, groups of humans or animals, devices such as dialysis machines, or even locations (typically for environmental scans).
    }
    property subject : TFhirResourceReference{Resource} read FSubject write SetSubject;

    {@member orderer
      The practitioner that holds legal responsibility for ordering the investigation.
    }
    property orderer : TFhirResourceReference{TFhirPractitioner} read FOrderer write SetOrderer;

    {@member identifierList
      Identifiers assigned to this order by the order or by the receiver.
    }
    property identifierList : TFhirIdentifierList read FIdentifierList;

    {@member encounter
      An encounter that provides additional information about the healthcare context in which this request is made.
    }
    property encounter : TFhirResourceReference{TFhirEncounter} read FEncounter write SetEncounter;

    {@member clinicalNotes
      An explanation or justification for why this diagnostic investigation is being requested.
    }
    property clinicalNotes : TFhirString read FClinicalNotes write SetClinicalNotes;
    {@member clinicalNotesST
      Typed access to An explanation or justification for why this diagnostic investigation is being requested.
    }
    property clinicalNotesST : String read GetClinicalNotesST write SetClinicalNotesST;

    {@member supportingInformationList
      Additional clinical information about the patient or specimen that may influence test interpretations.
    }
    property supportingInformationList : TFhirResourceReferenceList{Resource} read FSupportingInformationList;

    {@member specimenList
      One or more specimens that the diagnostic investigation is about.
    }
    property specimenList : TFhirResourceReferenceList{TFhirSpecimen} read FSpecimenList;

    {@member status
      The status of the order.
    }
    property status : TFhirEnum read FStatus write SetStatus;
    {@member statusST
      Typed access to The status of the order.
    }
    property statusST : TFhirDiagnosticOrderStatus read GetStatusST write SetStatusST;

    {@member priority
      The clinical priority associated with this order.
    }
    property priority : TFhirEnum read FPriority write SetPriority;
    {@member priorityST
      Typed access to The clinical priority associated with this order.
    }
    property priorityST : TFhirDiagnosticOrderPriority read GetPriorityST write SetPriorityST;

    {@member eventList
      A summary of the events of interest that have occurred as the request is processed. E.g. when the order was made, various processing steps (specimens received), when it was completed.
    }
    property eventList : TFhirDiagnosticOrderEventList read FEventList;

    {@member itemList
      The specific diagnostic investigations that are requested as part of this request. Sometimes, there can only be one item per request, but in most contexts, more than one investigation can be requested.
    }
    property itemList : TFhirDiagnosticOrderItemList read FItemList;

  end;


  {@Class TFhirDiagnosticReport : TFhirResource
    The findings and interpretation of diagnostic  tests performed on patients, groups of patients, devices, and locations, and/or specimens derived from these. The report includes clinical context such as requesting and provider information, and some mix of atomic results, images, textual and coded interpretation, and formatted representation of diagnostic reports.
  }
  {!.Net HL7Connect.Fhir.DiagnosticReport}
  TFhirDiagnosticReport = class (TFhirResource)
  private
    FName : TFhirCodeableConcept;
    FStatus : TFhirEnum;
    FIssued : TFhirDateTime;
    FSubject : TFhirResourceReference{Resource};
    FPerformer : TFhirResourceReference{Resource};
    FIdentifier : TFhirIdentifier;
    FrequestDetailList : TFhirResourceReferenceList{TFhirDiagnosticOrder};
    FServiceCategory : TFhirCodeableConcept;
    FDiagnostic : TFhirType;
    FspecimenList : TFhirResourceReferenceList{TFhirSpecimen};
    FresultList : TFhirResourceReferenceList{TFhirObservation};
    FimagingStudyList : TFhirResourceReferenceList{TFhirImagingStudy};
    FimageList : TFhirDiagnosticReportImageList;
    FConclusion : TFhirString;
    FcodedDiagnosisList : TFhirCodeableConceptList;
    FpresentedFormList : TFhirAttachmentList;
    Procedure SetName(value : TFhirCodeableConcept);
    Procedure SetStatus(value : TFhirEnum);
    Function GetStatusST : TFhirDiagnosticReportStatus;
    Procedure SetStatusST(value : TFhirDiagnosticReportStatus);
    Procedure SetIssued(value : TFhirDateTime);
    Function GetIssuedST : TDateAndTime;
    Procedure SetIssuedST(value : TDateAndTime);
    Procedure SetSubject(value : TFhirResourceReference{Resource});
    Procedure SetPerformer(value : TFhirResourceReference{Resource});
    Procedure SetIdentifier(value : TFhirIdentifier);
    Procedure SetServiceCategory(value : TFhirCodeableConcept);
    Procedure SetDiagnostic(value : TFhirType);
    Procedure SetConclusion(value : TFhirString);
    Function GetConclusionST : String;
    Procedure SetConclusionST(value : String);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    Function GetHasASummary : Boolean; Override;
    function GetResourceType : TFhirResourceType; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirDiagnosticReport; overload;
    function Clone : TFhirDiagnosticReport; overload;
    {!script show}
  published
    {@member name
      A code or name that describes this diagnostic report.
    }
    property name : TFhirCodeableConcept read FName write SetName;

    {@member status
      The status of the diagnostic report as a whole.
    }
    property status : TFhirEnum read FStatus write SetStatus;
    {@member statusST
      Typed access to The status of the diagnostic report as a whole.
    }
    property statusST : TFhirDiagnosticReportStatus read GetStatusST write SetStatusST;

    {@member issued
      The date and/or time that this version of the report was released from the source diagnostic service.
    }
    property issued : TFhirDateTime read FIssued write SetIssued;
    {@member issuedST
      Typed access to The date and/or time that this version of the report was released from the source diagnostic service.
    }
    property issuedST : TDateAndTime read GetIssuedST write SetIssuedST;

    {@member subject
      The subject of the report. Usually, but not always, this is a patient. However diagnostic services also perform analyses on specimens collected from a variety of other sources.
    }
    property subject : TFhirResourceReference{Resource} read FSubject write SetSubject;

    {@member performer
      The diagnostic service that is responsible for issuing the report.
    }
    property performer : TFhirResourceReference{Resource} read FPerformer write SetPerformer;

    {@member identifier
      The local ID assigned to the report by the order filler, usually by the Information System of the diagnostic service provider.
    }
    property identifier : TFhirIdentifier read FIdentifier write SetIdentifier;

    {@member requestDetailList
      Details concerning a test requested.
    }
    property requestDetailList : TFhirResourceReferenceList{TFhirDiagnosticOrder} read FRequestDetailList;

    {@member serviceCategory
      The section of the diagnostic service that performs the examination e.g. biochemistry, hematology, MRI.
    }
    property serviceCategory : TFhirCodeableConcept read FServiceCategory write SetServiceCategory;

    {@member diagnostic
      The time or time-period the observed values are related to. This is usually either the time of the procedure or of specimen collection(s), but very often the source of the date/time is not known, only the date/time itself.
    }
    property diagnostic : TFhirType read FDiagnostic write SetDiagnostic;

    {@member specimenList
      Details about the specimens on which this Disagnostic report is based.
    }
    property specimenList : TFhirResourceReferenceList{TFhirSpecimen} read FSpecimenList;

    {@member resultList
      Observations that are part of this diagnostic report. Observations can be simple name/value pairs (e.g. "atomic" results), or they can be grouping observations that include references to other members of the group (e.g. "panels").
    }
    property resultList : TFhirResourceReferenceList{TFhirObservation} read FResultList;

    {@member imagingStudyList
      One or more links to full details of any imaging performed during the diagnostic investigation. Typically, this is imaging performed by DICOM enabled modalities, but this is not required. A fully enabled PACS viewer can use this information to provide views of the source images.
    }
    property imagingStudyList : TFhirResourceReferenceList{TFhirImagingStudy} read FImagingStudyList;

    {@member imageList
      A list of key images associated with this report. The images are generally created during the diagnostic process, and may be directly of the patient, or of treated specimens (i.e. slides of interest).
    }
    property imageList : TFhirDiagnosticReportImageList read FImageList;

    {@member conclusion
      Concise and clinically contextualized narrative interpretation of the diagnostic report.
    }
    property conclusion : TFhirString read FConclusion write SetConclusion;
    {@member conclusionST
      Typed access to Concise and clinically contextualized narrative interpretation of the diagnostic report.
    }
    property conclusionST : String read GetConclusionST write SetConclusionST;

    {@member codedDiagnosisList
      Codes for the conclusion.
    }
    property codedDiagnosisList : TFhirCodeableConceptList read FCodedDiagnosisList;

    {@member presentedFormList
      Rich text representation of the entire result as issued by the diagnostic service. Multiple formats are allowed but they SHALL be semantically equivalent.
    }
    property presentedFormList : TFhirAttachmentList read FPresentedFormList;

  end;


  {@Class TFhirDocumentManifest : TFhirResource
    A manifest that defines a set of documents.
  }
  {!.Net HL7Connect.Fhir.DocumentManifest}
  TFhirDocumentManifest = class (TFhirResource)
  private
    FMasterIdentifier : TFhirIdentifier;
    FidentifierList : TFhirIdentifierList;
    FsubjectList : TFhirResourceReferenceList{Resource};
    FrecipientList : TFhirResourceReferenceList{Resource};
    FType_ : TFhirCodeableConcept;
    FauthorList : TFhirResourceReferenceList{Resource};
    FCreated : TFhirDateTime;
    FSource : TFhirUri;
    FStatus : TFhirEnum;
    FSupercedes : TFhirResourceReference{TFhirDocumentManifest};
    FDescription : TFhirString;
    FConfidentiality : TFhirCodeableConcept;
    FcontentList : TFhirResourceReferenceList{Resource};
    Procedure SetMasterIdentifier(value : TFhirIdentifier);
    Procedure SetType_(value : TFhirCodeableConcept);
    Procedure SetCreated(value : TFhirDateTime);
    Function GetCreatedST : TDateAndTime;
    Procedure SetCreatedST(value : TDateAndTime);
    Procedure SetSource(value : TFhirUri);
    Function GetSourceST : String;
    Procedure SetSourceST(value : String);
    Procedure SetStatus(value : TFhirEnum);
    Function GetStatusST : TFhirDocumentReferenceStatus;
    Procedure SetStatusST(value : TFhirDocumentReferenceStatus);
    Procedure SetSupercedes(value : TFhirResourceReference{TFhirDocumentManifest});
    Procedure SetDescription(value : TFhirString);
    Function GetDescriptionST : String;
    Procedure SetDescriptionST(value : String);
    Procedure SetConfidentiality(value : TFhirCodeableConcept);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    Function GetHasASummary : Boolean; Override;
    function GetResourceType : TFhirResourceType; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirDocumentManifest; overload;
    function Clone : TFhirDocumentManifest; overload;
    {!script show}
  published
    {@member masterIdentifier
      A single identifier that uniquely identifies this manifest. Principally used to refer to the manifest in non-FHIR contexts.
    }
    property masterIdentifier : TFhirIdentifier read FMasterIdentifier write SetMasterIdentifier;

    {@member identifierList
      Other identifiers associated with the document, including version independent, source record and workflow related identifiers.
    }
    property identifierList : TFhirIdentifierList read FIdentifierList;

    {@member subjectList
      Who or what the set of documents is about. The documents can be about a person, (patient or healthcare practitioner), a device (i.e. machine) or even a group of subjects (such as a document about a herd of farm animals, or a set of patients that share a common exposure). If the documents cross more than one subject, then more than one subject is allowed here (unusual use case).
    }
    property subjectList : TFhirResourceReferenceList{Resource} read FSubjectList;

    {@member recipientList
      A patient, practitioner, or organization for which this set of documents is intended.
    }
    property recipientList : TFhirResourceReferenceList{Resource} read FRecipientList;

    {@member type_
      Specifies the kind of this set of documents (e.g. Patient Summary, Discharge Summary, Prescription, etc.). The type of a set of documents may be the same as one of the documents in it - especially if there is only one - but it may be wider.
    }
    property type_ : TFhirCodeableConcept read FType_ write SetType_;

    {@member authorList
      Identifies who is responsible for adding the information to the document.
    }
    property authorList : TFhirResourceReferenceList{Resource} read FAuthorList;

    {@member created
      When the document manifest was created for submission to the server (not necessarily the same thing as the actual resource last modified time, since it may be modified, replicated etc).
    }
    property created : TFhirDateTime read FCreated write SetCreated;
    {@member createdST
      Typed access to When the document manifest was created for submission to the server (not necessarily the same thing as the actual resource last modified time, since it may be modified, replicated etc).
    }
    property createdST : TDateAndTime read GetCreatedST write SetCreatedST;

    {@member source
      Identifies the source system, application, or software that produced the document manifest.
    }
    property source : TFhirUri read FSource write SetSource;
    {@member sourceST
      Typed access to Identifies the source system, application, or software that produced the document manifest.
    }
    property sourceST : String read GetSourceST write SetSourceST;

    {@member status
      The status of this document manifest.
    }
    property status : TFhirEnum read FStatus write SetStatus;
    {@member statusST
      Typed access to The status of this document manifest.
    }
    property statusST : TFhirDocumentReferenceStatus read GetStatusST write SetStatusST;

    {@member supercedes
      Whether this document manifest replaces another.
    }
    property supercedes : TFhirResourceReference{TFhirDocumentManifest} read FSupercedes write SetSupercedes;

    {@member description
      Human-readable description of the source document. This is sometimes known as the "title".
    }
    property description : TFhirString read FDescription write SetDescription;
    {@member descriptionST
      Typed access to Human-readable description of the source document. This is sometimes known as the "title".
    }
    property descriptionST : String read GetDescriptionST write SetDescriptionST;

    {@member confidentiality
      A code specifying the level of confidentiality of this set of Documents.
    }
    property confidentiality : TFhirCodeableConcept read FConfidentiality write SetConfidentiality;

    {@member contentList
      The list of resources that describe the parts of this document reference. Usually, these would be document references, but direct references to binary attachments and images are also allowed.
    }
    property contentList : TFhirResourceReferenceList{Resource} read FContentList;

  end;


  {@Class TFhirDocumentReference : TFhirResource
    A reference to a document.
  }
  {!.Net HL7Connect.Fhir.DocumentReference}
  TFhirDocumentReference = class (TFhirResource)
  private
    FMasterIdentifier : TFhirIdentifier;
    FidentifierList : TFhirIdentifierList;
    FSubject : TFhirResourceReference{Resource};
    FType_ : TFhirCodeableConcept;
    FClass_ : TFhirCodeableConcept;
    FauthorList : TFhirResourceReferenceList{Resource};
    FCustodian : TFhirResourceReference{TFhirOrganization};
    FPolicyManager : TFhirUri;
    FAuthenticator : TFhirResourceReference{Resource};
    FCreated : TFhirDateTime;
    FIndexed : TFhirInstant;
    FStatus : TFhirEnum;
    FDocStatus : TFhirCodeableConcept;
    FrelatesToList : TFhirDocumentReferenceRelatesToList;
    FDescription : TFhirString;
    FconfidentialityList : TFhirCodeableConceptList;
    FPrimaryLanguage : TFhirCode;
    FMimeType : TFhirCode;
    FformatList : TFhirUriList;
    FSize : TFhirInteger;
    FHash : TFhirString;
    FLocation : TFhirUri;
    FService : TFhirDocumentReferenceService;
    FContext : TFhirDocumentReferenceContext;
    Procedure SetMasterIdentifier(value : TFhirIdentifier);
    Procedure SetSubject(value : TFhirResourceReference{Resource});
    Procedure SetType_(value : TFhirCodeableConcept);
    Procedure SetClass_(value : TFhirCodeableConcept);
    Procedure SetCustodian(value : TFhirResourceReference{TFhirOrganization});
    Procedure SetPolicyManager(value : TFhirUri);
    Function GetPolicyManagerST : String;
    Procedure SetPolicyManagerST(value : String);
    Procedure SetAuthenticator(value : TFhirResourceReference{Resource});
    Procedure SetCreated(value : TFhirDateTime);
    Function GetCreatedST : TDateAndTime;
    Procedure SetCreatedST(value : TDateAndTime);
    Procedure SetIndexed(value : TFhirInstant);
    Function GetIndexedST : TDateAndTime;
    Procedure SetIndexedST(value : TDateAndTime);
    Procedure SetStatus(value : TFhirEnum);
    Function GetStatusST : TFhirDocumentReferenceStatus;
    Procedure SetStatusST(value : TFhirDocumentReferenceStatus);
    Procedure SetDocStatus(value : TFhirCodeableConcept);
    Procedure SetDescription(value : TFhirString);
    Function GetDescriptionST : String;
    Procedure SetDescriptionST(value : String);
    Procedure SetPrimaryLanguage(value : TFhirCode);
    Function GetPrimaryLanguageST : String;
    Procedure SetPrimaryLanguageST(value : String);
    Procedure SetMimeType(value : TFhirCode);
    Function GetMimeTypeST : String;
    Procedure SetMimeTypeST(value : String);
    Procedure SetSize(value : TFhirInteger);
    Function GetSizeST : String;
    Procedure SetSizeST(value : String);
    Procedure SetHash(value : TFhirString);
    Function GetHashST : String;
    Procedure SetHashST(value : String);
    Procedure SetLocation(value : TFhirUri);
    Function GetLocationST : String;
    Procedure SetLocationST(value : String);
    Procedure SetService(value : TFhirDocumentReferenceService);
    Procedure SetContext(value : TFhirDocumentReferenceContext);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    Function GetHasASummary : Boolean; Override;
    function GetResourceType : TFhirResourceType; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirDocumentReference; overload;
    function Clone : TFhirDocumentReference; overload;
    {!script show}
  published
    {@member masterIdentifier
      Document identifier as assigned by the source of the document. This identifier is specific to this version of the document. This unique identifier may be used elsewhere to identify this version of the document.
    }
    property masterIdentifier : TFhirIdentifier read FMasterIdentifier write SetMasterIdentifier;

    {@member identifierList
      Other identifiers associated with the document, including version independent, source record and workflow related identifiers.
    }
    property identifierList : TFhirIdentifierList read FIdentifierList;

    {@member subject
      Who or what the document is about. The document can be about a person, (patient or healthcare practitioner), a device (I.e. machine) or even a group of subjects (such as a document about a herd of farm animals, or a set of patients that share a common exposure).
    }
    property subject : TFhirResourceReference{Resource} read FSubject write SetSubject;

    {@member type_
      Specifies the particular kind of document (e.g. Patient Summary, Discharge Summary, Prescription, etc.).
    }
    property type_ : TFhirCodeableConcept read FType_ write SetType_;

    {@member class_
      A categorization for the type of the document. This may be implied by or derived from the code specified in the Document Type.
    }
    property class_ : TFhirCodeableConcept read FClass_ write SetClass_;

    {@member authorList
      Identifies who is responsible for adding the information to the document.
    }
    property authorList : TFhirResourceReferenceList{Resource} read FAuthorList;

    {@member custodian
      Identifies the organization or group who is responsible for ongoing maintenance of and access to the document.
    }
    property custodian : TFhirResourceReference{TFhirOrganization} read FCustodian write SetCustodian;

    {@member policyManager
      A reference to a domain or server that manages policies under which the document is accessed and/or made available.
    }
    property policyManager : TFhirUri read FPolicyManager write SetPolicyManager;
    {@member policyManagerST
      Typed access to A reference to a domain or server that manages policies under which the document is accessed and/or made available.
    }
    property policyManagerST : String read GetPolicyManagerST write SetPolicyManagerST;

    {@member authenticator
      Which person or organization authenticates that this document is valid.
    }
    property authenticator : TFhirResourceReference{Resource} read FAuthenticator write SetAuthenticator;

    {@member created
      When the document was created.
    }
    property created : TFhirDateTime read FCreated write SetCreated;
    {@member createdST
      Typed access to When the document was created.
    }
    property createdST : TDateAndTime read GetCreatedST write SetCreatedST;

    {@member indexed
      When the document reference was created.
    }
    property indexed : TFhirInstant read FIndexed write SetIndexed;
    {@member indexedST
      Typed access to When the document reference was created.
    }
    property indexedST : TDateAndTime read GetIndexedST write SetIndexedST;

    {@member status
      The status of this document reference.
    }
    property status : TFhirEnum read FStatus write SetStatus;
    {@member statusST
      Typed access to The status of this document reference.
    }
    property statusST : TFhirDocumentReferenceStatus read GetStatusST write SetStatusST;

    {@member docStatus
      The status of the underlying document.
    }
    property docStatus : TFhirCodeableConcept read FDocStatus write SetDocStatus;

    {@member relatesToList
      Relationships that this document has with other document references that already exist.
    }
    property relatesToList : TFhirDocumentReferenceRelatesToList read FRelatesToList;

    {@member description
      Human-readable description of the source document. This is sometimes known as the "title".
    }
    property description : TFhirString read FDescription write SetDescription;
    {@member descriptionST
      Typed access to Human-readable description of the source document. This is sometimes known as the "title".
    }
    property descriptionST : String read GetDescriptionST write SetDescriptionST;

    {@member confidentialityList
      A code specifying the level of confidentiality of the XDS Document.
    }
    property confidentialityList : TFhirCodeableConceptList read FConfidentialityList;

    {@member primaryLanguage
      The primary language in which the source document is written.
    }
    property primaryLanguage : TFhirCode read FPrimaryLanguage write SetPrimaryLanguage;
    {@member primaryLanguageST
      Typed access to The primary language in which the source document is written.
    }
    property primaryLanguageST : String read GetPrimaryLanguageST write SetPrimaryLanguageST;

    {@member mimeType
      The mime type of the source document.
    }
    property mimeType : TFhirCode read FMimeType write SetMimeType;
    {@member mimeTypeST
      Typed access to The mime type of the source document.
    }
    property mimeTypeST : String read GetMimeTypeST write SetMimeTypeST;

    {@member formatList
      An identifier that identifies that the format and content of the document conforms to additional rules beyond the base format indicated in the mimeType.
    }
    property formatList : TFhirUriList read FFormatList;

    {@member size
      The size of the source document this reference refers to in bytes.
    }
    property size : TFhirInteger read FSize write SetSize;
    {@member sizeST
      Typed access to The size of the source document this reference refers to in bytes.
    }
    property sizeST : String read GetSizeST write SetSizeST;

    {@member hash
      A hash of the source document to ensure that changes have not occurred.
    }
    property hash : TFhirString read FHash write SetHash;
    {@member hashST
      Typed access to A hash of the source document to ensure that changes have not occurred.
    }
    property hashST : String read GetHashST write SetHashST;

    {@member location
      A url at which the document can be accessed.
    }
    property location : TFhirUri read FLocation write SetLocation;
    {@member locationST
      Typed access to A url at which the document can be accessed.
    }
    property locationST : String read GetLocationST write SetLocationST;

    {@member service
      A description of a service call that can be used to retrieve the document.
    }
    property service : TFhirDocumentReferenceService read FService write SetService;

    {@member context
      The clinical context in which the document was prepared.
    }
    property context : TFhirDocumentReferenceContext read FContext write SetContext;

  end;


  {@Class TFhirEncounter : TFhirResource
    An interaction between a patient and healthcare provider(s) for the purpose of providing healthcare service(s) or assessing the health status of a patient.
  }
  {!.Net HL7Connect.Fhir.Encounter}
  TFhirEncounter = class (TFhirResource)
  private
    FidentifierList : TFhirIdentifierList;
    FStatus : TFhirEnum;
    FClass_ : TFhirEnum;
    Ftype_List : TFhirCodeableConceptList;
    FSubject : TFhirResourceReference{TFhirPatient};
    FparticipantList : TFhirEncounterParticipantList;
    FFulfills : TFhirResourceReference{TFhirAppointment};
    FPeriod : TFhirPeriod;
    FLength : TFhirQuantity;
    FReason : TFhirCodeableConcept;
    FIndication : TFhirResourceReference{Resource};
    FPriority : TFhirCodeableConcept;
    FHospitalization : TFhirEncounterHospitalization;
    FlocationList : TFhirEncounterLocationList;
    FServiceProvider : TFhirResourceReference{TFhirOrganization};
    FPartOf : TFhirResourceReference{TFhirEncounter};
    Procedure SetStatus(value : TFhirEnum);
    Function GetStatusST : TFhirEncounterState;
    Procedure SetStatusST(value : TFhirEncounterState);
    Procedure SetClass_(value : TFhirEnum);
    Function GetClass_ST : TFhirEncounterClass;
    Procedure SetClass_ST(value : TFhirEncounterClass);
    Procedure SetSubject(value : TFhirResourceReference{TFhirPatient});
    Procedure SetFulfills(value : TFhirResourceReference{TFhirAppointment});
    Procedure SetPeriod(value : TFhirPeriod);
    Procedure SetLength(value : TFhirQuantity);
    Procedure SetReason(value : TFhirCodeableConcept);
    Procedure SetIndication(value : TFhirResourceReference{Resource});
    Procedure SetPriority(value : TFhirCodeableConcept);
    Procedure SetHospitalization(value : TFhirEncounterHospitalization);
    Procedure SetServiceProvider(value : TFhirResourceReference{TFhirOrganization});
    Procedure SetPartOf(value : TFhirResourceReference{TFhirEncounter});
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    Function GetHasASummary : Boolean; Override;
    function GetResourceType : TFhirResourceType; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirEncounter; overload;
    function Clone : TFhirEncounter; overload;
    {!script show}
  published
    {@member identifierList
      Identifier(s) by which this encounter is known.
    }
    property identifierList : TFhirIdentifierList read FIdentifierList;

    {@member status
      planned | in progress | onleave | finished | cancelled.
    }
    property status : TFhirEnum read FStatus write SetStatus;
    {@member statusST
      Typed access to planned | in progress | onleave | finished | cancelled.
    }
    property statusST : TFhirEncounterState read GetStatusST write SetStatusST;

    {@member class_
      inpatient | outpatient | ambulatory | emergency +.
    }
    property class_ : TFhirEnum read FClass_ write SetClass_;
    {@member class_ST
      Typed access to inpatient | outpatient | ambulatory | emergency +.
    }
    property class_ST : TFhirEncounterClass read GetClass_ST write SetClass_ST;

    {@member type_List
      Specific type of encounter (e.g. e-mail consultation, surgical day-care, skilled nursing, rehabilitation).
    }
    property type_List : TFhirCodeableConceptList read FType_List;

    {@member subject
      The patient present at the encounter.
    }
    property subject : TFhirResourceReference{TFhirPatient} read FSubject write SetSubject;

    {@member participantList
      The main practitioner responsible for providing the service.
    }
    property participantList : TFhirEncounterParticipantList read FParticipantList;

    {@member fulfills
      The appointment that scheduled this encounter.
    }
    property fulfills : TFhirResourceReference{TFhirAppointment} read FFulfills write SetFulfills;

    {@member period
      The start and end time of the encounter.
    }
    property period : TFhirPeriod read FPeriod write SetPeriod;

    {@member length
      Quantity of time the encounter lasted. This excludes the time during leaves of absence.
    }
    property length : TFhirQuantity read FLength write SetLength;

    {@member reason
      Reason the encounter takes place, expressed as a code. For admissions, this can be used for a coded admission diagnosis.
    }
    property reason : TFhirCodeableConcept read FReason write SetReason;

    {@member indication
      Reason the encounter takes place, as specified using information from another resource. For admissions, this is the admission diagnosis.
    }
    property indication : TFhirResourceReference{Resource} read FIndication write SetIndication;

    {@member priority
      Indicates the urgency of the encounter.
    }
    property priority : TFhirCodeableConcept read FPriority write SetPriority;

    {@member hospitalization
      Details about an admission to a clinic.
    }
    property hospitalization : TFhirEncounterHospitalization read FHospitalization write SetHospitalization;

    {@member locationList
      List of locations at which the patient has been.
    }
    property locationList : TFhirEncounterLocationList read FLocationList;

    {@member serviceProvider
      Department or team providing care.
    }
    property serviceProvider : TFhirResourceReference{TFhirOrganization} read FServiceProvider write SetServiceProvider;

    {@member partOf
      Another Encounter of which this encounter is a part of (administratively or in time).
    }
    property partOf : TFhirResourceReference{TFhirEncounter} read FPartOf write SetPartOf;

  end;


  {@Class TFhirFamilyHistory : TFhirResource
    Significant health events and conditions for people related to the subject relevant in the context of care for the subject.
  }
  {!.Net HL7Connect.Fhir.FamilyHistory}
  TFhirFamilyHistory = class (TFhirResource)
  private
    FidentifierList : TFhirIdentifierList;
    FSubject : TFhirResourceReference{TFhirPatient};
    FDate : TFhirDateTime;
    FNote : TFhirString;
    FrelationList : TFhirFamilyHistoryRelationList;
    Procedure SetSubject(value : TFhirResourceReference{TFhirPatient});
    Procedure SetDate(value : TFhirDateTime);
    Function GetDateST : TDateAndTime;
    Procedure SetDateST(value : TDateAndTime);
    Procedure SetNote(value : TFhirString);
    Function GetNoteST : String;
    Procedure SetNoteST(value : String);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    Function GetHasASummary : Boolean; Override;
    function GetResourceType : TFhirResourceType; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirFamilyHistory; overload;
    function Clone : TFhirFamilyHistory; overload;
    {!script show}
  published
    {@member identifierList
      This records identifiers associated with this family history record that are defined by business processes and/ or used to refer to it when a direct URL reference to the resource itself is not appropriate (e.g. in CDA documents, or in written / printed documentation).
    }
    property identifierList : TFhirIdentifierList read FIdentifierList;

    {@member subject
      The person who this history concerns.
    }
    property subject : TFhirResourceReference{TFhirPatient} read FSubject write SetSubject;

    {@member date
      The date (and possibly time) when the family history was taken.
    }
    property date : TFhirDateTime read FDate write SetDate;
    {@member dateST
      Typed access to The date (and possibly time) when the family history was taken.
    }
    property dateST : TDateAndTime read GetDateST write SetDateST;

    {@member note
      Conveys information about family history not specific to individual relations.
    }
    property note : TFhirString read FNote write SetNote;
    {@member noteST
      Typed access to Conveys information about family history not specific to individual relations.
    }
    property noteST : String read GetNoteST write SetNoteST;

    {@member relationList
      The related person. Each FamilyHistory resource contains the entire family history for a single person.
    }
    property relationList : TFhirFamilyHistoryRelationList read FRelationList;

  end;


  {@Class TFhirGroup : TFhirResource
    Represents a defined collection of entities that may be discussed or acted upon collectively but which are not expected to act collectively and are not formally or legally recognized.  I.e. A collection of entities that isn't an Organization.
  }
  {!.Net HL7Connect.Fhir.Group}
  TFhirGroup = class (TFhirResource)
  private
    FIdentifier : TFhirIdentifier;
    FType_ : TFhirEnum;
    FActual : TFhirBoolean;
    FCode : TFhirCodeableConcept;
    FName : TFhirString;
    FQuantity : TFhirInteger;
    FcharacteristicList : TFhirGroupCharacteristicList;
    FmemberList : TFhirResourceReferenceList{Resource};
    Procedure SetIdentifier(value : TFhirIdentifier);
    Procedure SetType_(value : TFhirEnum);
    Function GetType_ST : TFhirGroupType;
    Procedure SetType_ST(value : TFhirGroupType);
    Procedure SetActual(value : TFhirBoolean);
    Function GetActualST : Boolean;
    Procedure SetActualST(value : Boolean);
    Procedure SetCode(value : TFhirCodeableConcept);
    Procedure SetName(value : TFhirString);
    Function GetNameST : String;
    Procedure SetNameST(value : String);
    Procedure SetQuantity(value : TFhirInteger);
    Function GetQuantityST : String;
    Procedure SetQuantityST(value : String);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    Function GetHasASummary : Boolean; Override;
    function GetResourceType : TFhirResourceType; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirGroup; overload;
    function Clone : TFhirGroup; overload;
    {!script show}
  published
    {@member identifier
      A unique business identifier for this group.
    }
    property identifier : TFhirIdentifier read FIdentifier write SetIdentifier;

    {@member type_
      Identifies the broad classification of the kind of resources the group includes.
    }
    property type_ : TFhirEnum read FType_ write SetType_;
    {@member type_ST
      Typed access to Identifies the broad classification of the kind of resources the group includes.
    }
    property type_ST : TFhirGroupType read GetType_ST write SetType_ST;

    {@member actual
      If true, indicates that the resource refers to a specific group of real individuals.  If false, the group defines a set of intended individuals.
    }
    property actual : TFhirBoolean read FActual write SetActual;
    {@member actualST
      Typed access to If true, indicates that the resource refers to a specific group of real individuals.  If false, the group defines a set of intended individuals.
    }
    property actualST : Boolean read GetActualST write SetActualST;

    {@member code
      Provides a specific type of resource the group includes.  E.g. "cow", "syringe", etc.
    }
    property code : TFhirCodeableConcept read FCode write SetCode;

    {@member name
      A label assigned to the group for human identification and communication.
    }
    property name : TFhirString read FName write SetName;
    {@member nameST
      Typed access to A label assigned to the group for human identification and communication.
    }
    property nameST : String read GetNameST write SetNameST;

    {@member quantity
      A count of the number of resource instances that are part of the group.
    }
    property quantity : TFhirInteger read FQuantity write SetQuantity;
    {@member quantityST
      Typed access to A count of the number of resource instances that are part of the group.
    }
    property quantityST : String read GetQuantityST write SetQuantityST;

    {@member characteristicList
      Identifies the traits shared by members of the group.
    }
    property characteristicList : TFhirGroupCharacteristicList read FCharacteristicList;

    {@member memberList
      Identifies the resource instances that are members of the group.
    }
    property memberList : TFhirResourceReferenceList{Resource} read FMemberList;

  end;


  {@Class TFhirImagingStudy : TFhirResource
    Manifest of a set of images produced in study. The set of images may include every image in the study, or it may be an incomplete sample, such as a list of key images.
  }
  {!.Net HL7Connect.Fhir.ImagingStudy}
  TFhirImagingStudy = class (TFhirResource)
  private
    FDateTime : TFhirDateTime;
    FSubject : TFhirResourceReference{TFhirPatient};
    FUid : TFhirOid;
    FAccessionNo : TFhirIdentifier;
    FidentifierList : TFhirIdentifierList;
    ForderList : TFhirResourceReferenceList{TFhirDiagnosticOrder};
    FModality : TFhirEnumList;
    FReferrer : TFhirResourceReference{TFhirPractitioner};
    FAvailability : TFhirEnum;
    FUrl : TFhirUri;
    FNumberOfSeries : TFhirInteger;
    FNumberOfInstances : TFhirInteger;
    FClinicalInformation : TFhirString;
    Fprocedure_List : TFhirCodingList;
    FInterpreter : TFhirResourceReference{TFhirPractitioner};
    FDescription : TFhirString;
    FseriesList : TFhirImagingStudySeriesList;
    Procedure SetDateTime(value : TFhirDateTime);
    Function GetDateTimeST : TDateAndTime;
    Procedure SetDateTimeST(value : TDateAndTime);
    Procedure SetSubject(value : TFhirResourceReference{TFhirPatient});
    Procedure SetUid(value : TFhirOid);
    Function GetUidST : String;
    Procedure SetUidST(value : String);
    Procedure SetAccessionNo(value : TFhirIdentifier);
    Procedure SetReferrer(value : TFhirResourceReference{TFhirPractitioner});
    Procedure SetAvailability(value : TFhirEnum);
    Function GetAvailabilityST : TFhirInstanceAvailability;
    Procedure SetAvailabilityST(value : TFhirInstanceAvailability);
    Procedure SetUrl(value : TFhirUri);
    Function GetUrlST : String;
    Procedure SetUrlST(value : String);
    Procedure SetNumberOfSeries(value : TFhirInteger);
    Function GetNumberOfSeriesST : String;
    Procedure SetNumberOfSeriesST(value : String);
    Procedure SetNumberOfInstances(value : TFhirInteger);
    Function GetNumberOfInstancesST : String;
    Procedure SetNumberOfInstancesST(value : String);
    Procedure SetClinicalInformation(value : TFhirString);
    Function GetClinicalInformationST : String;
    Procedure SetClinicalInformationST(value : String);
    Procedure SetInterpreter(value : TFhirResourceReference{TFhirPractitioner});
    Procedure SetDescription(value : TFhirString);
    Function GetDescriptionST : String;
    Procedure SetDescriptionST(value : String);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    Function GetHasASummary : Boolean; Override;
    function GetResourceType : TFhirResourceType; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirImagingStudy; overload;
    function Clone : TFhirImagingStudy; overload;
    {!script show}
  published
    {@member dateTime
      Date and Time the study took place.
    }
    property dateTime : TFhirDateTime read FDateTime write SetDateTime;
    {@member dateTimeST
      Typed access to Date and Time the study took place.
    }
    property dateTimeST : TDateAndTime read GetDateTimeST write SetDateTimeST;

    {@member subject
      Who the images are of.
    }
    property subject : TFhirResourceReference{TFhirPatient} read FSubject write SetSubject;

    {@member uid
      Formal identifier for the study.
    }
    property uid : TFhirOid read FUid write SetUid;
    {@member uidST
      Typed access to Formal identifier for the study.
    }
    property uidST : String read GetUidST write SetUidST;

    {@member accessionNo
      Accession Number.
    }
    property accessionNo : TFhirIdentifier read FAccessionNo write SetAccessionNo;

    {@member identifierList
      Other identifiers for the study.
    }
    property identifierList : TFhirIdentifierList read FIdentifierList;

    {@member orderList
      A list of the diagnostic orders that resulted in this imaging study being performed.
    }
    property orderList : TFhirResourceReferenceList{TFhirDiagnosticOrder} read FOrderList;

    {@member modality
      A list of all the Series.ImageModality values that are actual acquisition modalities, i.e. those in the DICOM Context Group 29 (value set OID 1.2.840.10008.6.1.19).
    }
    property modality : TFhirEnumList read FModality;
    {@member referrer
      The requesting/referring physician.
    }
    property referrer : TFhirResourceReference{TFhirPractitioner} read FReferrer write SetReferrer;

    {@member availability
      Availability of study (online, offline or nearline).
    }
    property availability : TFhirEnum read FAvailability write SetAvailability;
    {@member availabilityST
      Typed access to Availability of study (online, offline or nearline).
    }
    property availabilityST : TFhirInstanceAvailability read GetAvailabilityST write SetAvailabilityST;

    {@member url
      WADO-RS URI where Study is available.
    }
    property url : TFhirUri read FUrl write SetUrl;
    {@member urlST
      Typed access to WADO-RS URI where Study is available.
    }
    property urlST : String read GetUrlST write SetUrlST;

    {@member numberOfSeries
      Number of Series in Study.
    }
    property numberOfSeries : TFhirInteger read FNumberOfSeries write SetNumberOfSeries;
    {@member numberOfSeriesST
      Typed access to Number of Series in Study.
    }
    property numberOfSeriesST : String read GetNumberOfSeriesST write SetNumberOfSeriesST;

    {@member numberOfInstances
      Number of SOP Instances in Study.
    }
    property numberOfInstances : TFhirInteger read FNumberOfInstances write SetNumberOfInstances;
    {@member numberOfInstancesST
      Typed access to Number of SOP Instances in Study.
    }
    property numberOfInstancesST : String read GetNumberOfInstancesST write SetNumberOfInstancesST;

    {@member clinicalInformation
      Diagnoses etc provided with request.
    }
    property clinicalInformation : TFhirString read FClinicalInformation write SetClinicalInformation;
    {@member clinicalInformationST
      Typed access to Diagnoses etc provided with request.
    }
    property clinicalInformationST : String read GetClinicalInformationST write SetClinicalInformationST;

    {@member procedure_List
      Type of procedure performed.
    }
    property procedure_List : TFhirCodingList read FProcedure_List;

    {@member interpreter
      Who read study and interpreted the images.
    }
    property interpreter : TFhirResourceReference{TFhirPractitioner} read FInterpreter write SetInterpreter;

    {@member description
      Institution-generated description or classification of the Study (component) performed.
    }
    property description : TFhirString read FDescription write SetDescription;
    {@member descriptionST
      Typed access to Institution-generated description or classification of the Study (component) performed.
    }
    property descriptionST : String read GetDescriptionST write SetDescriptionST;

    {@member seriesList
      Each study has one or more series of image instances.
    }
    property seriesList : TFhirImagingStudySeriesList read FSeriesList;

  end;


  {@Class TFhirImmunization : TFhirResource
    Immunization event information.
  }
  {!.Net HL7Connect.Fhir.Immunization}
  TFhirImmunization = class (TFhirResource)
  private
    FidentifierList : TFhirIdentifierList;
    FDate : TFhirDateTime;
    FVaccineType : TFhirCodeableConcept;
    FSubject : TFhirResourceReference{TFhirPatient};
    FRefusedIndicator : TFhirBoolean;
    FReported : TFhirBoolean;
    FPerformer : TFhirResourceReference{TFhirPractitioner};
    FRequester : TFhirResourceReference{TFhirPractitioner};
    FManufacturer : TFhirResourceReference{TFhirOrganization};
    FLocation : TFhirResourceReference{TFhirLocation};
    FLotNumber : TFhirString;
    FExpirationDate : TFhirDate;
    FSite : TFhirCodeableConcept;
    FRoute : TFhirCodeableConcept;
    FDoseQuantity : TFhirQuantity;
    FExplanation : TFhirImmunizationExplanation;
    FreactionList : TFhirImmunizationReactionList;
    FvaccinationProtocolList : TFhirImmunizationVaccinationProtocolList;
    Procedure SetDate(value : TFhirDateTime);
    Function GetDateST : TDateAndTime;
    Procedure SetDateST(value : TDateAndTime);
    Procedure SetVaccineType(value : TFhirCodeableConcept);
    Procedure SetSubject(value : TFhirResourceReference{TFhirPatient});
    Procedure SetRefusedIndicator(value : TFhirBoolean);
    Function GetRefusedIndicatorST : Boolean;
    Procedure SetRefusedIndicatorST(value : Boolean);
    Procedure SetReported(value : TFhirBoolean);
    Function GetReportedST : Boolean;
    Procedure SetReportedST(value : Boolean);
    Procedure SetPerformer(value : TFhirResourceReference{TFhirPractitioner});
    Procedure SetRequester(value : TFhirResourceReference{TFhirPractitioner});
    Procedure SetManufacturer(value : TFhirResourceReference{TFhirOrganization});
    Procedure SetLocation(value : TFhirResourceReference{TFhirLocation});
    Procedure SetLotNumber(value : TFhirString);
    Function GetLotNumberST : String;
    Procedure SetLotNumberST(value : String);
    Procedure SetExpirationDate(value : TFhirDate);
    Function GetExpirationDateST : TDateAndTime;
    Procedure SetExpirationDateST(value : TDateAndTime);
    Procedure SetSite(value : TFhirCodeableConcept);
    Procedure SetRoute(value : TFhirCodeableConcept);
    Procedure SetDoseQuantity(value : TFhirQuantity);
    Procedure SetExplanation(value : TFhirImmunizationExplanation);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    Function GetHasASummary : Boolean; Override;
    function GetResourceType : TFhirResourceType; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirImmunization; overload;
    function Clone : TFhirImmunization; overload;
    {!script show}
  published
    {@member identifierList
      A unique identifier assigned to this adverse reaction record.
    }
    property identifierList : TFhirIdentifierList read FIdentifierList;

    {@member date
      Date vaccine administered or was to be administered.
    }
    property date : TFhirDateTime read FDate write SetDate;
    {@member dateST
      Typed access to Date vaccine administered or was to be administered.
    }
    property dateST : TDateAndTime read GetDateST write SetDateST;

    {@member vaccineType
      Vaccine that was administered or was to be administered.
    }
    property vaccineType : TFhirCodeableConcept read FVaccineType write SetVaccineType;

    {@member subject
      The patient to whom the vaccine was to be administered.
    }
    property subject : TFhirResourceReference{TFhirPatient} read FSubject write SetSubject;

    {@member refusedIndicator
      Indicates if the vaccination was refused.
    }
    property refusedIndicator : TFhirBoolean read FRefusedIndicator write SetRefusedIndicator;
    {@member refusedIndicatorST
      Typed access to Indicates if the vaccination was refused.
    }
    property refusedIndicatorST : Boolean read GetRefusedIndicatorST write SetRefusedIndicatorST;

    {@member reported
      True if this administration was reported rather than directly administered.
    }
    property reported : TFhirBoolean read FReported write SetReported;
    {@member reportedST
      Typed access to True if this administration was reported rather than directly administered.
    }
    property reportedST : Boolean read GetReportedST write SetReportedST;

    {@member performer
      Clinician who administered the vaccine.
    }
    property performer : TFhirResourceReference{TFhirPractitioner} read FPerformer write SetPerformer;

    {@member requester
      Clinician who ordered the vaccination.
    }
    property requester : TFhirResourceReference{TFhirPractitioner} read FRequester write SetRequester;

    {@member manufacturer
      Name of vaccine manufacturer.
    }
    property manufacturer : TFhirResourceReference{TFhirOrganization} read FManufacturer write SetManufacturer;

    {@member location
      The service delivery location where the vaccine administration occurred.
    }
    property location : TFhirResourceReference{TFhirLocation} read FLocation write SetLocation;

    {@member lotNumber
      Lot number of the  vaccine product.
    }
    property lotNumber : TFhirString read FLotNumber write SetLotNumber;
    {@member lotNumberST
      Typed access to Lot number of the  vaccine product.
    }
    property lotNumberST : String read GetLotNumberST write SetLotNumberST;

    {@member expirationDate
      Date vaccine batch expires.
    }
    property expirationDate : TFhirDate read FExpirationDate write SetExpirationDate;
    {@member expirationDateST
      Typed access to Date vaccine batch expires.
    }
    property expirationDateST : TDateAndTime read GetExpirationDateST write SetExpirationDateST;

    {@member site
      Body site where vaccine was administered.
    }
    property site : TFhirCodeableConcept read FSite write SetSite;

    {@member route
      The path by which the vaccine product is taken into the body.
    }
    property route : TFhirCodeableConcept read FRoute write SetRoute;

    {@member doseQuantity
      The quantity of vaccine product that was administered.
    }
    property doseQuantity : TFhirQuantity read FDoseQuantity write SetDoseQuantity;

    {@member explanation
      Reasons why a vaccine was administered or refused.
    }
    property explanation : TFhirImmunizationExplanation read FExplanation write SetExplanation;

    {@member reactionList
      Categorical data indicating that an adverse event is associated in time to an immunization.
    }
    property reactionList : TFhirImmunizationReactionList read FReactionList;

    {@member vaccinationProtocolList
      Contains information about the protocol(s) under which the vaccine was administered.
    }
    property vaccinationProtocolList : TFhirImmunizationVaccinationProtocolList read FVaccinationProtocolList;

  end;


  {@Class TFhirImmunizationRecommendation : TFhirResource
    A patient's point-of-time immunization status and recommendation with optional supporting justification.
  }
  {!.Net HL7Connect.Fhir.ImmunizationRecommendation}
  TFhirImmunizationRecommendation = class (TFhirResource)
  private
    FidentifierList : TFhirIdentifierList;
    FSubject : TFhirResourceReference{TFhirPatient};
    FrecommendationList : TFhirImmunizationRecommendationRecommendationList;
    Procedure SetSubject(value : TFhirResourceReference{TFhirPatient});
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    Function GetHasASummary : Boolean; Override;
    function GetResourceType : TFhirResourceType; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirImmunizationRecommendation; overload;
    function Clone : TFhirImmunizationRecommendation; overload;
    {!script show}
  published
    {@member identifierList
      A unique identifier assigned to this particular recommendation record.
    }
    property identifierList : TFhirIdentifierList read FIdentifierList;

    {@member subject
      The patient who is the subject of the profile.
    }
    property subject : TFhirResourceReference{TFhirPatient} read FSubject write SetSubject;

    {@member recommendationList
      Vaccine administration recommendations.
    }
    property recommendationList : TFhirImmunizationRecommendationRecommendationList read FRecommendationList;

  end;


  {@Class TFhirList : TFhirResource
    A set of information summarized from a list of other resources.
  }
  {!.Net HL7Connect.Fhir.List}
  TFhirList = class (TFhirResource)
  private
    FidentifierList : TFhirIdentifierList;
    FCode : TFhirCodeableConcept;
    FSubject : TFhirResourceReference{Resource};
    FSource : TFhirResourceReference{Resource};
    FDate : TFhirDateTime;
    FOrdered : TFhirBoolean;
    FMode : TFhirEnum;
    FentryList : TFhirListEntryList;
    FEmptyReason : TFhirCodeableConcept;
    Procedure SetCode(value : TFhirCodeableConcept);
    Procedure SetSubject(value : TFhirResourceReference{Resource});
    Procedure SetSource(value : TFhirResourceReference{Resource});
    Procedure SetDate(value : TFhirDateTime);
    Function GetDateST : TDateAndTime;
    Procedure SetDateST(value : TDateAndTime);
    Procedure SetOrdered(value : TFhirBoolean);
    Function GetOrderedST : Boolean;
    Procedure SetOrderedST(value : Boolean);
    Procedure SetMode(value : TFhirEnum);
    Function GetModeST : TFhirListMode;
    Procedure SetModeST(value : TFhirListMode);
    Procedure SetEmptyReason(value : TFhirCodeableConcept);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    Function GetHasASummary : Boolean; Override;
    function GetResourceType : TFhirResourceType; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirList; overload;
    function Clone : TFhirList; overload;
    {!script show}
  published
    {@member identifierList
      Identifier for the List assigned for business purposes outside the context of FHIR.
    }
    property identifierList : TFhirIdentifierList read FIdentifierList;

    {@member code
      This code defines the purpose of the list - why it was created.
    }
    property code : TFhirCodeableConcept read FCode write SetCode;

    {@member subject
      The common subject (or patient) of the resources that are in the list, if there is one.
    }
    property subject : TFhirResourceReference{Resource} read FSubject write SetSubject;

    {@member source
      The entity responsible for deciding what the contents of the list were.
    }
    property source : TFhirResourceReference{Resource} read FSource write SetSource;

    {@member date
      The date that the list was prepared.
    }
    property date : TFhirDateTime read FDate write SetDate;
    {@member dateST
      Typed access to The date that the list was prepared.
    }
    property dateST : TDateAndTime read GetDateST write SetDateST;

    {@member ordered
      Whether items in the list have a meaningful order.
    }
    property ordered : TFhirBoolean read FOrdered write SetOrdered;
    {@member orderedST
      Typed access to Whether items in the list have a meaningful order.
    }
    property orderedST : Boolean read GetOrderedST write SetOrderedST;

    {@member mode
      How this list was prepared - whether it is a working list that is suitable for being maintained on an ongoing basis, or if it represents a snapshot of a list of items from another source, or whether it is a prepared list where items may be marked as added, modified or deleted.
    }
    property mode : TFhirEnum read FMode write SetMode;
    {@member modeST
      Typed access to How this list was prepared - whether it is a working list that is suitable for being maintained on an ongoing basis, or if it represents a snapshot of a list of items from another source, or whether it is a prepared list where items may be marked as added, modified or deleted.
    }
    property modeST : TFhirListMode read GetModeST write SetModeST;

    {@member entryList
      Entries in this list.
    }
    property entryList : TFhirListEntryList read FEntryList;

    {@member emptyReason
      If the list is empty, why the list is empty.
    }
    property emptyReason : TFhirCodeableConcept read FEmptyReason write SetEmptyReason;

  end;


  {@Class TFhirLocation : TFhirResource
    Details and position information for a physical place where services are provided  and resources and participants may be stored, found, contained or accommodated.
  }
  {!.Net HL7Connect.Fhir.Location}
  TFhirLocation = class (TFhirResource)
  private
    FidentifierList : TFhirIdentifierList;
    FName : TFhirString;
    FDescription : TFhirString;
    FType_ : TFhirCodeableConcept;
    FtelecomList : TFhirContactList;
    FAddress : TFhirAddress;
    FPhysicalType : TFhirCodeableConcept;
    FPosition : TFhirLocationPosition;
    FManagingOrganization : TFhirResourceReference{TFhirOrganization};
    FStatus : TFhirEnum;
    FPartOf : TFhirResourceReference{TFhirLocation};
    FMode : TFhirEnum;
    Procedure SetName(value : TFhirString);
    Function GetNameST : String;
    Procedure SetNameST(value : String);
    Procedure SetDescription(value : TFhirString);
    Function GetDescriptionST : String;
    Procedure SetDescriptionST(value : String);
    Procedure SetType_(value : TFhirCodeableConcept);
    Procedure SetAddress(value : TFhirAddress);
    Procedure SetPhysicalType(value : TFhirCodeableConcept);
    Procedure SetPosition(value : TFhirLocationPosition);
    Procedure SetManagingOrganization(value : TFhirResourceReference{TFhirOrganization});
    Procedure SetStatus(value : TFhirEnum);
    Function GetStatusST : TFhirLocationStatus;
    Procedure SetStatusST(value : TFhirLocationStatus);
    Procedure SetPartOf(value : TFhirResourceReference{TFhirLocation});
    Procedure SetMode(value : TFhirEnum);
    Function GetModeST : TFhirLocationMode;
    Procedure SetModeST(value : TFhirLocationMode);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    Function GetHasASummary : Boolean; Override;
    function GetResourceType : TFhirResourceType; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirLocation; overload;
    function Clone : TFhirLocation; overload;
    {!script show}
  published
    {@member identifierList
      Unique code or number identifying the location to its users.
    }
    property identifierList : TFhirIdentifierList read FIdentifierList;

    {@member name
      Name of the location as used by humans. Does not need to be unique.
    }
    property name : TFhirString read FName write SetName;
    {@member nameST
      Typed access to Name of the location as used by humans. Does not need to be unique.
    }
    property nameST : String read GetNameST write SetNameST;

    {@member description
      Description of the Location, which helps in finding or referencing the place.
    }
    property description : TFhirString read FDescription write SetDescription;
    {@member descriptionST
      Typed access to Description of the Location, which helps in finding or referencing the place.
    }
    property descriptionST : String read GetDescriptionST write SetDescriptionST;

    {@member type_
      Indicates the type of function performed at the location.
    }
    property type_ : TFhirCodeableConcept read FType_ write SetType_;

    {@member telecomList
      The contact details of communication devices available at the location. This can include phone numbers, fax numbers, mobile numbers, email addresses and web sites.
    }
    property telecomList : TFhirContactList read FTelecomList;

    {@member address
      Physical location.
    }
    property address : TFhirAddress read FAddress write SetAddress;

    {@member physicalType
      Physical form of the location, e.g. building, room, vehicle, road.
    }
    property physicalType : TFhirCodeableConcept read FPhysicalType write SetPhysicalType;

    {@member position
      The absolute geographic location of the Location, expressed in a KML compatible manner (see notes below for KML).
    }
    property position : TFhirLocationPosition read FPosition write SetPosition;

    {@member managingOrganization
      The organization that is responsible for the provisioning and upkeep of the location.
    }
    property managingOrganization : TFhirResourceReference{TFhirOrganization} read FManagingOrganization write SetManagingOrganization;

    {@member status
      active | suspended | inactive.
    }
    property status : TFhirEnum read FStatus write SetStatus;
    {@member statusST
      Typed access to active | suspended | inactive.
    }
    property statusST : TFhirLocationStatus read GetStatusST write SetStatusST;

    {@member partOf
      Another Location which this Location is physically part of.
    }
    property partOf : TFhirResourceReference{TFhirLocation} read FPartOf write SetPartOf;

    {@member mode
      Indicates whether a resource instance represents a specific location or a class of locations.
    }
    property mode : TFhirEnum read FMode write SetMode;
    {@member modeST
      Typed access to Indicates whether a resource instance represents a specific location or a class of locations.
    }
    property modeST : TFhirLocationMode read GetModeST write SetModeST;

  end;


  {@Class TFhirMedia : TFhirResource
    A photo, video, or audio recording acquired or used in healthcare. The actual content may be inline or provided by direct reference.
  }
  {!.Net HL7Connect.Fhir.Media}
  TFhirMedia = class (TFhirResource)
  private
    FType_ : TFhirEnum;
    FSubtype : TFhirCodeableConcept;
    FidentifierList : TFhirIdentifierList;
    FDateTime : TFhirDateTime;
    FSubject : TFhirResourceReference{Resource};
    FOperator : TFhirResourceReference{TFhirPractitioner};
    FView : TFhirCodeableConcept;
    FDeviceName : TFhirString;
    FHeight : TFhirInteger;
    FWidth : TFhirInteger;
    FFrames : TFhirInteger;
    FLength : TFhirInteger;
    FContent : TFhirAttachment;
    Procedure SetType_(value : TFhirEnum);
    Function GetType_ST : TFhirMediaType;
    Procedure SetType_ST(value : TFhirMediaType);
    Procedure SetSubtype(value : TFhirCodeableConcept);
    Procedure SetDateTime(value : TFhirDateTime);
    Function GetDateTimeST : TDateAndTime;
    Procedure SetDateTimeST(value : TDateAndTime);
    Procedure SetSubject(value : TFhirResourceReference{Resource});
    Procedure SetOperator(value : TFhirResourceReference{TFhirPractitioner});
    Procedure SetView(value : TFhirCodeableConcept);
    Procedure SetDeviceName(value : TFhirString);
    Function GetDeviceNameST : String;
    Procedure SetDeviceNameST(value : String);
    Procedure SetHeight(value : TFhirInteger);
    Function GetHeightST : String;
    Procedure SetHeightST(value : String);
    Procedure SetWidth(value : TFhirInteger);
    Function GetWidthST : String;
    Procedure SetWidthST(value : String);
    Procedure SetFrames(value : TFhirInteger);
    Function GetFramesST : String;
    Procedure SetFramesST(value : String);
    Procedure SetLength(value : TFhirInteger);
    Function GetLengthST : String;
    Procedure SetLengthST(value : String);
    Procedure SetContent(value : TFhirAttachment);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    Function GetHasASummary : Boolean; Override;
    function GetResourceType : TFhirResourceType; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirMedia; overload;
    function Clone : TFhirMedia; overload;
    {!script show}
  published
    {@member type_
      Whether the media is a photo (still image), an audio recording, or a video recording.
    }
    property type_ : TFhirEnum read FType_ write SetType_;
    {@member type_ST
      Typed access to Whether the media is a photo (still image), an audio recording, or a video recording.
    }
    property type_ST : TFhirMediaType read GetType_ST write SetType_ST;

    {@member subtype
      Details of the type of the media - usually, how it was acquired (what type of device). If images sourced from a DICOM system, are wrapped in a Media resource, then this is the modality.
    }
    property subtype : TFhirCodeableConcept read FSubtype write SetSubtype;

    {@member identifierList
      Identifiers associated with the image - these may include identifiers for the image itself, identifiers for the context of its collection (e.g. series ids) and context ids such as accession numbers or other workflow identifiers.
    }
    property identifierList : TFhirIdentifierList read FIdentifierList;

    {@member dateTime
      When the media was originally recorded. For video and audio, if the length of the recording is not insignificant, this is the end of the recording.
    }
    property dateTime : TFhirDateTime read FDateTime write SetDateTime;
    {@member dateTimeST
      Typed access to When the media was originally recorded. For video and audio, if the length of the recording is not insignificant, this is the end of the recording.
    }
    property dateTimeST : TDateAndTime read GetDateTimeST write SetDateTimeST;

    {@member subject
      Who/What this Media is a record of.
    }
    property subject : TFhirResourceReference{Resource} read FSubject write SetSubject;

    {@member operator
      The person who administered the collection of the image.
    }
    property operator : TFhirResourceReference{TFhirPractitioner} read FOperator write SetOperator;

    {@member view
      The name of the imaging view e.g Lateral or Antero-posterior (AP).
    }
    property view : TFhirCodeableConcept read FView write SetView;

    {@member deviceName
      The name of the device / manufacturer of the device  that was used to make the recording.
    }
    property deviceName : TFhirString read FDeviceName write SetDeviceName;
    {@member deviceNameST
      Typed access to The name of the device / manufacturer of the device  that was used to make the recording.
    }
    property deviceNameST : String read GetDeviceNameST write SetDeviceNameST;

    {@member height
      Height of the image in pixels(photo/video).
    }
    property height : TFhirInteger read FHeight write SetHeight;
    {@member heightST
      Typed access to Height of the image in pixels(photo/video).
    }
    property heightST : String read GetHeightST write SetHeightST;

    {@member width
      Width of the image in pixels (photo/video).
    }
    property width : TFhirInteger read FWidth write SetWidth;
    {@member widthST
      Typed access to Width of the image in pixels (photo/video).
    }
    property widthST : String read GetWidthST write SetWidthST;

    {@member frames
      The number of frames in a photo. This is used with a multi-page fax, or an imaging acquisition context that takes multiple slices in a single image, or an animated gif. If there is more than one frame, this SHALL have a value in order to alert interface software that a multi-frame capable rendering widget is required.
    }
    property frames : TFhirInteger read FFrames write SetFrames;
    {@member framesST
      Typed access to The number of frames in a photo. This is used with a multi-page fax, or an imaging acquisition context that takes multiple slices in a single image, or an animated gif. If there is more than one frame, this SHALL have a value in order to alert interface software that a multi-frame capable rendering widget is required.
    }
    property framesST : String read GetFramesST write SetFramesST;

    {@member length
      The length of the recording in seconds - for audio and video.
    }
    property length : TFhirInteger read FLength write SetLength;
    {@member lengthST
      Typed access to The length of the recording in seconds - for audio and video.
    }
    property lengthST : String read GetLengthST write SetLengthST;

    {@member content
      The actual content of the media - inline or by direct reference to the media source file.
    }
    property content : TFhirAttachment read FContent write SetContent;

  end;


  {@Class TFhirMedication : TFhirResource
    Primarily used for identification and definition of Medication, but also covers ingredients and packaging.
  }
  {!.Net HL7Connect.Fhir.Medication}
  TFhirMedication = class (TFhirResource)
  private
    FName : TFhirString;
    FCode : TFhirCodeableConcept;
    FIsBrand : TFhirBoolean;
    FManufacturer : TFhirResourceReference{TFhirOrganization};
    FKind : TFhirEnum;
    FProduct : TFhirMedicationProduct;
    FPackage : TFhirMedicationPackage;
    Procedure SetName(value : TFhirString);
    Function GetNameST : String;
    Procedure SetNameST(value : String);
    Procedure SetCode(value : TFhirCodeableConcept);
    Procedure SetIsBrand(value : TFhirBoolean);
    Function GetIsBrandST : Boolean;
    Procedure SetIsBrandST(value : Boolean);
    Procedure SetManufacturer(value : TFhirResourceReference{TFhirOrganization});
    Procedure SetKind(value : TFhirEnum);
    Function GetKindST : TFhirMedicationKind;
    Procedure SetKindST(value : TFhirMedicationKind);
    Procedure SetProduct(value : TFhirMedicationProduct);
    Procedure SetPackage(value : TFhirMedicationPackage);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    Function GetHasASummary : Boolean; Override;
    function GetResourceType : TFhirResourceType; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirMedication; overload;
    function Clone : TFhirMedication; overload;
    {!script show}
  published
    {@member name
      The common/commercial name of the medication absent information such as strength, form, etc.  E.g. Acetaminophen, Tylenol 3, etc.  The fully coordinated name is communicated as the display of Medication.code.
    }
    property name : TFhirString read FName write SetName;
    {@member nameST
      Typed access to The common/commercial name of the medication absent information such as strength, form, etc.  E.g. Acetaminophen, Tylenol 3, etc.  The fully coordinated name is communicated as the display of Medication.code.
    }
    property nameST : String read GetNameST write SetNameST;

    {@member code
      A code (or set of codes) that identify this medication.   Usage note: This could be a standard drug code such as a drug regulator code, RxNorm code, SNOMED CT code, etc. It could also be a local formulary code, optionally with translations to the standard drug codes.
    }
    property code : TFhirCodeableConcept read FCode write SetCode;

    {@member isBrand
      Set to true if the item is attributable to a specific manufacturer (even if we don't know who that is).
    }
    property isBrand : TFhirBoolean read FIsBrand write SetIsBrand;
    {@member isBrandST
      Typed access to Set to true if the item is attributable to a specific manufacturer (even if we don't know who that is).
    }
    property isBrandST : Boolean read GetIsBrandST write SetIsBrandST;

    {@member manufacturer
      Describes the details of the manufacturer.
    }
    property manufacturer : TFhirResourceReference{TFhirOrganization} read FManufacturer write SetManufacturer;

    {@member kind
      Medications are either a single administrable product or a package that contains one or more products.
    }
    property kind : TFhirEnum read FKind write SetKind;
    {@member kindST
      Typed access to Medications are either a single administrable product or a package that contains one or more products.
    }
    property kindST : TFhirMedicationKind read GetKindST write SetKindST;

    {@member product
      Information that only applies to products (not packages).
    }
    property product : TFhirMedicationProduct read FProduct write SetProduct;

    {@member package
      Information that only applies to packages (not products).
    }
    property package : TFhirMedicationPackage read FPackage write SetPackage;

  end;


  {@Class TFhirMedicationAdministration : TFhirResource
    Describes the event of a patient being given a dose of a medication.  This may be as simple as swallowing a tablet or it may be a long running infusion.

Related resources tie this event to the authorizing prescription, and the specific encounter between patient and health care practitioner.
  }
  {!.Net HL7Connect.Fhir.MedicationAdministration}
  TFhirMedicationAdministration = class (TFhirResource)
  private
    FidentifierList : TFhirIdentifierList;
    FStatus : TFhirEnum;
    FPatient : TFhirResourceReference{TFhirPatient};
    FPractitioner : TFhirResourceReference{TFhirPractitioner};
    FEncounter : TFhirResourceReference{TFhirEncounter};
    FPrescription : TFhirResourceReference{TFhirMedicationPrescription};
    FWasNotGiven : TFhirBoolean;
    FreasonNotGivenList : TFhirCodeableConceptList;
    FWhenGiven : TFhirPeriod;
    FMedication : TFhirResourceReference{TFhirMedication};
    FdeviceList : TFhirResourceReferenceList{TFhirDevice};
    FdosageList : TFhirMedicationAdministrationDosageList;
    Procedure SetStatus(value : TFhirEnum);
    Function GetStatusST : TFhirMedicationAdminStatus;
    Procedure SetStatusST(value : TFhirMedicationAdminStatus);
    Procedure SetPatient(value : TFhirResourceReference{TFhirPatient});
    Procedure SetPractitioner(value : TFhirResourceReference{TFhirPractitioner});
    Procedure SetEncounter(value : TFhirResourceReference{TFhirEncounter});
    Procedure SetPrescription(value : TFhirResourceReference{TFhirMedicationPrescription});
    Procedure SetWasNotGiven(value : TFhirBoolean);
    Function GetWasNotGivenST : Boolean;
    Procedure SetWasNotGivenST(value : Boolean);
    Procedure SetWhenGiven(value : TFhirPeriod);
    Procedure SetMedication(value : TFhirResourceReference{TFhirMedication});
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    Function GetHasASummary : Boolean; Override;
    function GetResourceType : TFhirResourceType; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirMedicationAdministration; overload;
    function Clone : TFhirMedicationAdministration; overload;
    {!script show}
  published
    {@member identifierList
      External identifier - FHIR will generate its own internal IDs (probably URLs) which do not need to be explicitly managed by the resource.  The identifier here is one that would be used by another non-FHIR system - for example an automated medication pump would provide a record each time it operated; an administration while the patient was off the ward might be made with a different system and entered after the event.  Particularly important if these records have to be updated.
    }
    property identifierList : TFhirIdentifierList read FIdentifierList;

    {@member status
      Will generally be set to show that the administration has been completed.  For some long running administrations such as infusions it is possible for an administration to be started but not completed or it may be paused while some other process is under way.
    }
    property status : TFhirEnum read FStatus write SetStatus;
    {@member statusST
      Typed access to Will generally be set to show that the administration has been completed.  For some long running administrations such as infusions it is possible for an administration to be started but not completed or it may be paused while some other process is under way.
    }
    property statusST : TFhirMedicationAdminStatus read GetStatusST write SetStatusST;

    {@member patient
      The person or animal to whom the medication was given.
    }
    property patient : TFhirResourceReference{TFhirPatient} read FPatient write SetPatient;

    {@member practitioner
      The individual who was responsible for giving the medication to the patient.
    }
    property practitioner : TFhirResourceReference{TFhirPractitioner} read FPractitioner write SetPractitioner;

    {@member encounter
      The visit or admission the or other contact between patient and health care provider the medication administration was performed as part of.
    }
    property encounter : TFhirResourceReference{TFhirEncounter} read FEncounter write SetEncounter;

    {@member prescription
      The original request, instruction or authority to perform the administration.
    }
    property prescription : TFhirResourceReference{TFhirMedicationPrescription} read FPrescription write SetPrescription;

    {@member wasNotGiven
      Set this to true if the record is saying that the medication was NOT administered.
    }
    property wasNotGiven : TFhirBoolean read FWasNotGiven write SetWasNotGiven;
    {@member wasNotGivenST
      Typed access to Set this to true if the record is saying that the medication was NOT administered.
    }
    property wasNotGivenST : Boolean read GetWasNotGivenST write SetWasNotGivenST;

    {@member reasonNotGivenList
      A code indicating why the administration was not performed.
    }
    property reasonNotGivenList : TFhirCodeableConceptList read FReasonNotGivenList;

    {@member whenGiven
      An interval of time during which the administration took place.  For many administrations, such as swallowing a tablet the lower and upper values of the interval will be the same.
    }
    property whenGiven : TFhirPeriod read FWhenGiven write SetWhenGiven;

    {@member medication
      Identifies the medication that was administered. This is either a link to a resource representing the details of the medication or a simple attribute carrying a code that identifies the medication from a known list of medications.
    }
    property medication : TFhirResourceReference{TFhirMedication} read FMedication write SetMedication;

    {@member deviceList
      The device used in administering the medication to the patient.  E.g. a particular infusion pump.
    }
    property deviceList : TFhirResourceReferenceList{TFhirDevice} read FDeviceList;

    {@member dosageList
      Provides details of how much of the medication was administered.
    }
    property dosageList : TFhirMedicationAdministrationDosageList read FDosageList;

  end;


  {@Class TFhirMedicationDispense : TFhirResource
    Dispensing a medication to a named patient.  This includes a description of the supply provided and the instructions for administering the medication.
  }
  {!.Net HL7Connect.Fhir.MedicationDispense}
  TFhirMedicationDispense = class (TFhirResource)
  private
    FIdentifier : TFhirIdentifier;
    FStatus : TFhirEnum;
    FPatient : TFhirResourceReference{TFhirPatient};
    FDispenser : TFhirResourceReference{TFhirPractitioner};
    FauthorizingPrescriptionList : TFhirResourceReferenceList{TFhirMedicationPrescription};
    FdispenseList : TFhirMedicationDispenseDispenseList;
    FSubstitution : TFhirMedicationDispenseSubstitution;
    Procedure SetIdentifier(value : TFhirIdentifier);
    Procedure SetStatus(value : TFhirEnum);
    Function GetStatusST : TFhirMedicationDispenseStatus;
    Procedure SetStatusST(value : TFhirMedicationDispenseStatus);
    Procedure SetPatient(value : TFhirResourceReference{TFhirPatient});
    Procedure SetDispenser(value : TFhirResourceReference{TFhirPractitioner});
    Procedure SetSubstitution(value : TFhirMedicationDispenseSubstitution);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    Function GetHasASummary : Boolean; Override;
    function GetResourceType : TFhirResourceType; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirMedicationDispense; overload;
    function Clone : TFhirMedicationDispense; overload;
    {!script show}
  published
    {@member identifier
      Identifier assigned by the dispensing facility - this is an identifier assigned outside FHIR.
    }
    property identifier : TFhirIdentifier read FIdentifier write SetIdentifier;

    {@member status
      A code specifying the state of the set of dispense events.
    }
    property status : TFhirEnum read FStatus write SetStatus;
    {@member statusST
      Typed access to A code specifying the state of the set of dispense events.
    }
    property statusST : TFhirMedicationDispenseStatus read GetStatusST write SetStatusST;

    {@member patient
      A link to a resource representing the person to whom the medication will be given.
    }
    property patient : TFhirResourceReference{TFhirPatient} read FPatient write SetPatient;

    {@member dispenser
      The individual responsible for dispensing the medication.
    }
    property dispenser : TFhirResourceReference{TFhirPractitioner} read FDispenser write SetDispenser;

    {@member authorizingPrescriptionList
      Indicates the medication order that is being dispensed against.
    }
    property authorizingPrescriptionList : TFhirResourceReferenceList{TFhirMedicationPrescription} read FAuthorizingPrescriptionList;

    {@member dispenseList
      Indicates the details of the dispense event such as the days supply and quantity of medication dispensed.
    }
    property dispenseList : TFhirMedicationDispenseDispenseList read FDispenseList;

    {@member substitution
      Indicates whether or not substitution was made as part of the dispense.  In some cases substitution will be expected but doesn't happen, in other cases substitution is not expected but does happen.  This block explains what substitition did or did not happen and why.
    }
    property substitution : TFhirMedicationDispenseSubstitution read FSubstitution write SetSubstitution;

  end;


  {@Class TFhirMedicationPrescription : TFhirResource
    An order for both supply of the medication and the instructions for administration of the medicine to a patient.
  }
  {!.Net HL7Connect.Fhir.MedicationPrescription}
  TFhirMedicationPrescription = class (TFhirResource)
  private
    FidentifierList : TFhirIdentifierList;
    FDateWritten : TFhirDateTime;
    FStatus : TFhirEnum;
    FPatient : TFhirResourceReference{TFhirPatient};
    FPrescriber : TFhirResourceReference{TFhirPractitioner};
    FEncounter : TFhirResourceReference{TFhirEncounter};
    FReason : TFhirType;
    FMedication : TFhirResourceReference{TFhirMedication};
    FdosageInstructionList : TFhirMedicationPrescriptionDosageInstructionList;
    FDispense : TFhirMedicationPrescriptionDispense;
    FSubstitution : TFhirMedicationPrescriptionSubstitution;
    Procedure SetDateWritten(value : TFhirDateTime);
    Function GetDateWrittenST : TDateAndTime;
    Procedure SetDateWrittenST(value : TDateAndTime);
    Procedure SetStatus(value : TFhirEnum);
    Function GetStatusST : TFhirMedicationPrescriptionStatus;
    Procedure SetStatusST(value : TFhirMedicationPrescriptionStatus);
    Procedure SetPatient(value : TFhirResourceReference{TFhirPatient});
    Procedure SetPrescriber(value : TFhirResourceReference{TFhirPractitioner});
    Procedure SetEncounter(value : TFhirResourceReference{TFhirEncounter});
    Procedure SetReason(value : TFhirType);
    Procedure SetMedication(value : TFhirResourceReference{TFhirMedication});
    Procedure SetDispense(value : TFhirMedicationPrescriptionDispense);
    Procedure SetSubstitution(value : TFhirMedicationPrescriptionSubstitution);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    Function GetHasASummary : Boolean; Override;
    function GetResourceType : TFhirResourceType; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirMedicationPrescription; overload;
    function Clone : TFhirMedicationPrescription; overload;
    {!script show}
  published
    {@member identifierList
      External identifier - one that would be used by another non-FHIR system - for example a re-imbursement system might issue its own id for each prescription that is created.  This is particularly important where FHIR only provides part of an erntire workflow process where records have to be tracked through an entire system.
    }
    property identifierList : TFhirIdentifierList read FIdentifierList;

    {@member dateWritten
      The date (and perhaps time) when the prescription was written.
    }
    property dateWritten : TFhirDateTime read FDateWritten write SetDateWritten;
    {@member dateWrittenST
      Typed access to The date (and perhaps time) when the prescription was written.
    }
    property dateWrittenST : TDateAndTime read GetDateWrittenST write SetDateWrittenST;

    {@member status
      A code specifying the state of the order.  Generally this will be active or completed state.
    }
    property status : TFhirEnum read FStatus write SetStatus;
    {@member statusST
      Typed access to A code specifying the state of the order.  Generally this will be active or completed state.
    }
    property statusST : TFhirMedicationPrescriptionStatus read GetStatusST write SetStatusST;

    {@member patient
      A link to a resource representing the person to whom the medication will be given.
    }
    property patient : TFhirResourceReference{TFhirPatient} read FPatient write SetPatient;

    {@member prescriber
      The healthcare professional responsible for authorizing the prescription.
    }
    property prescriber : TFhirResourceReference{TFhirPractitioner} read FPrescriber write SetPrescriber;

    {@member encounter
      A link to a resource that identifies the particular occurrence of contact between patient and health care provider.
    }
    property encounter : TFhirResourceReference{TFhirEncounter} read FEncounter write SetEncounter;

    {@member reason
      Can be the reason or the indication for writing the prescription.
    }
    property reason : TFhirType read FReason write SetReason;

    {@member medication
      Identifies the medication being administered. This is either a link to a resource representing the details of the medication or a simple attribute carrying a code that identifies the medication from a known list of medications.
    }
    property medication : TFhirResourceReference{TFhirMedication} read FMedication write SetMedication;

    {@member dosageInstructionList
      Indicates how the medication is to be used by the patient.
    }
    property dosageInstructionList : TFhirMedicationPrescriptionDosageInstructionList read FDosageInstructionList;

    {@member dispense
      Deals with details of the dispense part of the order.
    }
    property dispense : TFhirMedicationPrescriptionDispense read FDispense write SetDispense;

    {@member substitution
      Indicates whether or not substitution can or should be part of the dispense. In some cases substitution must happen, in other cases substitution must not happen, and in others it does not matter. This block explains the prescriber's intent. If nothing is specified substitution may be done.
    }
    property substitution : TFhirMedicationPrescriptionSubstitution read FSubstitution write SetSubstitution;

  end;


  {@Class TFhirMedicationStatement : TFhirResource
    A record of medication being taken by a patient, or that the medication has been given to a patient where the record is the result of a report from the patient or another clinician.
  }
  {!.Net HL7Connect.Fhir.MedicationStatement}
  TFhirMedicationStatement = class (TFhirResource)
  private
    FidentifierList : TFhirIdentifierList;
    FPatient : TFhirResourceReference{TFhirPatient};
    FWasNotGiven : TFhirBoolean;
    FreasonNotGivenList : TFhirCodeableConceptList;
    FWhenGiven : TFhirPeriod;
    FMedication : TFhirResourceReference{TFhirMedication};
    FdeviceList : TFhirResourceReferenceList{TFhirDevice};
    FdosageList : TFhirMedicationStatementDosageList;
    Procedure SetPatient(value : TFhirResourceReference{TFhirPatient});
    Procedure SetWasNotGiven(value : TFhirBoolean);
    Function GetWasNotGivenST : Boolean;
    Procedure SetWasNotGivenST(value : Boolean);
    Procedure SetWhenGiven(value : TFhirPeriod);
    Procedure SetMedication(value : TFhirResourceReference{TFhirMedication});
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    Function GetHasASummary : Boolean; Override;
    function GetResourceType : TFhirResourceType; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirMedicationStatement; overload;
    function Clone : TFhirMedicationStatement; overload;
    {!script show}
  published
    {@member identifierList
      External identifier - FHIR will generate its own internal IDs (probably URLs) which do not need to be explicitly managed by the resource.  The identifier here is one that would be used by another non-FHIR system - for example an automated medication pump would provide a record each time it operated; an administration while the patient was off the ward might be made with a different system and entered after the event.  Particularly important if these records have to be updated.
    }
    property identifierList : TFhirIdentifierList read FIdentifierList;

    {@member patient
      The person or animal who is /was taking the medication.
    }
    property patient : TFhirResourceReference{TFhirPatient} read FPatient write SetPatient;

    {@member wasNotGiven
      Set this to true if the record is saying that the medication was NOT taken.
    }
    property wasNotGiven : TFhirBoolean read FWasNotGiven write SetWasNotGiven;
    {@member wasNotGivenST
      Typed access to Set this to true if the record is saying that the medication was NOT taken.
    }
    property wasNotGivenST : Boolean read GetWasNotGivenST write SetWasNotGivenST;

    {@member reasonNotGivenList
      A code indicating why the medication was not taken.
    }
    property reasonNotGivenList : TFhirCodeableConceptList read FReasonNotGivenList;

    {@member whenGiven
      The interval of time during which it is being asserted that the patient was taking the medication.
    }
    property whenGiven : TFhirPeriod read FWhenGiven write SetWhenGiven;

    {@member medication
      Identifies the medication being administered. This is either a link to a resource representing the details of the medication or a simple attribute carrying a code that identifies the medication from a known list of medications.
    }
    property medication : TFhirResourceReference{TFhirMedication} read FMedication write SetMedication;

    {@member deviceList
      An identifier or a link to a resource that identifies a device used in administering the medication to the patient.
    }
    property deviceList : TFhirResourceReferenceList{TFhirDevice} read FDeviceList;

    {@member dosageList
      Indicates how the medication is/was used by the patient.
    }
    property dosageList : TFhirMedicationStatementDosageList read FDosageList;

  end;


  {@Class TFhirMessageHeader : TFhirResource
    The header for a message exchange that is either requesting or responding to an action.  The resource(s) that are the subject of the action as well as other Information related to the action are typically transmitted in a bundle in which the MessageHeader resource instance is the first resource in the bundle.
  }
  {!.Net HL7Connect.Fhir.MessageHeader}
  TFhirMessageHeader = class (TFhirResource)
  private
    FIdentifier : TFhirId;
    FTimestamp : TFhirInstant;
    FEvent : TFhirCoding;
    FResponse : TFhirMessageHeaderResponse;
    FSource : TFhirMessageHeaderSource;
    FdestinationList : TFhirMessageHeaderDestinationList;
    FEnterer : TFhirResourceReference{TFhirPractitioner};
    FAuthor : TFhirResourceReference{TFhirPractitioner};
    FReceiver : TFhirResourceReference{Resource};
    FResponsible : TFhirResourceReference{Resource};
    FReason : TFhirCodeableConcept;
    FdataList : TFhirResourceReferenceList{Resource};
    Procedure SetIdentifier(value : TFhirId);
    Function GetIdentifierST : String;
    Procedure SetIdentifierST(value : String);
    Procedure SetTimestamp(value : TFhirInstant);
    Function GetTimestampST : TDateAndTime;
    Procedure SetTimestampST(value : TDateAndTime);
    Procedure SetEvent(value : TFhirCoding);
    Procedure SetResponse(value : TFhirMessageHeaderResponse);
    Procedure SetSource(value : TFhirMessageHeaderSource);
    Procedure SetEnterer(value : TFhirResourceReference{TFhirPractitioner});
    Procedure SetAuthor(value : TFhirResourceReference{TFhirPractitioner});
    Procedure SetReceiver(value : TFhirResourceReference{Resource});
    Procedure SetResponsible(value : TFhirResourceReference{Resource});
    Procedure SetReason(value : TFhirCodeableConcept);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    Function GetHasASummary : Boolean; Override;
    function GetResourceType : TFhirResourceType; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirMessageHeader; overload;
    function Clone : TFhirMessageHeader; overload;
    {!script show}
  published
    {@member identifier
      The identifier of this message.
    }
    property identifier : TFhirId read FIdentifier write SetIdentifier;
    {@member identifierST
      Typed access to The identifier of this message.
    }
    property identifierST : String read GetIdentifierST write SetIdentifierST;

    {@member timestamp
      The time that the message was sent.
    }
    property timestamp : TFhirInstant read FTimestamp write SetTimestamp;
    {@member timestampST
      Typed access to The time that the message was sent.
    }
    property timestampST : TDateAndTime read GetTimestampST write SetTimestampST;

    {@member event
      Code that identifies the event this message represents and connects it with it's definition. Events defined as part of the FHIR specification have the system value "http://hl7.org/fhir/message-type".
    }
    property event : TFhirCoding read FEvent write SetEvent;

    {@member response
      Information about the message that this message is a response to.  Only present if this message is a response.
    }
    property response : TFhirMessageHeaderResponse read FResponse write SetResponse;

    {@member source
      The source application from which this message originated.
    }
    property source : TFhirMessageHeaderSource read FSource write SetSource;

    {@member destinationList
      The destination application which the message is intended for.
    }
    property destinationList : TFhirMessageHeaderDestinationList read FDestinationList;

    {@member enterer
      The person or device that performed the data entry leading to this message. Where there is more than one candidate, pick the most proximal to the message. Can provide other enterers in extensions.
    }
    property enterer : TFhirResourceReference{TFhirPractitioner} read FEnterer write SetEnterer;

    {@member author
      The logical author of the message - the person or device that decided the described event should happen. Where there is more than one candidate, pick the most proximal to the MessageHeader. Can provide other authors in extensions.
    }
    property author : TFhirResourceReference{TFhirPractitioner} read FAuthor write SetAuthor;

    {@member receiver
      Allows data conveyed by a message to be addressed to a particular person or department when routing to a specific application isn't sufficient.
    }
    property receiver : TFhirResourceReference{Resource} read FReceiver write SetReceiver;

    {@member responsible
      The person or organization that accepts overall responsibility for the contents of the message. The implication is that the message event happened under the policies of the responsible party.
    }
    property responsible : TFhirResourceReference{Resource} read FResponsible write SetResponsible;

    {@member reason
      Coded indication of the cause for the event - indicates  a reason for the occurance of the event that is a focus of this message.
    }
    property reason : TFhirCodeableConcept read FReason write SetReason;

    {@member dataList
      The actual data of the message - a reference to the root/focus class of the event.
    }
    property dataList : TFhirResourceReferenceList{Resource} read FDataList;

  end;


  {@Class TFhirNamespace : TFhirResource
    A curated namespace that issues unique symbols within that namespace for the identification of concepts, people, devices, etc.  Represents a "System" used within the Identifier and Coding data types.
  }
  {!.Net HL7Connect.Fhir.Namespace}
  TFhirNamespace = class (TFhirResource)
  private
    FType_ : TFhirEnum;
    FName : TFhirString;
    FStatus : TFhirEnum;
    FCountry : TFhirCode;
    FCategory : TFhirCodeableConcept;
    FResponsible : TFhirString;
    FDescription : TFhirString;
    FUsage : TFhirString;
    FuniqueIdList : TFhirNamespaceUniqueIdList;
    FContact : TFhirNamespaceContact;
    FReplacedBy : TFhirResourceReference{TFhirNamespace};
    Procedure SetType_(value : TFhirEnum);
    Function GetType_ST : TFhirNamespaceType;
    Procedure SetType_ST(value : TFhirNamespaceType);
    Procedure SetName(value : TFhirString);
    Function GetNameST : String;
    Procedure SetNameST(value : String);
    Procedure SetStatus(value : TFhirEnum);
    Function GetStatusST : TFhirNamespaceStatus;
    Procedure SetStatusST(value : TFhirNamespaceStatus);
    Procedure SetCountry(value : TFhirCode);
    Function GetCountryST : String;
    Procedure SetCountryST(value : String);
    Procedure SetCategory(value : TFhirCodeableConcept);
    Procedure SetResponsible(value : TFhirString);
    Function GetResponsibleST : String;
    Procedure SetResponsibleST(value : String);
    Procedure SetDescription(value : TFhirString);
    Function GetDescriptionST : String;
    Procedure SetDescriptionST(value : String);
    Procedure SetUsage(value : TFhirString);
    Function GetUsageST : String;
    Procedure SetUsageST(value : String);
    Procedure SetContact(value : TFhirNamespaceContact);
    Procedure SetReplacedBy(value : TFhirResourceReference{TFhirNamespace});
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    Function GetHasASummary : Boolean; Override;
    function GetResourceType : TFhirResourceType; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirNamespace; overload;
    function Clone : TFhirNamespace; overload;
    {!script show}
  published
    {@member type_
      Indicates the purpose for the namespace - what kinds of things does it make unique?.
    }
    property type_ : TFhirEnum read FType_ write SetType_;
    {@member type_ST
      Typed access to Indicates the purpose for the namespace - what kinds of things does it make unique?.
    }
    property type_ST : TFhirNamespaceType read GetType_ST write SetType_ST;

    {@member name
      The descriptive name of this particular identifier type or code system.
    }
    property name : TFhirString read FName write SetName;
    {@member nameST
      Typed access to The descriptive name of this particular identifier type or code system.
    }
    property nameST : String read GetNameST write SetNameST;

    {@member status
      Indicates whether the namespace is "ready for use" or not.
    }
    property status : TFhirEnum read FStatus write SetStatus;
    {@member statusST
      Typed access to Indicates whether the namespace is "ready for use" or not.
    }
    property statusST : TFhirNamespaceStatus read GetStatusST write SetStatusST;

    {@member country
      If present, indicates that the identifier or code system is principally intended for use or applies to entities within the specified country.  For example, the country associated with a national code system.
    }
    property country : TFhirCode read FCountry write SetCountry;
    {@member countryST
      Typed access to If present, indicates that the identifier or code system is principally intended for use or applies to entities within the specified country.  For example, the country associated with a national code system.
    }
    property countryST : String read GetCountryST write SetCountryST;

    {@member category
      Categorizes a namespace for easier search by grouping related namespaces.
    }
    property category : TFhirCodeableConcept read FCategory write SetCategory;

    {@member responsible
      The name of the organization that is responsible for issuing identifiers or codes for this namespace and ensuring their non-collision.
    }
    property responsible : TFhirString read FResponsible write SetResponsible;
    {@member responsibleST
      Typed access to The name of the organization that is responsible for issuing identifiers or codes for this namespace and ensuring their non-collision.
    }
    property responsibleST : String read GetResponsibleST write SetResponsibleST;

    {@member description
      Details about what the namespace identifies including scope, granularity, version labeling, etc.
    }
    property description : TFhirString read FDescription write SetDescription;
    {@member descriptionST
      Typed access to Details about what the namespace identifies including scope, granularity, version labeling, etc.
    }
    property descriptionST : String read GetDescriptionST write SetDescriptionST;

    {@member usage
      Provides guidance on the use of the namespace, including the handling of formatting characters, use of upper vs. lower case, etc.
    }
    property usage : TFhirString read FUsage write SetUsage;
    {@member usageST
      Typed access to Provides guidance on the use of the namespace, including the handling of formatting characters, use of upper vs. lower case, etc.
    }
    property usageST : String read GetUsageST write SetUsageST;

    {@member uniqueIdList
      Indicates how the system may be identified when referenced in electronic exchange.
    }
    property uniqueIdList : TFhirNamespaceUniqueIdList read FUniqueIdList;

    {@member contact
      The person who can be contacted about this system registration entry.
    }
    property contact : TFhirNamespaceContact read FContact write SetContact;

    {@member replacedBy
      For namespaces that are retired, indicates the namespace that should be used in their place (if any).
    }
    property replacedBy : TFhirResourceReference{TFhirNamespace} read FReplacedBy write SetReplacedBy;

  end;


  {@Class TFhirObservation : TFhirResource
    Measurements and simple assertions made about a patient, device or other subject.
  }
  {!.Net HL7Connect.Fhir.Observation}
  TFhirObservation = class (TFhirResource)
  private
    FName : TFhirCodeableConcept;
    FValue : TFhirType;
    FInterpretation : TFhirCodeableConcept;
    FComments : TFhirString;
    FApplies : TFhirType;
    FIssued : TFhirInstant;
    FStatus : TFhirEnum;
    FReliability : TFhirEnum;
    FBodySite : TFhirCodeableConcept;
    FMethod : TFhirCodeableConcept;
    FIdentifier : TFhirIdentifier;
    FSubject : TFhirResourceReference{Resource};
    FSpecimen : TFhirResourceReference{TFhirSpecimen};
    FperformerList : TFhirResourceReferenceList{Resource};
    FEncounter : TFhirResourceReference{TFhirEncounter};
    FreferenceRangeList : TFhirObservationReferenceRangeList;
    FrelatedList : TFhirObservationRelatedList;
    Procedure SetName(value : TFhirCodeableConcept);
    Procedure SetValue(value : TFhirType);
    Procedure SetInterpretation(value : TFhirCodeableConcept);
    Procedure SetComments(value : TFhirString);
    Function GetCommentsST : String;
    Procedure SetCommentsST(value : String);
    Procedure SetApplies(value : TFhirType);
    Procedure SetIssued(value : TFhirInstant);
    Function GetIssuedST : TDateAndTime;
    Procedure SetIssuedST(value : TDateAndTime);
    Procedure SetStatus(value : TFhirEnum);
    Function GetStatusST : TFhirObservationStatus;
    Procedure SetStatusST(value : TFhirObservationStatus);
    Procedure SetReliability(value : TFhirEnum);
    Function GetReliabilityST : TFhirObservationReliability;
    Procedure SetReliabilityST(value : TFhirObservationReliability);
    Procedure SetBodySite(value : TFhirCodeableConcept);
    Procedure SetMethod(value : TFhirCodeableConcept);
    Procedure SetIdentifier(value : TFhirIdentifier);
    Procedure SetSubject(value : TFhirResourceReference{Resource});
    Procedure SetSpecimen(value : TFhirResourceReference{TFhirSpecimen});
    Procedure SetEncounter(value : TFhirResourceReference{TFhirEncounter});
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    Function GetHasASummary : Boolean; Override;
    function GetResourceType : TFhirResourceType; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirObservation; overload;
    function Clone : TFhirObservation; overload;
    {!script show}
  published
    {@member name
      Describes what was observed. Sometimes this is called the observation "code".
    }
    property name : TFhirCodeableConcept read FName write SetName;

    {@member value
      The information determined as a result of making the observation, if the information has a simple value.
    }
    property value : TFhirType read FValue write SetValue;

    {@member interpretation
      The assessment made based on the result of the observation.
    }
    property interpretation : TFhirCodeableConcept read FInterpretation write SetInterpretation;

    {@member comments
      May include statements about significant, unexpected or unreliable values, or information about the source of the value where this may be relevant to the interpretation of the result.
    }
    property comments : TFhirString read FComments write SetComments;
    {@member commentsST
      Typed access to May include statements about significant, unexpected or unreliable values, or information about the source of the value where this may be relevant to the interpretation of the result.
    }
    property commentsST : String read GetCommentsST write SetCommentsST;

    {@member applies
      The time or time-period the observed value is asserted as being true. For biological subjects - e.g. human patients - this is usually called the "physiologically relevant time". This is usually either the time of the procedure or of specimen collection, but very often the source of the date/time is not known, only the date/time itself.
    }
    property applies : TFhirType read FApplies write SetApplies;

    {@member issued
      Date/Time this was made available.
    }
    property issued : TFhirInstant read FIssued write SetIssued;
    {@member issuedST
      Typed access to Date/Time this was made available.
    }
    property issuedST : TDateAndTime read GetIssuedST write SetIssuedST;

    {@member status
      The status of the result value.
    }
    property status : TFhirEnum read FStatus write SetStatus;
    {@member statusST
      Typed access to The status of the result value.
    }
    property statusST : TFhirObservationStatus read GetStatusST write SetStatusST;

    {@member reliability
      An estimate of the degree to which quality issues have impacted on the value reported.
    }
    property reliability : TFhirEnum read FReliability write SetReliability;
    {@member reliabilityST
      Typed access to An estimate of the degree to which quality issues have impacted on the value reported.
    }
    property reliabilityST : TFhirObservationReliability read GetReliabilityST write SetReliabilityST;

    {@member bodySite
      Indicates where on the subject's body the observation was made.
    }
    property bodySite : TFhirCodeableConcept read FBodySite write SetBodySite;

    {@member method
      Indicates the mechanism used to perform the observation.
    }
    property method : TFhirCodeableConcept read FMethod write SetMethod;

    {@member identifier
      A unique identifier for the simple observation.
    }
    property identifier : TFhirIdentifier read FIdentifier write SetIdentifier;

    {@member subject
      The thing the observation is being made about.
    }
    property subject : TFhirResourceReference{Resource} read FSubject write SetSubject;

    {@member specimen
      The specimen that was used when this observation was made.
    }
    property specimen : TFhirResourceReference{TFhirSpecimen} read FSpecimen write SetSpecimen;

    {@member performerList
      Who was responsible for asserting the observed value as "true".
    }
    property performerList : TFhirResourceReferenceList{Resource} read FPerformerList;

    {@member encounter
      The healthcare event  ( e.g. a patient and healthcare provider interaction ) that relates to this observation.
    }
    property encounter : TFhirResourceReference{TFhirEncounter} read FEncounter write SetEncounter;

    {@member referenceRangeList
      Guidance on how to interpret the value by comparison to a normal or recommended range.
    }
    property referenceRangeList : TFhirObservationReferenceRangeList read FReferenceRangeList;

    {@member relatedList
      Related observations - either components, or previous observations, or statements of derivation.
    }
    property relatedList : TFhirObservationRelatedList read FRelatedList;

  end;


  {@Class TFhirOperationDefinition : TFhirResource
    A formal computable definition of an operation (on the RESTful interface) or a named query (using the search interaction).
  }
  {!.Net HL7Connect.Fhir.OperationDefinition}
  TFhirOperationDefinition = class (TFhirResource)
  private
    FIdentifier : TFhirUri;
    FVersion : TFhirString;
    FTitle : TFhirString;
    FPublisher : TFhirString;
    FtelecomList : TFhirContactList;
    FDescription : TFhirString;
    FcodeList : TFhirCodingList;
    FStatus : TFhirEnum;
    FExperimental : TFhirBoolean;
    FDate : TFhirDateTime;
    FKind : TFhirEnum;
    FName : TFhirCode;
    FNotes : TFhirString;
    FBase : TFhirResourceReference{TFhirOperationDefinition};
    FSystem : TFhirBoolean;
    Ftype_List : TFhirCodeList;
    FInstance : TFhirBoolean;
    FparameterList : TFhirOperationDefinitionParameterList;
    Procedure SetIdentifier(value : TFhirUri);
    Function GetIdentifierST : String;
    Procedure SetIdentifierST(value : String);
    Procedure SetVersion(value : TFhirString);
    Function GetVersionST : String;
    Procedure SetVersionST(value : String);
    Procedure SetTitle(value : TFhirString);
    Function GetTitleST : String;
    Procedure SetTitleST(value : String);
    Procedure SetPublisher(value : TFhirString);
    Function GetPublisherST : String;
    Procedure SetPublisherST(value : String);
    Procedure SetDescription(value : TFhirString);
    Function GetDescriptionST : String;
    Procedure SetDescriptionST(value : String);
    Procedure SetStatus(value : TFhirEnum);
    Function GetStatusST : TFhirResourceProfileStatus;
    Procedure SetStatusST(value : TFhirResourceProfileStatus);
    Procedure SetExperimental(value : TFhirBoolean);
    Function GetExperimentalST : Boolean;
    Procedure SetExperimentalST(value : Boolean);
    Procedure SetDate(value : TFhirDateTime);
    Function GetDateST : TDateAndTime;
    Procedure SetDateST(value : TDateAndTime);
    Procedure SetKind(value : TFhirEnum);
    Function GetKindST : TFhirOperationKind;
    Procedure SetKindST(value : TFhirOperationKind);
    Procedure SetName(value : TFhirCode);
    Function GetNameST : String;
    Procedure SetNameST(value : String);
    Procedure SetNotes(value : TFhirString);
    Function GetNotesST : String;
    Procedure SetNotesST(value : String);
    Procedure SetBase(value : TFhirResourceReference{TFhirOperationDefinition});
    Procedure SetSystem(value : TFhirBoolean);
    Function GetSystemST : Boolean;
    Procedure SetSystemST(value : Boolean);
    Procedure SetInstance(value : TFhirBoolean);
    Function GetInstanceST : Boolean;
    Procedure SetInstanceST(value : Boolean);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    Function GetHasASummary : Boolean; Override;
    function GetResourceType : TFhirResourceType; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirOperationDefinition; overload;
    function Clone : TFhirOperationDefinition; overload;
    {!script show}
  published
    {@member identifier
      The identifier that is used to identify this operation definition when it is referenced in a specification, model, design or an instance (should be globally unique OID, UUID, or URI).
    }
    property identifier : TFhirUri read FIdentifier write SetIdentifier;
    {@member identifierST
      Typed access to The identifier that is used to identify this operation definition when it is referenced in a specification, model, design or an instance (should be globally unique OID, UUID, or URI).
    }
    property identifierST : String read GetIdentifierST write SetIdentifierST;

    {@member version
      The identifier that is used to identify this version of the profile when it is referenced in a specification, model, design or instance. This is an arbitrary value managed by the profile author manually and the value should be a timestamp.
    }
    property version : TFhirString read FVersion write SetVersion;
    {@member versionST
      Typed access to The identifier that is used to identify this version of the profile when it is referenced in a specification, model, design or instance. This is an arbitrary value managed by the profile author manually and the value should be a timestamp.
    }
    property versionST : String read GetVersionST write SetVersionST;

    {@member title
      A free text natural language name identifying the Profile.
    }
    property title : TFhirString read FTitle write SetTitle;
    {@member titleST
      Typed access to A free text natural language name identifying the Profile.
    }
    property titleST : String read GetTitleST write SetTitleST;

    {@member publisher
      Details of the individual or organization who accepts responsibility for publishing the profile.
    }
    property publisher : TFhirString read FPublisher write SetPublisher;
    {@member publisherST
      Typed access to Details of the individual or organization who accepts responsibility for publishing the profile.
    }
    property publisherST : String read GetPublisherST write SetPublisherST;

    {@member telecomList
      Contact details to assist a user in finding and communicating with the publisher.
    }
    property telecomList : TFhirContactList read FTelecomList;

    {@member description
      A free text natural language description of the profile and its use.
    }
    property description : TFhirString read FDescription write SetDescription;
    {@member descriptionST
      Typed access to A free text natural language description of the profile and its use.
    }
    property descriptionST : String read GetDescriptionST write SetDescriptionST;

    {@member codeList
      A set of terms from external terminologies that may be used to assist with indexing and searching of templates.
    }
    property codeList : TFhirCodingList read FCodeList;

    {@member status
      The status of the profile.
    }
    property status : TFhirEnum read FStatus write SetStatus;
    {@member statusST
      Typed access to The status of the profile.
    }
    property statusST : TFhirResourceProfileStatus read GetStatusST write SetStatusST;

    {@member experimental
      This profile was authored for testing purposes (or education/evaluation/marketing), and is not intended to be used for genuine usage.
    }
    property experimental : TFhirBoolean read FExperimental write SetExperimental;
    {@member experimentalST
      Typed access to This profile was authored for testing purposes (or education/evaluation/marketing), and is not intended to be used for genuine usage.
    }
    property experimentalST : Boolean read GetExperimentalST write SetExperimentalST;

    {@member date
      The date that this version of the profile was published.
    }
    property date : TFhirDateTime read FDate write SetDate;
    {@member dateST
      Typed access to The date that this version of the profile was published.
    }
    property dateST : TDateAndTime read GetDateST write SetDateST;

    {@member kind
      Whether this is operation or named query.
    }
    property kind : TFhirEnum read FKind write SetKind;
    {@member kindST
      Typed access to Whether this is operation or named query.
    }
    property kindST : TFhirOperationKind read GetKindST write SetKindST;

    {@member name
      The name used to invoke the operation.
    }
    property name : TFhirCode read FName write SetName;
    {@member nameST
      Typed access to The name used to invoke the operation.
    }
    property nameST : String read GetNameST write SetNameST;

    {@member notes
      Additional information about how to use this operation or named query.
    }
    property notes : TFhirString read FNotes write SetNotes;
    {@member notesST
      Typed access to Additional information about how to use this operation or named query.
    }
    property notesST : String read GetNotesST write SetNotesST;

    {@member base
      Indicates that this operation definition is a constraining profile on the base.
    }
    property base : TFhirResourceReference{TFhirOperationDefinition} read FBase write SetBase;

    {@member system
      Indicates whether this operation or named query can be invoked at the system level (e.g. without needing to choose a resource type for the context).
    }
    property system : TFhirBoolean read FSystem write SetSystem;
    {@member systemST
      Typed access to Indicates whether this operation or named query can be invoked at the system level (e.g. without needing to choose a resource type for the context).
    }
    property systemST : Boolean read GetSystemST write SetSystemST;

    {@member type_List
      Indicates whether this operation or named query can be invoked at the resource type level for any given resource type level (e.g. without needing to choose a resource type for the context).
    }
    property type_List : TFhirCodeList read FType_List;

    {@member instance
      Indicates whether this operation can be invoked on a particular instance of one of the given types.
    }
    property instance : TFhirBoolean read FInstance write SetInstance;
    {@member instanceST
      Typed access to Indicates whether this operation can be invoked on a particular instance of one of the given types.
    }
    property instanceST : Boolean read GetInstanceST write SetInstanceST;

    {@member parameterList
      Parameters for the operation/query.
    }
    property parameterList : TFhirOperationDefinitionParameterList read FParameterList;

  end;


  {@Class TFhirOperationOutcome : TFhirResource
    A collection of error, warning or information messages that result from a system action.
  }
  {!.Net HL7Connect.Fhir.OperationOutcome}
  TFhirOperationOutcome = class (TFhirResource)
  private
    FissueList : TFhirOperationOutcomeIssueList;
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    Function GetHasASummary : Boolean; Override;
    function GetResourceType : TFhirResourceType; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirOperationOutcome; overload;
    function Clone : TFhirOperationOutcome; overload;
    {!script show}
  published
    {@member issueList
      An error, warning or information message that results from a system action.
    }
    property issueList : TFhirOperationOutcomeIssueList read FIssueList;

  end;


  {@Class TFhirOrder : TFhirResource
    A request to perform an action.
  }
  {!.Net HL7Connect.Fhir.Order}
  TFhirOrder = class (TFhirResource)
  private
    FidentifierList : TFhirIdentifierList;
    FDate : TFhirDateTime;
    FSubject : TFhirResourceReference{TFhirPatient};
    FSource : TFhirResourceReference{TFhirPractitioner};
    FTarget : TFhirResourceReference{Resource};
    FReason : TFhirType;
    FAuthority : TFhirResourceReference{Resource};
    FWhen : TFhirOrderWhen;
    FdetailList : TFhirResourceReferenceList{Resource};
    Procedure SetDate(value : TFhirDateTime);
    Function GetDateST : TDateAndTime;
    Procedure SetDateST(value : TDateAndTime);
    Procedure SetSubject(value : TFhirResourceReference{TFhirPatient});
    Procedure SetSource(value : TFhirResourceReference{TFhirPractitioner});
    Procedure SetTarget(value : TFhirResourceReference{Resource});
    Procedure SetReason(value : TFhirType);
    Procedure SetAuthority(value : TFhirResourceReference{Resource});
    Procedure SetWhen(value : TFhirOrderWhen);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    Function GetHasASummary : Boolean; Override;
    function GetResourceType : TFhirResourceType; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirOrder; overload;
    function Clone : TFhirOrder; overload;
    {!script show}
  published
    {@member identifierList
      Identifiers assigned to this order by the orderer or by the receiver.
    }
    property identifierList : TFhirIdentifierList read FIdentifierList;

    {@member date
      When the order was made.
    }
    property date : TFhirDateTime read FDate write SetDate;
    {@member dateST
      Typed access to When the order was made.
    }
    property dateST : TDateAndTime read GetDateST write SetDateST;

    {@member subject
      Patient this order is about.
    }
    property subject : TFhirResourceReference{TFhirPatient} read FSubject write SetSubject;

    {@member source
      Who initiated the order.
    }
    property source : TFhirResourceReference{TFhirPractitioner} read FSource write SetSource;

    {@member target
      Who is intended to fulfill the order.
    }
    property target : TFhirResourceReference{Resource} read FTarget write SetTarget;

    {@member reason
      Text - why the order was made.
    }
    property reason : TFhirType read FReason write SetReason;

    {@member authority
      If required by policy.
    }
    property authority : TFhirResourceReference{Resource} read FAuthority write SetAuthority;

    {@member when
      When order should be fulfilled.
    }
    property when : TFhirOrderWhen read FWhen write SetWhen;

    {@member detailList
      What action is being ordered.
    }
    property detailList : TFhirResourceReferenceList{Resource} read FDetailList;

  end;


  {@Class TFhirOrderResponse : TFhirResource
    A response to an order.
  }
  {!.Net HL7Connect.Fhir.OrderResponse}
  TFhirOrderResponse = class (TFhirResource)
  private
    FidentifierList : TFhirIdentifierList;
    FRequest : TFhirResourceReference{TFhirOrder};
    FDate : TFhirDateTime;
    FWho : TFhirResourceReference{Resource};
    FAuthority : TFhirType;
    FCode : TFhirEnum;
    FDescription : TFhirString;
    FfulfillmentList : TFhirResourceReferenceList{Resource};
    Procedure SetRequest(value : TFhirResourceReference{TFhirOrder});
    Procedure SetDate(value : TFhirDateTime);
    Function GetDateST : TDateAndTime;
    Procedure SetDateST(value : TDateAndTime);
    Procedure SetWho(value : TFhirResourceReference{Resource});
    Procedure SetAuthority(value : TFhirType);
    Procedure SetCode(value : TFhirEnum);
    Function GetCodeST : TFhirOrderOutcomeCode;
    Procedure SetCodeST(value : TFhirOrderOutcomeCode);
    Procedure SetDescription(value : TFhirString);
    Function GetDescriptionST : String;
    Procedure SetDescriptionST(value : String);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    Function GetHasASummary : Boolean; Override;
    function GetResourceType : TFhirResourceType; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirOrderResponse; overload;
    function Clone : TFhirOrderResponse; overload;
    {!script show}
  published
    {@member identifierList
      Identifiers assigned to this order. The identifiers are usually assigned by the system responding to the order, but they may be provided or added to by other systems.
    }
    property identifierList : TFhirIdentifierList read FIdentifierList;

    {@member request
      A reference to the order that this is in response to.
    }
    property request : TFhirResourceReference{TFhirOrder} read FRequest write SetRequest;

    {@member date
      The date and time at which this order response was made (created/posted).
    }
    property date : TFhirDateTime read FDate write SetDate;
    {@member dateST
      Typed access to The date and time at which this order response was made (created/posted).
    }
    property dateST : TDateAndTime read GetDateST write SetDateST;

    {@member who
      The person, organization, or device credited with making the response.
    }
    property who : TFhirResourceReference{Resource} read FWho write SetWho;

    {@member authority
      A reference to an authority policy that is the reason for the response. Usually this is used when the order is rejected, to provide a reason for rejection.
    }
    property authority : TFhirType read FAuthority write SetAuthority;

    {@member code
      What this response says about the status of the original order.
    }
    property code : TFhirEnum read FCode write SetCode;
    {@member codeST
      Typed access to What this response says about the status of the original order.
    }
    property codeST : TFhirOrderOutcomeCode read GetCodeST write SetCodeST;

    {@member description
      Additional description about the response - e.g. a text description provided by a human user when making decisions about the order.
    }
    property description : TFhirString read FDescription write SetDescription;
    {@member descriptionST
      Typed access to Additional description about the response - e.g. a text description provided by a human user when making decisions about the order.
    }
    property descriptionST : String read GetDescriptionST write SetDescriptionST;

    {@member fulfillmentList
      Links to resources that provide details of the outcome of performing the order. E.g. Diagnostic Reports in a response that is made to an order that referenced a diagnostic order.
    }
    property fulfillmentList : TFhirResourceReferenceList{Resource} read FFulfillmentList;

  end;


  {@Class TFhirOrganization : TFhirResource
    A formally or informally recognized grouping of people or organizations formed for the purpose of achieving some form of collective action.  Includes companies, institutions, corporations, departments, community groups, healthcare practice groups, etc.
  }
  {!.Net HL7Connect.Fhir.Organization}
  TFhirOrganization = class (TFhirResource)
  private
    FidentifierList : TFhirIdentifierList;
    FName : TFhirString;
    FType_ : TFhirCodeableConcept;
    FtelecomList : TFhirContactList;
    FaddressList : TFhirAddressList;
    FPartOf : TFhirResourceReference{TFhirOrganization};
    FcontactList : TFhirOrganizationContactList;
    FlocationList : TFhirResourceReferenceList{TFhirLocation};
    FActive : TFhirBoolean;
    Procedure SetName(value : TFhirString);
    Function GetNameST : String;
    Procedure SetNameST(value : String);
    Procedure SetType_(value : TFhirCodeableConcept);
    Procedure SetPartOf(value : TFhirResourceReference{TFhirOrganization});
    Procedure SetActive(value : TFhirBoolean);
    Function GetActiveST : Boolean;
    Procedure SetActiveST(value : Boolean);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    Function GetHasASummary : Boolean; Override;
    function GetResourceType : TFhirResourceType; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirOrganization; overload;
    function Clone : TFhirOrganization; overload;
    {!script show}
  published
    {@member identifierList
      Identifier for the organization that is used to identify the organization across multiple disparate systems.
    }
    property identifierList : TFhirIdentifierList read FIdentifierList;

    {@member name
      A name associated with the organization.
    }
    property name : TFhirString read FName write SetName;
    {@member nameST
      Typed access to A name associated with the organization.
    }
    property nameST : String read GetNameST write SetNameST;

    {@member type_
      The kind of organization that this is.
    }
    property type_ : TFhirCodeableConcept read FType_ write SetType_;

    {@member telecomList
      A contact detail for the organization.
    }
    property telecomList : TFhirContactList read FTelecomList;

    {@member addressList
      An address for the organization.
    }
    property addressList : TFhirAddressList read FAddressList;

    {@member partOf
      The organization of which this organization forms a part.
    }
    property partOf : TFhirResourceReference{TFhirOrganization} read FPartOf write SetPartOf;

    {@member contactList
      Contact for the organization for a certain purpose.
    }
    property contactList : TFhirOrganizationContactList read FContactList;

    {@member locationList
      Location(s) the organization uses to provide services.
    }
    property locationList : TFhirResourceReferenceList{TFhirLocation} read FLocationList;

    {@member active
      Whether the organization's record is still in active use.
    }
    property active : TFhirBoolean read FActive write SetActive;
    {@member activeST
      Typed access to Whether the organization's record is still in active use.
    }
    property activeST : Boolean read GetActiveST write SetActiveST;

  end;


  {@Class TFhirOther : TFhirResource
    Other is a conformant for handling resource concepts not yet defined for FHIR or outside HL7's scope of interest.
  }
  {!.Net HL7Connect.Fhir.Other}
  TFhirOther = class (TFhirResource)
  private
    FidentifierList : TFhirIdentifierList;
    FCode : TFhirCodeableConcept;
    FSubject : TFhirResourceReference{Resource};
    FAuthor : TFhirResourceReference{Resource};
    FCreated : TFhirDate;
    Procedure SetCode(value : TFhirCodeableConcept);
    Procedure SetSubject(value : TFhirResourceReference{Resource});
    Procedure SetAuthor(value : TFhirResourceReference{Resource});
    Procedure SetCreated(value : TFhirDate);
    Function GetCreatedST : TDateAndTime;
    Procedure SetCreatedST(value : TDateAndTime);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    Function GetHasASummary : Boolean; Override;
    function GetResourceType : TFhirResourceType; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirOther; overload;
    function Clone : TFhirOther; overload;
    {!script show}
  published
    {@member identifierList
      Identifier assigned to the resource for business purposes, outside the context of FHIR.
    }
    property identifierList : TFhirIdentifierList read FIdentifierList;

    {@member code
      Identifies the 'type' of resource - equivalent to the resource name for other resources.
    }
    property code : TFhirCodeableConcept read FCode write SetCode;

    {@member subject
      Identifies the patient, practitioner, device or any other resource that is the "focus" of this resoruce.
    }
    property subject : TFhirResourceReference{Resource} read FSubject write SetSubject;

    {@member author
      Indicates who was responsible for creating the resource instance.
    }
    property author : TFhirResourceReference{Resource} read FAuthor write SetAuthor;

    {@member created
      Identifies when the resource was first created.
    }
    property created : TFhirDate read FCreated write SetCreated;
    {@member createdST
      Typed access to Identifies when the resource was first created.
    }
    property createdST : TDateAndTime read GetCreatedST write SetCreatedST;

  end;


  {@Class TFhirPatient : TFhirResource
    Demographics and other administrative information about a person or animal receiving care or other health-related services.
  }
  {!.Net HL7Connect.Fhir.Patient}
  TFhirPatient = class (TFhirResource)
  private
    FidentifierList : TFhirIdentifierList;
    FnameList : TFhirHumanNameList;
    FtelecomList : TFhirContactList;
    FGender : TFhirEnum;
    FBirthDate : TFhirDateTime;
    FDeceased : TFhirType;
    FaddressList : TFhirAddressList;
    FMaritalStatus : TFhirCodeableConcept;
    FMultipleBirth : TFhirType;
    FphotoList : TFhirAttachmentList;
    FcontactList : TFhirPatientContactList;
    FAnimal : TFhirPatientAnimal;
    FcommunicationList : TFhirCodeableConceptList;
    FcareProviderList : TFhirResourceReferenceList{Resource};
    FManagingOrganization : TFhirResourceReference{TFhirOrganization};
    Flink_List : TFhirPatientLinkList;
    FActive : TFhirBoolean;
    Procedure SetGender(value : TFhirEnum);
    Function GetGenderST : TFhirAdministrativeGender;
    Procedure SetGenderST(value : TFhirAdministrativeGender);
    Procedure SetBirthDate(value : TFhirDateTime);
    Function GetBirthDateST : TDateAndTime;
    Procedure SetBirthDateST(value : TDateAndTime);
    Procedure SetDeceased(value : TFhirType);
    Procedure SetMaritalStatus(value : TFhirCodeableConcept);
    Procedure SetMultipleBirth(value : TFhirType);
    Procedure SetAnimal(value : TFhirPatientAnimal);
    Procedure SetManagingOrganization(value : TFhirResourceReference{TFhirOrganization});
    Procedure SetActive(value : TFhirBoolean);
    Function GetActiveST : Boolean;
    Procedure SetActiveST(value : Boolean);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    Function GetHasASummary : Boolean; Override;
    function GetResourceType : TFhirResourceType; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirPatient; overload;
    function Clone : TFhirPatient; overload;
    {!script show}
  published
    {@member identifierList
      An identifier that applies to this person as a patient.
    }
    property identifierList : TFhirIdentifierList read FIdentifierList;

    {@member nameList
      A name associated with the individual.
    }
    property nameList : TFhirHumanNameList read FNameList;

    {@member telecomList
      A contact detail (e.g. a telephone number or an email address) by which the individual may be contacted.
    }
    property telecomList : TFhirContactList read FTelecomList;

    {@member gender
      Administrative Gender - the gender that the patient is considered to have for administration and record keeping purposes.
    }
    property gender : TFhirEnum read FGender write SetGender;
    {@member genderST
      Typed access to Administrative Gender - the gender that the patient is considered to have for administration and record keeping purposes.
    }
    property genderST : TFhirAdministrativeGender read GetGenderST write SetGenderST;

    {@member birthDate
      The date and time of birth for the individual.
    }
    property birthDate : TFhirDateTime read FBirthDate write SetBirthDate;
    {@member birthDateST
      Typed access to The date and time of birth for the individual.
    }
    property birthDateST : TDateAndTime read GetBirthDateST write SetBirthDateST;

    {@member deceased
      Indicates if the individual is deceased or not.
    }
    property deceased : TFhirType read FDeceased write SetDeceased;

    {@member addressList
      Addresses for the individual.
    }
    property addressList : TFhirAddressList read FAddressList;

    {@member maritalStatus
      This field contains a patient's most recent marital (civil) status.
    }
    property maritalStatus : TFhirCodeableConcept read FMaritalStatus write SetMaritalStatus;

    {@member multipleBirth
      Indicates whether the patient is part of a multiple or indicates the actual birth order.
    }
    property multipleBirth : TFhirType read FMultipleBirth write SetMultipleBirth;

    {@member photoList
      Image of the person.
    }
    property photoList : TFhirAttachmentList read FPhotoList;

    {@member contactList
      A contact party (e.g. guardian, partner, friend) for the patient.
    }
    property contactList : TFhirPatientContactList read FContactList;

    {@member animal
      This element has a value if the patient is an animal.
    }
    property animal : TFhirPatientAnimal read FAnimal write SetAnimal;

    {@member communicationList
      Languages which may be used to communicate with the patient about his or her health.
    }
    property communicationList : TFhirCodeableConceptList read FCommunicationList;

    {@member careProviderList
      Patient's nominated care provider.
    }
    property careProviderList : TFhirResourceReferenceList{Resource} read FCareProviderList;

    {@member managingOrganization
      Organization that is the custodian of the patient record.
    }
    property managingOrganization : TFhirResourceReference{TFhirOrganization} read FManagingOrganization write SetManagingOrganization;

    {@member link_List
      Link to another patient resource that concerns the same actual person.
    }
    property link_List : TFhirPatientLinkList read FLink_List;

    {@member active
      Whether this patient record is in active use.
    }
    property active : TFhirBoolean read FActive write SetActive;
    {@member activeST
      Typed access to Whether this patient record is in active use.
    }
    property activeST : Boolean read GetActiveST write SetActiveST;

  end;


  {@Class TFhirPractitioner : TFhirResource
    A person who is directly or indirectly involved in the provisioning of healthcare.
  }
  {!.Net HL7Connect.Fhir.Practitioner}
  TFhirPractitioner = class (TFhirResource)
  private
    FidentifierList : TFhirIdentifierList;
    FName : TFhirHumanName;
    FtelecomList : TFhirContactList;
    FaddressList : TFhirAddressList;
    FGender : TFhirEnum;
    FBirthDate : TFhirDateTime;
    FphotoList : TFhirAttachmentList;
    FOrganization : TFhirResourceReference{TFhirOrganization};
    FroleList : TFhirCodeableConceptList;
    FspecialtyList : TFhirCodeableConceptList;
    FPeriod : TFhirPeriod;
    FlocationList : TFhirResourceReferenceList{TFhirLocation};
    FqualificationList : TFhirPractitionerQualificationList;
    FcommunicationList : TFhirCodeableConceptList;
    Procedure SetName(value : TFhirHumanName);
    Procedure SetGender(value : TFhirEnum);
    Function GetGenderST : TFhirAdministrativeGender;
    Procedure SetGenderST(value : TFhirAdministrativeGender);
    Procedure SetBirthDate(value : TFhirDateTime);
    Function GetBirthDateST : TDateAndTime;
    Procedure SetBirthDateST(value : TDateAndTime);
    Procedure SetOrganization(value : TFhirResourceReference{TFhirOrganization});
    Procedure SetPeriod(value : TFhirPeriod);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    Function GetHasASummary : Boolean; Override;
    function GetResourceType : TFhirResourceType; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirPractitioner; overload;
    function Clone : TFhirPractitioner; overload;
    {!script show}
  published
    {@member identifierList
      An identifier that applies to this person in this role.
    }
    property identifierList : TFhirIdentifierList read FIdentifierList;

    {@member name
      A name associated with the person.
    }
    property name : TFhirHumanName read FName write SetName;

    {@member telecomList
      A contact detail for the practitioner, e.g. a telephone number or an email address.
    }
    property telecomList : TFhirContactList read FTelecomList;

    {@member addressList
      The postal address where the practitioner can be found or visited or to which mail can be delivered.
    }
    property addressList : TFhirAddressList read FAddressList;

    {@member gender
      Administrative Gender - the gender that the person is considered to have for administration and record keeping purposes.
    }
    property gender : TFhirEnum read FGender write SetGender;
    {@member genderST
      Typed access to Administrative Gender - the gender that the person is considered to have for administration and record keeping purposes.
    }
    property genderST : TFhirAdministrativeGender read GetGenderST write SetGenderST;

    {@member birthDate
      The date and time of birth for the practitioner.
    }
    property birthDate : TFhirDateTime read FBirthDate write SetBirthDate;
    {@member birthDateST
      Typed access to The date and time of birth for the practitioner.
    }
    property birthDateST : TDateAndTime read GetBirthDateST write SetBirthDateST;

    {@member photoList
      Image of the person.
    }
    property photoList : TFhirAttachmentList read FPhotoList;

    {@member organization
      The organization that the practitioner represents.
    }
    property organization : TFhirResourceReference{TFhirOrganization} read FOrganization write SetOrganization;

    {@member roleList
      Roles which this practitioner is authorized to perform for the organization.
    }
    property roleList : TFhirCodeableConceptList read FRoleList;

    {@member specialtyList
      Specific specialty of the practitioner.
    }
    property specialtyList : TFhirCodeableConceptList read FSpecialtyList;

    {@member period
      The period during which the person is authorized to act as a practitioner in these role(s) for the organization.
    }
    property period : TFhirPeriod read FPeriod write SetPeriod;

    {@member locationList
      The location(s) at which this practitioner provides care.
    }
    property locationList : TFhirResourceReferenceList{TFhirLocation} read FLocationList;

    {@member qualificationList
      Qualifications obtained by training and certification.
    }
    property qualificationList : TFhirPractitionerQualificationList read FQualificationList;

    {@member communicationList
      A language the practitioner is able to use in patient communication.
    }
    property communicationList : TFhirCodeableConceptList read FCommunicationList;

  end;


  {@Class TFhirProcedure : TFhirResource
    An action that is performed on a patient. This can be a physical 'thing' like an operation, or less invasive like counseling or hypnotherapy.
  }
  {!.Net HL7Connect.Fhir.Procedure}
  TFhirProcedure = class (TFhirResource)
  private
    FidentifierList : TFhirIdentifierList;
    FSubject : TFhirResourceReference{TFhirPatient};
    FType_ : TFhirCodeableConcept;
    FbodySiteList : TFhirCodeableConceptList;
    FindicationList : TFhirCodeableConceptList;
    FperformerList : TFhirProcedurePerformerList;
    FDate : TFhirPeriod;
    FEncounter : TFhirResourceReference{TFhirEncounter};
    FOutcome : TFhirString;
    FreportList : TFhirResourceReferenceList{TFhirDiagnosticReport};
    FcomplicationList : TFhirCodeableConceptList;
    FFollowUp : TFhirString;
    FrelatedItemList : TFhirProcedureRelatedItemList;
    FNotes : TFhirString;
    Procedure SetSubject(value : TFhirResourceReference{TFhirPatient});
    Procedure SetType_(value : TFhirCodeableConcept);
    Procedure SetDate(value : TFhirPeriod);
    Procedure SetEncounter(value : TFhirResourceReference{TFhirEncounter});
    Procedure SetOutcome(value : TFhirString);
    Function GetOutcomeST : String;
    Procedure SetOutcomeST(value : String);
    Procedure SetFollowUp(value : TFhirString);
    Function GetFollowUpST : String;
    Procedure SetFollowUpST(value : String);
    Procedure SetNotes(value : TFhirString);
    Function GetNotesST : String;
    Procedure SetNotesST(value : String);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    Function GetHasASummary : Boolean; Override;
    function GetResourceType : TFhirResourceType; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirProcedure; overload;
    function Clone : TFhirProcedure; overload;
    {!script show}
  published
    {@member identifierList
      This records identifiers associated with this procedure that are defined by business processed and/ or used to refer to it when a direct URL reference to the resource itself is not appropriate (e.g. in CDA documents, or in written / printed documentation).
    }
    property identifierList : TFhirIdentifierList read FIdentifierList;

    {@member subject
      The person on whom the procedure was performed.
    }
    property subject : TFhirResourceReference{TFhirPatient} read FSubject write SetSubject;

    {@member type_
      The specific procedure that is performed. Use text if the exact nature of the procedure can't be coded.
    }
    property type_ : TFhirCodeableConcept read FType_ write SetType_;

    {@member bodySiteList
      Detailed and structured anatomical location information. Multiple locations are allowed - e.g. multiple punch biopsies of a lesion.
    }
    property bodySiteList : TFhirCodeableConceptList read FBodySiteList;

    {@member indicationList
      The reason why the procedure was performed. This may be due to a Condition, may be coded entity of some type, or may simply be present as text.
    }
    property indicationList : TFhirCodeableConceptList read FIndicationList;

    {@member performerList
      Limited to 'real' people rather than equipment.
    }
    property performerList : TFhirProcedurePerformerList read FPerformerList;

    {@member date
      The dates over which the procedure was performed. Allows a period to support complex procedures that span more than one date, and also allows for the length of the procedure to be captured.
    }
    property date : TFhirPeriod read FDate write SetDate;

    {@member encounter
      The encounter during which the procedure was performed.
    }
    property encounter : TFhirResourceReference{TFhirEncounter} read FEncounter write SetEncounter;

    {@member outcome
      What was the outcome of the procedure - did it resolve reasons why the procedure was performed?.
    }
    property outcome : TFhirString read FOutcome write SetOutcome;
    {@member outcomeST
      Typed access to What was the outcome of the procedure - did it resolve reasons why the procedure was performed?.
    }
    property outcomeST : String read GetOutcomeST write SetOutcomeST;

    {@member reportList
      This could be a histology result. There could potentially be multiple reports - e.g. if this was a procedure that made multiple biopsies.
    }
    property reportList : TFhirResourceReferenceList{TFhirDiagnosticReport} read FReportList;

    {@member complicationList
      Any complications that occurred during the procedure, or in the immediate post-operative period. These are generally tracked separately from the notes, which typically will describe the procedure itself rather than any 'post procedure' issues.
    }
    property complicationList : TFhirCodeableConceptList read FComplicationList;

    {@member followUp
      If the procedure required specific follow up - e.g. removal of sutures. The followup may be represented as a simple note, or potentially could be more complex in which case the CarePlan resource can be used.
    }
    property followUp : TFhirString read FFollowUp write SetFollowUp;
    {@member followUpST
      Typed access to If the procedure required specific follow up - e.g. removal of sutures. The followup may be represented as a simple note, or potentially could be more complex in which case the CarePlan resource can be used.
    }
    property followUpST : String read GetFollowUpST write SetFollowUpST;

    {@member relatedItemList
      Procedures may be related to other items such as procedures or medications. For example treating wound dehiscence following a previous procedure.
    }
    property relatedItemList : TFhirProcedureRelatedItemList read FRelatedItemList;

    {@member notes
      Any other notes about the procedure - e.g. the operative notes.
    }
    property notes : TFhirString read FNotes write SetNotes;
    {@member notesST
      Typed access to Any other notes about the procedure - e.g. the operative notes.
    }
    property notesST : String read GetNotesST write SetNotesST;

  end;


  {@Class TFhirProfile : TFhirResource
    A Resource Profile - a statement of use of one or more FHIR Resources.  It may include constraints on Resources and Data Types, Terminology Binding Statements and Extension Definitions.
  }
  {!.Net HL7Connect.Fhir.Profile}
  TFhirProfile = class (TFhirResource)
  private
    FUrl : TFhirUri;
    FidentifierList : TFhirIdentifierList;
    FVersion : TFhirString;
    FName : TFhirString;
    FPublisher : TFhirString;
    FtelecomList : TFhirContactList;
    FDescription : TFhirString;
    FcodeList : TFhirCodingList;
    FStatus : TFhirEnum;
    FExperimental : TFhirBoolean;
    FDate : TFhirDateTime;
    FRequirements : TFhirString;
    FFhirVersion : TFhirId;
    FmappingList : TFhirProfileMappingList;
    FstructureList : TFhirProfileStructureList;
    FextensionDefnList : TFhirProfileExtensionDefnList;
    Procedure SetUrl(value : TFhirUri);
    Function GetUrlST : String;
    Procedure SetUrlST(value : String);
    Procedure SetVersion(value : TFhirString);
    Function GetVersionST : String;
    Procedure SetVersionST(value : String);
    Procedure SetName(value : TFhirString);
    Function GetNameST : String;
    Procedure SetNameST(value : String);
    Procedure SetPublisher(value : TFhirString);
    Function GetPublisherST : String;
    Procedure SetPublisherST(value : String);
    Procedure SetDescription(value : TFhirString);
    Function GetDescriptionST : String;
    Procedure SetDescriptionST(value : String);
    Procedure SetStatus(value : TFhirEnum);
    Function GetStatusST : TFhirResourceProfileStatus;
    Procedure SetStatusST(value : TFhirResourceProfileStatus);
    Procedure SetExperimental(value : TFhirBoolean);
    Function GetExperimentalST : Boolean;
    Procedure SetExperimentalST(value : Boolean);
    Procedure SetDate(value : TFhirDateTime);
    Function GetDateST : TDateAndTime;
    Procedure SetDateST(value : TDateAndTime);
    Procedure SetRequirements(value : TFhirString);
    Function GetRequirementsST : String;
    Procedure SetRequirementsST(value : String);
    Procedure SetFhirVersion(value : TFhirId);
    Function GetFhirVersionST : String;
    Procedure SetFhirVersionST(value : String);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    Function GetHasASummary : Boolean; Override;
    function GetResourceType : TFhirResourceType; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirProfile; overload;
    function Clone : TFhirProfile; overload;
    {!script show}
  published
    {@member url
      The URL at which this profile is (or will be) published, and which is used to reference this profile in extension urls and tag values in operational FHIR systems.
    }
    property url : TFhirUri read FUrl write SetUrl;
    {@member urlST
      Typed access to The URL at which this profile is (or will be) published, and which is used to reference this profile in extension urls and tag values in operational FHIR systems.
    }
    property urlST : String read GetUrlST write SetUrlST;

    {@member identifierList
      Formal identifier that is used to identify this profile when it is represented in other formats, or referenced in a specification, model, design or an instance  (should be globally unique OID, UUID, or URI), (if it's not possible to use the literal URI).
    }
    property identifierList : TFhirIdentifierList read FIdentifierList;

    {@member version
      The identifier that is used to identify this version of the profile when it is referenced in a specification, model, design or instance. This is an arbitrary value managed by the profile author manually and the value should be a timestamp.
    }
    property version : TFhirString read FVersion write SetVersion;
    {@member versionST
      Typed access to The identifier that is used to identify this version of the profile when it is referenced in a specification, model, design or instance. This is an arbitrary value managed by the profile author manually and the value should be a timestamp.
    }
    property versionST : String read GetVersionST write SetVersionST;

    {@member name
      A free text natural language name identifying the Profile.
    }
    property name : TFhirString read FName write SetName;
    {@member nameST
      Typed access to A free text natural language name identifying the Profile.
    }
    property nameST : String read GetNameST write SetNameST;

    {@member publisher
      Details of the individual or organization who accepts responsibility for publishing the profile.
    }
    property publisher : TFhirString read FPublisher write SetPublisher;
    {@member publisherST
      Typed access to Details of the individual or organization who accepts responsibility for publishing the profile.
    }
    property publisherST : String read GetPublisherST write SetPublisherST;

    {@member telecomList
      Contact details to assist a user in finding and communicating with the publisher.
    }
    property telecomList : TFhirContactList read FTelecomList;

    {@member description
      A free text natural language description of the profile and its use.
    }
    property description : TFhirString read FDescription write SetDescription;
    {@member descriptionST
      Typed access to A free text natural language description of the profile and its use.
    }
    property descriptionST : String read GetDescriptionST write SetDescriptionST;

    {@member codeList
      A set of terms from external terminologies that may be used to assist with indexing and searching of templates.
    }
    property codeList : TFhirCodingList read FCodeList;

    {@member status
      The status of the profile.
    }
    property status : TFhirEnum read FStatus write SetStatus;
    {@member statusST
      Typed access to The status of the profile.
    }
    property statusST : TFhirResourceProfileStatus read GetStatusST write SetStatusST;

    {@member experimental
      This profile was authored for testing purposes (or education/evaluation/marketing), and is not intended to be used for genuine usage.
    }
    property experimental : TFhirBoolean read FExperimental write SetExperimental;
    {@member experimentalST
      Typed access to This profile was authored for testing purposes (or education/evaluation/marketing), and is not intended to be used for genuine usage.
    }
    property experimentalST : Boolean read GetExperimentalST write SetExperimentalST;

    {@member date
      The date that this version of the profile was published.
    }
    property date : TFhirDateTime read FDate write SetDate;
    {@member dateST
      Typed access to The date that this version of the profile was published.
    }
    property dateST : TDateAndTime read GetDateST write SetDateST;

    {@member requirements
      The Scope and Usage that this profile was created to meet.
    }
    property requirements : TFhirString read FRequirements write SetRequirements;
    {@member requirementsST
      Typed access to The Scope and Usage that this profile was created to meet.
    }
    property requirementsST : String read GetRequirementsST write SetRequirementsST;

    {@member fhirVersion
      The version of the FHIR specification on which this profile is based.
    }
    property fhirVersion : TFhirId read FFhirVersion write SetFhirVersion;
    {@member fhirVersionST
      Typed access to The version of the FHIR specification on which this profile is based.
    }
    property fhirVersionST : String read GetFhirVersionST write SetFhirVersionST;

    {@member mappingList
      An external specification that the content is mapped to.
    }
    property mappingList : TFhirProfileMappingList read FMappingList;

    {@member structureList
      A constraint statement about what contents a resource or data type may have.
    }
    property structureList : TFhirProfileStructureList read FStructureList;

    {@member extensionDefnList
      An extension defined as part of the profile.
    }
    property extensionDefnList : TFhirProfileExtensionDefnList read FExtensionDefnList;

  end;


  {@Class TFhirProvenance : TFhirResource
    Provenance information that describes the activity that led to the creation of a set of resources. This information can be used to help determine their reliability or trace where the information in them came from. The focus of the provenance resource is record keeping, audit and traceability, and not explicit statements of clinical significance.
  }
  {!.Net HL7Connect.Fhir.Provenance}
  TFhirProvenance = class (TFhirResource)
  private
    FtargetList : TFhirResourceReferenceList{Resource};
    FPeriod : TFhirPeriod;
    FRecorded : TFhirInstant;
    FReason : TFhirCodeableConcept;
    FLocation : TFhirResourceReference{TFhirLocation};
    FpolicyList : TFhirUriList;
    FagentList : TFhirProvenanceAgentList;
    FentityList : TFhirProvenanceEntityList;
    FIntegritySignature : TFhirString;
    Procedure SetPeriod(value : TFhirPeriod);
    Procedure SetRecorded(value : TFhirInstant);
    Function GetRecordedST : TDateAndTime;
    Procedure SetRecordedST(value : TDateAndTime);
    Procedure SetReason(value : TFhirCodeableConcept);
    Procedure SetLocation(value : TFhirResourceReference{TFhirLocation});
    Procedure SetIntegritySignature(value : TFhirString);
    Function GetIntegritySignatureST : String;
    Procedure SetIntegritySignatureST(value : String);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    Function GetHasASummary : Boolean; Override;
    function GetResourceType : TFhirResourceType; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirProvenance; overload;
    function Clone : TFhirProvenance; overload;
    {!script show}
  published
    {@member targetList
      The resource(s) that were generated by  the activity described in this resource. A provenance can point to more than one target if multiple resources were created/updated by the same activity.
    }
    property targetList : TFhirResourceReferenceList{Resource} read FTargetList;

    {@member period
      The period during which the activity occurred.
    }
    property period : TFhirPeriod read FPeriod write SetPeriod;

    {@member recorded
      The instant of time at which the activity was recorded.
    }
    property recorded : TFhirInstant read FRecorded write SetRecorded;
    {@member recordedST
      Typed access to The instant of time at which the activity was recorded.
    }
    property recordedST : TDateAndTime read GetRecordedST write SetRecordedST;

    {@member reason
      The reason that the activity was taking place.
    }
    property reason : TFhirCodeableConcept read FReason write SetReason;

    {@member location
      Where the activity occurred, if relevant.
    }
    property location : TFhirResourceReference{TFhirLocation} read FLocation write SetLocation;

    {@member policyList
      Policy or plan the activity was defined by. Typically, a single activity may have multiple applicable policy documents, such as patient consent, guarantor funding, etc.
    }
    property policyList : TFhirUriList read FPolicyList;

    {@member agentList
      An agent takes a role in an activity such that the agent can be assigned some degree of responsibility for the activity taking place. An agent can be a person, a piece of software, an inanimate object, an organization, or other entities that may be ascribed responsibility.
    }
    property agentList : TFhirProvenanceAgentList read FAgentList;

    {@member entityList
      An entity used in this activity.
    }
    property entityList : TFhirProvenanceEntityList read FEntityList;

    {@member integritySignature
      A digital signature on the target resource(s). The signature should match a Provenance.agent.reference in the provenance resource. The signature is only added to support checking cryptographic integrity of the resource, and not to represent workflow and clinical aspects of the signing process, or to support non-repudiation.
    }
    property integritySignature : TFhirString read FIntegritySignature write SetIntegritySignature;
    {@member integritySignatureST
      Typed access to A digital signature on the target resource(s). The signature should match a Provenance.agent.reference in the provenance resource. The signature is only added to support checking cryptographic integrity of the resource, and not to represent workflow and clinical aspects of the signing process, or to support non-repudiation.
    }
    property integritySignatureST : String read GetIntegritySignatureST write SetIntegritySignatureST;

  end;


  {@Class TFhirQuery : TFhirResource
    A description of a query with a set of parameters.
  }
  {!.Net HL7Connect.Fhir.Query}
  TFhirQuery = class (TFhirResource)
  private
    FIdentifier : TFhirUri;
    FparameterList : TFhirExtensionList;
    FResponse : TFhirQueryResponse;
    Procedure SetIdentifier(value : TFhirUri);
    Function GetIdentifierST : String;
    Procedure SetIdentifierST(value : String);
    Procedure SetResponse(value : TFhirQueryResponse);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    Function GetHasASummary : Boolean; Override;
    function GetResourceType : TFhirResourceType; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirQuery; overload;
    function Clone : TFhirQuery; overload;
    {!script show}
  published
    {@member identifier
      Links query and its response(s).
    }
    property identifier : TFhirUri read FIdentifier write SetIdentifier;
    {@member identifierST
      Typed access to Links query and its response(s).
    }
    property identifierST : String read GetIdentifierST write SetIdentifierST;

    {@member parameterList
      Set of query parameters with values.
    }
    property parameterList : TFhirExtensionList read FParameterList;

    {@member response
      If this is a response to a query.
    }
    property response : TFhirQueryResponse read FResponse write SetResponse;

  end;


  {@Class TFhirQuestionnaire : TFhirResource
    A structured set of questions intended to guide the collection of answers. The questions are ordered and grouped into coherent subsets, corresponding to the structure of the grouping of the underlying questions.
  }
  {!.Net HL7Connect.Fhir.Questionnaire}
  TFhirQuestionnaire = class (TFhirResource)
  private
    FidentifierList : TFhirIdentifierList;
    FVersion : TFhirString;
    FStatus : TFhirEnum;
    FDate : TFhirDateTime;
    FPublisher : TFhirString;
    FGroup : TFhirQuestionnaireGroup;
    Procedure SetVersion(value : TFhirString);
    Function GetVersionST : String;
    Procedure SetVersionST(value : String);
    Procedure SetStatus(value : TFhirEnum);
    Function GetStatusST : TFhirQuestionnaireStatus;
    Procedure SetStatusST(value : TFhirQuestionnaireStatus);
    Procedure SetDate(value : TFhirDateTime);
    Function GetDateST : TDateAndTime;
    Procedure SetDateST(value : TDateAndTime);
    Procedure SetPublisher(value : TFhirString);
    Function GetPublisherST : String;
    Procedure SetPublisherST(value : String);
    Procedure SetGroup(value : TFhirQuestionnaireGroup);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    Function GetHasASummary : Boolean; Override;
    function GetResourceType : TFhirResourceType; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirQuestionnaire; overload;
    function Clone : TFhirQuestionnaire; overload;
    {!script show}
  published
    {@member identifierList
      This records identifiers associated with this question set that are defined by business processed and/ or used to refer to it when a direct URL reference to the resource itself is not appropriate (e.g. in CDA documents, or in written / printed documentation).
    }
    property identifierList : TFhirIdentifierList read FIdentifierList;

    {@member version
      The version number assigned by the publisher for business reasons.  It may remain the same when the resource is updated.
    }
    property version : TFhirString read FVersion write SetVersion;
    {@member versionST
      Typed access to The version number assigned by the publisher for business reasons.  It may remain the same when the resource is updated.
    }
    property versionST : String read GetVersionST write SetVersionST;

    {@member status
      The lifecycle status of the questionnaire as a whole.
    }
    property status : TFhirEnum read FStatus write SetStatus;
    {@member statusST
      Typed access to The lifecycle status of the questionnaire as a whole.
    }
    property statusST : TFhirQuestionnaireStatus read GetStatusST write SetStatusST;

    {@member date
      The date that this version of the questionnaire was authored.
    }
    property date : TFhirDateTime read FDate write SetDate;
    {@member dateST
      Typed access to The date that this version of the questionnaire was authored.
    }
    property dateST : TDateAndTime read GetDateST write SetDateST;

    {@member publisher
      Organization responsible for developing and maintaining the questionnaire.
    }
    property publisher : TFhirString read FPublisher write SetPublisher;
    {@member publisherST
      Typed access to Organization responsible for developing and maintaining the questionnaire.
    }
    property publisherST : String read GetPublisherST write SetPublisherST;

    {@member group
      A collection of related questions (or further groupings of questions).
    }
    property group : TFhirQuestionnaireGroup read FGroup write SetGroup;

  end;


  {@Class TFhirQuestionnaireAnswers : TFhirResource
    A structured set of questions and their answers. The questions are ordered and grouped into coherent subsets, corresponding to the structure of the grouping of the underlying questions.
  }
  {!.Net HL7Connect.Fhir.QuestionnaireAnswers}
  TFhirQuestionnaireAnswers = class (TFhirResource)
  private
    FIdentifier : TFhirIdentifier;
    FQuestionnaire : TFhirResourceReference{TFhirQuestionnaire};
    FStatus : TFhirEnum;
    FSubject : TFhirResourceReference{Resource};
    FAuthor : TFhirResourceReference{Resource};
    FAuthored : TFhirDateTime;
    FSource : TFhirResourceReference{Resource};
    FEncounter : TFhirResourceReference{TFhirEncounter};
    FGroup : TFhirQuestionnaireAnswersGroup;
    Procedure SetIdentifier(value : TFhirIdentifier);
    Procedure SetQuestionnaire(value : TFhirResourceReference{TFhirQuestionnaire});
    Procedure SetStatus(value : TFhirEnum);
    Function GetStatusST : TFhirQuestionnaireAnswersStatus;
    Procedure SetStatusST(value : TFhirQuestionnaireAnswersStatus);
    Procedure SetSubject(value : TFhirResourceReference{Resource});
    Procedure SetAuthor(value : TFhirResourceReference{Resource});
    Procedure SetAuthored(value : TFhirDateTime);
    Function GetAuthoredST : TDateAndTime;
    Procedure SetAuthoredST(value : TDateAndTime);
    Procedure SetSource(value : TFhirResourceReference{Resource});
    Procedure SetEncounter(value : TFhirResourceReference{TFhirEncounter});
    Procedure SetGroup(value : TFhirQuestionnaireAnswersGroup);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    Function GetHasASummary : Boolean; Override;
    function GetResourceType : TFhirResourceType; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirQuestionnaireAnswers; overload;
    function Clone : TFhirQuestionnaireAnswers; overload;
    {!script show}
  published
    {@member identifier
      A business identifier assigned to a particular completed (or partially completed) questionnaire.
    }
    property identifier : TFhirIdentifier read FIdentifier write SetIdentifier;

    {@member questionnaire
      Indicates the Questionnaire resource that defines the form for which answers are being provided.
    }
    property questionnaire : TFhirResourceReference{TFhirQuestionnaire} read FQuestionnaire write SetQuestionnaire;

    {@member status
      The lifecycle status of the questionnaire answers as a whole.
    }
    property status : TFhirEnum read FStatus write SetStatus;
    {@member statusST
      Typed access to The lifecycle status of the questionnaire answers as a whole.
    }
    property statusST : TFhirQuestionnaireAnswersStatus read GetStatusST write SetStatusST;

    {@member subject
      The subject of the questionnaire answers.  This could be a patient, organization, practitioner, device, etc.  This is who/what the answers apply to, but is not necessarily the source of information.
    }
    property subject : TFhirResourceReference{Resource} read FSubject write SetSubject;

    {@member author
      Person who received the answers to the questions in the QuestionnaireAnswers and recorded them in the system.
    }
    property author : TFhirResourceReference{Resource} read FAuthor write SetAuthor;

    {@member authored
      The date and/or time that this version of the questionnaire answers was authored.
    }
    property authored : TFhirDateTime read FAuthored write SetAuthored;
    {@member authoredST
      Typed access to The date and/or time that this version of the questionnaire answers was authored.
    }
    property authoredST : TDateAndTime read GetAuthoredST write SetAuthoredST;

    {@member source
      The person who answered the questions about the subject. Only used when this is not the subject him/herself.
    }
    property source : TFhirResourceReference{Resource} read FSource write SetSource;

    {@member encounter
      Encounter during which this set of questionnaire answers were collected. When there were multiple encounters, this is the one considered most relevant to the context of the answers.
    }
    property encounter : TFhirResourceReference{TFhirEncounter} read FEncounter write SetEncounter;

    {@member group
      A group of questions to a possibly similarly grouped set of questions in the questionnaire answers.
    }
    property group : TFhirQuestionnaireAnswersGroup read FGroup write SetGroup;

  end;


  {@Class TFhirReferralRequest : TFhirResource
    Used to record and send details about a request for referral service or transfer of a patient to the care of another provider or provider organisation.
  }
  {!.Net HL7Connect.Fhir.ReferralRequest}
  TFhirReferralRequest = class (TFhirResource)
  private
    FStatus : TFhirEnum;
    FidentifierList : TFhirIdentifierList;
    FType_ : TFhirCodeableConcept;
    FSpecialty : TFhirCodeableConcept;
    FPriority : TFhirCodeableConcept;
    FSubject : TFhirResourceReference{TFhirPatient};
    FRequester : TFhirResourceReference{Resource};
    FrecipientList : TFhirResourceReferenceList{Resource};
    FEncounter : TFhirResourceReference{TFhirEncounter};
    FDateSent : TFhirDateTime;
    FReason : TFhirCodeableConcept;
    FDescription : TFhirString;
    FserviceRequestedList : TFhirCodeableConceptList;
    FsupportingInformationList : TFhirResourceReferenceList{Resource};
    FFulfillmentTime : TFhirPeriod;
    Procedure SetStatus(value : TFhirEnum);
    Function GetStatusST : TFhirReferralstatus;
    Procedure SetStatusST(value : TFhirReferralstatus);
    Procedure SetType_(value : TFhirCodeableConcept);
    Procedure SetSpecialty(value : TFhirCodeableConcept);
    Procedure SetPriority(value : TFhirCodeableConcept);
    Procedure SetSubject(value : TFhirResourceReference{TFhirPatient});
    Procedure SetRequester(value : TFhirResourceReference{Resource});
    Procedure SetEncounter(value : TFhirResourceReference{TFhirEncounter});
    Procedure SetDateSent(value : TFhirDateTime);
    Function GetDateSentST : TDateAndTime;
    Procedure SetDateSentST(value : TDateAndTime);
    Procedure SetReason(value : TFhirCodeableConcept);
    Procedure SetDescription(value : TFhirString);
    Function GetDescriptionST : String;
    Procedure SetDescriptionST(value : String);
    Procedure SetFulfillmentTime(value : TFhirPeriod);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    Function GetHasASummary : Boolean; Override;
    function GetResourceType : TFhirResourceType; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirReferralRequest; overload;
    function Clone : TFhirReferralRequest; overload;
    {!script show}
  published
    {@member status
      The workflow status of the referral or transfer of care request.
    }
    property status : TFhirEnum read FStatus write SetStatus;
    {@member statusST
      Typed access to The workflow status of the referral or transfer of care request.
    }
    property statusST : TFhirReferralstatus read GetStatusST write SetStatusST;

    {@member identifierList
      Business Id that uniquely identifies the referral/care transfer request instance.
    }
    property identifierList : TFhirIdentifierList read FIdentifierList;

    {@member type_
      An indication of the type of referral (or where applicable the type of transfer of care) request.
    }
    property type_ : TFhirCodeableConcept read FType_ write SetType_;

    {@member specialty
      Indication of the clinical domain or discipline to which the referral or transfer of care request is sent.
    }
    property specialty : TFhirCodeableConcept read FSpecialty write SetSpecialty;

    {@member priority
      An indication of the urgency of referral (or where applicable the type of transfer of care) request.
    }
    property priority : TFhirCodeableConcept read FPriority write SetPriority;

    {@member subject
      The patient who is the subject of a referral or transfer of care request.
    }
    property subject : TFhirResourceReference{TFhirPatient} read FSubject write SetSubject;

    {@member requester
      The healthcare provider or provider organization who/which initaited the referral/transfer of care request. Can also be  Patient (a self referral).
    }
    property requester : TFhirResourceReference{Resource} read FRequester write SetRequester;

    {@member recipientList
      The healthcare provider(s) or provider organization(s) who/which is to receive the referral/transfer of care request.
    }
    property recipientList : TFhirResourceReferenceList{Resource} read FRecipientList;

    {@member encounter
      The encounter at which the request for referral or transfer of care is initiated.
    }
    property encounter : TFhirResourceReference{TFhirEncounter} read FEncounter write SetEncounter;

    {@member dateSent
      Date/DateTime the request for referral or transfer of care is sent by the author.
    }
    property dateSent : TFhirDateTime read FDateSent write SetDateSent;
    {@member dateSentST
      Typed access to Date/DateTime the request for referral or transfer of care is sent by the author.
    }
    property dateSentST : TDateAndTime read GetDateSentST write SetDateSentST;

    {@member reason
      Description of clinical condition indicating why referral/transfer of care is requested.
    }
    property reason : TFhirCodeableConcept read FReason write SetReason;

    {@member description
      The reason gives a short description of why the referral is being made, the description expands on this to support a more complete clinical summary.
    }
    property description : TFhirString read FDescription write SetDescription;
    {@member descriptionST
      Typed access to The reason gives a short description of why the referral is being made, the description expands on this to support a more complete clinical summary.
    }
    property descriptionST : String read GetDescriptionST write SetDescriptionST;

    {@member serviceRequestedList
      The service(s) that is/are requested to be provided to the patient.
    }
    property serviceRequestedList : TFhirCodeableConceptList read FServiceRequestedList;

    {@member supportingInformationList
      Any additional (administrative, financial or clinical) information required to support request for referral or transfer of care.
    }
    property supportingInformationList : TFhirResourceReferenceList{Resource} read FSupportingInformationList;

    {@member fulfillmentTime
      The period of time within which the services identified in the referral/transfer of care is specified or required to occur.
    }
    property fulfillmentTime : TFhirPeriod read FFulfillmentTime write SetFulfillmentTime;

  end;


  {@Class TFhirRelatedPerson : TFhirResource
    Information about a person that is involved in the care for a patient, but who is not the target of healthcare, nor has a formal responsibility in the care process.
  }
  {!.Net HL7Connect.Fhir.RelatedPerson}
  TFhirRelatedPerson = class (TFhirResource)
  private
    FidentifierList : TFhirIdentifierList;
    FPatient : TFhirResourceReference{TFhirPatient};
    FRelationship : TFhirCodeableConcept;
    FName : TFhirHumanName;
    FtelecomList : TFhirContactList;
    FGender : TFhirEnum;
    FAddress : TFhirAddress;
    FphotoList : TFhirAttachmentList;
    Procedure SetPatient(value : TFhirResourceReference{TFhirPatient});
    Procedure SetRelationship(value : TFhirCodeableConcept);
    Procedure SetName(value : TFhirHumanName);
    Procedure SetGender(value : TFhirEnum);
    Function GetGenderST : TFhirAdministrativeGender;
    Procedure SetGenderST(value : TFhirAdministrativeGender);
    Procedure SetAddress(value : TFhirAddress);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    Function GetHasASummary : Boolean; Override;
    function GetResourceType : TFhirResourceType; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirRelatedPerson; overload;
    function Clone : TFhirRelatedPerson; overload;
    {!script show}
  published
    {@member identifierList
      Identifier for a person within a particular scope.
    }
    property identifierList : TFhirIdentifierList read FIdentifierList;

    {@member patient
      The patient this person is related to.
    }
    property patient : TFhirResourceReference{TFhirPatient} read FPatient write SetPatient;

    {@member relationship
      The nature of the relationship between a patient and the related person.
    }
    property relationship : TFhirCodeableConcept read FRelationship write SetRelationship;

    {@member name
      A name associated with the person.
    }
    property name : TFhirHumanName read FName write SetName;

    {@member telecomList
      A contact detail for the person, e.g. a telephone number or an email address.
    }
    property telecomList : TFhirContactList read FTelecomList;

    {@member gender
      Administrative Gender - the gender that the person is considered to have for administration and record keeping purposes.
    }
    property gender : TFhirEnum read FGender write SetGender;
    {@member genderST
      Typed access to Administrative Gender - the gender that the person is considered to have for administration and record keeping purposes.
    }
    property genderST : TFhirAdministrativeGender read GetGenderST write SetGenderST;

    {@member address
      Address where the related person can be contacted or visited.
    }
    property address : TFhirAddress read FAddress write SetAddress;

    {@member photoList
      Image of the person.
    }
    property photoList : TFhirAttachmentList read FPhotoList;

  end;


  {@Class TFhirRiskAssessment : TFhirResource
    An assessment of the likely outcome(s) for a patient or other subject as well as the likelihood of each outcome.
  }
  {!.Net HL7Connect.Fhir.RiskAssessment}
  TFhirRiskAssessment = class (TFhirResource)
  private
    FSubject : TFhirResourceReference{Resource};
    FDate : TFhirDateTime;
    FCondition : TFhirResourceReference{TFhirCondition};
    FPerformer : TFhirResourceReference{Resource};
    FIdentifier : TFhirIdentifier;
    FMethod : TFhirCodeableConcept;
    FbasisList : TFhirResourceReferenceList{Resource};
    FpredictionList : TFhirRiskAssessmentPredictionList;
    FMitigation : TFhirString;
    Procedure SetSubject(value : TFhirResourceReference{Resource});
    Procedure SetDate(value : TFhirDateTime);
    Function GetDateST : TDateAndTime;
    Procedure SetDateST(value : TDateAndTime);
    Procedure SetCondition(value : TFhirResourceReference{TFhirCondition});
    Procedure SetPerformer(value : TFhirResourceReference{Resource});
    Procedure SetIdentifier(value : TFhirIdentifier);
    Procedure SetMethod(value : TFhirCodeableConcept);
    Procedure SetMitigation(value : TFhirString);
    Function GetMitigationST : String;
    Procedure SetMitigationST(value : String);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    Function GetHasASummary : Boolean; Override;
    function GetResourceType : TFhirResourceType; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirRiskAssessment; overload;
    function Clone : TFhirRiskAssessment; overload;
    {!script show}
  published
    {@member subject
      The patient or group the risk assessment applies to.
    }
    property subject : TFhirResourceReference{Resource} read FSubject write SetSubject;

    {@member date
      The date (and possibly time) the risk assessment was performed.
    }
    property date : TFhirDateTime read FDate write SetDate;
    {@member dateST
      Typed access to The date (and possibly time) the risk assessment was performed.
    }
    property dateST : TDateAndTime read GetDateST write SetDateST;

    {@member condition
      For assessments or prognosis specific to a particular condition, indicates the condition being assessed.
    }
    property condition : TFhirResourceReference{TFhirCondition} read FCondition write SetCondition;

    {@member performer
      The provider or software application that performed the assessment.
    }
    property performer : TFhirResourceReference{Resource} read FPerformer write SetPerformer;

    {@member identifier
      Business identifier assigned to the risk assessment.
    }
    property identifier : TFhirIdentifier read FIdentifier write SetIdentifier;

    {@member method
      The algorithm, processs or mechanism used to evaluate the risk.
    }
    property method : TFhirCodeableConcept read FMethod write SetMethod;

    {@member basisList
      Indicates the source data considered as part of the assessment (FamilyHistory, Observations, Procedures, Conditions, etc.).
    }
    property basisList : TFhirResourceReferenceList{Resource} read FBasisList;

    {@member predictionList
      Describes the expected outcome for the subject.
    }
    property predictionList : TFhirRiskAssessmentPredictionList read FPredictionList;

    {@member mitigation
      A description of the steps that might be taken to reduce the identified risk(s).
    }
    property mitigation : TFhirString read FMitigation write SetMitigation;
    {@member mitigationST
      Typed access to A description of the steps that might be taken to reduce the identified risk(s).
    }
    property mitigationST : String read GetMitigationST write SetMitigationST;

  end;


  {@Class TFhirSecurityEvent : TFhirResource
    A record of an event made for purposes of maintaining a security log. Typical uses include detection of intrusion attempts and monitoring for inappropriate usage.
  }
  {!.Net HL7Connect.Fhir.SecurityEvent}
  TFhirSecurityEvent = class (TFhirResource)
  private
    FEvent : TFhirSecurityEventEvent;
    FparticipantList : TFhirSecurityEventParticipantList;
    FSource : TFhirSecurityEventSource;
    Fobject_List : TFhirSecurityEventObjectList;
    Procedure SetEvent(value : TFhirSecurityEventEvent);
    Procedure SetSource(value : TFhirSecurityEventSource);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    Function GetHasASummary : Boolean; Override;
    function GetResourceType : TFhirResourceType; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirSecurityEvent; overload;
    function Clone : TFhirSecurityEvent; overload;
    {!script show}
  published
    {@member event
      Identifies the name, action type, time, and disposition of the audited event.
    }
    property event : TFhirSecurityEventEvent read FEvent write SetEvent;

    {@member participantList
      A person, a hardware device or software process.
    }
    property participantList : TFhirSecurityEventParticipantList read FParticipantList;

    {@member source
      Application systems and processes.
    }
    property source : TFhirSecurityEventSource read FSource write SetSource;

    {@member object_List
      Specific instances of data or objects that have been accessed.
    }
    property object_List : TFhirSecurityEventObjectList read FObject_List;

  end;


  {@Class TFhirSlot : TFhirResource
    (informative) A slot of time on a schedule that may be available for booking appointments.
  }
  {!.Net HL7Connect.Fhir.Slot}
  TFhirSlot = class (TFhirResource)
  private
    FidentifierList : TFhirIdentifierList;
    FType_ : TFhirCodeableConcept;
    FAvailability : TFhirResourceReference{TFhirAvailability};
    FFreeBusyType : TFhirEnum;
    FStart : TFhirInstant;
    FEnd_ : TFhirInstant;
    FOverbooked : TFhirBoolean;
    FComment : TFhirString;
    FLastModified : TFhirDateTime;
    Procedure SetType_(value : TFhirCodeableConcept);
    Procedure SetAvailability(value : TFhirResourceReference{TFhirAvailability});
    Procedure SetFreeBusyType(value : TFhirEnum);
    Function GetFreeBusyTypeST : TFhirSlotstatus;
    Procedure SetFreeBusyTypeST(value : TFhirSlotstatus);
    Procedure SetStart(value : TFhirInstant);
    Function GetStartST : TDateAndTime;
    Procedure SetStartST(value : TDateAndTime);
    Procedure SetEnd_(value : TFhirInstant);
    Function GetEnd_ST : TDateAndTime;
    Procedure SetEnd_ST(value : TDateAndTime);
    Procedure SetOverbooked(value : TFhirBoolean);
    Function GetOverbookedST : Boolean;
    Procedure SetOverbookedST(value : Boolean);
    Procedure SetComment(value : TFhirString);
    Function GetCommentST : String;
    Procedure SetCommentST(value : String);
    Procedure SetLastModified(value : TFhirDateTime);
    Function GetLastModifiedST : TDateAndTime;
    Procedure SetLastModifiedST(value : TDateAndTime);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    Function GetHasASummary : Boolean; Override;
    function GetResourceType : TFhirResourceType; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirSlot; overload;
    function Clone : TFhirSlot; overload;
    {!script show}
  published
    {@member identifierList
      External Ids for this item.
    }
    property identifierList : TFhirIdentifierList read FIdentifierList;

    {@member type_
      The type of appointments that can be booked into this slot (ideally this would be an identifiable service - which is at a location, rather than the location itself). If provided then this overrides the value provided on the availability resource.
    }
    property type_ : TFhirCodeableConcept read FType_ write SetType_;

    {@member availability
      The availability resource that this slot defines an interval of status information.
    }
    property availability : TFhirResourceReference{TFhirAvailability} read FAvailability write SetAvailability;

    {@member freeBusyType
      BUSY | FREE | BUSY-UNAVAILABLE | BUSY-TENTATIVE.
    }
    property freeBusyType : TFhirEnum read FFreeBusyType write SetFreeBusyType;
    {@member freeBusyTypeST
      Typed access to BUSY | FREE | BUSY-UNAVAILABLE | BUSY-TENTATIVE.
    }
    property freeBusyTypeST : TFhirSlotstatus read GetFreeBusyTypeST write SetFreeBusyTypeST;

    {@member start
      Date/Time that the slot is to begin.
    }
    property start : TFhirInstant read FStart write SetStart;
    {@member startST
      Typed access to Date/Time that the slot is to begin.
    }
    property startST : TDateAndTime read GetStartST write SetStartST;

    {@member end_
      Date/Time that the slot is to conclude.
    }
    property end_ : TFhirInstant read FEnd_ write SetEnd_;
    {@member end_ST
      Typed access to Date/Time that the slot is to conclude.
    }
    property end_ST : TDateAndTime read GetEnd_ST write SetEnd_ST;

    {@member overbooked
      This slot has already been overbooked, appointments are unlikely to be accepted for this time.
    }
    property overbooked : TFhirBoolean read FOverbooked write SetOverbooked;
    {@member overbookedST
      Typed access to This slot has already been overbooked, appointments are unlikely to be accepted for this time.
    }
    property overbookedST : Boolean read GetOverbookedST write SetOverbookedST;

    {@member comment
      Comments on the slot to describe any extended information. Such as custom constraints on the slot.
    }
    property comment : TFhirString read FComment write SetComment;
    {@member commentST
      Typed access to Comments on the slot to describe any extended information. Such as custom constraints on the slot.
    }
    property commentST : String read GetCommentST write SetCommentST;

    {@member lastModified
      When this slot was created, or last revised.
    }
    property lastModified : TFhirDateTime read FLastModified write SetLastModified;
    {@member lastModifiedST
      Typed access to When this slot was created, or last revised.
    }
    property lastModifiedST : TDateAndTime read GetLastModifiedST write SetLastModifiedST;

  end;


  {@Class TFhirSpecimen : TFhirResource
    Sample for analysis.
  }
  {!.Net HL7Connect.Fhir.Specimen}
  TFhirSpecimen = class (TFhirResource)
  private
    FidentifierList : TFhirIdentifierList;
    FType_ : TFhirCodeableConcept;
    FsourceList : TFhirSpecimenSourceList;
    FSubject : TFhirResourceReference{Resource};
    FAccessionIdentifier : TFhirIdentifier;
    FReceivedTime : TFhirDateTime;
    FCollection : TFhirSpecimenCollection;
    FtreatmentList : TFhirSpecimenTreatmentList;
    FcontainerList : TFhirSpecimenContainerList;
    Procedure SetType_(value : TFhirCodeableConcept);
    Procedure SetSubject(value : TFhirResourceReference{Resource});
    Procedure SetAccessionIdentifier(value : TFhirIdentifier);
    Procedure SetReceivedTime(value : TFhirDateTime);
    Function GetReceivedTimeST : TDateAndTime;
    Procedure SetReceivedTimeST(value : TDateAndTime);
    Procedure SetCollection(value : TFhirSpecimenCollection);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    Function GetHasASummary : Boolean; Override;
    function GetResourceType : TFhirResourceType; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirSpecimen; overload;
    function Clone : TFhirSpecimen; overload;
    {!script show}
  published
    {@member identifierList
      Id for specimen.
    }
    property identifierList : TFhirIdentifierList read FIdentifierList;

    {@member type_
      Kind of material that forms the specimen.
    }
    property type_ : TFhirCodeableConcept read FType_ write SetType_;

    {@member sourceList
      Parent specimen from which the focal specimen was a component.
    }
    property sourceList : TFhirSpecimenSourceList read FSourceList;

    {@member subject
      Where the specimen came from. This may be the patient(s) or from the environment or  a device.
    }
    property subject : TFhirResourceReference{Resource} read FSubject write SetSubject;

    {@member accessionIdentifier
      The identifier assigned by the lab when accessioning specimen(s). This is not necessarily the same as the specimen identifier, depending on local lab procedures.
    }
    property accessionIdentifier : TFhirIdentifier read FAccessionIdentifier write SetAccessionIdentifier;

    {@member receivedTime
      Time when specimen was received for processing or testing.
    }
    property receivedTime : TFhirDateTime read FReceivedTime write SetReceivedTime;
    {@member receivedTimeST
      Typed access to Time when specimen was received for processing or testing.
    }
    property receivedTimeST : TDateAndTime read GetReceivedTimeST write SetReceivedTimeST;

    {@member collection
      Details concerning the specimen collection.
    }
    property collection : TFhirSpecimenCollection read FCollection write SetCollection;

    {@member treatmentList
      Details concerning treatment and processing steps for the specimen.
    }
    property treatmentList : TFhirSpecimenTreatmentList read FTreatmentList;

    {@member containerList
      The container holding the specimen.  The recursive nature of containers; i.e. blood in tube in tray in rack is not addressed here.
    }
    property containerList : TFhirSpecimenContainerList read FContainerList;

  end;


  {@Class TFhirSubscription : TFhirResource
    Todo.
  }
  {!.Net HL7Connect.Fhir.Subscription}
  TFhirSubscription = class (TFhirResource)
  private
    FCriteria : TFhirString;
    FcontactList : TFhirContactList;
    FReason : TFhirString;
    FStatus : TFhirEnum;
    FError : TFhirString;
    FChannel : TFhirSubscriptionChannel;
    FEnd_ : TFhirInstant;
    FtagList : TFhirSubscriptionTagList;
    Procedure SetCriteria(value : TFhirString);
    Function GetCriteriaST : String;
    Procedure SetCriteriaST(value : String);
    Procedure SetReason(value : TFhirString);
    Function GetReasonST : String;
    Procedure SetReasonST(value : String);
    Procedure SetStatus(value : TFhirEnum);
    Function GetStatusST : TFhirSubscriptionStatus;
    Procedure SetStatusST(value : TFhirSubscriptionStatus);
    Procedure SetError(value : TFhirString);
    Function GetErrorST : String;
    Procedure SetErrorST(value : String);
    Procedure SetChannel(value : TFhirSubscriptionChannel);
    Procedure SetEnd_(value : TFhirInstant);
    Function GetEnd_ST : TDateAndTime;
    Procedure SetEnd_ST(value : TDateAndTime);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    Function GetHasASummary : Boolean; Override;
    function GetResourceType : TFhirResourceType; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirSubscription; overload;
    function Clone : TFhirSubscription; overload;
    {!script show}
  published
    {@member criteria
      Todo.
    }
    property criteria : TFhirString read FCriteria write SetCriteria;
    {@member criteriaST
      Typed access to Todo.
    }
    property criteriaST : String read GetCriteriaST write SetCriteriaST;

    {@member contactList
      Todo.
    }
    property contactList : TFhirContactList read FContactList;

    {@member reason
      Todo.
    }
    property reason : TFhirString read FReason write SetReason;
    {@member reasonST
      Typed access to Todo.
    }
    property reasonST : String read GetReasonST write SetReasonST;

    {@member status
      Todo.
    }
    property status : TFhirEnum read FStatus write SetStatus;
    {@member statusST
      Typed access to Todo.
    }
    property statusST : TFhirSubscriptionStatus read GetStatusST write SetStatusST;

    {@member error
      Todo.
    }
    property error : TFhirString read FError write SetError;
    {@member errorST
      Typed access to Todo.
    }
    property errorST : String read GetErrorST write SetErrorST;

    {@member channel
      Todo.
    }
    property channel : TFhirSubscriptionChannel read FChannel write SetChannel;

    {@member end_
      Todo.
    }
    property end_ : TFhirInstant read FEnd_ write SetEnd_;
    {@member end_ST
      Typed access to Todo.
    }
    property end_ST : TDateAndTime read GetEnd_ST write SetEnd_ST;

    {@member tagList
      Todo.
    }
    property tagList : TFhirSubscriptionTagList read FTagList;

  end;


  {@Class TFhirSubstance : TFhirResource
    A homogeneous material with a definite composition.
  }
  {!.Net HL7Connect.Fhir.Substance}
  TFhirSubstance = class (TFhirResource)
  private
    FType_ : TFhirCodeableConcept;
    FDescription : TFhirString;
    FInstance : TFhirSubstanceInstance;
    FingredientList : TFhirSubstanceIngredientList;
    Procedure SetType_(value : TFhirCodeableConcept);
    Procedure SetDescription(value : TFhirString);
    Function GetDescriptionST : String;
    Procedure SetDescriptionST(value : String);
    Procedure SetInstance(value : TFhirSubstanceInstance);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    Function GetHasASummary : Boolean; Override;
    function GetResourceType : TFhirResourceType; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirSubstance; overload;
    function Clone : TFhirSubstance; overload;
    {!script show}
  published
    {@member type_
      A code (or set of codes) that identify this substance.
    }
    property type_ : TFhirCodeableConcept read FType_ write SetType_;

    {@member description
      A description of the substance - its appearance, handling requirements, and other usage notes.
    }
    property description : TFhirString read FDescription write SetDescription;
    {@member descriptionST
      Typed access to A description of the substance - its appearance, handling requirements, and other usage notes.
    }
    property descriptionST : String read GetDescriptionST write SetDescriptionST;

    {@member instance
      Substance may be used to describe a kind of substance, or a specific package/container of the substance: an instance.
    }
    property instance : TFhirSubstanceInstance read FInstance write SetInstance;

    {@member ingredientList
      A substance can be composed of other substances.
    }
    property ingredientList : TFhirSubstanceIngredientList read FIngredientList;

  end;


  {@Class TFhirSupply : TFhirResource
    A supply - a  request for something, and provision of what is supplied.
  }
  {!.Net HL7Connect.Fhir.Supply}
  TFhirSupply = class (TFhirResource)
  private
    FKind : TFhirCodeableConcept;
    FIdentifier : TFhirIdentifier;
    FStatus : TFhirEnum;
    FOrderedItem : TFhirResourceReference{Resource};
    FPatient : TFhirResourceReference{TFhirPatient};
    FdispenseList : TFhirSupplyDispenseList;
    Procedure SetKind(value : TFhirCodeableConcept);
    Procedure SetIdentifier(value : TFhirIdentifier);
    Procedure SetStatus(value : TFhirEnum);
    Function GetStatusST : TFhirValuesetSupplyStatus;
    Procedure SetStatusST(value : TFhirValuesetSupplyStatus);
    Procedure SetOrderedItem(value : TFhirResourceReference{Resource});
    Procedure SetPatient(value : TFhirResourceReference{TFhirPatient});
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    Function GetHasASummary : Boolean; Override;
    function GetResourceType : TFhirResourceType; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirSupply; overload;
    function Clone : TFhirSupply; overload;
    {!script show}
  published
    {@member kind
      Category of supply, e.g.  central, non-stock, etc. This is used to support work flows associated with the supply process.
    }
    property kind : TFhirCodeableConcept read FKind write SetKind;

    {@member identifier
      Unique identifier for this supply request.
    }
    property identifier : TFhirIdentifier read FIdentifier write SetIdentifier;

    {@member status
      Status of the supply request.
    }
    property status : TFhirEnum read FStatus write SetStatus;
    {@member statusST
      Typed access to Status of the supply request.
    }
    property statusST : TFhirValuesetSupplyStatus read GetStatusST write SetStatusST;

    {@member orderedItem
      The item that is requested to be supplied.
    }
    property orderedItem : TFhirResourceReference{Resource} read FOrderedItem write SetOrderedItem;

    {@member patient
      A link to a resource representing the person whom the ordered item is for.
    }
    property patient : TFhirResourceReference{TFhirPatient} read FPatient write SetPatient;

    {@member dispenseList
      Indicates the details of the dispense event such as the days supply and quantity of a supply dispensed.
    }
    property dispenseList : TFhirSupplyDispenseList read FDispenseList;

  end;


  {@Class TFhirValueSet : TFhirResource
    A value set specifies a set of codes drawn from one or more code systems.
  }
  {!.Net HL7Connect.Fhir.ValueSet}
  TFhirValueSet = class (TFhirResource)
  private
    FIdentifier : TFhirString;
    FVersion : TFhirString;
    FName : TFhirString;
    FPurpose : TFhirString;
    FImmutable : TFhirBoolean;
    FPublisher : TFhirString;
    FtelecomList : TFhirContactList;
    FDescription : TFhirString;
    FCopyright : TFhirString;
    FStatus : TFhirEnum;
    FExperimental : TFhirBoolean;
    FExtensible : TFhirBoolean;
    FDate : TFhirDateTime;
    FStableDate : TFhirDate;
    FDefine : TFhirValueSetDefine;
    FCompose : TFhirValueSetCompose;
    FExpansion : TFhirValueSetExpansion;
    Procedure SetIdentifier(value : TFhirString);
    Function GetIdentifierST : String;
    Procedure SetIdentifierST(value : String);
    Procedure SetVersion(value : TFhirString);
    Function GetVersionST : String;
    Procedure SetVersionST(value : String);
    Procedure SetName(value : TFhirString);
    Function GetNameST : String;
    Procedure SetNameST(value : String);
    Procedure SetPurpose(value : TFhirString);
    Function GetPurposeST : String;
    Procedure SetPurposeST(value : String);
    Procedure SetImmutable(value : TFhirBoolean);
    Function GetImmutableST : Boolean;
    Procedure SetImmutableST(value : Boolean);
    Procedure SetPublisher(value : TFhirString);
    Function GetPublisherST : String;
    Procedure SetPublisherST(value : String);
    Procedure SetDescription(value : TFhirString);
    Function GetDescriptionST : String;
    Procedure SetDescriptionST(value : String);
    Procedure SetCopyright(value : TFhirString);
    Function GetCopyrightST : String;
    Procedure SetCopyrightST(value : String);
    Procedure SetStatus(value : TFhirEnum);
    Function GetStatusST : TFhirValuesetStatus;
    Procedure SetStatusST(value : TFhirValuesetStatus);
    Procedure SetExperimental(value : TFhirBoolean);
    Function GetExperimentalST : Boolean;
    Procedure SetExperimentalST(value : Boolean);
    Procedure SetExtensible(value : TFhirBoolean);
    Function GetExtensibleST : Boolean;
    Procedure SetExtensibleST(value : Boolean);
    Procedure SetDate(value : TFhirDateTime);
    Function GetDateST : TDateAndTime;
    Procedure SetDateST(value : TDateAndTime);
    Procedure SetStableDate(value : TFhirDate);
    Function GetStableDateST : TDateAndTime;
    Procedure SetStableDateST(value : TDateAndTime);
    Procedure SetDefine(value : TFhirValueSetDefine);
    Procedure SetCompose(value : TFhirValueSetCompose);
    Procedure SetExpansion(value : TFhirValueSetExpansion);
  protected
    Procedure GetChildrenByName(child_name : string; list : TFHIRObjectList); override;
    Procedure ListProperties(oList : TFHIRPropertyList; bInheritedProperties : Boolean); Override;
    Function GetHasASummary : Boolean; Override;
    function GetResourceType : TFhirResourceType; override;
  public
    constructor Create; Override;
    destructor Destroy; override;
    {!script hide}
    procedure Assign(oSource : TAdvObject); override;
    function Link : TFhirValueSet; overload;
    function Clone : TFhirValueSet; overload;
    {!script show}
  published
    {@member identifier
      The identifier that is used to identify this value set when it is referenced in a specification, model, design or an instance (should be globally unique OID, UUID, or URI).
    }
    property identifier : TFhirString read FIdentifier write SetIdentifier;
    {@member identifierST
      Typed access to The identifier that is used to identify this value set when it is referenced in a specification, model, design or an instance (should be globally unique OID, UUID, or URI).
    }
    property identifierST : String read GetIdentifierST write SetIdentifierST;

    {@member version
      The identifier that is used to identify this version of the value set when it is referenced in a specification, model, design or instance. This is an arbitrary value managed by the profile author manually and the value should be a timestamp.
    }
    property version : TFhirString read FVersion write SetVersion;
    {@member versionST
      Typed access to The identifier that is used to identify this version of the value set when it is referenced in a specification, model, design or instance. This is an arbitrary value managed by the profile author manually and the value should be a timestamp.
    }
    property versionST : String read GetVersionST write SetVersionST;

    {@member name
      A free text natural language name describing the value set.
    }
    property name : TFhirString read FName write SetName;
    {@member nameST
      Typed access to A free text natural language name describing the value set.
    }
    property nameST : String read GetNameST write SetNameST;

    {@member purpose
      This should describe "the semantic space" to be included in the value set. This can also describe the approach taken to build the value set.
    }
    property purpose : TFhirString read FPurpose write SetPurpose;
    {@member purposeST
      Typed access to This should describe "the semantic space" to be included in the value set. This can also describe the approach taken to build the value set.
    }
    property purposeST : String read GetPurposeST write SetPurposeST;

    {@member immutable
      If this is set to 'true', then no new versions of the content logical definition can be created.  Note: Other metadata might still change.
    }
    property immutable : TFhirBoolean read FImmutable write SetImmutable;
    {@member immutableST
      Typed access to If this is set to 'true', then no new versions of the content logical definition can be created.  Note: Other metadata might still change.
    }
    property immutableST : Boolean read GetImmutableST write SetImmutableST;

    {@member publisher
      The name of the individual or organization that published the value set.
    }
    property publisher : TFhirString read FPublisher write SetPublisher;
    {@member publisherST
      Typed access to The name of the individual or organization that published the value set.
    }
    property publisherST : String read GetPublisherST write SetPublisherST;

    {@member telecomList
      Contacts of the publisher to assist a user in finding and communicating with the publisher.
    }
    property telecomList : TFhirContactList read FTelecomList;

    {@member description
      A free text natural language description of the use of the value set - reason for definition, conditions of use, etc. The description may include a list of.
    }
    property description : TFhirString read FDescription write SetDescription;
    {@member descriptionST
      Typed access to A free text natural language description of the use of the value set - reason for definition, conditions of use, etc. The description may include a list of.
    }
    property descriptionST : String read GetDescriptionST write SetDescriptionST;

    {@member copyright
      A copyright statement relating to the value set and/or its contents. These are generally legal restrictions on the use and publishing of the value set.
    }
    property copyright : TFhirString read FCopyright write SetCopyright;
    {@member copyrightST
      Typed access to A copyright statement relating to the value set and/or its contents. These are generally legal restrictions on the use and publishing of the value set.
    }
    property copyrightST : String read GetCopyrightST write SetCopyrightST;

    {@member status
      The status of the value set.
    }
    property status : TFhirEnum read FStatus write SetStatus;
    {@member statusST
      Typed access to The status of the value set.
    }
    property statusST : TFhirValuesetStatus read GetStatusST write SetStatusST;

    {@member experimental
      This valueset was authored for testing purposes (or education/evaluation/marketing), and is not intended to be used for genuine usage.
    }
    property experimental : TFhirBoolean read FExperimental write SetExperimental;
    {@member experimentalST
      Typed access to This valueset was authored for testing purposes (or education/evaluation/marketing), and is not intended to be used for genuine usage.
    }
    property experimentalST : Boolean read GetExperimentalST write SetExperimentalST;

    {@member extensible
      Whether this is intended to be used with an extensible binding or not.
    }
    property extensible : TFhirBoolean read FExtensible write SetExtensible;
    {@member extensibleST
      Typed access to Whether this is intended to be used with an extensible binding or not.
    }
    property extensibleST : Boolean read GetExtensibleST write SetExtensibleST;

    {@member date
      The date that the value set status was last changed.
    }
    property date : TFhirDateTime read FDate write SetDate;
    {@member dateST
      Typed access to The date that the value set status was last changed.
    }
    property dateST : TDateAndTime read GetDateST write SetDateST;

    {@member stableDate
      If a Stability Date is expanded by evaluating the Content Logical Definition using the current version of all referenced code system(s) and value sets as of the Stability Date.
    }
    property stableDate : TFhirDate read FStableDate write SetStableDate;
    {@member stableDateST
      Typed access to If a Stability Date is expanded by evaluating the Content Logical Definition using the current version of all referenced code system(s) and value sets as of the Stability Date.
    }
    property stableDateST : TDateAndTime read GetStableDateST write SetStableDateST;

    {@member define
      When value set defines its own codes.
    }
    property define : TFhirValueSetDefine read FDefine write SetDefine;

    {@member compose
      When value set includes codes from elsewhere.
    }
    property compose : TFhirValueSetCompose read FCompose write SetCompose;

    {@member expansion
      When value set is an expansion.
    }
    property expansion : TFhirValueSetExpansion read FExpansion write SetExpansion;

  end;


 TFhirResourceFactory = class (TFHIRBaseFactory)
  public
    {@member newEnum
      create a new enum
    }
    {!script nolink}
    function newEnum : TFhirEnum;
    {@member makeEnum
      create a new enum with the given value
    }
    {!script nolink}
    function makeEnum(value : String) : TFhirEnum;
    {@member newInteger
      create a new integer
    }
    {!script nolink}
    function newInteger : TFhirInteger;
    {@member makeInteger
      create a new integer with the given value
    }
    {!script nolink}
    function makeInteger(value : String) : TFhirInteger;
    {@member newDateTime
      create a new dateTime
    }
    {!script nolink}
    function newDateTime : TFhirDateTime;
    {@member makeDateTime
      create a new dateTime with the given value
    }
    {!script nolink}
    function makeDateTime(value : TDateAndTime) : TFhirDateTime;
    {@member newDate
      create a new date
    }
    {!script nolink}
    function newDate : TFhirDate;
    {@member makeDate
      create a new date with the given value
    }
    {!script nolink}
    function makeDate(value : TDateAndTime) : TFhirDate;
    {@member newDecimal
      create a new decimal
    }
    {!script nolink}
    function newDecimal : TFhirDecimal;
    {@member makeDecimal
      create a new decimal with the given value
    }
    {!script nolink}
    function makeDecimal(value : String) : TFhirDecimal;
    {@member newUri
      create a new uri
    }
    {!script nolink}
    function newUri : TFhirUri;
    {@member makeUri
      create a new uri with the given value
    }
    {!script nolink}
    function makeUri(value : String) : TFhirUri;
    {@member newBase64Binary
      create a new base64Binary
    }
    {!script nolink}
    function newBase64Binary : TFhirBase64Binary;
    {@member makeBase64Binary
      create a new base64Binary with the given value
    }
    {!script nolink}
    function makeBase64Binary(value : String) : TFhirBase64Binary;
    {@member newTime
      create a new time
    }
    {!script nolink}
    function newTime : TFhirTime;
    {@member makeTime
      create a new time with the given value
    }
    {!script nolink}
    function makeTime(value : String) : TFhirTime;
    {@member newString
      create a new string
    }
    {!script nolink}
    function newString : TFhirString;
    {@member makeString
      create a new string with the given value
    }
    {!script nolink}
    function makeString(value : String) : TFhirString;
    {@member newBoolean
      create a new boolean
    }
    {!script nolink}
    function newBoolean : TFhirBoolean;
    {@member makeBoolean
      create a new boolean with the given value
    }
    {!script nolink}
    function makeBoolean(value : Boolean) : TFhirBoolean;
    {@member newInstant
      create a new instant
    }
    {!script nolink}
    function newInstant : TFhirInstant;
    {@member makeInstant
      create a new instant with the given value
    }
    {!script nolink}
    function makeInstant(value : TDateAndTime) : TFhirInstant;
    {@member newCode
      create a new code
    }
    {!script nolink}
    function newCode : TFhirCode;
    {@member makeCode
      create a new code with the given value
    }
    {!script nolink}
    function makeCode(value : String) : TFhirCode;
    {@member newId
      create a new id
    }
    {!script nolink}
    function newId : TFhirId;
    {@member makeId
      create a new id with the given value
    }
    {!script nolink}
    function makeId(value : String) : TFhirId;
    {@member newOid
      create a new oid
    }
    {!script nolink}
    function newOid : TFhirOid;
    {@member makeOid
      create a new oid with the given value
    }
    {!script nolink}
    function makeOid(value : String) : TFhirOid;
    {@member newUuid
      create a new uuid
    }
    {!script nolink}
    function newUuid : TFhirUuid;
    {@member makeUuid
      create a new uuid with the given value
    }
    {!script nolink}
    function makeUuid(value : String) : TFhirUuid;
    {@member newExtension
      create a new Extension
    }
    {!script nolink}
    function newExtension : TFhirExtension;
    {@member newNarrative
      create a new Narrative
    }
    {!script nolink}
    function newNarrative : TFhirNarrative;
    {@member newPeriod
      create a new Period
    }
    {!script nolink}
    function newPeriod : TFhirPeriod;
    {@member newCoding
      create a new Coding
    }
    {!script nolink}
    function newCoding : TFhirCoding;
    {@member newRange
      create a new Range
    }
    {!script nolink}
    function newRange : TFhirRange;
    {@member newQuantity
      create a new Quantity
    }
    {!script nolink}
    function newQuantity : TFhirQuantity;
    {@member newAttachment
      create a new Attachment
    }
    {!script nolink}
    function newAttachment : TFhirAttachment;
    {@member newRatio
      create a new Ratio
    }
    {!script nolink}
    function newRatio : TFhirRatio;
    {@member newSampledData
      create a new SampledData
    }
    {!script nolink}
    function newSampledData : TFhirSampledData;
    {@member newResourceReference
      create a new ResourceReference
    }
    {!script nolink}
    function newResourceReference : TFhirResourceReference;
    {@member newCodeableConcept
      create a new CodeableConcept
    }
    {!script nolink}
    function newCodeableConcept : TFhirCodeableConcept;
    {@member newIdentifier
      create a new Identifier
    }
    {!script nolink}
    function newIdentifier : TFhirIdentifier;
    {@member newScheduleRepeat
      create a new repeat
    }
    {!script nolink}
    function newScheduleRepeat : TFhirScheduleRepeat;
    {@member newSchedule
      create a new Schedule
    }
    {!script nolink}
    function newSchedule : TFhirSchedule;
    {@member newContact
      create a new Contact
    }
    {!script nolink}
    function newContact : TFhirContact;
    {@member newAddress
      create a new Address
    }
    {!script nolink}
    function newAddress : TFhirAddress;
    {@member newHumanName
      create a new HumanName
    }
    {!script nolink}
    function newHumanName : TFhirHumanName;
    {@member newAdverseReactionSymptom
      create a new symptom
    }
    {!script nolink}
    function newAdverseReactionSymptom : TFhirAdverseReactionSymptom;
    {@member newAdverseReactionExposure
      create a new exposure
    }
    {!script nolink}
    function newAdverseReactionExposure : TFhirAdverseReactionExposure;
    {@member newAdverseReaction
      create a new AdverseReaction
    }
    {!script nolink}
    function newAdverseReaction : TFhirAdverseReaction;
    {@member newAlert
      create a new Alert
    }
    {!script nolink}
    function newAlert : TFhirAlert;
    {@member newAllergyIntolerance
      create a new AllergyIntolerance
    }
    {!script nolink}
    function newAllergyIntolerance : TFhirAllergyIntolerance;
    {@member newAppointmentParticipant
      create a new participant
    }
    {!script nolink}
    function newAppointmentParticipant : TFhirAppointmentParticipant;
    {@member newAppointment
      create a new Appointment
    }
    {!script nolink}
    function newAppointment : TFhirAppointment;
    {@member newAppointmentResponse
      create a new AppointmentResponse
    }
    {!script nolink}
    function newAppointmentResponse : TFhirAppointmentResponse;
    {@member newAvailability
      create a new Availability
    }
    {!script nolink}
    function newAvailability : TFhirAvailability;
    {@member newCarePlanParticipant
      create a new participant
    }
    {!script nolink}
    function newCarePlanParticipant : TFhirCarePlanParticipant;
    {@member newCarePlanGoal
      create a new goal
    }
    {!script nolink}
    function newCarePlanGoal : TFhirCarePlanGoal;
    {@member newCarePlanActivity
      create a new activity
    }
    {!script nolink}
    function newCarePlanActivity : TFhirCarePlanActivity;
    {@member newCarePlanActivitySimple
      create a new simple
    }
    {!script nolink}
    function newCarePlanActivitySimple : TFhirCarePlanActivitySimple;
    {@member newCarePlan
      create a new CarePlan
    }
    {!script nolink}
    function newCarePlan : TFhirCarePlan;
    {@member newCompositionAttester
      create a new attester
    }
    {!script nolink}
    function newCompositionAttester : TFhirCompositionAttester;
    {@member newCompositionEvent
      create a new event
    }
    {!script nolink}
    function newCompositionEvent : TFhirCompositionEvent;
    {@member newCompositionSection
      create a new section
    }
    {!script nolink}
    function newCompositionSection : TFhirCompositionSection;
    {@member newComposition
      create a new Composition
    }
    {!script nolink}
    function newComposition : TFhirComposition;
    {@member newConceptMapElement
      create a new element
    }
    {!script nolink}
    function newConceptMapElement : TFhirConceptMapElement;
    {@member newConceptMapElementDependsOn
      create a new dependsOn
    }
    {!script nolink}
    function newConceptMapElementDependsOn : TFhirConceptMapElementDependsOn;
    {@member newConceptMapElementMap
      create a new map
    }
    {!script nolink}
    function newConceptMapElementMap : TFhirConceptMapElementMap;
    {@member newConceptMap
      create a new ConceptMap
    }
    {!script nolink}
    function newConceptMap : TFhirConceptMap;
    {@member newConditionStage
      create a new stage
    }
    {!script nolink}
    function newConditionStage : TFhirConditionStage;
    {@member newConditionEvidence
      create a new evidence
    }
    {!script nolink}
    function newConditionEvidence : TFhirConditionEvidence;
    {@member newConditionLocation
      create a new location
    }
    {!script nolink}
    function newConditionLocation : TFhirConditionLocation;
    {@member newConditionRelatedItem
      create a new relatedItem
    }
    {!script nolink}
    function newConditionRelatedItem : TFhirConditionRelatedItem;
    {@member newCondition
      create a new Condition
    }
    {!script nolink}
    function newCondition : TFhirCondition;
    {@member newConformanceSoftware
      create a new software
    }
    {!script nolink}
    function newConformanceSoftware : TFhirConformanceSoftware;
    {@member newConformanceImplementation
      create a new implementation
    }
    {!script nolink}
    function newConformanceImplementation : TFhirConformanceImplementation;
    {@member newConformanceRest
      create a new rest
    }
    {!script nolink}
    function newConformanceRest : TFhirConformanceRest;
    {@member newConformanceRestSecurity
      create a new security
    }
    {!script nolink}
    function newConformanceRestSecurity : TFhirConformanceRestSecurity;
    {@member newConformanceRestSecurityCertificate
      create a new certificate
    }
    {!script nolink}
    function newConformanceRestSecurityCertificate : TFhirConformanceRestSecurityCertificate;
    {@member newConformanceRestResource
      create a new resource
    }
    {!script nolink}
    function newConformanceRestResource : TFhirConformanceRestResource;
    {@member newConformanceRestResourceInteraction
      create a new interaction
    }
    {!script nolink}
    function newConformanceRestResourceInteraction : TFhirConformanceRestResourceInteraction;
    {@member newConformanceRestResourceSearchParam
      create a new searchParam
    }
    {!script nolink}
    function newConformanceRestResourceSearchParam : TFhirConformanceRestResourceSearchParam;
    {@member newConformanceRestInteraction
      create a new interaction
    }
    {!script nolink}
    function newConformanceRestInteraction : TFhirConformanceRestInteraction;
    {@member newConformanceRestOperation
      create a new operation
    }
    {!script nolink}
    function newConformanceRestOperation : TFhirConformanceRestOperation;
    {@member newConformanceMessaging
      create a new messaging
    }
    {!script nolink}
    function newConformanceMessaging : TFhirConformanceMessaging;
    {@member newConformanceMessagingEvent
      create a new event
    }
    {!script nolink}
    function newConformanceMessagingEvent : TFhirConformanceMessagingEvent;
    {@member newConformanceDocument
      create a new document
    }
    {!script nolink}
    function newConformanceDocument : TFhirConformanceDocument;
    {@member newConformance
      create a new Conformance
    }
    {!script nolink}
    function newConformance : TFhirConformance;
    {@member newContraindicationMitigation
      create a new mitigation
    }
    {!script nolink}
    function newContraindicationMitigation : TFhirContraindicationMitigation;
    {@member newContraindication
      create a new Contraindication
    }
    {!script nolink}
    function newContraindication : TFhirContraindication;
    {@member newDataElementBinding
      create a new binding
    }
    {!script nolink}
    function newDataElementBinding : TFhirDataElementBinding;
    {@member newDataElementMapping
      create a new mapping
    }
    {!script nolink}
    function newDataElementMapping : TFhirDataElementMapping;
    {@member newDataElement
      create a new DataElement
    }
    {!script nolink}
    function newDataElement : TFhirDataElement;
    {@member newDevice
      create a new Device
    }
    {!script nolink}
    function newDevice : TFhirDevice;
    {@member newDeviceObservationReportVirtualDevice
      create a new virtualDevice
    }
    {!script nolink}
    function newDeviceObservationReportVirtualDevice : TFhirDeviceObservationReportVirtualDevice;
    {@member newDeviceObservationReportVirtualDeviceChannel
      create a new channel
    }
    {!script nolink}
    function newDeviceObservationReportVirtualDeviceChannel : TFhirDeviceObservationReportVirtualDeviceChannel;
    {@member newDeviceObservationReportVirtualDeviceChannelMetric
      create a new metric
    }
    {!script nolink}
    function newDeviceObservationReportVirtualDeviceChannelMetric : TFhirDeviceObservationReportVirtualDeviceChannelMetric;
    {@member newDeviceObservationReport
      create a new DeviceObservationReport
    }
    {!script nolink}
    function newDeviceObservationReport : TFhirDeviceObservationReport;
    {@member newDiagnosticOrderEvent
      create a new event
    }
    {!script nolink}
    function newDiagnosticOrderEvent : TFhirDiagnosticOrderEvent;
    {@member newDiagnosticOrderItem
      create a new item
    }
    {!script nolink}
    function newDiagnosticOrderItem : TFhirDiagnosticOrderItem;
    {@member newDiagnosticOrder
      create a new DiagnosticOrder
    }
    {!script nolink}
    function newDiagnosticOrder : TFhirDiagnosticOrder;
    {@member newDiagnosticReportImage
      create a new image
    }
    {!script nolink}
    function newDiagnosticReportImage : TFhirDiagnosticReportImage;
    {@member newDiagnosticReport
      create a new DiagnosticReport
    }
    {!script nolink}
    function newDiagnosticReport : TFhirDiagnosticReport;
    {@member newDocumentManifest
      create a new DocumentManifest
    }
    {!script nolink}
    function newDocumentManifest : TFhirDocumentManifest;
    {@member newDocumentReferenceRelatesTo
      create a new relatesTo
    }
    {!script nolink}
    function newDocumentReferenceRelatesTo : TFhirDocumentReferenceRelatesTo;
    {@member newDocumentReferenceService
      create a new service
    }
    {!script nolink}
    function newDocumentReferenceService : TFhirDocumentReferenceService;
    {@member newDocumentReferenceServiceParameter
      create a new parameter
    }
    {!script nolink}
    function newDocumentReferenceServiceParameter : TFhirDocumentReferenceServiceParameter;
    {@member newDocumentReferenceContext
      create a new context
    }
    {!script nolink}
    function newDocumentReferenceContext : TFhirDocumentReferenceContext;
    {@member newDocumentReference
      create a new DocumentReference
    }
    {!script nolink}
    function newDocumentReference : TFhirDocumentReference;
    {@member newEncounterParticipant
      create a new participant
    }
    {!script nolink}
    function newEncounterParticipant : TFhirEncounterParticipant;
    {@member newEncounterHospitalization
      create a new hospitalization
    }
    {!script nolink}
    function newEncounterHospitalization : TFhirEncounterHospitalization;
    {@member newEncounterHospitalizationAccomodation
      create a new accomodation
    }
    {!script nolink}
    function newEncounterHospitalizationAccomodation : TFhirEncounterHospitalizationAccomodation;
    {@member newEncounterLocation
      create a new location
    }
    {!script nolink}
    function newEncounterLocation : TFhirEncounterLocation;
    {@member newEncounter
      create a new Encounter
    }
    {!script nolink}
    function newEncounter : TFhirEncounter;
    {@member newFamilyHistoryRelation
      create a new relation
    }
    {!script nolink}
    function newFamilyHistoryRelation : TFhirFamilyHistoryRelation;
    {@member newFamilyHistoryRelationCondition
      create a new condition
    }
    {!script nolink}
    function newFamilyHistoryRelationCondition : TFhirFamilyHistoryRelationCondition;
    {@member newFamilyHistory
      create a new FamilyHistory
    }
    {!script nolink}
    function newFamilyHistory : TFhirFamilyHistory;
    {@member newGroupCharacteristic
      create a new characteristic
    }
    {!script nolink}
    function newGroupCharacteristic : TFhirGroupCharacteristic;
    {@member newGroup
      create a new Group
    }
    {!script nolink}
    function newGroup : TFhirGroup;
    {@member newImagingStudySeries
      create a new series
    }
    {!script nolink}
    function newImagingStudySeries : TFhirImagingStudySeries;
    {@member newImagingStudySeriesInstance
      create a new instance
    }
    {!script nolink}
    function newImagingStudySeriesInstance : TFhirImagingStudySeriesInstance;
    {@member newImagingStudy
      create a new ImagingStudy
    }
    {!script nolink}
    function newImagingStudy : TFhirImagingStudy;
    {@member newImmunizationExplanation
      create a new explanation
    }
    {!script nolink}
    function newImmunizationExplanation : TFhirImmunizationExplanation;
    {@member newImmunizationReaction
      create a new reaction
    }
    {!script nolink}
    function newImmunizationReaction : TFhirImmunizationReaction;
    {@member newImmunizationVaccinationProtocol
      create a new vaccinationProtocol
    }
    {!script nolink}
    function newImmunizationVaccinationProtocol : TFhirImmunizationVaccinationProtocol;
    {@member newImmunization
      create a new Immunization
    }
    {!script nolink}
    function newImmunization : TFhirImmunization;
    {@member newImmunizationRecommendationRecommendation
      create a new recommendation
    }
    {!script nolink}
    function newImmunizationRecommendationRecommendation : TFhirImmunizationRecommendationRecommendation;
    {@member newImmunizationRecommendationRecommendationDateCriterion
      create a new dateCriterion
    }
    {!script nolink}
    function newImmunizationRecommendationRecommendationDateCriterion : TFhirImmunizationRecommendationRecommendationDateCriterion;
    {@member newImmunizationRecommendationRecommendationProtocol
      create a new protocol
    }
    {!script nolink}
    function newImmunizationRecommendationRecommendationProtocol : TFhirImmunizationRecommendationRecommendationProtocol;
    {@member newImmunizationRecommendation
      create a new ImmunizationRecommendation
    }
    {!script nolink}
    function newImmunizationRecommendation : TFhirImmunizationRecommendation;
    {@member newListEntry
      create a new entry
    }
    {!script nolink}
    function newListEntry : TFhirListEntry;
    {@member newList
      create a new List
    }
    {!script nolink}
    function newList : TFhirList;
    {@member newLocationPosition
      create a new position
    }
    {!script nolink}
    function newLocationPosition : TFhirLocationPosition;
    {@member newLocation
      create a new Location
    }
    {!script nolink}
    function newLocation : TFhirLocation;
    {@member newMedia
      create a new Media
    }
    {!script nolink}
    function newMedia : TFhirMedia;
    {@member newMedicationProduct
      create a new product
    }
    {!script nolink}
    function newMedicationProduct : TFhirMedicationProduct;
    {@member newMedicationProductIngredient
      create a new ingredient
    }
    {!script nolink}
    function newMedicationProductIngredient : TFhirMedicationProductIngredient;
    {@member newMedicationPackage
      create a new package
    }
    {!script nolink}
    function newMedicationPackage : TFhirMedicationPackage;
    {@member newMedicationPackageContent
      create a new content
    }
    {!script nolink}
    function newMedicationPackageContent : TFhirMedicationPackageContent;
    {@member newMedication
      create a new Medication
    }
    {!script nolink}
    function newMedication : TFhirMedication;
    {@member newMedicationAdministrationDosage
      create a new dosage
    }
    {!script nolink}
    function newMedicationAdministrationDosage : TFhirMedicationAdministrationDosage;
    {@member newMedicationAdministration
      create a new MedicationAdministration
    }
    {!script nolink}
    function newMedicationAdministration : TFhirMedicationAdministration;
    {@member newMedicationDispenseDispense
      create a new dispense
    }
    {!script nolink}
    function newMedicationDispenseDispense : TFhirMedicationDispenseDispense;
    {@member newMedicationDispenseDispenseDosage
      create a new dosage
    }
    {!script nolink}
    function newMedicationDispenseDispenseDosage : TFhirMedicationDispenseDispenseDosage;
    {@member newMedicationDispenseSubstitution
      create a new substitution
    }
    {!script nolink}
    function newMedicationDispenseSubstitution : TFhirMedicationDispenseSubstitution;
    {@member newMedicationDispense
      create a new MedicationDispense
    }
    {!script nolink}
    function newMedicationDispense : TFhirMedicationDispense;
    {@member newMedicationPrescriptionDosageInstruction
      create a new dosageInstruction
    }
    {!script nolink}
    function newMedicationPrescriptionDosageInstruction : TFhirMedicationPrescriptionDosageInstruction;
    {@member newMedicationPrescriptionDispense
      create a new dispense
    }
    {!script nolink}
    function newMedicationPrescriptionDispense : TFhirMedicationPrescriptionDispense;
    {@member newMedicationPrescriptionSubstitution
      create a new substitution
    }
    {!script nolink}
    function newMedicationPrescriptionSubstitution : TFhirMedicationPrescriptionSubstitution;
    {@member newMedicationPrescription
      create a new MedicationPrescription
    }
    {!script nolink}
    function newMedicationPrescription : TFhirMedicationPrescription;
    {@member newMedicationStatementDosage
      create a new dosage
    }
    {!script nolink}
    function newMedicationStatementDosage : TFhirMedicationStatementDosage;
    {@member newMedicationStatement
      create a new MedicationStatement
    }
    {!script nolink}
    function newMedicationStatement : TFhirMedicationStatement;
    {@member newMessageHeaderResponse
      create a new response
    }
    {!script nolink}
    function newMessageHeaderResponse : TFhirMessageHeaderResponse;
    {@member newMessageHeaderSource
      create a new source
    }
    {!script nolink}
    function newMessageHeaderSource : TFhirMessageHeaderSource;
    {@member newMessageHeaderDestination
      create a new destination
    }
    {!script nolink}
    function newMessageHeaderDestination : TFhirMessageHeaderDestination;
    {@member newMessageHeader
      create a new MessageHeader
    }
    {!script nolink}
    function newMessageHeader : TFhirMessageHeader;
    {@member newNamespaceUniqueId
      create a new uniqueId
    }
    {!script nolink}
    function newNamespaceUniqueId : TFhirNamespaceUniqueId;
    {@member newNamespaceContact
      create a new contact
    }
    {!script nolink}
    function newNamespaceContact : TFhirNamespaceContact;
    {@member newNamespace
      create a new Namespace
    }
    {!script nolink}
    function newNamespace : TFhirNamespace;
    {@member newObservationReferenceRange
      create a new referenceRange
    }
    {!script nolink}
    function newObservationReferenceRange : TFhirObservationReferenceRange;
    {@member newObservationRelated
      create a new related
    }
    {!script nolink}
    function newObservationRelated : TFhirObservationRelated;
    {@member newObservation
      create a new Observation
    }
    {!script nolink}
    function newObservation : TFhirObservation;
    {@member newOperationDefinitionParameter
      create a new parameter
    }
    {!script nolink}
    function newOperationDefinitionParameter : TFhirOperationDefinitionParameter;
    {@member newOperationDefinition
      create a new OperationDefinition
    }
    {!script nolink}
    function newOperationDefinition : TFhirOperationDefinition;
    {@member newOperationOutcomeIssue
      create a new issue
    }
    {!script nolink}
    function newOperationOutcomeIssue : TFhirOperationOutcomeIssue;
    {@member newOperationOutcome
      create a new OperationOutcome
    }
    {!script nolink}
    function newOperationOutcome : TFhirOperationOutcome;
    {@member newOrderWhen
      create a new when
    }
    {!script nolink}
    function newOrderWhen : TFhirOrderWhen;
    {@member newOrder
      create a new Order
    }
    {!script nolink}
    function newOrder : TFhirOrder;
    {@member newOrderResponse
      create a new OrderResponse
    }
    {!script nolink}
    function newOrderResponse : TFhirOrderResponse;
    {@member newOrganizationContact
      create a new contact
    }
    {!script nolink}
    function newOrganizationContact : TFhirOrganizationContact;
    {@member newOrganization
      create a new Organization
    }
    {!script nolink}
    function newOrganization : TFhirOrganization;
    {@member newOther
      create a new Other
    }
    {!script nolink}
    function newOther : TFhirOther;
    {@member newPatientContact
      create a new contact
    }
    {!script nolink}
    function newPatientContact : TFhirPatientContact;
    {@member newPatientAnimal
      create a new animal
    }
    {!script nolink}
    function newPatientAnimal : TFhirPatientAnimal;
    {@member newPatientLink
      create a new link
    }
    {!script nolink}
    function newPatientLink : TFhirPatientLink;
    {@member newPatient
      create a new Patient
    }
    {!script nolink}
    function newPatient : TFhirPatient;
    {@member newPractitionerQualification
      create a new qualification
    }
    {!script nolink}
    function newPractitionerQualification : TFhirPractitionerQualification;
    {@member newPractitioner
      create a new Practitioner
    }
    {!script nolink}
    function newPractitioner : TFhirPractitioner;
    {@member newProcedurePerformer
      create a new performer
    }
    {!script nolink}
    function newProcedurePerformer : TFhirProcedurePerformer;
    {@member newProcedureRelatedItem
      create a new relatedItem
    }
    {!script nolink}
    function newProcedureRelatedItem : TFhirProcedureRelatedItem;
    {@member newProcedure
      create a new Procedure
    }
    {!script nolink}
    function newProcedure : TFhirProcedure;
    {@member newProfileMapping
      create a new mapping
    }
    {!script nolink}
    function newProfileMapping : TFhirProfileMapping;
    {@member newProfileStructure
      create a new structure
    }
    {!script nolink}
    function newProfileStructure : TFhirProfileStructure;
    {@member newProfileStructureSnapshot
      create a new snapshot
    }
    {!script nolink}
    function newProfileStructureSnapshot : TFhirProfileStructureSnapshot;
    {@member newProfileStructureSnapshotElement
      create a new element
    }
    {!script nolink}
    function newProfileStructureSnapshotElement : TFhirProfileStructureSnapshotElement;
    {@member newProfileStructureSnapshotElementSlicing
      create a new slicing
    }
    {!script nolink}
    function newProfileStructureSnapshotElementSlicing : TFhirProfileStructureSnapshotElementSlicing;
    {@member newProfileStructureSnapshotElementDefinition
      create a new definition
    }
    {!script nolink}
    function newProfileStructureSnapshotElementDefinition : TFhirProfileStructureSnapshotElementDefinition;
    {@member newProfileStructureSnapshotElementDefinitionType
      create a new type
    }
    {!script nolink}
    function newProfileStructureSnapshotElementDefinitionType : TFhirProfileStructureSnapshotElementDefinitionType;
    {@member newProfileStructureSnapshotElementDefinitionConstraint
      create a new constraint
    }
    {!script nolink}
    function newProfileStructureSnapshotElementDefinitionConstraint : TFhirProfileStructureSnapshotElementDefinitionConstraint;
    {@member newProfileStructureSnapshotElementDefinitionBinding
      create a new binding
    }
    {!script nolink}
    function newProfileStructureSnapshotElementDefinitionBinding : TFhirProfileStructureSnapshotElementDefinitionBinding;
    {@member newProfileStructureSnapshotElementDefinitionMapping
      create a new mapping
    }
    {!script nolink}
    function newProfileStructureSnapshotElementDefinitionMapping : TFhirProfileStructureSnapshotElementDefinitionMapping;
    {@member newProfileStructureSearchParam
      create a new searchParam
    }
    {!script nolink}
    function newProfileStructureSearchParam : TFhirProfileStructureSearchParam;
    {@member newProfileExtensionDefn
      create a new extensionDefn
    }
    {!script nolink}
    function newProfileExtensionDefn : TFhirProfileExtensionDefn;
    {@member newProfile
      create a new Profile
    }
    {!script nolink}
    function newProfile : TFhirProfile;
    {@member newProvenanceAgent
      create a new agent
    }
    {!script nolink}
    function newProvenanceAgent : TFhirProvenanceAgent;
    {@member newProvenanceEntity
      create a new entity
    }
    {!script nolink}
    function newProvenanceEntity : TFhirProvenanceEntity;
    {@member newProvenance
      create a new Provenance
    }
    {!script nolink}
    function newProvenance : TFhirProvenance;
    {@member newQueryResponse
      create a new response
    }
    {!script nolink}
    function newQueryResponse : TFhirQueryResponse;
    {@member newQuery
      create a new Query
    }
    {!script nolink}
    function newQuery : TFhirQuery;
    {@member newQuestionnaireGroup
      create a new group
    }
    {!script nolink}
    function newQuestionnaireGroup : TFhirQuestionnaireGroup;
    {@member newQuestionnaireGroupQuestion
      create a new question
    }
    {!script nolink}
    function newQuestionnaireGroupQuestion : TFhirQuestionnaireGroupQuestion;
    {@member newQuestionnaire
      create a new Questionnaire
    }
    {!script nolink}
    function newQuestionnaire : TFhirQuestionnaire;
    {@member newQuestionnaireAnswersGroup
      create a new group
    }
    {!script nolink}
    function newQuestionnaireAnswersGroup : TFhirQuestionnaireAnswersGroup;
    {@member newQuestionnaireAnswersGroupQuestion
      create a new question
    }
    {!script nolink}
    function newQuestionnaireAnswersGroupQuestion : TFhirQuestionnaireAnswersGroupQuestion;
    {@member newQuestionnaireAnswersGroupQuestionAnswer
      create a new answer
    }
    {!script nolink}
    function newQuestionnaireAnswersGroupQuestionAnswer : TFhirQuestionnaireAnswersGroupQuestionAnswer;
    {@member newQuestionnaireAnswers
      create a new QuestionnaireAnswers
    }
    {!script nolink}
    function newQuestionnaireAnswers : TFhirQuestionnaireAnswers;
    {@member newReferralRequest
      create a new ReferralRequest
    }
    {!script nolink}
    function newReferralRequest : TFhirReferralRequest;
    {@member newRelatedPerson
      create a new RelatedPerson
    }
    {!script nolink}
    function newRelatedPerson : TFhirRelatedPerson;
    {@member newRiskAssessmentPrediction
      create a new prediction
    }
    {!script nolink}
    function newRiskAssessmentPrediction : TFhirRiskAssessmentPrediction;
    {@member newRiskAssessment
      create a new RiskAssessment
    }
    {!script nolink}
    function newRiskAssessment : TFhirRiskAssessment;
    {@member newSecurityEventEvent
      create a new event
    }
    {!script nolink}
    function newSecurityEventEvent : TFhirSecurityEventEvent;
    {@member newSecurityEventParticipant
      create a new participant
    }
    {!script nolink}
    function newSecurityEventParticipant : TFhirSecurityEventParticipant;
    {@member newSecurityEventParticipantNetwork
      create a new network
    }
    {!script nolink}
    function newSecurityEventParticipantNetwork : TFhirSecurityEventParticipantNetwork;
    {@member newSecurityEventSource
      create a new source
    }
    {!script nolink}
    function newSecurityEventSource : TFhirSecurityEventSource;
    {@member newSecurityEventObject
      create a new object
    }
    {!script nolink}
    function newSecurityEventObject : TFhirSecurityEventObject;
    {@member newSecurityEventObjectDetail
      create a new detail
    }
    {!script nolink}
    function newSecurityEventObjectDetail : TFhirSecurityEventObjectDetail;
    {@member newSecurityEvent
      create a new SecurityEvent
    }
    {!script nolink}
    function newSecurityEvent : TFhirSecurityEvent;
    {@member newSlot
      create a new Slot
    }
    {!script nolink}
    function newSlot : TFhirSlot;
    {@member newSpecimenSource
      create a new source
    }
    {!script nolink}
    function newSpecimenSource : TFhirSpecimenSource;
    {@member newSpecimenCollection
      create a new collection
    }
    {!script nolink}
    function newSpecimenCollection : TFhirSpecimenCollection;
    {@member newSpecimenTreatment
      create a new treatment
    }
    {!script nolink}
    function newSpecimenTreatment : TFhirSpecimenTreatment;
    {@member newSpecimenContainer
      create a new container
    }
    {!script nolink}
    function newSpecimenContainer : TFhirSpecimenContainer;
    {@member newSpecimen
      create a new Specimen
    }
    {!script nolink}
    function newSpecimen : TFhirSpecimen;
    {@member newSubscriptionChannel
      create a new channel
    }
    {!script nolink}
    function newSubscriptionChannel : TFhirSubscriptionChannel;
    {@member newSubscriptionTag
      create a new tag
    }
    {!script nolink}
    function newSubscriptionTag : TFhirSubscriptionTag;
    {@member newSubscription
      create a new Subscription
    }
    {!script nolink}
    function newSubscription : TFhirSubscription;
    {@member newSubstanceInstance
      create a new instance
    }
    {!script nolink}
    function newSubstanceInstance : TFhirSubstanceInstance;
    {@member newSubstanceIngredient
      create a new ingredient
    }
    {!script nolink}
    function newSubstanceIngredient : TFhirSubstanceIngredient;
    {@member newSubstance
      create a new Substance
    }
    {!script nolink}
    function newSubstance : TFhirSubstance;
    {@member newSupplyDispense
      create a new dispense
    }
    {!script nolink}
    function newSupplyDispense : TFhirSupplyDispense;
    {@member newSupply
      create a new Supply
    }
    {!script nolink}
    function newSupply : TFhirSupply;
    {@member newValueSetDefine
      create a new define
    }
    {!script nolink}
    function newValueSetDefine : TFhirValueSetDefine;
    {@member newValueSetDefineConcept
      create a new concept
    }
    {!script nolink}
    function newValueSetDefineConcept : TFhirValueSetDefineConcept;
    {@member newValueSetCompose
      create a new compose
    }
    {!script nolink}
    function newValueSetCompose : TFhirValueSetCompose;
    {@member newValueSetComposeInclude
      create a new include
    }
    {!script nolink}
    function newValueSetComposeInclude : TFhirValueSetComposeInclude;
    {@member newValueSetComposeIncludeFilter
      create a new filter
    }
    {!script nolink}
    function newValueSetComposeIncludeFilter : TFhirValueSetComposeIncludeFilter;
    {@member newValueSetExpansion
      create a new expansion
    }
    {!script nolink}
    function newValueSetExpansion : TFhirValueSetExpansion;
    {@member newValueSetExpansionContains
      create a new contains
    }
    {!script nolink}
    function newValueSetExpansionContains : TFhirValueSetExpansionContains;
    {@member newValueSet
      create a new ValueSet
    }
    {!script nolink}
    function newValueSet : TFhirValueSet;
  end;

implementation

{ TFhirResource }

constructor TFhirResource.Create;
begin
  FContainedList := TFhirResourceList.create;
  inherited;
end;

destructor TFhirResource.Destroy;
begin
  FText.Free;
  FContainedList.Free;
  inherited;
end;

procedure TFhirResource.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'contained') then
    list.addAll(FContainedList);
  if (child_name = 'text') then
    list.add(text.Link);
end;

procedure TFhirResource.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'contained', 'Resource', FContainedList.Link));
  oList.add(TFHIRProperty.create(self, 'text', 'Narrative', FText.Link));
end;

procedure TFhirResource.Assign(oSource : TAdvObject);
begin
  inherited;
  FFormat := TFhirResource(oSource).FFormat;
  containedList.assign(TFhirResource(oSource).containedList);
  text := TFhirResource(oSource).text.Clone;
end;

function TFhirResource.Link : TFhirResource;
begin
  result := TFhirResource(inherited Link);
end;

function TFhirResource.Clone : TFhirResource;
begin
  result := TFhirResource(inherited Clone);
end;

procedure TFhirResource.SetText(value : TFhirNarrative);
begin
  FText.Free;
  FText := value;
end;

procedure TFhirResource.SetLanguage(value : TFhirCode);
begin
  FLanguage.Free;
  FLanguage := value;
end;

constructor TFhirBinary.Create;
begin
  inherited;
  FContent := TAdvBuffer.create;
end;

destructor TFhirBinary.Destroy;
begin
  FContent.free;
  inherited;
end;

function TFhirBinary.GetResourceType : TFhirResourceType;
begin
  result := frtBinary;
end;

function TFhirBinary.GetHasASummary : Boolean;
begin
  result := false;
end;


{ TFhirResourceList }
procedure TFhirResourceList.AddItem(value: TFhirResource);
begin
  assert(value.ClassName = 'TFhirResource', 'Attempt to add an item of type '+value.ClassName+' to a List of TFhirResource');
  add(value);
end;


procedure TFhirResourceList.ClearItems;
begin
  Clear;
end;

function TFhirResourceList.Clone: TFhirResourceList;
begin
  result := TFhirResourceList(inherited Clone);
end;

function TFhirResourceList.Count: Integer;
begin
  result := Inherited Count;
end;

function TFhirResourceList.GetItemN(index: Integer): TFhirResource;
begin
  result := TFhirResource(ObjectByIndex[index]);
end;

function TFhirResourceList.IndexOf(value: TFhirResource): Integer;
begin
  result := IndexByReference(value);
end;


procedure TFhirResourceList.InsertItem(index: Integer; value: TFhirResource);
begin
  assert(value is TFhirResource);
  Inherited Insert(index, value);
end;

function TFhirResourceList.Item(index: Integer): TFhirResource;
begin
  result := TFhirResource(ObjectByIndex[index]);
end;

function TFhirResourceList.Link: TFhirResourceList;
begin
  result := TFhirResourceList(inherited Link);
end;

procedure TFhirResourceList.Remove(index: Integer);
begin
  DeleteByIndex(index);
end;

procedure TFhirResourceList.SetItemByIndex(index: Integer; value: TFhirResource);
begin
  assert(value is TFhirResource);
  FhirResources[index] := value;
end;

procedure TFhirResourceList.SetItemN(index: Integer; value: TFhirResource);
begin
  assert(value is TFhirResource);
  ObjectByIndex[index] := value;
end;

{ TFhirAdverseReaction }

constructor TFhirAdverseReaction.Create;
begin
  inherited;
  FIdentifierList := TFhirIdentifierList.Create;
  FSymptomList := TFhirAdverseReactionSymptomList.Create;
  FExposureList := TFhirAdverseReactionExposureList.Create;
end;

destructor TFhirAdverseReaction.Destroy;
begin
  FIdentifierList.Free;
  FDate.free;
  FSubject.free;
  FDidNotOccurFlag.free;
  FRecorder.free;
  FSymptomList.Free;
  FExposureList.Free;
  inherited;
end;

function TFhirAdverseReaction.GetResourceType : TFhirResourceType;
begin
  result := frtAdverseReaction;
end;

function TFhirAdverseReaction.GetHasASummary : Boolean;
begin
  result := false;
end;

procedure TFhirAdverseReaction.Assign(oSource : TAdvObject);
begin
  inherited;
  FIdentifierList.Assign(TFhirAdverseReaction(oSource).FIdentifierList);
  date := TFhirAdverseReaction(oSource).date.Clone;
  subject := TFhirAdverseReaction(oSource).subject.Clone;
  didNotOccurFlag := TFhirAdverseReaction(oSource).didNotOccurFlag.Clone;
  recorder := TFhirAdverseReaction(oSource).recorder.Clone;
  FSymptomList.Assign(TFhirAdverseReaction(oSource).FSymptomList);
  FExposureList.Assign(TFhirAdverseReaction(oSource).FExposureList);
end;

procedure TFhirAdverseReaction.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'identifier') Then
     list.addAll(FIdentifierList);
  if (child_name = 'date') Then
     list.add(Date.Link);
  if (child_name = 'subject') Then
     list.add(Subject.Link);
  if (child_name = 'didNotOccurFlag') Then
     list.add(DidNotOccurFlag.Link);
  if (child_name = 'recorder') Then
     list.add(Recorder.Link);
  if (child_name = 'symptom') Then
     list.addAll(FSymptomList);
  if (child_name = 'exposure') Then
     list.addAll(FExposureList);
end;

procedure TFhirAdverseReaction.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'date', 'dateTime', FDate.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'subject', 'Resource(Patient)', FSubject.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'didNotOccurFlag', 'boolean', FDidNotOccurFlag.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'recorder', 'Resource(Practitioner|Patient)', FRecorder.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'symptom', '', FSymptomList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'exposure', '', FExposureList.Link)){3};
end;

function TFhirAdverseReaction.Link : TFhirAdverseReaction;
begin
  result := TFhirAdverseReaction(inherited Link);
end;

function TFhirAdverseReaction.Clone : TFhirAdverseReaction;
begin
  result := TFhirAdverseReaction(inherited Clone);
end;

{ TFhirAdverseReaction }

Procedure TFhirAdverseReaction.SetDate(value : TFhirDateTime);
begin
  FDate.free;
  FDate := value;
end;

Function TFhirAdverseReaction.GetDateST : TDateAndTime;
begin
  if FDate = nil then
    result := nil
  else
    result := Date.value;
end;

Procedure TFhirAdverseReaction.SetDateST(value : TDateAndTime);
begin
  if value <> nil then
  begin
    if FDate = nil then
      FDate := TFhirDateTime.create;
    FDate.value := value
  end
  else if FDate <> nil then
    FDate.value := nil;
end;

Procedure TFhirAdverseReaction.SetSubject(value : TFhirResourceReference{TFhirPatient});
begin
  FSubject.free;
  FSubject := value;
end;

Procedure TFhirAdverseReaction.SetDidNotOccurFlag(value : TFhirBoolean);
begin
  FDidNotOccurFlag.free;
  FDidNotOccurFlag := value;
end;

Function TFhirAdverseReaction.GetDidNotOccurFlagST : Boolean;
begin
  if FDidNotOccurFlag = nil then
    result := false
  else
    result := DidNotOccurFlag.value;
end;

Procedure TFhirAdverseReaction.SetDidNotOccurFlagST(value : Boolean);
begin
  if FDidNotOccurFlag = nil then
    FDidNotOccurFlag := TFhirBoolean.create;
  FDidNotOccurFlag.value := value
end;

Procedure TFhirAdverseReaction.SetRecorder(value : TFhirResourceReference{Resource});
begin
  FRecorder.free;
  FRecorder := value;
end;


{ TFhirAlert }

constructor TFhirAlert.Create;
begin
  inherited;
  FIdentifierList := TFhirIdentifierList.Create;
end;

destructor TFhirAlert.Destroy;
begin
  FIdentifierList.Free;
  FCategory.free;
  FStatus.free;
  FSubject.free;
  FAuthor.free;
  FNote.free;
  inherited;
end;

function TFhirAlert.GetResourceType : TFhirResourceType;
begin
  result := frtAlert;
end;

function TFhirAlert.GetHasASummary : Boolean;
begin
  result := false;
end;

procedure TFhirAlert.Assign(oSource : TAdvObject);
begin
  inherited;
  FIdentifierList.Assign(TFhirAlert(oSource).FIdentifierList);
  category := TFhirAlert(oSource).category.Clone;
  FStatus := TFhirAlert(oSource).FStatus.Link;
  subject := TFhirAlert(oSource).subject.Clone;
  author := TFhirAlert(oSource).author.Clone;
  note := TFhirAlert(oSource).note.Clone;
end;

procedure TFhirAlert.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'identifier') Then
     list.addAll(FIdentifierList);
  if (child_name = 'category') Then
     list.add(Category.Link);
  if (child_name = 'status') Then
     list.add(FStatus.Link);
  if (child_name = 'subject') Then
     list.add(Subject.Link);
  if (child_name = 'author') Then
     list.add(Author.Link);
  if (child_name = 'note') Then
     list.add(Note.Link);
end;

procedure TFhirAlert.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'category', 'CodeableConcept', FCategory.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'status', 'code', FStatus.Link));{1}
  oList.add(TFHIRProperty.create(self, 'subject', 'Resource(Patient)', FSubject.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'author', 'Resource(Practitioner|Patient|Device)', FAuthor.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'note', 'string', FNote.Link.Link));{2}
end;

function TFhirAlert.Link : TFhirAlert;
begin
  result := TFhirAlert(inherited Link);
end;

function TFhirAlert.Clone : TFhirAlert;
begin
  result := TFhirAlert(inherited Clone);
end;

{ TFhirAlert }

Procedure TFhirAlert.SetCategory(value : TFhirCodeableConcept);
begin
  FCategory.free;
  FCategory := value;
end;

Procedure TFhirAlert.SetStatus(value : TFhirEnum);
begin
  FStatus.free;
  FStatus := value;
end;

Function TFhirAlert.GetStatusST : TFhirAlertStatus;
begin
  if FStatus = nil then
    result := TFhirAlertStatus(0)
  else
    result := TFhirAlertStatus(StringArrayIndexOf(CODES_TFhirAlertStatus, Status.value));
end;

Procedure TFhirAlert.SetStatusST(value : TFhirAlertStatus);
begin
  if ord(value) = 0 then
    Status := nil
  else
    Status := TFhirEnum.create(CODES_TFhirAlertStatus[value]);
end;

Procedure TFhirAlert.SetSubject(value : TFhirResourceReference{TFhirPatient});
begin
  FSubject.free;
  FSubject := value;
end;

Procedure TFhirAlert.SetAuthor(value : TFhirResourceReference{Resource});
begin
  FAuthor.free;
  FAuthor := value;
end;

Procedure TFhirAlert.SetNote(value : TFhirString);
begin
  FNote.free;
  FNote := value;
end;

Function TFhirAlert.GetNoteST : String;
begin
  if FNote = nil then
    result := ''
  else
    result := Note.value;
end;

Procedure TFhirAlert.SetNoteST(value : String);
begin
  if value <> '' then
  begin
    if FNote = nil then
      FNote := TFhirString.create;
    FNote.value := value
  end
  else if FNote <> nil then
    FNote.value := '';
end;


{ TFhirAllergyIntolerance }

constructor TFhirAllergyIntolerance.Create;
begin
  inherited;
  FIdentifierList := TFhirIdentifierList.Create;
  FReactionList := TFhirResourceReferenceList{TFhirAdverseReaction}.Create;
  FSensitivityTestList := TFhirResourceReferenceList{TFhirObservation}.Create;
end;

destructor TFhirAllergyIntolerance.Destroy;
begin
  FIdentifierList.Free;
  FCriticality.free;
  FSensitivityType.free;
  FRecordedDate.free;
  FStatus.free;
  FSubject.free;
  FRecorder.free;
  FSubstance.free;
  FReactionList.Free;
  FSensitivityTestList.Free;
  inherited;
end;

function TFhirAllergyIntolerance.GetResourceType : TFhirResourceType;
begin
  result := frtAllergyIntolerance;
end;

function TFhirAllergyIntolerance.GetHasASummary : Boolean;
begin
  result := false;
end;

procedure TFhirAllergyIntolerance.Assign(oSource : TAdvObject);
begin
  inherited;
  FIdentifierList.Assign(TFhirAllergyIntolerance(oSource).FIdentifierList);
  FCriticality := TFhirAllergyIntolerance(oSource).FCriticality.Link;
  FSensitivityType := TFhirAllergyIntolerance(oSource).FSensitivityType.Link;
  recordedDate := TFhirAllergyIntolerance(oSource).recordedDate.Clone;
  FStatus := TFhirAllergyIntolerance(oSource).FStatus.Link;
  subject := TFhirAllergyIntolerance(oSource).subject.Clone;
  recorder := TFhirAllergyIntolerance(oSource).recorder.Clone;
  substance := TFhirAllergyIntolerance(oSource).substance.Clone;
  FReactionList.Assign(TFhirAllergyIntolerance(oSource).FReactionList);
  FSensitivityTestList.Assign(TFhirAllergyIntolerance(oSource).FSensitivityTestList);
end;

procedure TFhirAllergyIntolerance.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'identifier') Then
     list.addAll(FIdentifierList);
  if (child_name = 'criticality') Then
     list.add(FCriticality.Link);
  if (child_name = 'sensitivityType') Then
     list.add(FSensitivityType.Link);
  if (child_name = 'recordedDate') Then
     list.add(RecordedDate.Link);
  if (child_name = 'status') Then
     list.add(FStatus.Link);
  if (child_name = 'subject') Then
     list.add(Subject.Link);
  if (child_name = 'recorder') Then
     list.add(Recorder.Link);
  if (child_name = 'substance') Then
     list.add(Substance.Link);
  if (child_name = 'reaction') Then
     list.addAll(FReactionList);
  if (child_name = 'sensitivityTest') Then
     list.addAll(FSensitivityTestList);
end;

procedure TFhirAllergyIntolerance.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'criticality', 'code', FCriticality.Link));{1}
  oList.add(TFHIRProperty.create(self, 'sensitivityType', 'code', FSensitivityType.Link));{1}
  oList.add(TFHIRProperty.create(self, 'recordedDate', 'dateTime', FRecordedDate.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'status', 'code', FStatus.Link));{1}
  oList.add(TFHIRProperty.create(self, 'subject', 'Resource(Patient)', FSubject.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'recorder', 'Resource(Practitioner|Patient)', FRecorder.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'substance', 'Resource(Substance)', FSubstance.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'reaction', 'Resource(AdverseReaction)', FReactionList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'sensitivityTest', 'Resource(Observation)', FSensitivityTestList.Link)){3};
end;

function TFhirAllergyIntolerance.Link : TFhirAllergyIntolerance;
begin
  result := TFhirAllergyIntolerance(inherited Link);
end;

function TFhirAllergyIntolerance.Clone : TFhirAllergyIntolerance;
begin
  result := TFhirAllergyIntolerance(inherited Clone);
end;

{ TFhirAllergyIntolerance }

Procedure TFhirAllergyIntolerance.SetCriticality(value : TFhirEnum);
begin
  FCriticality.free;
  FCriticality := value;
end;

Function TFhirAllergyIntolerance.GetCriticalityST : TFhirCriticality;
begin
  if FCriticality = nil then
    result := TFhirCriticality(0)
  else
    result := TFhirCriticality(StringArrayIndexOf(CODES_TFhirCriticality, Criticality.value));
end;

Procedure TFhirAllergyIntolerance.SetCriticalityST(value : TFhirCriticality);
begin
  if ord(value) = 0 then
    Criticality := nil
  else
    Criticality := TFhirEnum.create(CODES_TFhirCriticality[value]);
end;

Procedure TFhirAllergyIntolerance.SetSensitivityType(value : TFhirEnum);
begin
  FSensitivityType.free;
  FSensitivityType := value;
end;

Function TFhirAllergyIntolerance.GetSensitivityTypeST : TFhirSensitivitytype;
begin
  if FSensitivityType = nil then
    result := TFhirSensitivitytype(0)
  else
    result := TFhirSensitivitytype(StringArrayIndexOf(CODES_TFhirSensitivitytype, SensitivityType.value));
end;

Procedure TFhirAllergyIntolerance.SetSensitivityTypeST(value : TFhirSensitivitytype);
begin
  if ord(value) = 0 then
    SensitivityType := nil
  else
    SensitivityType := TFhirEnum.create(CODES_TFhirSensitivitytype[value]);
end;

Procedure TFhirAllergyIntolerance.SetRecordedDate(value : TFhirDateTime);
begin
  FRecordedDate.free;
  FRecordedDate := value;
end;

Function TFhirAllergyIntolerance.GetRecordedDateST : TDateAndTime;
begin
  if FRecordedDate = nil then
    result := nil
  else
    result := RecordedDate.value;
end;

Procedure TFhirAllergyIntolerance.SetRecordedDateST(value : TDateAndTime);
begin
  if value <> nil then
  begin
    if FRecordedDate = nil then
      FRecordedDate := TFhirDateTime.create;
    FRecordedDate.value := value
  end
  else if FRecordedDate <> nil then
    FRecordedDate.value := nil;
end;

Procedure TFhirAllergyIntolerance.SetStatus(value : TFhirEnum);
begin
  FStatus.free;
  FStatus := value;
end;

Function TFhirAllergyIntolerance.GetStatusST : TFhirSensitivitystatus;
begin
  if FStatus = nil then
    result := TFhirSensitivitystatus(0)
  else
    result := TFhirSensitivitystatus(StringArrayIndexOf(CODES_TFhirSensitivitystatus, Status.value));
end;

Procedure TFhirAllergyIntolerance.SetStatusST(value : TFhirSensitivitystatus);
begin
  if ord(value) = 0 then
    Status := nil
  else
    Status := TFhirEnum.create(CODES_TFhirSensitivitystatus[value]);
end;

Procedure TFhirAllergyIntolerance.SetSubject(value : TFhirResourceReference{TFhirPatient});
begin
  FSubject.free;
  FSubject := value;
end;

Procedure TFhirAllergyIntolerance.SetRecorder(value : TFhirResourceReference{Resource});
begin
  FRecorder.free;
  FRecorder := value;
end;

Procedure TFhirAllergyIntolerance.SetSubstance(value : TFhirResourceReference{TFhirSubstance});
begin
  FSubstance.free;
  FSubstance := value;
end;


{ TFhirAppointment }

constructor TFhirAppointment.Create;
begin
  inherited;
  FIdentifierList := TFhirIdentifierList.Create;
  FSlotList := TFhirResourceReferenceList{TFhirSlot}.Create;
  FParticipantList := TFhirAppointmentParticipantList.Create;
end;

destructor TFhirAppointment.Destroy;
begin
  FIdentifierList.Free;
  FPriority.free;
  FStatus.free;
  FType_.free;
  FReason.free;
  FDescription.free;
  FStart.free;
  FEnd_.free;
  FSlotList.Free;
  FLocation.free;
  FComment.free;
  FOrder.free;
  FParticipantList.Free;
  FLastModifiedBy.free;
  FLastModified.free;
  inherited;
end;

function TFhirAppointment.GetResourceType : TFhirResourceType;
begin
  result := frtAppointment;
end;

function TFhirAppointment.GetHasASummary : Boolean;
begin
  result := false;
end;

procedure TFhirAppointment.Assign(oSource : TAdvObject);
begin
  inherited;
  FIdentifierList.Assign(TFhirAppointment(oSource).FIdentifierList);
  priority := TFhirAppointment(oSource).priority.Clone;
  status := TFhirAppointment(oSource).status.Clone;
  type_ := TFhirAppointment(oSource).type_.Clone;
  reason := TFhirAppointment(oSource).reason.Clone;
  description := TFhirAppointment(oSource).description.Clone;
  start := TFhirAppointment(oSource).start.Clone;
  end_ := TFhirAppointment(oSource).end_.Clone;
  FSlotList.Assign(TFhirAppointment(oSource).FSlotList);
  location := TFhirAppointment(oSource).location.Clone;
  comment := TFhirAppointment(oSource).comment.Clone;
  order := TFhirAppointment(oSource).order.Clone;
  FParticipantList.Assign(TFhirAppointment(oSource).FParticipantList);
  lastModifiedBy := TFhirAppointment(oSource).lastModifiedBy.Clone;
  lastModified := TFhirAppointment(oSource).lastModified.Clone;
end;

procedure TFhirAppointment.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'identifier') Then
     list.addAll(FIdentifierList);
  if (child_name = 'priority') Then
     list.add(Priority.Link);
  if (child_name = 'status') Then
     list.add(Status.Link);
  if (child_name = 'type_') Then
     list.add(Type_.Link);
  if (child_name = 'reason') Then
     list.add(Reason.Link);
  if (child_name = 'description') Then
     list.add(Description.Link);
  if (child_name = 'start') Then
     list.add(Start.Link);
  if (child_name = 'end_') Then
     list.add(End_.Link);
  if (child_name = 'slot') Then
     list.addAll(FSlotList);
  if (child_name = 'location') Then
     list.add(Location.Link);
  if (child_name = 'comment') Then
     list.add(Comment.Link);
  if (child_name = 'order') Then
     list.add(Order.Link);
  if (child_name = 'participant') Then
     list.addAll(FParticipantList);
  if (child_name = 'lastModifiedBy') Then
     list.add(LastModifiedBy.Link);
  if (child_name = 'lastModified') Then
     list.add(LastModified.Link);
end;

procedure TFhirAppointment.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'priority', 'integer', FPriority.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'status', 'code', FStatus.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'type', 'CodeableConcept', FType_.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'reason', 'CodeableConcept', FReason.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'description', 'string', FDescription.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'start', 'instant', FStart.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'end', 'instant', FEnd_.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'slot', 'Resource(Slot)', FSlotList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'location', 'Resource(Location)', FLocation.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'comment', 'string', FComment.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'order', 'Resource(Order)', FOrder.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'participant', '', FParticipantList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'lastModifiedBy', 'Resource(Practitioner|Patient|RelatedPerson)', FLastModifiedBy.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'lastModified', 'dateTime', FLastModified.Link.Link));{2}
end;

function TFhirAppointment.Link : TFhirAppointment;
begin
  result := TFhirAppointment(inherited Link);
end;

function TFhirAppointment.Clone : TFhirAppointment;
begin
  result := TFhirAppointment(inherited Clone);
end;

{ TFhirAppointment }

Procedure TFhirAppointment.SetPriority(value : TFhirInteger);
begin
  FPriority.free;
  FPriority := value;
end;

Function TFhirAppointment.GetPriorityST : String;
begin
  if FPriority = nil then
    result := ''
  else
    result := Priority.value;
end;

Procedure TFhirAppointment.SetPriorityST(value : String);
begin
  if value <> '' then
  begin
    if FPriority = nil then
      FPriority := TFhirInteger.create;
    FPriority.value := value
  end
  else if FPriority <> nil then
    FPriority.value := '';
end;

Procedure TFhirAppointment.SetStatus(value : TFhirCode);
begin
  FStatus.free;
  FStatus := value;
end;

Function TFhirAppointment.GetStatusST : String;
begin
  if FStatus = nil then
    result := ''
  else
    result := Status.value;
end;

Procedure TFhirAppointment.SetStatusST(value : String);
begin
  if value <> '' then
  begin
    if FStatus = nil then
      FStatus := TFhirCode.create;
    FStatus.value := value
  end
  else if FStatus <> nil then
    FStatus.value := '';
end;

Procedure TFhirAppointment.SetType_(value : TFhirCodeableConcept);
begin
  FType_.free;
  FType_ := value;
end;

Procedure TFhirAppointment.SetReason(value : TFhirCodeableConcept);
begin
  FReason.free;
  FReason := value;
end;

Procedure TFhirAppointment.SetDescription(value : TFhirString);
begin
  FDescription.free;
  FDescription := value;
end;

Function TFhirAppointment.GetDescriptionST : String;
begin
  if FDescription = nil then
    result := ''
  else
    result := Description.value;
end;

Procedure TFhirAppointment.SetDescriptionST(value : String);
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

Procedure TFhirAppointment.SetStart(value : TFhirInstant);
begin
  FStart.free;
  FStart := value;
end;

Function TFhirAppointment.GetStartST : TDateAndTime;
begin
  if FStart = nil then
    result := nil
  else
    result := Start.value;
end;

Procedure TFhirAppointment.SetStartST(value : TDateAndTime);
begin
  if value <> nil then
  begin
    if FStart = nil then
      FStart := TFhirInstant.create;
    FStart.value := value
  end
  else if FStart <> nil then
    FStart.value := nil;
end;

Procedure TFhirAppointment.SetEnd_(value : TFhirInstant);
begin
  FEnd_.free;
  FEnd_ := value;
end;

Function TFhirAppointment.GetEnd_ST : TDateAndTime;
begin
  if FEnd_ = nil then
    result := nil
  else
    result := End_.value;
end;

Procedure TFhirAppointment.SetEnd_ST(value : TDateAndTime);
begin
  if value <> nil then
  begin
    if FEnd_ = nil then
      FEnd_ := TFhirInstant.create;
    FEnd_.value := value
  end
  else if FEnd_ <> nil then
    FEnd_.value := nil;
end;

Procedure TFhirAppointment.SetLocation(value : TFhirResourceReference{TFhirLocation});
begin
  FLocation.free;
  FLocation := value;
end;

Procedure TFhirAppointment.SetComment(value : TFhirString);
begin
  FComment.free;
  FComment := value;
end;

Function TFhirAppointment.GetCommentST : String;
begin
  if FComment = nil then
    result := ''
  else
    result := Comment.value;
end;

Procedure TFhirAppointment.SetCommentST(value : String);
begin
  if value <> '' then
  begin
    if FComment = nil then
      FComment := TFhirString.create;
    FComment.value := value
  end
  else if FComment <> nil then
    FComment.value := '';
end;

Procedure TFhirAppointment.SetOrder(value : TFhirResourceReference{TFhirOrder});
begin
  FOrder.free;
  FOrder := value;
end;

Procedure TFhirAppointment.SetLastModifiedBy(value : TFhirResourceReference{Resource});
begin
  FLastModifiedBy.free;
  FLastModifiedBy := value;
end;

Procedure TFhirAppointment.SetLastModified(value : TFhirDateTime);
begin
  FLastModified.free;
  FLastModified := value;
end;

Function TFhirAppointment.GetLastModifiedST : TDateAndTime;
begin
  if FLastModified = nil then
    result := nil
  else
    result := LastModified.value;
end;

Procedure TFhirAppointment.SetLastModifiedST(value : TDateAndTime);
begin
  if value <> nil then
  begin
    if FLastModified = nil then
      FLastModified := TFhirDateTime.create;
    FLastModified.value := value
  end
  else if FLastModified <> nil then
    FLastModified.value := nil;
end;


{ TFhirAppointmentResponse }

constructor TFhirAppointmentResponse.Create;
begin
  inherited;
  FIdentifierList := TFhirIdentifierList.Create;
  FParticipantTypeList := TFhirCodeableConceptList.Create;
  FIndividualList := TFhirResourceReferenceList{Resource}.Create;
end;

destructor TFhirAppointmentResponse.Destroy;
begin
  FIdentifierList.Free;
  FAppointment.free;
  FParticipantTypeList.Free;
  FIndividualList.Free;
  FParticipantStatus.free;
  FComment.free;
  FStart.free;
  FEnd_.free;
  FLastModifiedBy.free;
  FLastModified.free;
  inherited;
end;

function TFhirAppointmentResponse.GetResourceType : TFhirResourceType;
begin
  result := frtAppointmentResponse;
end;

function TFhirAppointmentResponse.GetHasASummary : Boolean;
begin
  result := false;
end;

procedure TFhirAppointmentResponse.Assign(oSource : TAdvObject);
begin
  inherited;
  FIdentifierList.Assign(TFhirAppointmentResponse(oSource).FIdentifierList);
  appointment := TFhirAppointmentResponse(oSource).appointment.Clone;
  FParticipantTypeList.Assign(TFhirAppointmentResponse(oSource).FParticipantTypeList);
  FIndividualList.Assign(TFhirAppointmentResponse(oSource).FIndividualList);
  FParticipantStatus := TFhirAppointmentResponse(oSource).FParticipantStatus.Link;
  comment := TFhirAppointmentResponse(oSource).comment.Clone;
  start := TFhirAppointmentResponse(oSource).start.Clone;
  end_ := TFhirAppointmentResponse(oSource).end_.Clone;
  lastModifiedBy := TFhirAppointmentResponse(oSource).lastModifiedBy.Clone;
  lastModified := TFhirAppointmentResponse(oSource).lastModified.Clone;
end;

procedure TFhirAppointmentResponse.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'identifier') Then
     list.addAll(FIdentifierList);
  if (child_name = 'appointment') Then
     list.add(Appointment.Link);
  if (child_name = 'participantType') Then
     list.addAll(FParticipantTypeList);
  if (child_name = 'individual') Then
     list.addAll(FIndividualList);
  if (child_name = 'participantStatus') Then
     list.add(FParticipantStatus.Link);
  if (child_name = 'comment') Then
     list.add(Comment.Link);
  if (child_name = 'start') Then
     list.add(Start.Link);
  if (child_name = 'end_') Then
     list.add(End_.Link);
  if (child_name = 'lastModifiedBy') Then
     list.add(LastModifiedBy.Link);
  if (child_name = 'lastModified') Then
     list.add(LastModified.Link);
end;

procedure TFhirAppointmentResponse.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'appointment', 'Resource(Appointment)', FAppointment.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'participantType', 'CodeableConcept', FParticipantTypeList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'individual', 'Resource(Any)', FIndividualList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'participantStatus', 'code', FParticipantStatus.Link));{1}
  oList.add(TFHIRProperty.create(self, 'comment', 'string', FComment.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'start', 'instant', FStart.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'end', 'instant', FEnd_.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'lastModifiedBy', 'Resource(Practitioner|Patient|RelatedPerson)', FLastModifiedBy.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'lastModified', 'dateTime', FLastModified.Link.Link));{2}
end;

function TFhirAppointmentResponse.Link : TFhirAppointmentResponse;
begin
  result := TFhirAppointmentResponse(inherited Link);
end;

function TFhirAppointmentResponse.Clone : TFhirAppointmentResponse;
begin
  result := TFhirAppointmentResponse(inherited Clone);
end;

{ TFhirAppointmentResponse }

Procedure TFhirAppointmentResponse.SetAppointment(value : TFhirResourceReference{TFhirAppointment});
begin
  FAppointment.free;
  FAppointment := value;
end;

Procedure TFhirAppointmentResponse.SetParticipantStatus(value : TFhirEnum);
begin
  FParticipantStatus.free;
  FParticipantStatus := value;
end;

Function TFhirAppointmentResponse.GetParticipantStatusST : TFhirParticipantstatus;
begin
  if FParticipantStatus = nil then
    result := TFhirParticipantstatus(0)
  else
    result := TFhirParticipantstatus(StringArrayIndexOf(CODES_TFhirParticipantstatus, ParticipantStatus.value));
end;

Procedure TFhirAppointmentResponse.SetParticipantStatusST(value : TFhirParticipantstatus);
begin
  if ord(value) = 0 then
    ParticipantStatus := nil
  else
    ParticipantStatus := TFhirEnum.create(CODES_TFhirParticipantstatus[value]);
end;

Procedure TFhirAppointmentResponse.SetComment(value : TFhirString);
begin
  FComment.free;
  FComment := value;
end;

Function TFhirAppointmentResponse.GetCommentST : String;
begin
  if FComment = nil then
    result := ''
  else
    result := Comment.value;
end;

Procedure TFhirAppointmentResponse.SetCommentST(value : String);
begin
  if value <> '' then
  begin
    if FComment = nil then
      FComment := TFhirString.create;
    FComment.value := value
  end
  else if FComment <> nil then
    FComment.value := '';
end;

Procedure TFhirAppointmentResponse.SetStart(value : TFhirInstant);
begin
  FStart.free;
  FStart := value;
end;

Function TFhirAppointmentResponse.GetStartST : TDateAndTime;
begin
  if FStart = nil then
    result := nil
  else
    result := Start.value;
end;

Procedure TFhirAppointmentResponse.SetStartST(value : TDateAndTime);
begin
  if value <> nil then
  begin
    if FStart = nil then
      FStart := TFhirInstant.create;
    FStart.value := value
  end
  else if FStart <> nil then
    FStart.value := nil;
end;

Procedure TFhirAppointmentResponse.SetEnd_(value : TFhirInstant);
begin
  FEnd_.free;
  FEnd_ := value;
end;

Function TFhirAppointmentResponse.GetEnd_ST : TDateAndTime;
begin
  if FEnd_ = nil then
    result := nil
  else
    result := End_.value;
end;

Procedure TFhirAppointmentResponse.SetEnd_ST(value : TDateAndTime);
begin
  if value <> nil then
  begin
    if FEnd_ = nil then
      FEnd_ := TFhirInstant.create;
    FEnd_.value := value
  end
  else if FEnd_ <> nil then
    FEnd_.value := nil;
end;

Procedure TFhirAppointmentResponse.SetLastModifiedBy(value : TFhirResourceReference{Resource});
begin
  FLastModifiedBy.free;
  FLastModifiedBy := value;
end;

Procedure TFhirAppointmentResponse.SetLastModified(value : TFhirDateTime);
begin
  FLastModified.free;
  FLastModified := value;
end;

Function TFhirAppointmentResponse.GetLastModifiedST : TDateAndTime;
begin
  if FLastModified = nil then
    result := nil
  else
    result := LastModified.value;
end;

Procedure TFhirAppointmentResponse.SetLastModifiedST(value : TDateAndTime);
begin
  if value <> nil then
  begin
    if FLastModified = nil then
      FLastModified := TFhirDateTime.create;
    FLastModified.value := value
  end
  else if FLastModified <> nil then
    FLastModified.value := nil;
end;


{ TFhirAvailability }

constructor TFhirAvailability.Create;
begin
  inherited;
  FIdentifierList := TFhirIdentifierList.Create;
  FType_List := TFhirCodeableConceptList.Create;
end;

destructor TFhirAvailability.Destroy;
begin
  FIdentifierList.Free;
  FType_List.Free;
  FActor.free;
  FPlanningHorizon.free;
  FComment.free;
  FLastModified.free;
  inherited;
end;

function TFhirAvailability.GetResourceType : TFhirResourceType;
begin
  result := frtAvailability;
end;

function TFhirAvailability.GetHasASummary : Boolean;
begin
  result := false;
end;

procedure TFhirAvailability.Assign(oSource : TAdvObject);
begin
  inherited;
  FIdentifierList.Assign(TFhirAvailability(oSource).FIdentifierList);
  FType_List.Assign(TFhirAvailability(oSource).FType_List);
  actor := TFhirAvailability(oSource).actor.Clone;
  planningHorizon := TFhirAvailability(oSource).planningHorizon.Clone;
  comment := TFhirAvailability(oSource).comment.Clone;
  lastModified := TFhirAvailability(oSource).lastModified.Clone;
end;

procedure TFhirAvailability.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'identifier') Then
     list.addAll(FIdentifierList);
  if (child_name = 'type_') Then
     list.addAll(FType_List);
  if (child_name = 'actor') Then
     list.add(Actor.Link);
  if (child_name = 'planningHorizon') Then
     list.add(PlanningHorizon.Link);
  if (child_name = 'comment') Then
     list.add(Comment.Link);
  if (child_name = 'lastModified') Then
     list.add(LastModified.Link);
end;

procedure TFhirAvailability.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'type', 'CodeableConcept', FType_List.Link)){3};
  oList.add(TFHIRProperty.create(self, 'actor', 'Resource(Any)', FActor.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'planningHorizon', 'Period', FPlanningHorizon.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'comment', 'string', FComment.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'lastModified', 'dateTime', FLastModified.Link.Link));{2}
end;

function TFhirAvailability.Link : TFhirAvailability;
begin
  result := TFhirAvailability(inherited Link);
end;

function TFhirAvailability.Clone : TFhirAvailability;
begin
  result := TFhirAvailability(inherited Clone);
end;

{ TFhirAvailability }

Procedure TFhirAvailability.SetActor(value : TFhirResourceReference{Resource});
begin
  FActor.free;
  FActor := value;
end;

Procedure TFhirAvailability.SetPlanningHorizon(value : TFhirPeriod);
begin
  FPlanningHorizon.free;
  FPlanningHorizon := value;
end;

Procedure TFhirAvailability.SetComment(value : TFhirString);
begin
  FComment.free;
  FComment := value;
end;

Function TFhirAvailability.GetCommentST : String;
begin
  if FComment = nil then
    result := ''
  else
    result := Comment.value;
end;

Procedure TFhirAvailability.SetCommentST(value : String);
begin
  if value <> '' then
  begin
    if FComment = nil then
      FComment := TFhirString.create;
    FComment.value := value
  end
  else if FComment <> nil then
    FComment.value := '';
end;

Procedure TFhirAvailability.SetLastModified(value : TFhirDateTime);
begin
  FLastModified.free;
  FLastModified := value;
end;

Function TFhirAvailability.GetLastModifiedST : TDateAndTime;
begin
  if FLastModified = nil then
    result := nil
  else
    result := LastModified.value;
end;

Procedure TFhirAvailability.SetLastModifiedST(value : TDateAndTime);
begin
  if value <> nil then
  begin
    if FLastModified = nil then
      FLastModified := TFhirDateTime.create;
    FLastModified.value := value
  end
  else if FLastModified <> nil then
    FLastModified.value := nil;
end;


{ TFhirCarePlan }

constructor TFhirCarePlan.Create;
begin
  inherited;
  FIdentifierList := TFhirIdentifierList.Create;
  FConcernList := TFhirResourceReferenceList{TFhirCondition}.Create;
  FParticipantList := TFhirCarePlanParticipantList.Create;
  FGoalList := TFhirCarePlanGoalList.Create;
  FActivityList := TFhirCarePlanActivityList.Create;
end;

destructor TFhirCarePlan.Destroy;
begin
  FIdentifierList.Free;
  FPatient.free;
  FStatus.free;
  FPeriod.free;
  FModified.free;
  FConcernList.Free;
  FParticipantList.Free;
  FGoalList.Free;
  FActivityList.Free;
  FNotes.free;
  inherited;
end;

function TFhirCarePlan.GetResourceType : TFhirResourceType;
begin
  result := frtCarePlan;
end;

function TFhirCarePlan.GetHasASummary : Boolean;
begin
  result := false;
end;

procedure TFhirCarePlan.Assign(oSource : TAdvObject);
begin
  inherited;
  FIdentifierList.Assign(TFhirCarePlan(oSource).FIdentifierList);
  patient := TFhirCarePlan(oSource).patient.Clone;
  FStatus := TFhirCarePlan(oSource).FStatus.Link;
  period := TFhirCarePlan(oSource).period.Clone;
  modified := TFhirCarePlan(oSource).modified.Clone;
  FConcernList.Assign(TFhirCarePlan(oSource).FConcernList);
  FParticipantList.Assign(TFhirCarePlan(oSource).FParticipantList);
  FGoalList.Assign(TFhirCarePlan(oSource).FGoalList);
  FActivityList.Assign(TFhirCarePlan(oSource).FActivityList);
  notes := TFhirCarePlan(oSource).notes.Clone;
end;

procedure TFhirCarePlan.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'identifier') Then
     list.addAll(FIdentifierList);
  if (child_name = 'patient') Then
     list.add(Patient.Link);
  if (child_name = 'status') Then
     list.add(FStatus.Link);
  if (child_name = 'period') Then
     list.add(Period.Link);
  if (child_name = 'modified') Then
     list.add(Modified.Link);
  if (child_name = 'concern') Then
     list.addAll(FConcernList);
  if (child_name = 'participant') Then
     list.addAll(FParticipantList);
  if (child_name = 'goal') Then
     list.addAll(FGoalList);
  if (child_name = 'activity') Then
     list.addAll(FActivityList);
  if (child_name = 'notes') Then
     list.add(Notes.Link);
end;

procedure TFhirCarePlan.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'patient', 'Resource(Patient)', FPatient.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'status', 'code', FStatus.Link));{1}
  oList.add(TFHIRProperty.create(self, 'period', 'Period', FPeriod.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'modified', 'dateTime', FModified.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'concern', 'Resource(Condition)', FConcernList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'participant', '', FParticipantList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'goal', '', FGoalList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'activity', '', FActivityList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'notes', 'string', FNotes.Link.Link));{2}
end;

function TFhirCarePlan.Link : TFhirCarePlan;
begin
  result := TFhirCarePlan(inherited Link);
end;

function TFhirCarePlan.Clone : TFhirCarePlan;
begin
  result := TFhirCarePlan(inherited Clone);
end;

{ TFhirCarePlan }

Procedure TFhirCarePlan.SetPatient(value : TFhirResourceReference{TFhirPatient});
begin
  FPatient.free;
  FPatient := value;
end;

Procedure TFhirCarePlan.SetStatus(value : TFhirEnum);
begin
  FStatus.free;
  FStatus := value;
end;

Function TFhirCarePlan.GetStatusST : TFhirCarePlanStatus;
begin
  if FStatus = nil then
    result := TFhirCarePlanStatus(0)
  else
    result := TFhirCarePlanStatus(StringArrayIndexOf(CODES_TFhirCarePlanStatus, Status.value));
end;

Procedure TFhirCarePlan.SetStatusST(value : TFhirCarePlanStatus);
begin
  if ord(value) = 0 then
    Status := nil
  else
    Status := TFhirEnum.create(CODES_TFhirCarePlanStatus[value]);
end;

Procedure TFhirCarePlan.SetPeriod(value : TFhirPeriod);
begin
  FPeriod.free;
  FPeriod := value;
end;

Procedure TFhirCarePlan.SetModified(value : TFhirDateTime);
begin
  FModified.free;
  FModified := value;
end;

Function TFhirCarePlan.GetModifiedST : TDateAndTime;
begin
  if FModified = nil then
    result := nil
  else
    result := Modified.value;
end;

Procedure TFhirCarePlan.SetModifiedST(value : TDateAndTime);
begin
  if value <> nil then
  begin
    if FModified = nil then
      FModified := TFhirDateTime.create;
    FModified.value := value
  end
  else if FModified <> nil then
    FModified.value := nil;
end;

Procedure TFhirCarePlan.SetNotes(value : TFhirString);
begin
  FNotes.free;
  FNotes := value;
end;

Function TFhirCarePlan.GetNotesST : String;
begin
  if FNotes = nil then
    result := ''
  else
    result := Notes.value;
end;

Procedure TFhirCarePlan.SetNotesST(value : String);
begin
  if value <> '' then
  begin
    if FNotes = nil then
      FNotes := TFhirString.create;
    FNotes.value := value
  end
  else if FNotes <> nil then
    FNotes.value := '';
end;


{ TFhirComposition }

constructor TFhirComposition.Create;
begin
  inherited;
  FAuthorList := TFhirResourceReferenceList{Resource}.Create;
  FAttesterList := TFhirCompositionAttesterList.Create;
  FSectionList := TFhirCompositionSectionList.Create;
end;

destructor TFhirComposition.Destroy;
begin
  FIdentifier.free;
  FDate.free;
  FType_.free;
  FClass_.free;
  FTitle.free;
  FStatus.free;
  FConfidentiality.free;
  FSubject.free;
  FAuthorList.Free;
  FAttesterList.Free;
  FCustodian.free;
  FEvent.free;
  FEncounter.free;
  FSectionList.Free;
  inherited;
end;

function TFhirComposition.GetResourceType : TFhirResourceType;
begin
  result := frtComposition;
end;

function TFhirComposition.GetHasASummary : Boolean;
begin
  result := true;
end;

procedure TFhirComposition.Assign(oSource : TAdvObject);
begin
  inherited;
  identifier := TFhirComposition(oSource).identifier.Clone;
  date := TFhirComposition(oSource).date.Clone;
  type_ := TFhirComposition(oSource).type_.Clone;
  class_ := TFhirComposition(oSource).class_.Clone;
  title := TFhirComposition(oSource).title.Clone;
  FStatus := TFhirComposition(oSource).FStatus.Link;
  confidentiality := TFhirComposition(oSource).confidentiality.Clone;
  subject := TFhirComposition(oSource).subject.Clone;
  FAuthorList.Assign(TFhirComposition(oSource).FAuthorList);
  FAttesterList.Assign(TFhirComposition(oSource).FAttesterList);
  custodian := TFhirComposition(oSource).custodian.Clone;
  event := TFhirComposition(oSource).event.Clone;
  encounter := TFhirComposition(oSource).encounter.Clone;
  FSectionList.Assign(TFhirComposition(oSource).FSectionList);
end;

procedure TFhirComposition.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'identifier') Then
     list.add(Identifier.Link);
  if (child_name = 'date') Then
     list.add(Date.Link);
  if (child_name = 'type_') Then
     list.add(Type_.Link);
  if (child_name = 'class_') Then
     list.add(Class_.Link);
  if (child_name = 'title') Then
     list.add(Title.Link);
  if (child_name = 'status') Then
     list.add(FStatus.Link);
  if (child_name = 'confidentiality') Then
     list.add(Confidentiality.Link);
  if (child_name = 'subject') Then
     list.add(Subject.Link);
  if (child_name = 'author') Then
     list.addAll(FAuthorList);
  if (child_name = 'attester') Then
     list.addAll(FAttesterList);
  if (child_name = 'custodian') Then
     list.add(Custodian.Link);
  if (child_name = 'event') Then
     list.add(Event.Link);
  if (child_name = 'encounter') Then
     list.add(Encounter.Link);
  if (child_name = 'section') Then
     list.addAll(FSectionList);
end;

procedure TFhirComposition.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifier.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'date', 'dateTime', FDate.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'type', 'CodeableConcept', FType_.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'class', 'CodeableConcept', FClass_.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'title', 'string', FTitle.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'status', 'code', FStatus.Link));{1}
  oList.add(TFHIRProperty.create(self, 'confidentiality', 'Coding', FConfidentiality.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'subject', 'Resource(Patient|Practitioner|Group|Device|Location)', FSubject.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'author', 'Resource(Practitioner|Device|Patient|RelatedPerson)', FAuthorList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'attester', '', FAttesterList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'custodian', 'Resource(Organization)', FCustodian.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'event', '', FEvent.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'encounter', 'Resource(Encounter)', FEncounter.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'section', '', FSectionList.Link)){3};
end;

function TFhirComposition.Link : TFhirComposition;
begin
  result := TFhirComposition(inherited Link);
end;

function TFhirComposition.Clone : TFhirComposition;
begin
  result := TFhirComposition(inherited Clone);
end;

{ TFhirComposition }

Procedure TFhirComposition.SetIdentifier(value : TFhirIdentifier);
begin
  FIdentifier.free;
  FIdentifier := value;
end;

Procedure TFhirComposition.SetDate(value : TFhirDateTime);
begin
  FDate.free;
  FDate := value;
end;

Function TFhirComposition.GetDateST : TDateAndTime;
begin
  if FDate = nil then
    result := nil
  else
    result := Date.value;
end;

Procedure TFhirComposition.SetDateST(value : TDateAndTime);
begin
  if value <> nil then
  begin
    if FDate = nil then
      FDate := TFhirDateTime.create;
    FDate.value := value
  end
  else if FDate <> nil then
    FDate.value := nil;
end;

Procedure TFhirComposition.SetType_(value : TFhirCodeableConcept);
begin
  FType_.free;
  FType_ := value;
end;

Procedure TFhirComposition.SetClass_(value : TFhirCodeableConcept);
begin
  FClass_.free;
  FClass_ := value;
end;

Procedure TFhirComposition.SetTitle(value : TFhirString);
begin
  FTitle.free;
  FTitle := value;
end;

Function TFhirComposition.GetTitleST : String;
begin
  if FTitle = nil then
    result := ''
  else
    result := Title.value;
end;

Procedure TFhirComposition.SetTitleST(value : String);
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

Procedure TFhirComposition.SetStatus(value : TFhirEnum);
begin
  FStatus.free;
  FStatus := value;
end;

Function TFhirComposition.GetStatusST : TFhirCompositionStatus;
begin
  if FStatus = nil then
    result := TFhirCompositionStatus(0)
  else
    result := TFhirCompositionStatus(StringArrayIndexOf(CODES_TFhirCompositionStatus, Status.value));
end;

Procedure TFhirComposition.SetStatusST(value : TFhirCompositionStatus);
begin
  if ord(value) = 0 then
    Status := nil
  else
    Status := TFhirEnum.create(CODES_TFhirCompositionStatus[value]);
end;

Procedure TFhirComposition.SetConfidentiality(value : TFhirCoding);
begin
  FConfidentiality.free;
  FConfidentiality := value;
end;

Procedure TFhirComposition.SetSubject(value : TFhirResourceReference{Resource});
begin
  FSubject.free;
  FSubject := value;
end;

Procedure TFhirComposition.SetCustodian(value : TFhirResourceReference{TFhirOrganization});
begin
  FCustodian.free;
  FCustodian := value;
end;

Procedure TFhirComposition.SetEvent(value : TFhirCompositionEvent);
begin
  FEvent.free;
  FEvent := value;
end;

Procedure TFhirComposition.SetEncounter(value : TFhirResourceReference{TFhirEncounter});
begin
  FEncounter.free;
  FEncounter := value;
end;


{ TFhirConceptMap }

constructor TFhirConceptMap.Create;
begin
  inherited;
  FTelecomList := TFhirContactList.Create;
  FElementList := TFhirConceptMapElementList.Create;
end;

destructor TFhirConceptMap.Destroy;
begin
  FIdentifier.free;
  FVersion.free;
  FName.free;
  FPublisher.free;
  FTelecomList.Free;
  FDescription.free;
  FCopyright.free;
  FStatus.free;
  FExperimental.free;
  FDate.free;
  FSource.free;
  FTarget.free;
  FElementList.Free;
  inherited;
end;

function TFhirConceptMap.GetResourceType : TFhirResourceType;
begin
  result := frtConceptMap;
end;

function TFhirConceptMap.GetHasASummary : Boolean;
begin
  result := true;
end;

procedure TFhirConceptMap.Assign(oSource : TAdvObject);
  begin
  inherited;
  identifier := TFhirConceptMap(oSource).identifier.Clone;
  version := TFhirConceptMap(oSource).version.Clone;
  name := TFhirConceptMap(oSource).name.Clone;
  publisher := TFhirConceptMap(oSource).publisher.Clone;
  FTelecomList.Assign(TFhirConceptMap(oSource).FTelecomList);
  description := TFhirConceptMap(oSource).description.Clone;
  copyright := TFhirConceptMap(oSource).copyright.Clone;
  FStatus := TFhirConceptMap(oSource).FStatus.Link;
  experimental := TFhirConceptMap(oSource).experimental.Clone;
  date := TFhirConceptMap(oSource).date.Clone;
  source := TFhirConceptMap(oSource).source.Clone;
  target := TFhirConceptMap(oSource).target.Clone;
  FElementList.Assign(TFhirConceptMap(oSource).FElementList);
end;

procedure TFhirConceptMap.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'identifier') Then
     list.add(Identifier.Link);
  if (child_name = 'version') Then
     list.add(Version.Link);
  if (child_name = 'name') Then
     list.add(Name.Link);
  if (child_name = 'publisher') Then
     list.add(Publisher.Link);
  if (child_name = 'telecom') Then
     list.addAll(FTelecomList);
  if (child_name = 'description') Then
     list.add(Description.Link);
  if (child_name = 'copyright') Then
     list.add(Copyright.Link);
  if (child_name = 'status') Then
     list.add(FStatus.Link);
  if (child_name = 'experimental') Then
     list.add(Experimental.Link);
  if (child_name = 'date') Then
     list.add(Date.Link);
  if (child_name = 'source') Then
     list.add(Source.Link);
  if (child_name = 'target') Then
     list.add(Target.Link);
  if (child_name = 'element') Then
     list.addAll(FElementList);
end;

procedure TFhirConceptMap.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'string', FIdentifier.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'version', 'string', FVersion.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'name', 'string', FName.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'publisher', 'string', FPublisher.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'telecom', 'Contact', FTelecomList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'description', 'string', FDescription.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'copyright', 'string', FCopyright.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'status', 'code', FStatus.Link));{1}
  oList.add(TFHIRProperty.create(self, 'experimental', 'boolean', FExperimental.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'date', 'dateTime', FDate.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'source[x]', 'uri|Resource(ValueSet|Profile)', FSource.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'target[x]', 'uri|Resource(ValueSet|Profile)', FTarget.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'element', '', FElementList.Link)){3};
end;

function TFhirConceptMap.Link : TFhirConceptMap;
begin
  result := TFhirConceptMap(inherited Link);
end;

function TFhirConceptMap.Clone : TFhirConceptMap;
  begin
  result := TFhirConceptMap(inherited Clone);
end;

{ TFhirConceptMap }

Procedure TFhirConceptMap.SetIdentifier(value : TFhirString);
begin
  FIdentifier.free;
  FIdentifier := value;
end;

Function TFhirConceptMap.GetIdentifierST : String;
begin
  if FIdentifier = nil then
    result := ''
  else
    result := Identifier.value;
end;

Procedure TFhirConceptMap.SetIdentifierST(value : String);
begin
  if value <> '' then
  begin
    if FIdentifier = nil then
      FIdentifier := TFhirString.create;
    FIdentifier.value := value
  end
  else if FIdentifier <> nil then
    FIdentifier.value := '';
end;

Procedure TFhirConceptMap.SetVersion(value : TFhirString);
begin
  FVersion.free;
  FVersion := value;
end;

Function TFhirConceptMap.GetVersionST : String;
begin
  if FVersion = nil then
    result := ''
  else
    result := Version.value;
end;

Procedure TFhirConceptMap.SetVersionST(value : String);
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

Procedure TFhirConceptMap.SetName(value : TFhirString);
begin
  FName.free;
  FName := value;
end;

Function TFhirConceptMap.GetNameST : String;
begin
  if FName = nil then
    result := ''
  else
    result := Name.value;
end;

Procedure TFhirConceptMap.SetNameST(value : String);
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

Procedure TFhirConceptMap.SetPublisher(value : TFhirString);
begin
  FPublisher.free;
  FPublisher := value;
end;

Function TFhirConceptMap.GetPublisherST : String;
begin
  if FPublisher = nil then
    result := ''
  else
    result := Publisher.value;
end;

Procedure TFhirConceptMap.SetPublisherST(value : String);
begin
  if value <> '' then
  begin
    if FPublisher = nil then
      FPublisher := TFhirString.create;
    FPublisher.value := value
  end
  else if FPublisher <> nil then
    FPublisher.value := '';
end;

Procedure TFhirConceptMap.SetDescription(value : TFhirString);
begin
  FDescription.free;
  FDescription := value;
end;

Function TFhirConceptMap.GetDescriptionST : String;
begin
  if FDescription = nil then
    result := ''
  else
    result := Description.value;
end;

Procedure TFhirConceptMap.SetDescriptionST(value : String);
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

Procedure TFhirConceptMap.SetCopyright(value : TFhirString);
begin
  FCopyright.free;
  FCopyright := value;
end;

Function TFhirConceptMap.GetCopyrightST : String;
begin
  if FCopyright = nil then
    result := ''
  else
    result := Copyright.value;
end;

Procedure TFhirConceptMap.SetCopyrightST(value : String);
begin
  if value <> '' then
begin
    if FCopyright = nil then
      FCopyright := TFhirString.create;
    FCopyright.value := value
  end
  else if FCopyright <> nil then
    FCopyright.value := '';
end;

Procedure TFhirConceptMap.SetStatus(value : TFhirEnum);
begin
  FStatus.free;
  FStatus := value;
end;

Function TFhirConceptMap.GetStatusST : TFhirValuesetStatus;
begin
  if FStatus = nil then
    result := TFhirValuesetStatus(0)
  else
    result := TFhirValuesetStatus(StringArrayIndexOf(CODES_TFhirValuesetStatus, Status.value));
end;

Procedure TFhirConceptMap.SetStatusST(value : TFhirValuesetStatus);
begin
  if ord(value) = 0 then
    Status := nil
  else
    Status := TFhirEnum.create(CODES_TFhirValuesetStatus[value]);
end;

Procedure TFhirConceptMap.SetExperimental(value : TFhirBoolean);
begin
  FExperimental.free;
  FExperimental := value;
end;

Function TFhirConceptMap.GetExperimentalST : Boolean;
begin
  if FExperimental = nil then
    result := false
  else
    result := Experimental.value;
end;

Procedure TFhirConceptMap.SetExperimentalST(value : Boolean);
begin
  if FExperimental = nil then
    FExperimental := TFhirBoolean.create;
  FExperimental.value := value
end;

Procedure TFhirConceptMap.SetDate(value : TFhirDateTime);
begin
  FDate.free;
  FDate := value;
end;

Function TFhirConceptMap.GetDateST : TDateAndTime;
begin
  if FDate = nil then
    result := nil
  else
    result := Date.value;
end;

Procedure TFhirConceptMap.SetDateST(value : TDateAndTime);
begin
  if value <> nil then
  begin
    if FDate = nil then
      FDate := TFhirDateTime.create;
    FDate.value := value
  end
  else if FDate <> nil then
    FDate.value := nil;
end;

Procedure TFhirConceptMap.SetSource(value : TFhirType);
begin
  FSource.free;
  FSource := value;
end;

Procedure TFhirConceptMap.SetTarget(value : TFhirType);
begin
  FTarget.free;
  FTarget := value;
end;


{ TFhirCondition }

constructor TFhirCondition.Create;
begin
  inherited;
  FIdentifierList := TFhirIdentifierList.Create;
  FEvidenceList := TFhirConditionEvidenceList.Create;
  FLocationList := TFhirConditionLocationList.Create;
  FRelatedItemList := TFhirConditionRelatedItemList.Create;
end;

destructor TFhirCondition.Destroy;
begin
  FIdentifierList.Free;
  FSubject.free;
  FEncounter.free;
  FAsserter.free;
  FDateAsserted.free;
  FCode.free;
  FCategory.free;
  FStatus.free;
  FCertainty.free;
  FSeverity.free;
  FOnset.free;
  FAbatement.free;
  FStage.free;
  FEvidenceList.Free;
  FLocationList.Free;
  FRelatedItemList.Free;
  FNotes.free;
  inherited;
end;

function TFhirCondition.GetResourceType : TFhirResourceType;
begin
  result := frtCondition;
end;

function TFhirCondition.GetHasASummary : Boolean;
begin
  result := false;
end;

procedure TFhirCondition.Assign(oSource : TAdvObject);
begin
  inherited;
  FIdentifierList.Assign(TFhirCondition(oSource).FIdentifierList);
  subject := TFhirCondition(oSource).subject.Clone;
  encounter := TFhirCondition(oSource).encounter.Clone;
  asserter := TFhirCondition(oSource).asserter.Clone;
  dateAsserted := TFhirCondition(oSource).dateAsserted.Clone;
  code := TFhirCondition(oSource).code.Clone;
  category := TFhirCondition(oSource).category.Clone;
  FStatus := TFhirCondition(oSource).FStatus.Link;
  certainty := TFhirCondition(oSource).certainty.Clone;
  severity := TFhirCondition(oSource).severity.Clone;
  onset := TFhirCondition(oSource).onset.Clone;
  abatement := TFhirCondition(oSource).abatement.Clone;
  stage := TFhirCondition(oSource).stage.Clone;
  FEvidenceList.Assign(TFhirCondition(oSource).FEvidenceList);
  FLocationList.Assign(TFhirCondition(oSource).FLocationList);
  FRelatedItemList.Assign(TFhirCondition(oSource).FRelatedItemList);
  notes := TFhirCondition(oSource).notes.Clone;
end;

procedure TFhirCondition.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'identifier') Then
     list.addAll(FIdentifierList);
  if (child_name = 'subject') Then
     list.add(Subject.Link);
  if (child_name = 'encounter') Then
     list.add(Encounter.Link);
  if (child_name = 'asserter') Then
     list.add(Asserter.Link);
  if (child_name = 'dateAsserted') Then
     list.add(DateAsserted.Link);
  if (child_name = 'code') Then
     list.add(Code.Link);
  if (child_name = 'category') Then
     list.add(Category.Link);
  if (child_name = 'status') Then
     list.add(FStatus.Link);
  if (child_name = 'certainty') Then
     list.add(Certainty.Link);
  if (child_name = 'severity') Then
     list.add(Severity.Link);
  if (child_name = 'onset') Then
     list.add(Onset.Link);
  if (child_name = 'abatement') Then
     list.add(Abatement.Link);
  if (child_name = 'stage') Then
     list.add(Stage.Link);
  if (child_name = 'evidence') Then
     list.addAll(FEvidenceList);
  if (child_name = 'location') Then
     list.addAll(FLocationList);
  if (child_name = 'relatedItem') Then
     list.addAll(FRelatedItemList);
  if (child_name = 'notes') Then
     list.add(Notes.Link);
end;

procedure TFhirCondition.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'subject', 'Resource(Patient)', FSubject.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'encounter', 'Resource(Encounter)', FEncounter.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'asserter', 'Resource(Practitioner|Patient)', FAsserter.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'dateAsserted', 'date', FDateAsserted.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'code', 'CodeableConcept', FCode.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'category', 'CodeableConcept', FCategory.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'status', 'code', FStatus.Link));{1}
  oList.add(TFHIRProperty.create(self, 'certainty', 'CodeableConcept', FCertainty.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'severity', 'CodeableConcept', FSeverity.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'onset[x]', 'date|Age', FOnset.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'abatement[x]', 'date|Age|boolean', FAbatement.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'stage', '', FStage.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'evidence', '', FEvidenceList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'location', '', FLocationList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'relatedItem', '', FRelatedItemList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'notes', 'string', FNotes.Link.Link));{2}
end;

function TFhirCondition.Link : TFhirCondition;
begin
  result := TFhirCondition(inherited Link);
end;

function TFhirCondition.Clone : TFhirCondition;
begin
  result := TFhirCondition(inherited Clone);
end;

{ TFhirCondition }

Procedure TFhirCondition.SetSubject(value : TFhirResourceReference{TFhirPatient});
begin
  FSubject.free;
  FSubject := value;
end;

Procedure TFhirCondition.SetEncounter(value : TFhirResourceReference{TFhirEncounter});
begin
  FEncounter.free;
  FEncounter := value;
end;

Procedure TFhirCondition.SetAsserter(value : TFhirResourceReference{Resource});
begin
  FAsserter.free;
  FAsserter := value;
end;

Procedure TFhirCondition.SetDateAsserted(value : TFhirDate);
begin
  FDateAsserted.free;
  FDateAsserted := value;
end;

Function TFhirCondition.GetDateAssertedST : TDateAndTime;
begin
  if FDateAsserted = nil then
    result := nil
  else
    result := DateAsserted.value;
end;

Procedure TFhirCondition.SetDateAssertedST(value : TDateAndTime);
begin
  if value <> nil then
  begin
    if FDateAsserted = nil then
      FDateAsserted := TFhirDate.create;
    FDateAsserted.value := value
  end
  else if FDateAsserted <> nil then
    FDateAsserted.value := nil;
end;

Procedure TFhirCondition.SetCode(value : TFhirCodeableConcept);
begin
  FCode.free;
  FCode := value;
end;

Procedure TFhirCondition.SetCategory(value : TFhirCodeableConcept);
begin
  FCategory.free;
  FCategory := value;
end;

Procedure TFhirCondition.SetStatus(value : TFhirEnum);
begin
  FStatus.free;
  FStatus := value;
end;

Function TFhirCondition.GetStatusST : TFhirConditionStatus;
begin
  if FStatus = nil then
    result := TFhirConditionStatus(0)
  else
    result := TFhirConditionStatus(StringArrayIndexOf(CODES_TFhirConditionStatus, Status.value));
end;

Procedure TFhirCondition.SetStatusST(value : TFhirConditionStatus);
begin
  if ord(value) = 0 then
    Status := nil
  else
    Status := TFhirEnum.create(CODES_TFhirConditionStatus[value]);
end;

Procedure TFhirCondition.SetCertainty(value : TFhirCodeableConcept);
begin
  FCertainty.free;
  FCertainty := value;
end;

Procedure TFhirCondition.SetSeverity(value : TFhirCodeableConcept);
begin
  FSeverity.free;
  FSeverity := value;
end;

Procedure TFhirCondition.SetOnset(value : TFhirType);
begin
  FOnset.free;
  FOnset := value;
end;

Procedure TFhirCondition.SetAbatement(value : TFhirType);
begin
  FAbatement.free;
  FAbatement := value;
end;

Procedure TFhirCondition.SetStage(value : TFhirConditionStage);
begin
  FStage.free;
  FStage := value;
end;

Procedure TFhirCondition.SetNotes(value : TFhirString);
begin
  FNotes.free;
  FNotes := value;
end;

Function TFhirCondition.GetNotesST : String;
begin
  if FNotes = nil then
    result := ''
  else
    result := Notes.value;
end;

Procedure TFhirCondition.SetNotesST(value : String);
begin
  if value <> '' then
begin
    if FNotes = nil then
      FNotes := TFhirString.create;
    FNotes.value := value
  end
  else if FNotes <> nil then
    FNotes.value := '';
end;


{ TFhirConformance }

constructor TFhirConformance.Create;
begin
  inherited;
  FTelecomList := TFhirContactList.Create;
  FFormatList := TFhirCodeList.Create;
  FProfileList := TFhirResourceReferenceList{TFhirProfile}.Create;
  FRestList := TFhirConformanceRestList.Create;
  FMessagingList := TFhirConformanceMessagingList.Create;
  FDocumentList := TFhirConformanceDocumentList.Create;
end;

destructor TFhirConformance.Destroy;
begin
  FIdentifier.free;
  FVersion.free;
  FName.free;
  FPublisher.free;
  FTelecomList.Free;
  FDescription.free;
  FStatus.free;
  FExperimental.free;
  FDate.free;
  FSoftware.free;
  FImplementation_.free;
  FFhirVersion.free;
  FAcceptUnknown.free;
  FFormatList.Free;
  FProfileList.Free;
  FRestList.Free;
  FMessagingList.Free;
  FDocumentList.Free;
  inherited;
end;

function TFhirConformance.GetResourceType : TFhirResourceType;
begin
  result := frtConformance;
end;

function TFhirConformance.GetHasASummary : Boolean;
begin
  result := true;
end;

procedure TFhirConformance.Assign(oSource : TAdvObject);
begin
  inherited;
  identifier := TFhirConformance(oSource).identifier.Clone;
  version := TFhirConformance(oSource).version.Clone;
  name := TFhirConformance(oSource).name.Clone;
  publisher := TFhirConformance(oSource).publisher.Clone;
  FTelecomList.Assign(TFhirConformance(oSource).FTelecomList);
  description := TFhirConformance(oSource).description.Clone;
  FStatus := TFhirConformance(oSource).FStatus.Link;
  experimental := TFhirConformance(oSource).experimental.Clone;
  date := TFhirConformance(oSource).date.Clone;
  software := TFhirConformance(oSource).software.Clone;
  implementation_ := TFhirConformance(oSource).implementation_.Clone;
  fhirVersion := TFhirConformance(oSource).fhirVersion.Clone;
  acceptUnknown := TFhirConformance(oSource).acceptUnknown.Clone;
  FFormatList.Assign(TFhirConformance(oSource).FFormatList);
  FProfileList.Assign(TFhirConformance(oSource).FProfileList);
  FRestList.Assign(TFhirConformance(oSource).FRestList);
  FMessagingList.Assign(TFhirConformance(oSource).FMessagingList);
  FDocumentList.Assign(TFhirConformance(oSource).FDocumentList);
end;

procedure TFhirConformance.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'identifier') Then
     list.add(Identifier.Link);
  if (child_name = 'version') Then
     list.add(Version.Link);
  if (child_name = 'name') Then
     list.add(Name.Link);
  if (child_name = 'publisher') Then
     list.add(Publisher.Link);
  if (child_name = 'telecom') Then
     list.addAll(FTelecomList);
  if (child_name = 'description') Then
     list.add(Description.Link);
  if (child_name = 'status') Then
     list.add(FStatus.Link);
  if (child_name = 'experimental') Then
     list.add(Experimental.Link);
  if (child_name = 'date') Then
     list.add(Date.Link);
  if (child_name = 'software') Then
     list.add(Software.Link);
  if (child_name = 'implementation_') Then
     list.add(Implementation_.Link);
  if (child_name = 'fhirVersion') Then
     list.add(FhirVersion.Link);
  if (child_name = 'acceptUnknown') Then
     list.add(AcceptUnknown.Link);
  if (child_name = 'format') Then
     list.addAll(FFormatList);
  if (child_name = 'profile') Then
     list.addAll(FProfileList);
  if (child_name = 'rest') Then
     list.addAll(FRestList);
  if (child_name = 'messaging') Then
     list.addAll(FMessagingList);
  if (child_name = 'document') Then
     list.addAll(FDocumentList);
end;

procedure TFhirConformance.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'string', FIdentifier.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'version', 'string', FVersion.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'name', 'string', FName.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'publisher', 'string', FPublisher.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'telecom', 'Contact', FTelecomList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'description', 'string', FDescription.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'status', 'code', FStatus.Link));{1}
  oList.add(TFHIRProperty.create(self, 'experimental', 'boolean', FExperimental.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'date', 'dateTime', FDate.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'software', '', FSoftware.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'implementation', '', FImplementation_.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'fhirVersion', 'id', FFhirVersion.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'acceptUnknown', 'boolean', FAcceptUnknown.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'format', 'code', FFormatList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'profile', 'Resource(Profile)', FProfileList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'rest', '', FRestList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'messaging', '', FMessagingList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'document', '', FDocumentList.Link)){3};
end;

function TFhirConformance.Link : TFhirConformance;
begin
  result := TFhirConformance(inherited Link);
end;

function TFhirConformance.Clone : TFhirConformance;
begin
  result := TFhirConformance(inherited Clone);
end;

{ TFhirConformance }

Procedure TFhirConformance.SetIdentifier(value : TFhirString);
begin
  FIdentifier.free;
  FIdentifier := value;
end;

Function TFhirConformance.GetIdentifierST : String;
begin
  if FIdentifier = nil then
    result := ''
  else
    result := Identifier.value;
end;

Procedure TFhirConformance.SetIdentifierST(value : String);
begin
  if value <> '' then
  begin
    if FIdentifier = nil then
      FIdentifier := TFhirString.create;
    FIdentifier.value := value
  end
  else if FIdentifier <> nil then
    FIdentifier.value := '';
end;

Procedure TFhirConformance.SetVersion(value : TFhirString);
begin
  FVersion.free;
  FVersion := value;
end;

Function TFhirConformance.GetVersionST : String;
begin
  if FVersion = nil then
    result := ''
  else
    result := Version.value;
end;

Procedure TFhirConformance.SetVersionST(value : String);
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

Procedure TFhirConformance.SetName(value : TFhirString);
begin
  FName.free;
  FName := value;
end;

Function TFhirConformance.GetNameST : String;
begin
  if FName = nil then
    result := ''
  else
    result := Name.value;
end;

Procedure TFhirConformance.SetNameST(value : String);
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

Procedure TFhirConformance.SetPublisher(value : TFhirString);
begin
  FPublisher.free;
  FPublisher := value;
end;

Function TFhirConformance.GetPublisherST : String;
begin
  if FPublisher = nil then
    result := ''
  else
    result := Publisher.value;
end;

Procedure TFhirConformance.SetPublisherST(value : String);
begin
  if value <> '' then
begin
    if FPublisher = nil then
      FPublisher := TFhirString.create;
    FPublisher.value := value
  end
  else if FPublisher <> nil then
    FPublisher.value := '';
end;

Procedure TFhirConformance.SetDescription(value : TFhirString);
begin
  FDescription.free;
  FDescription := value;
end;

Function TFhirConformance.GetDescriptionST : String;
begin
  if FDescription = nil then
    result := ''
  else
    result := Description.value;
end;

Procedure TFhirConformance.SetDescriptionST(value : String);
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

Procedure TFhirConformance.SetStatus(value : TFhirEnum);
begin
  FStatus.free;
  FStatus := value;
end;

Function TFhirConformance.GetStatusST : TFhirConformanceStatementStatus;
begin
  if FStatus = nil then
    result := TFhirConformanceStatementStatus(0)
  else
    result := TFhirConformanceStatementStatus(StringArrayIndexOf(CODES_TFhirConformanceStatementStatus, Status.value));
end;

Procedure TFhirConformance.SetStatusST(value : TFhirConformanceStatementStatus);
begin
  if ord(value) = 0 then
    Status := nil
  else
    Status := TFhirEnum.create(CODES_TFhirConformanceStatementStatus[value]);
end;

Procedure TFhirConformance.SetExperimental(value : TFhirBoolean);
begin
  FExperimental.free;
  FExperimental := value;
end;

Function TFhirConformance.GetExperimentalST : Boolean;
begin
  if FExperimental = nil then
    result := false
  else
    result := Experimental.value;
end;

Procedure TFhirConformance.SetExperimentalST(value : Boolean);
begin
  if FExperimental = nil then
    FExperimental := TFhirBoolean.create;
  FExperimental.value := value
end;

Procedure TFhirConformance.SetDate(value : TFhirDateTime);
begin
  FDate.free;
  FDate := value;
end;

Function TFhirConformance.GetDateST : TDateAndTime;
begin
  if FDate = nil then
    result := nil
  else
    result := Date.value;
end;

Procedure TFhirConformance.SetDateST(value : TDateAndTime);
begin
  if value <> nil then
begin
    if FDate = nil then
      FDate := TFhirDateTime.create;
    FDate.value := value
  end
  else if FDate <> nil then
    FDate.value := nil;
end;

Procedure TFhirConformance.SetSoftware(value : TFhirConformanceSoftware);
begin
  FSoftware.free;
  FSoftware := value;
end;

Procedure TFhirConformance.SetImplementation_(value : TFhirConformanceImplementation);
begin
  FImplementation_.free;
  FImplementation_ := value;
end;

Procedure TFhirConformance.SetFhirVersion(value : TFhirId);
begin
  FFhirVersion.free;
  FFhirVersion := value;
end;

Function TFhirConformance.GetFhirVersionST : String;
begin
  if FFhirVersion = nil then
    result := ''
  else
    result := FhirVersion.value;
end;

Procedure TFhirConformance.SetFhirVersionST(value : String);
begin
  if value <> '' then
  begin
    if FFhirVersion = nil then
      FFhirVersion := TFhirId.create;
    FFhirVersion.value := value
  end
  else if FFhirVersion <> nil then
    FFhirVersion.value := '';
end;

Procedure TFhirConformance.SetAcceptUnknown(value : TFhirBoolean);
begin
  FAcceptUnknown.free;
  FAcceptUnknown := value;
end;

Function TFhirConformance.GetAcceptUnknownST : Boolean;
begin
  if FAcceptUnknown = nil then
    result := false
  else
    result := AcceptUnknown.value;
end;

Procedure TFhirConformance.SetAcceptUnknownST(value : Boolean);
begin
  if FAcceptUnknown = nil then
    FAcceptUnknown := TFhirBoolean.create;
  FAcceptUnknown.value := value
end;


{ TFhirContraindication }

constructor TFhirContraindication.Create;
begin
  inherited;
  FImplicatedList := TFhirResourceReferenceList{Resource}.Create;
  FMitigationList := TFhirContraindicationMitigationList.Create;
end;

destructor TFhirContraindication.Destroy;
begin
  FPatient.free;
  FCategory.free;
  FSeverity.free;
  FImplicatedList.Free;
  FDetail.free;
  FDate.free;
  FAuthor.free;
  FIdentifier.free;
  FReference.free;
  FMitigationList.Free;
  inherited;
end;

function TFhirContraindication.GetResourceType : TFhirResourceType;
begin
  result := frtContraindication;
end;

function TFhirContraindication.GetHasASummary : Boolean;
begin
  result := true;
end;

procedure TFhirContraindication.Assign(oSource : TAdvObject);
begin
  inherited;
  patient := TFhirContraindication(oSource).patient.Clone;
  category := TFhirContraindication(oSource).category.Clone;
  severity := TFhirContraindication(oSource).severity.Clone;
  FImplicatedList.Assign(TFhirContraindication(oSource).FImplicatedList);
  detail := TFhirContraindication(oSource).detail.Clone;
  date := TFhirContraindication(oSource).date.Clone;
  author := TFhirContraindication(oSource).author.Clone;
  identifier := TFhirContraindication(oSource).identifier.Clone;
  reference := TFhirContraindication(oSource).reference.Clone;
  FMitigationList.Assign(TFhirContraindication(oSource).FMitigationList);
end;

procedure TFhirContraindication.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'patient') Then
     list.add(Patient.Link);
  if (child_name = 'category') Then
     list.add(Category.Link);
  if (child_name = 'severity') Then
     list.add(Severity.Link);
  if (child_name = 'implicated') Then
     list.addAll(FImplicatedList);
  if (child_name = 'detail') Then
     list.add(Detail.Link);
  if (child_name = 'date') Then
     list.add(Date.Link);
  if (child_name = 'author') Then
     list.add(Author.Link);
  if (child_name = 'identifier') Then
     list.add(Identifier.Link);
  if (child_name = 'reference') Then
     list.add(Reference.Link);
  if (child_name = 'mitigation') Then
     list.addAll(FMitigationList);
end;

procedure TFhirContraindication.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'patient', 'Resource(Patient)', FPatient.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'category', 'CodeableConcept', FCategory.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'severity', 'code', FSeverity.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'implicated', 'Resource(Any)', FImplicatedList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'detail', 'string', FDetail.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'date', 'dateTime', FDate.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'author', 'Resource(Practitioner|Device)', FAuthor.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifier.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'reference', 'uri', FReference.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'mitigation', '', FMitigationList.Link)){3};
end;

function TFhirContraindication.Link : TFhirContraindication;
begin
  result := TFhirContraindication(inherited Link);
end;

function TFhirContraindication.Clone : TFhirContraindication;
begin
  result := TFhirContraindication(inherited Clone);
end;

{ TFhirContraindication }

Procedure TFhirContraindication.SetPatient(value : TFhirResourceReference{TFhirPatient});
begin
  FPatient.free;
  FPatient := value;
end;

Procedure TFhirContraindication.SetCategory(value : TFhirCodeableConcept);
begin
  FCategory.free;
  FCategory := value;
end;

Procedure TFhirContraindication.SetSeverity(value : TFhirCode);
begin
  FSeverity.free;
  FSeverity := value;
end;

Function TFhirContraindication.GetSeverityST : String;
begin
  if FSeverity = nil then
    result := ''
  else
    result := Severity.value;
end;

Procedure TFhirContraindication.SetSeverityST(value : String);
begin
  if value <> '' then
  begin
    if FSeverity = nil then
      FSeverity := TFhirCode.create;
    FSeverity.value := value
  end
  else if FSeverity <> nil then
    FSeverity.value := '';
end;

Procedure TFhirContraindication.SetDetail(value : TFhirString);
begin
  FDetail.free;
  FDetail := value;
end;

Function TFhirContraindication.GetDetailST : String;
begin
  if FDetail = nil then
    result := ''
  else
    result := Detail.value;
end;

Procedure TFhirContraindication.SetDetailST(value : String);
begin
  if value <> '' then
  begin
    if FDetail = nil then
      FDetail := TFhirString.create;
    FDetail.value := value
  end
  else if FDetail <> nil then
    FDetail.value := '';
end;

Procedure TFhirContraindication.SetDate(value : TFhirDateTime);
begin
  FDate.free;
  FDate := value;
end;

Function TFhirContraindication.GetDateST : TDateAndTime;
begin
  if FDate = nil then
    result := nil
  else
    result := Date.value;
end;

Procedure TFhirContraindication.SetDateST(value : TDateAndTime);
begin
  if value <> nil then
  begin
    if FDate = nil then
      FDate := TFhirDateTime.create;
    FDate.value := value
  end
  else if FDate <> nil then
    FDate.value := nil;
end;

Procedure TFhirContraindication.SetAuthor(value : TFhirResourceReference{Resource});
begin
  FAuthor.free;
  FAuthor := value;
end;

Procedure TFhirContraindication.SetIdentifier(value : TFhirIdentifier);
begin
  FIdentifier.free;
  FIdentifier := value;
end;

Procedure TFhirContraindication.SetReference(value : TFhirUri);
begin
  FReference.free;
  FReference := value;
end;

Function TFhirContraindication.GetReferenceST : String;
begin
  if FReference = nil then
    result := ''
  else
    result := Reference.value;
end;

Procedure TFhirContraindication.SetReferenceST(value : String);
begin
  if value <> '' then
  begin
    if FReference = nil then
      FReference := TFhirUri.create;
    FReference.value := value
  end
  else if FReference <> nil then
    FReference.value := '';
end;


{ TFhirDataElement }

constructor TFhirDataElement.Create;
begin
  inherited;
  FTelecomList := TFhirContactList.Create;
  FCategoryList := TFhirCodeableConceptList.Create;
  FCodeList := TFhirCodingList.Create;
  FSynonymList := TFhirStringList.Create;
  FMappingList := TFhirDataElementMappingList.Create;
end;

destructor TFhirDataElement.Destroy;
begin
  FIdentifier.free;
  FVersion.free;
  FPublisher.free;
  FTelecomList.Free;
  FStatus.free;
  FDate.free;
  FName.free;
  FCategoryList.Free;
  FCodeList.Free;
  FQuestion.free;
  FDefinition.free;
  FComments.free;
  FRequirements.free;
  FSynonymList.Free;
  FType_.free;
  FExample.free;
  FMaxLength.free;
  FUnits.free;
  FBinding.free;
  FMappingList.Free;
  inherited;
end;

function TFhirDataElement.GetResourceType : TFhirResourceType;
begin
  result := frtDataElement;
end;

function TFhirDataElement.GetHasASummary : Boolean;
begin
  result := true;
end;

procedure TFhirDataElement.Assign(oSource : TAdvObject);
begin
  inherited;
  identifier := TFhirDataElement(oSource).identifier.Clone;
  version := TFhirDataElement(oSource).version.Clone;
  publisher := TFhirDataElement(oSource).publisher.Clone;
  FTelecomList.Assign(TFhirDataElement(oSource).FTelecomList);
  FStatus := TFhirDataElement(oSource).FStatus.Link;
  date := TFhirDataElement(oSource).date.Clone;
  name := TFhirDataElement(oSource).name.Clone;
  FCategoryList.Assign(TFhirDataElement(oSource).FCategoryList);
  FCodeList.Assign(TFhirDataElement(oSource).FCodeList);
  question := TFhirDataElement(oSource).question.Clone;
  definition := TFhirDataElement(oSource).definition.Clone;
  comments := TFhirDataElement(oSource).comments.Clone;
  requirements := TFhirDataElement(oSource).requirements.Clone;
  FSynonymList.Assign(TFhirDataElement(oSource).FSynonymList);
  type_ := TFhirDataElement(oSource).type_.Clone;
  example := TFhirDataElement(oSource).example.Clone;
  maxLength := TFhirDataElement(oSource).maxLength.Clone;
  units := TFhirDataElement(oSource).units.Clone;
  binding := TFhirDataElement(oSource).binding.Clone;
  FMappingList.Assign(TFhirDataElement(oSource).FMappingList);
end;

procedure TFhirDataElement.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'identifier') Then
     list.add(Identifier.Link);
  if (child_name = 'version') Then
     list.add(Version.Link);
  if (child_name = 'publisher') Then
     list.add(Publisher.Link);
  if (child_name = 'telecom') Then
     list.addAll(FTelecomList);
  if (child_name = 'status') Then
     list.add(FStatus.Link);
  if (child_name = 'date') Then
     list.add(Date.Link);
  if (child_name = 'name') Then
     list.add(Name.Link);
  if (child_name = 'category') Then
     list.addAll(FCategoryList);
  if (child_name = 'code') Then
     list.addAll(FCodeList);
  if (child_name = 'question') Then
     list.add(Question.Link);
  if (child_name = 'definition') Then
     list.add(Definition.Link);
  if (child_name = 'comments') Then
     list.add(Comments.Link);
  if (child_name = 'requirements') Then
     list.add(Requirements.Link);
  if (child_name = 'synonym') Then
     list.addAll(FSynonymList);
  if (child_name = 'type_') Then
     list.add(Type_.Link);
  if (child_name = 'example') Then
     list.add(Example.Link);
  if (child_name = 'maxLength') Then
     list.add(MaxLength.Link);
  if (child_name = 'units') Then
     list.add(Units.Link);
  if (child_name = 'binding') Then
     list.add(Binding.Link);
  if (child_name = 'mapping') Then
     list.addAll(FMappingList);
end;

procedure TFhirDataElement.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifier.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'version', 'string', FVersion.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'publisher', 'string', FPublisher.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'telecom', 'Contact', FTelecomList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'status', 'code', FStatus.Link));{1}
  oList.add(TFHIRProperty.create(self, 'date', 'dateTime', FDate.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'name', 'string', FName.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'category', 'CodeableConcept', FCategoryList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'code', 'Coding', FCodeList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'question', 'string', FQuestion.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'definition', 'string', FDefinition.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'comments', 'string', FComments.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'requirements', 'string', FRequirements.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'synonym', 'string', FSynonymList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'type', 'code', FType_.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'example[x]', '*', FExample.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'maxLength', 'integer', FMaxLength.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'units', 'CodeableConcept', FUnits.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'binding', '', FBinding.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'mapping', '', FMappingList.Link)){3};
end;

function TFhirDataElement.Link : TFhirDataElement;
begin
  result := TFhirDataElement(inherited Link);
end;

function TFhirDataElement.Clone : TFhirDataElement;
begin
  result := TFhirDataElement(inherited Clone);
end;

{ TFhirDataElement }

Procedure TFhirDataElement.SetIdentifier(value : TFhirIdentifier);
begin
  FIdentifier.free;
  FIdentifier := value;
end;

Procedure TFhirDataElement.SetVersion(value : TFhirString);
begin
  FVersion.free;
  FVersion := value;
end;

Function TFhirDataElement.GetVersionST : String;
begin
  if FVersion = nil then
    result := ''
  else
    result := Version.value;
end;

Procedure TFhirDataElement.SetVersionST(value : String);
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

Procedure TFhirDataElement.SetPublisher(value : TFhirString);
begin
  FPublisher.free;
  FPublisher := value;
end;

Function TFhirDataElement.GetPublisherST : String;
begin
  if FPublisher = nil then
    result := ''
  else
    result := Publisher.value;
end;

Procedure TFhirDataElement.SetPublisherST(value : String);
begin
  if value <> '' then
  begin
    if FPublisher = nil then
      FPublisher := TFhirString.create;
    FPublisher.value := value
  end
  else if FPublisher <> nil then
    FPublisher.value := '';
end;

Procedure TFhirDataElement.SetStatus(value : TFhirEnum);
begin
  FStatus.free;
  FStatus := value;
end;

Function TFhirDataElement.GetStatusST : TFhirResourceObservationDefStatus;
begin
  if FStatus = nil then
    result := TFhirResourceObservationDefStatus(0)
  else
    result := TFhirResourceObservationDefStatus(StringArrayIndexOf(CODES_TFhirResourceObservationDefStatus, Status.value));
end;

Procedure TFhirDataElement.SetStatusST(value : TFhirResourceObservationDefStatus);
begin
  if ord(value) = 0 then
    Status := nil
  else
    Status := TFhirEnum.create(CODES_TFhirResourceObservationDefStatus[value]);
end;

Procedure TFhirDataElement.SetDate(value : TFhirDateTime);
begin
  FDate.free;
  FDate := value;
end;

Function TFhirDataElement.GetDateST : TDateAndTime;
begin
  if FDate = nil then
    result := nil
  else
    result := Date.value;
end;

Procedure TFhirDataElement.SetDateST(value : TDateAndTime);
begin
  if value <> nil then
begin
    if FDate = nil then
      FDate := TFhirDateTime.create;
    FDate.value := value
  end
  else if FDate <> nil then
    FDate.value := nil;
end;

Procedure TFhirDataElement.SetName(value : TFhirString);
begin
  FName.free;
  FName := value;
end;

Function TFhirDataElement.GetNameST : String;
begin
  if FName = nil then
    result := ''
  else
    result := Name.value;
end;

Procedure TFhirDataElement.SetNameST(value : String);
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

Procedure TFhirDataElement.SetQuestion(value : TFhirString);
begin
  FQuestion.free;
  FQuestion := value;
end;

Function TFhirDataElement.GetQuestionST : String;
begin
  if FQuestion = nil then
    result := ''
  else
    result := Question.value;
end;

Procedure TFhirDataElement.SetQuestionST(value : String);
begin
  if value <> '' then
  begin
    if FQuestion = nil then
      FQuestion := TFhirString.create;
    FQuestion.value := value
  end
  else if FQuestion <> nil then
    FQuestion.value := '';
end;

Procedure TFhirDataElement.SetDefinition(value : TFhirString);
begin
  FDefinition.free;
  FDefinition := value;
end;

Function TFhirDataElement.GetDefinitionST : String;
begin
  if FDefinition = nil then
    result := ''
  else
    result := Definition.value;
end;

Procedure TFhirDataElement.SetDefinitionST(value : String);
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

Procedure TFhirDataElement.SetComments(value : TFhirString);
begin
  FComments.free;
  FComments := value;
end;

Function TFhirDataElement.GetCommentsST : String;
begin
  if FComments = nil then
    result := ''
  else
    result := Comments.value;
end;

Procedure TFhirDataElement.SetCommentsST(value : String);
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

Procedure TFhirDataElement.SetRequirements(value : TFhirString);
begin
  FRequirements.free;
  FRequirements := value;
end;

Function TFhirDataElement.GetRequirementsST : String;
begin
  if FRequirements = nil then
    result := ''
  else
    result := Requirements.value;
end;

Procedure TFhirDataElement.SetRequirementsST(value : String);
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

Procedure TFhirDataElement.SetType_(value : TFhirCode);
begin
  FType_.free;
  FType_ := value;
end;

Function TFhirDataElement.GetType_ST : String;
begin
  if FType_ = nil then
    result := ''
  else
    result := Type_.value;
end;

Procedure TFhirDataElement.SetType_ST(value : String);
begin
  if value <> '' then
  begin
    if FType_ = nil then
      FType_ := TFhirCode.create;
    FType_.value := value
  end
  else if FType_ <> nil then
    FType_.value := '';
end;

Procedure TFhirDataElement.SetExample(value : TFhirType);
begin
  FExample.free;
  FExample := value;
end;

Procedure TFhirDataElement.SetMaxLength(value : TFhirInteger);
begin
  FMaxLength.free;
  FMaxLength := value;
end;

Function TFhirDataElement.GetMaxLengthST : String;
begin
  if FMaxLength = nil then
    result := ''
  else
    result := MaxLength.value;
end;

Procedure TFhirDataElement.SetMaxLengthST(value : String);
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

Procedure TFhirDataElement.SetUnits(value : TFhirCodeableConcept);
begin
  FUnits.free;
  FUnits := value;
end;

Procedure TFhirDataElement.SetBinding(value : TFhirDataElementBinding);
begin
  FBinding.free;
  FBinding := value;
end;


{ TFhirDevice }

constructor TFhirDevice.Create;
begin
  inherited;
  FIdentifierList := TFhirIdentifierList.Create;
  FContactList := TFhirContactList.Create;
end;

destructor TFhirDevice.Destroy;
begin
  FIdentifierList.Free;
  FType_.free;
  FManufacturer.free;
  FModel.free;
  FVersion.free;
  FExpiry.free;
  FUdi.free;
  FLotNumber.free;
  FOwner.free;
  FLocation.free;
  FPatient.free;
  FContactList.Free;
  FUrl.free;
  inherited;
end;

function TFhirDevice.GetResourceType : TFhirResourceType;
begin
  result := frtDevice;
end;

function TFhirDevice.GetHasASummary : Boolean;
begin
  result := false;
end;

procedure TFhirDevice.Assign(oSource : TAdvObject);
begin
  inherited;
  FIdentifierList.Assign(TFhirDevice(oSource).FIdentifierList);
  type_ := TFhirDevice(oSource).type_.Clone;
  manufacturer := TFhirDevice(oSource).manufacturer.Clone;
  model := TFhirDevice(oSource).model.Clone;
  version := TFhirDevice(oSource).version.Clone;
  expiry := TFhirDevice(oSource).expiry.Clone;
  udi := TFhirDevice(oSource).udi.Clone;
  lotNumber := TFhirDevice(oSource).lotNumber.Clone;
  owner := TFhirDevice(oSource).owner.Clone;
  location := TFhirDevice(oSource).location.Clone;
  patient := TFhirDevice(oSource).patient.Clone;
  FContactList.Assign(TFhirDevice(oSource).FContactList);
  url := TFhirDevice(oSource).url.Clone;
end;

procedure TFhirDevice.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'identifier') Then
     list.addAll(FIdentifierList);
  if (child_name = 'type_') Then
     list.add(Type_.Link);
  if (child_name = 'manufacturer') Then
     list.add(Manufacturer.Link);
  if (child_name = 'model') Then
     list.add(Model.Link);
  if (child_name = 'version') Then
     list.add(Version.Link);
  if (child_name = 'expiry') Then
     list.add(Expiry.Link);
  if (child_name = 'udi') Then
     list.add(Udi.Link);
  if (child_name = 'lotNumber') Then
     list.add(LotNumber.Link);
  if (child_name = 'owner') Then
     list.add(Owner.Link);
  if (child_name = 'location') Then
     list.add(Location.Link);
  if (child_name = 'patient') Then
     list.add(Patient.Link);
  if (child_name = 'contact') Then
     list.addAll(FContactList);
  if (child_name = 'url') Then
     list.add(Url.Link);
end;

procedure TFhirDevice.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'type', 'CodeableConcept', FType_.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'manufacturer', 'string', FManufacturer.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'model', 'string', FModel.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'version', 'string', FVersion.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'expiry', 'date', FExpiry.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'udi', 'string', FUdi.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'lotNumber', 'string', FLotNumber.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'owner', 'Resource(Organization)', FOwner.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'location', 'Resource(Location)', FLocation.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'patient', 'Resource(Patient)', FPatient.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'contact', 'Contact', FContactList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'url', 'uri', FUrl.Link.Link));{2}
end;

function TFhirDevice.Link : TFhirDevice;
begin
  result := TFhirDevice(inherited Link);
end;

function TFhirDevice.Clone : TFhirDevice;
begin
  result := TFhirDevice(inherited Clone);
end;

{ TFhirDevice }

Procedure TFhirDevice.SetType_(value : TFhirCodeableConcept);
begin
  FType_.free;
  FType_ := value;
end;

Procedure TFhirDevice.SetManufacturer(value : TFhirString);
begin
  FManufacturer.free;
  FManufacturer := value;
end;

Function TFhirDevice.GetManufacturerST : String;
begin
  if FManufacturer = nil then
    result := ''
  else
    result := Manufacturer.value;
end;

Procedure TFhirDevice.SetManufacturerST(value : String);
begin
  if value <> '' then
  begin
    if FManufacturer = nil then
      FManufacturer := TFhirString.create;
    FManufacturer.value := value
  end
  else if FManufacturer <> nil then
    FManufacturer.value := '';
end;

Procedure TFhirDevice.SetModel(value : TFhirString);
begin
  FModel.free;
  FModel := value;
end;

Function TFhirDevice.GetModelST : String;
begin
  if FModel = nil then
    result := ''
  else
    result := Model.value;
end;

Procedure TFhirDevice.SetModelST(value : String);
begin
  if value <> '' then
  begin
    if FModel = nil then
      FModel := TFhirString.create;
    FModel.value := value
  end
  else if FModel <> nil then
    FModel.value := '';
end;

Procedure TFhirDevice.SetVersion(value : TFhirString);
begin
  FVersion.free;
  FVersion := value;
end;

Function TFhirDevice.GetVersionST : String;
begin
  if FVersion = nil then
    result := ''
  else
    result := Version.value;
end;

Procedure TFhirDevice.SetVersionST(value : String);
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

Procedure TFhirDevice.SetExpiry(value : TFhirDate);
begin
  FExpiry.free;
  FExpiry := value;
end;

Function TFhirDevice.GetExpiryST : TDateAndTime;
begin
  if FExpiry = nil then
    result := nil
  else
    result := Expiry.value;
end;

Procedure TFhirDevice.SetExpiryST(value : TDateAndTime);
begin
  if value <> nil then
  begin
    if FExpiry = nil then
      FExpiry := TFhirDate.create;
    FExpiry.value := value
  end
  else if FExpiry <> nil then
    FExpiry.value := nil;
end;

Procedure TFhirDevice.SetUdi(value : TFhirString);
begin
  FUdi.free;
  FUdi := value;
end;

Function TFhirDevice.GetUdiST : String;
begin
  if FUdi = nil then
    result := ''
  else
    result := Udi.value;
end;

Procedure TFhirDevice.SetUdiST(value : String);
begin
  if value <> '' then
  begin
    if FUdi = nil then
      FUdi := TFhirString.create;
    FUdi.value := value
  end
  else if FUdi <> nil then
    FUdi.value := '';
end;

Procedure TFhirDevice.SetLotNumber(value : TFhirString);
begin
  FLotNumber.free;
  FLotNumber := value;
end;

Function TFhirDevice.GetLotNumberST : String;
begin
  if FLotNumber = nil then
    result := ''
  else
    result := LotNumber.value;
end;

Procedure TFhirDevice.SetLotNumberST(value : String);
begin
  if value <> '' then
  begin
    if FLotNumber = nil then
      FLotNumber := TFhirString.create;
    FLotNumber.value := value
  end
  else if FLotNumber <> nil then
    FLotNumber.value := '';
end;

Procedure TFhirDevice.SetOwner(value : TFhirResourceReference{TFhirOrganization});
begin
  FOwner.free;
  FOwner := value;
end;

Procedure TFhirDevice.SetLocation(value : TFhirResourceReference{TFhirLocation});
begin
  FLocation.free;
  FLocation := value;
end;

Procedure TFhirDevice.SetPatient(value : TFhirResourceReference{TFhirPatient});
begin
  FPatient.free;
  FPatient := value;
end;

Procedure TFhirDevice.SetUrl(value : TFhirUri);
begin
  FUrl.free;
  FUrl := value;
end;

Function TFhirDevice.GetUrlST : String;
begin
  if FUrl = nil then
    result := ''
  else
    result := Url.value;
end;

Procedure TFhirDevice.SetUrlST(value : String);
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


{ TFhirDeviceObservationReport }

constructor TFhirDeviceObservationReport.Create;
begin
  inherited;
  FVirtualDeviceList := TFhirDeviceObservationReportVirtualDeviceList.Create;
end;

destructor TFhirDeviceObservationReport.Destroy;
begin
  FInstant.free;
  FIdentifier.free;
  FSource.free;
  FSubject.free;
  FVirtualDeviceList.Free;
  inherited;
end;

function TFhirDeviceObservationReport.GetResourceType : TFhirResourceType;
begin
  result := frtDeviceObservationReport;
end;

function TFhirDeviceObservationReport.GetHasASummary : Boolean;
begin
  result := false;
end;

procedure TFhirDeviceObservationReport.Assign(oSource : TAdvObject);
begin
  inherited;
  instant := TFhirDeviceObservationReport(oSource).instant.Clone;
  identifier := TFhirDeviceObservationReport(oSource).identifier.Clone;
  source := TFhirDeviceObservationReport(oSource).source.Clone;
  subject := TFhirDeviceObservationReport(oSource).subject.Clone;
  FVirtualDeviceList.Assign(TFhirDeviceObservationReport(oSource).FVirtualDeviceList);
end;

procedure TFhirDeviceObservationReport.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'instant') Then
     list.add(Instant.Link);
  if (child_name = 'identifier') Then
     list.add(Identifier.Link);
  if (child_name = 'source') Then
     list.add(Source.Link);
  if (child_name = 'subject') Then
     list.add(Subject.Link);
  if (child_name = 'virtualDevice') Then
     list.addAll(FVirtualDeviceList);
end;

procedure TFhirDeviceObservationReport.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'instant', 'instant', FInstant.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifier.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'source', 'Resource(Device)', FSource.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'subject', 'Resource(Patient|Device|Location)', FSubject.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'virtualDevice', '', FVirtualDeviceList.Link)){3};
end;

function TFhirDeviceObservationReport.Link : TFhirDeviceObservationReport;
begin
  result := TFhirDeviceObservationReport(inherited Link);
end;

function TFhirDeviceObservationReport.Clone : TFhirDeviceObservationReport;
begin
  result := TFhirDeviceObservationReport(inherited Clone);
end;

{ TFhirDeviceObservationReport }

Procedure TFhirDeviceObservationReport.SetInstant(value : TFhirInstant);
begin
  FInstant.free;
  FInstant := value;
end;

Function TFhirDeviceObservationReport.GetInstantST : TDateAndTime;
begin
  if FInstant = nil then
    result := nil
  else
    result := Instant.value;
end;

Procedure TFhirDeviceObservationReport.SetInstantST(value : TDateAndTime);
begin
  if value <> nil then
  begin
    if FInstant = nil then
      FInstant := TFhirInstant.create;
    FInstant.value := value
  end
  else if FInstant <> nil then
    FInstant.value := nil;
end;

Procedure TFhirDeviceObservationReport.SetIdentifier(value : TFhirIdentifier);
begin
  FIdentifier.free;
  FIdentifier := value;
end;

Procedure TFhirDeviceObservationReport.SetSource(value : TFhirResourceReference{TFhirDevice});
begin
  FSource.free;
  FSource := value;
end;

Procedure TFhirDeviceObservationReport.SetSubject(value : TFhirResourceReference{Resource});
begin
  FSubject.free;
  FSubject := value;
end;


{ TFhirDiagnosticOrder }

constructor TFhirDiagnosticOrder.Create;
begin
  inherited;
  FIdentifierList := TFhirIdentifierList.Create;
  FSupportingInformationList := TFhirResourceReferenceList{Resource}.Create;
  FSpecimenList := TFhirResourceReferenceList{TFhirSpecimen}.Create;
  FEventList := TFhirDiagnosticOrderEventList.Create;
  FItemList := TFhirDiagnosticOrderItemList.Create;
end;

destructor TFhirDiagnosticOrder.Destroy;
begin
  FSubject.free;
  FOrderer.free;
  FIdentifierList.Free;
  FEncounter.free;
  FClinicalNotes.free;
  FSupportingInformationList.Free;
  FSpecimenList.Free;
  FStatus.free;
  FPriority.free;
  FEventList.Free;
  FItemList.Free;
  inherited;
end;

function TFhirDiagnosticOrder.GetResourceType : TFhirResourceType;
begin
  result := frtDiagnosticOrder;
end;

function TFhirDiagnosticOrder.GetHasASummary : Boolean;
begin
  result := false;
end;

procedure TFhirDiagnosticOrder.Assign(oSource : TAdvObject);
begin
  inherited;
  subject := TFhirDiagnosticOrder(oSource).subject.Clone;
  orderer := TFhirDiagnosticOrder(oSource).orderer.Clone;
  FIdentifierList.Assign(TFhirDiagnosticOrder(oSource).FIdentifierList);
  encounter := TFhirDiagnosticOrder(oSource).encounter.Clone;
  clinicalNotes := TFhirDiagnosticOrder(oSource).clinicalNotes.Clone;
  FSupportingInformationList.Assign(TFhirDiagnosticOrder(oSource).FSupportingInformationList);
  FSpecimenList.Assign(TFhirDiagnosticOrder(oSource).FSpecimenList);
  FStatus := TFhirDiagnosticOrder(oSource).FStatus.Link;
  FPriority := TFhirDiagnosticOrder(oSource).FPriority.Link;
  FEventList.Assign(TFhirDiagnosticOrder(oSource).FEventList);
  FItemList.Assign(TFhirDiagnosticOrder(oSource).FItemList);
end;

procedure TFhirDiagnosticOrder.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'subject') Then
     list.add(Subject.Link);
  if (child_name = 'orderer') Then
     list.add(Orderer.Link);
  if (child_name = 'identifier') Then
     list.addAll(FIdentifierList);
  if (child_name = 'encounter') Then
     list.add(Encounter.Link);
  if (child_name = 'clinicalNotes') Then
     list.add(ClinicalNotes.Link);
  if (child_name = 'supportingInformation') Then
     list.addAll(FSupportingInformationList);
  if (child_name = 'specimen') Then
     list.addAll(FSpecimenList);
  if (child_name = 'status') Then
     list.add(FStatus.Link);
  if (child_name = 'priority') Then
     list.add(FPriority.Link);
  if (child_name = 'event') Then
     list.addAll(FEventList);
  if (child_name = 'item') Then
     list.addAll(FItemList);
end;

procedure TFhirDiagnosticOrder.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'subject', 'Resource(Patient|Group|Location|Device)', FSubject.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'orderer', 'Resource(Practitioner)', FOrderer.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'encounter', 'Resource(Encounter)', FEncounter.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'clinicalNotes', 'string', FClinicalNotes.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'supportingInformation', 'Resource(Observation|Condition)', FSupportingInformationList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'specimen', 'Resource(Specimen)', FSpecimenList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'status', 'code', FStatus.Link));{1}
  oList.add(TFHIRProperty.create(self, 'priority', 'code', FPriority.Link));{1}
  oList.add(TFHIRProperty.create(self, 'event', '', FEventList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'item', '', FItemList.Link)){3};
end;

function TFhirDiagnosticOrder.Link : TFhirDiagnosticOrder;
begin
  result := TFhirDiagnosticOrder(inherited Link);
end;

function TFhirDiagnosticOrder.Clone : TFhirDiagnosticOrder;
begin
  result := TFhirDiagnosticOrder(inherited Clone);
end;

{ TFhirDiagnosticOrder }

Procedure TFhirDiagnosticOrder.SetSubject(value : TFhirResourceReference{Resource});
begin
  FSubject.free;
  FSubject := value;
end;

Procedure TFhirDiagnosticOrder.SetOrderer(value : TFhirResourceReference{TFhirPractitioner});
begin
  FOrderer.free;
  FOrderer := value;
end;

Procedure TFhirDiagnosticOrder.SetEncounter(value : TFhirResourceReference{TFhirEncounter});
begin
  FEncounter.free;
  FEncounter := value;
end;

Procedure TFhirDiagnosticOrder.SetClinicalNotes(value : TFhirString);
begin
  FClinicalNotes.free;
  FClinicalNotes := value;
end;

Function TFhirDiagnosticOrder.GetClinicalNotesST : String;
begin
  if FClinicalNotes = nil then
    result := ''
  else
    result := ClinicalNotes.value;
end;

Procedure TFhirDiagnosticOrder.SetClinicalNotesST(value : String);
begin
  if value <> '' then
  begin
    if FClinicalNotes = nil then
      FClinicalNotes := TFhirString.create;
    FClinicalNotes.value := value
  end
  else if FClinicalNotes <> nil then
    FClinicalNotes.value := '';
end;

Procedure TFhirDiagnosticOrder.SetStatus(value : TFhirEnum);
begin
  FStatus.free;
  FStatus := value;
end;

Function TFhirDiagnosticOrder.GetStatusST : TFhirDiagnosticOrderStatus;
begin
  if FStatus = nil then
    result := TFhirDiagnosticOrderStatus(0)
  else
    result := TFhirDiagnosticOrderStatus(StringArrayIndexOf(CODES_TFhirDiagnosticOrderStatus, Status.value));
end;

Procedure TFhirDiagnosticOrder.SetStatusST(value : TFhirDiagnosticOrderStatus);
begin
  if ord(value) = 0 then
    Status := nil
  else
    Status := TFhirEnum.create(CODES_TFhirDiagnosticOrderStatus[value]);
end;

Procedure TFhirDiagnosticOrder.SetPriority(value : TFhirEnum);
begin
  FPriority.free;
  FPriority := value;
end;

Function TFhirDiagnosticOrder.GetPriorityST : TFhirDiagnosticOrderPriority;
begin
  if FPriority = nil then
    result := TFhirDiagnosticOrderPriority(0)
  else
    result := TFhirDiagnosticOrderPriority(StringArrayIndexOf(CODES_TFhirDiagnosticOrderPriority, Priority.value));
end;

Procedure TFhirDiagnosticOrder.SetPriorityST(value : TFhirDiagnosticOrderPriority);
begin
  if ord(value) = 0 then
    Priority := nil
  else
    Priority := TFhirEnum.create(CODES_TFhirDiagnosticOrderPriority[value]);
end;


{ TFhirDiagnosticReport }

constructor TFhirDiagnosticReport.Create;
begin
  inherited;
  FRequestDetailList := TFhirResourceReferenceList{TFhirDiagnosticOrder}.Create;
  FSpecimenList := TFhirResourceReferenceList{TFhirSpecimen}.Create;
  FResultList := TFhirResourceReferenceList{TFhirObservation}.Create;
  FImagingStudyList := TFhirResourceReferenceList{TFhirImagingStudy}.Create;
  FImageList := TFhirDiagnosticReportImageList.Create;
  FCodedDiagnosisList := TFhirCodeableConceptList.Create;
  FPresentedFormList := TFhirAttachmentList.Create;
end;

destructor TFhirDiagnosticReport.Destroy;
begin
  FName.free;
  FStatus.free;
  FIssued.free;
  FSubject.free;
  FPerformer.free;
  FIdentifier.free;
  FRequestDetailList.Free;
  FServiceCategory.free;
  FDiagnostic.free;
  FSpecimenList.Free;
  FResultList.Free;
  FImagingStudyList.Free;
  FImageList.Free;
  FConclusion.free;
  FCodedDiagnosisList.Free;
  FPresentedFormList.Free;
  inherited;
end;

function TFhirDiagnosticReport.GetResourceType : TFhirResourceType;
begin
  result := frtDiagnosticReport;
end;

function TFhirDiagnosticReport.GetHasASummary : Boolean;
begin
  result := true;
end;

procedure TFhirDiagnosticReport.Assign(oSource : TAdvObject);
begin
  inherited;
  name := TFhirDiagnosticReport(oSource).name.Clone;
  FStatus := TFhirDiagnosticReport(oSource).FStatus.Link;
  issued := TFhirDiagnosticReport(oSource).issued.Clone;
  subject := TFhirDiagnosticReport(oSource).subject.Clone;
  performer := TFhirDiagnosticReport(oSource).performer.Clone;
  identifier := TFhirDiagnosticReport(oSource).identifier.Clone;
  FRequestDetailList.Assign(TFhirDiagnosticReport(oSource).FRequestDetailList);
  serviceCategory := TFhirDiagnosticReport(oSource).serviceCategory.Clone;
  diagnostic := TFhirDiagnosticReport(oSource).diagnostic.Clone;
  FSpecimenList.Assign(TFhirDiagnosticReport(oSource).FSpecimenList);
  FResultList.Assign(TFhirDiagnosticReport(oSource).FResultList);
  FImagingStudyList.Assign(TFhirDiagnosticReport(oSource).FImagingStudyList);
  FImageList.Assign(TFhirDiagnosticReport(oSource).FImageList);
  conclusion := TFhirDiagnosticReport(oSource).conclusion.Clone;
  FCodedDiagnosisList.Assign(TFhirDiagnosticReport(oSource).FCodedDiagnosisList);
  FPresentedFormList.Assign(TFhirDiagnosticReport(oSource).FPresentedFormList);
end;

procedure TFhirDiagnosticReport.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'name') Then
     list.add(Name.Link);
  if (child_name = 'status') Then
     list.add(FStatus.Link);
  if (child_name = 'issued') Then
     list.add(Issued.Link);
  if (child_name = 'subject') Then
     list.add(Subject.Link);
  if (child_name = 'performer') Then
     list.add(Performer.Link);
  if (child_name = 'identifier') Then
     list.add(Identifier.Link);
  if (child_name = 'requestDetail') Then
     list.addAll(FRequestDetailList);
  if (child_name = 'serviceCategory') Then
     list.add(ServiceCategory.Link);
  if (child_name = 'diagnostic') Then
     list.add(Diagnostic.Link);
  if (child_name = 'specimen') Then
     list.addAll(FSpecimenList);
  if (child_name = 'result') Then
     list.addAll(FResultList);
  if (child_name = 'imagingStudy') Then
     list.addAll(FImagingStudyList);
  if (child_name = 'image') Then
     list.addAll(FImageList);
  if (child_name = 'conclusion') Then
     list.add(Conclusion.Link);
  if (child_name = 'codedDiagnosis') Then
     list.addAll(FCodedDiagnosisList);
  if (child_name = 'presentedForm') Then
     list.addAll(FPresentedFormList);
end;

procedure TFhirDiagnosticReport.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'name', 'CodeableConcept', FName.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'status', 'code', FStatus.Link));{1}
  oList.add(TFHIRProperty.create(self, 'issued', 'dateTime', FIssued.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'subject', 'Resource(Patient|Group|Device|Location)', FSubject.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'performer', 'Resource(Practitioner|Organization)', FPerformer.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifier.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'requestDetail', 'Resource(DiagnosticOrder)', FRequestDetailList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'serviceCategory', 'CodeableConcept', FServiceCategory.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'diagnostic[x]', 'dateTime|Period', FDiagnostic.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'specimen', 'Resource(Specimen)', FSpecimenList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'result', 'Resource(Observation)', FResultList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'imagingStudy', 'Resource(ImagingStudy)', FImagingStudyList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'image', '', FImageList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'conclusion', 'string', FConclusion.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'codedDiagnosis', 'CodeableConcept', FCodedDiagnosisList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'presentedForm', 'Attachment', FPresentedFormList.Link)){3};
end;

function TFhirDiagnosticReport.Link : TFhirDiagnosticReport;
begin
  result := TFhirDiagnosticReport(inherited Link);
end;

function TFhirDiagnosticReport.Clone : TFhirDiagnosticReport;
begin
  result := TFhirDiagnosticReport(inherited Clone);
end;

{ TFhirDiagnosticReport }

Procedure TFhirDiagnosticReport.SetName(value : TFhirCodeableConcept);
begin
  FName.free;
  FName := value;
end;

Procedure TFhirDiagnosticReport.SetStatus(value : TFhirEnum);
begin
  FStatus.free;
  FStatus := value;
end;

Function TFhirDiagnosticReport.GetStatusST : TFhirDiagnosticReportStatus;
begin
  if FStatus = nil then
    result := TFhirDiagnosticReportStatus(0)
  else
    result := TFhirDiagnosticReportStatus(StringArrayIndexOf(CODES_TFhirDiagnosticReportStatus, Status.value));
end;

Procedure TFhirDiagnosticReport.SetStatusST(value : TFhirDiagnosticReportStatus);
begin
  if ord(value) = 0 then
    Status := nil
  else
    Status := TFhirEnum.create(CODES_TFhirDiagnosticReportStatus[value]);
end;

Procedure TFhirDiagnosticReport.SetIssued(value : TFhirDateTime);
begin
  FIssued.free;
  FIssued := value;
end;

Function TFhirDiagnosticReport.GetIssuedST : TDateAndTime;
begin
  if FIssued = nil then
    result := nil
  else
    result := Issued.value;
end;

Procedure TFhirDiagnosticReport.SetIssuedST(value : TDateAndTime);
begin
  if value <> nil then
  begin
    if FIssued = nil then
      FIssued := TFhirDateTime.create;
    FIssued.value := value
  end
  else if FIssued <> nil then
    FIssued.value := nil;
end;

Procedure TFhirDiagnosticReport.SetSubject(value : TFhirResourceReference{Resource});
begin
  FSubject.free;
  FSubject := value;
end;

Procedure TFhirDiagnosticReport.SetPerformer(value : TFhirResourceReference{Resource});
begin
  FPerformer.free;
  FPerformer := value;
end;

Procedure TFhirDiagnosticReport.SetIdentifier(value : TFhirIdentifier);
begin
  FIdentifier.free;
  FIdentifier := value;
end;

Procedure TFhirDiagnosticReport.SetServiceCategory(value : TFhirCodeableConcept);
begin
  FServiceCategory.free;
  FServiceCategory := value;
end;

Procedure TFhirDiagnosticReport.SetDiagnostic(value : TFhirType);
begin
  FDiagnostic.free;
  FDiagnostic := value;
end;

Procedure TFhirDiagnosticReport.SetConclusion(value : TFhirString);
begin
  FConclusion.free;
  FConclusion := value;
end;

Function TFhirDiagnosticReport.GetConclusionST : String;
begin
  if FConclusion = nil then
    result := ''
  else
    result := Conclusion.value;
end;

Procedure TFhirDiagnosticReport.SetConclusionST(value : String);
begin
  if value <> '' then
  begin
    if FConclusion = nil then
      FConclusion := TFhirString.create;
    FConclusion.value := value
  end
  else if FConclusion <> nil then
    FConclusion.value := '';
end;


{ TFhirDocumentManifest }

constructor TFhirDocumentManifest.Create;
begin
  inherited;
  FIdentifierList := TFhirIdentifierList.Create;
  FSubjectList := TFhirResourceReferenceList{Resource}.Create;
  FRecipientList := TFhirResourceReferenceList{Resource}.Create;
  FAuthorList := TFhirResourceReferenceList{Resource}.Create;
  FContentList := TFhirResourceReferenceList{Resource}.Create;
end;

destructor TFhirDocumentManifest.Destroy;
begin
  FMasterIdentifier.free;
  FIdentifierList.Free;
  FSubjectList.Free;
  FRecipientList.Free;
  FType_.free;
  FAuthorList.Free;
  FCreated.free;
  FSource.free;
  FStatus.free;
  FSupercedes.free;
  FDescription.free;
  FConfidentiality.free;
  FContentList.Free;
  inherited;
end;

function TFhirDocumentManifest.GetResourceType : TFhirResourceType;
begin
  result := frtDocumentManifest;
end;

function TFhirDocumentManifest.GetHasASummary : Boolean;
begin
  result := false;
end;

procedure TFhirDocumentManifest.Assign(oSource : TAdvObject);
begin
  inherited;
  masterIdentifier := TFhirDocumentManifest(oSource).masterIdentifier.Clone;
  FIdentifierList.Assign(TFhirDocumentManifest(oSource).FIdentifierList);
  FSubjectList.Assign(TFhirDocumentManifest(oSource).FSubjectList);
  FRecipientList.Assign(TFhirDocumentManifest(oSource).FRecipientList);
  type_ := TFhirDocumentManifest(oSource).type_.Clone;
  FAuthorList.Assign(TFhirDocumentManifest(oSource).FAuthorList);
  created := TFhirDocumentManifest(oSource).created.Clone;
  source := TFhirDocumentManifest(oSource).source.Clone;
  FStatus := TFhirDocumentManifest(oSource).FStatus.Link;
  supercedes := TFhirDocumentManifest(oSource).supercedes.Clone;
  description := TFhirDocumentManifest(oSource).description.Clone;
  confidentiality := TFhirDocumentManifest(oSource).confidentiality.Clone;
  FContentList.Assign(TFhirDocumentManifest(oSource).FContentList);
end;

procedure TFhirDocumentManifest.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'masterIdentifier') Then
     list.add(MasterIdentifier.Link);
  if (child_name = 'identifier') Then
     list.addAll(FIdentifierList);
  if (child_name = 'subject') Then
     list.addAll(FSubjectList);
  if (child_name = 'recipient') Then
     list.addAll(FRecipientList);
  if (child_name = 'type_') Then
     list.add(Type_.Link);
  if (child_name = 'author') Then
     list.addAll(FAuthorList);
  if (child_name = 'created') Then
     list.add(Created.Link);
  if (child_name = 'source') Then
     list.add(Source.Link);
  if (child_name = 'status') Then
     list.add(FStatus.Link);
  if (child_name = 'supercedes') Then
     list.add(Supercedes.Link);
  if (child_name = 'description') Then
     list.add(Description.Link);
  if (child_name = 'confidentiality') Then
     list.add(Confidentiality.Link);
  if (child_name = 'content') Then
     list.addAll(FContentList);
end;

procedure TFhirDocumentManifest.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'masterIdentifier', 'Identifier', FMasterIdentifier.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'subject', 'Resource(Patient|Practitioner|Group|Device)', FSubjectList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'recipient', 'Resource(Patient|Practitioner|Organization)', FRecipientList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'type', 'CodeableConcept', FType_.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'author', 'Resource(Practitioner|Device|Patient|RelatedPerson)', FAuthorList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'created', 'dateTime', FCreated.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'source', 'uri', FSource.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'status', 'code', FStatus.Link));{1}
  oList.add(TFHIRProperty.create(self, 'supercedes', 'Resource(DocumentManifest)', FSupercedes.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'description', 'string', FDescription.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'confidentiality', 'CodeableConcept', FConfidentiality.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'content', 'Resource(DocumentReference|Binary|Media)', FContentList.Link)){3};
end;

function TFhirDocumentManifest.Link : TFhirDocumentManifest;
begin
  result := TFhirDocumentManifest(inherited Link);
end;

function TFhirDocumentManifest.Clone : TFhirDocumentManifest;
begin
  result := TFhirDocumentManifest(inherited Clone);
end;

{ TFhirDocumentManifest }

Procedure TFhirDocumentManifest.SetMasterIdentifier(value : TFhirIdentifier);
begin
  FMasterIdentifier.free;
  FMasterIdentifier := value;
end;

Procedure TFhirDocumentManifest.SetType_(value : TFhirCodeableConcept);
begin
  FType_.free;
  FType_ := value;
end;

Procedure TFhirDocumentManifest.SetCreated(value : TFhirDateTime);
begin
  FCreated.free;
  FCreated := value;
end;

Function TFhirDocumentManifest.GetCreatedST : TDateAndTime;
begin
  if FCreated = nil then
    result := nil
  else
    result := Created.value;
end;

Procedure TFhirDocumentManifest.SetCreatedST(value : TDateAndTime);
begin
  if value <> nil then
  begin
    if FCreated = nil then
      FCreated := TFhirDateTime.create;
    FCreated.value := value
  end
  else if FCreated <> nil then
    FCreated.value := nil;
end;

Procedure TFhirDocumentManifest.SetSource(value : TFhirUri);
begin
  FSource.free;
  FSource := value;
end;

Function TFhirDocumentManifest.GetSourceST : String;
begin
  if FSource = nil then
    result := ''
  else
    result := Source.value;
end;

Procedure TFhirDocumentManifest.SetSourceST(value : String);
begin
  if value <> '' then
  begin
    if FSource = nil then
      FSource := TFhirUri.create;
    FSource.value := value
  end
  else if FSource <> nil then
    FSource.value := '';
end;

Procedure TFhirDocumentManifest.SetStatus(value : TFhirEnum);
begin
  FStatus.free;
  FStatus := value;
end;

Function TFhirDocumentManifest.GetStatusST : TFhirDocumentReferenceStatus;
begin
  if FStatus = nil then
    result := TFhirDocumentReferenceStatus(0)
  else
    result := TFhirDocumentReferenceStatus(StringArrayIndexOf(CODES_TFhirDocumentReferenceStatus, Status.value));
end;

Procedure TFhirDocumentManifest.SetStatusST(value : TFhirDocumentReferenceStatus);
begin
  if ord(value) = 0 then
    Status := nil
  else
    Status := TFhirEnum.create(CODES_TFhirDocumentReferenceStatus[value]);
end;

Procedure TFhirDocumentManifest.SetSupercedes(value : TFhirResourceReference{TFhirDocumentManifest});
begin
  FSupercedes.free;
  FSupercedes := value;
end;

Procedure TFhirDocumentManifest.SetDescription(value : TFhirString);
begin
  FDescription.free;
  FDescription := value;
end;

Function TFhirDocumentManifest.GetDescriptionST : String;
begin
  if FDescription = nil then
    result := ''
  else
    result := Description.value;
end;

Procedure TFhirDocumentManifest.SetDescriptionST(value : String);
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

Procedure TFhirDocumentManifest.SetConfidentiality(value : TFhirCodeableConcept);
begin
  FConfidentiality.free;
  FConfidentiality := value;
end;


{ TFhirDocumentReference }

constructor TFhirDocumentReference.Create;
begin
  inherited;
  FIdentifierList := TFhirIdentifierList.Create;
  FAuthorList := TFhirResourceReferenceList{Resource}.Create;
  FRelatesToList := TFhirDocumentReferenceRelatesToList.Create;
  FConfidentialityList := TFhirCodeableConceptList.Create;
  FFormatList := TFhirUriList.Create;
end;

destructor TFhirDocumentReference.Destroy;
begin
  FMasterIdentifier.free;
  FIdentifierList.Free;
  FSubject.free;
  FType_.free;
  FClass_.free;
  FAuthorList.Free;
  FCustodian.free;
  FPolicyManager.free;
  FAuthenticator.free;
  FCreated.free;
  FIndexed.free;
  FStatus.free;
  FDocStatus.free;
  FRelatesToList.Free;
  FDescription.free;
  FConfidentialityList.Free;
  FPrimaryLanguage.free;
  FMimeType.free;
  FFormatList.Free;
  FSize.free;
  FHash.free;
  FLocation.free;
  FService.free;
  FContext.free;
  inherited;
end;

function TFhirDocumentReference.GetResourceType : TFhirResourceType;
begin
  result := frtDocumentReference;
end;

function TFhirDocumentReference.GetHasASummary : Boolean;
begin
  result := false;
end;

procedure TFhirDocumentReference.Assign(oSource : TAdvObject);
begin
  inherited;
  masterIdentifier := TFhirDocumentReference(oSource).masterIdentifier.Clone;
  FIdentifierList.Assign(TFhirDocumentReference(oSource).FIdentifierList);
  subject := TFhirDocumentReference(oSource).subject.Clone;
  type_ := TFhirDocumentReference(oSource).type_.Clone;
  class_ := TFhirDocumentReference(oSource).class_.Clone;
  FAuthorList.Assign(TFhirDocumentReference(oSource).FAuthorList);
  custodian := TFhirDocumentReference(oSource).custodian.Clone;
  policyManager := TFhirDocumentReference(oSource).policyManager.Clone;
  authenticator := TFhirDocumentReference(oSource).authenticator.Clone;
  created := TFhirDocumentReference(oSource).created.Clone;
  indexed := TFhirDocumentReference(oSource).indexed.Clone;
  FStatus := TFhirDocumentReference(oSource).FStatus.Link;
  docStatus := TFhirDocumentReference(oSource).docStatus.Clone;
  FRelatesToList.Assign(TFhirDocumentReference(oSource).FRelatesToList);
  description := TFhirDocumentReference(oSource).description.Clone;
  FConfidentialityList.Assign(TFhirDocumentReference(oSource).FConfidentialityList);
  primaryLanguage := TFhirDocumentReference(oSource).primaryLanguage.Clone;
  mimeType := TFhirDocumentReference(oSource).mimeType.Clone;
  FFormatList.Assign(TFhirDocumentReference(oSource).FFormatList);
  size := TFhirDocumentReference(oSource).size.Clone;
  hash := TFhirDocumentReference(oSource).hash.Clone;
  location := TFhirDocumentReference(oSource).location.Clone;
  service := TFhirDocumentReference(oSource).service.Clone;
  context := TFhirDocumentReference(oSource).context.Clone;
end;

procedure TFhirDocumentReference.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'masterIdentifier') Then
     list.add(MasterIdentifier.Link);
  if (child_name = 'identifier') Then
     list.addAll(FIdentifierList);
  if (child_name = 'subject') Then
     list.add(Subject.Link);
  if (child_name = 'type_') Then
     list.add(Type_.Link);
  if (child_name = 'class_') Then
     list.add(Class_.Link);
  if (child_name = 'author') Then
     list.addAll(FAuthorList);
  if (child_name = 'custodian') Then
     list.add(Custodian.Link);
  if (child_name = 'policyManager') Then
     list.add(PolicyManager.Link);
  if (child_name = 'authenticator') Then
     list.add(Authenticator.Link);
  if (child_name = 'created') Then
     list.add(Created.Link);
  if (child_name = 'indexed') Then
     list.add(Indexed.Link);
  if (child_name = 'status') Then
     list.add(FStatus.Link);
  if (child_name = 'docStatus') Then
     list.add(DocStatus.Link);
  if (child_name = 'relatesTo') Then
     list.addAll(FRelatesToList);
  if (child_name = 'description') Then
     list.add(Description.Link);
  if (child_name = 'confidentiality') Then
     list.addAll(FConfidentialityList);
  if (child_name = 'primaryLanguage') Then
     list.add(PrimaryLanguage.Link);
  if (child_name = 'mimeType') Then
     list.add(MimeType.Link);
  if (child_name = 'format') Then
     list.addAll(FFormatList);
  if (child_name = 'size') Then
     list.add(Size.Link);
  if (child_name = 'hash') Then
     list.add(Hash.Link);
  if (child_name = 'location') Then
     list.add(Location.Link);
  if (child_name = 'service') Then
     list.add(Service.Link);
  if (child_name = 'context') Then
     list.add(Context.Link);
end;

procedure TFhirDocumentReference.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'masterIdentifier', 'Identifier', FMasterIdentifier.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'subject', 'Resource(Patient|Practitioner|Group|Device)', FSubject.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'type', 'CodeableConcept', FType_.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'class', 'CodeableConcept', FClass_.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'author', 'Resource(Practitioner|Device|Patient|RelatedPerson)', FAuthorList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'custodian', 'Resource(Organization)', FCustodian.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'policyManager', 'uri', FPolicyManager.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'authenticator', 'Resource(Practitioner|Organization)', FAuthenticator.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'created', 'dateTime', FCreated.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'indexed', 'instant', FIndexed.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'status', 'code', FStatus.Link));{1}
  oList.add(TFHIRProperty.create(self, 'docStatus', 'CodeableConcept', FDocStatus.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'relatesTo', '', FRelatesToList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'description', 'string', FDescription.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'confidentiality', 'CodeableConcept', FConfidentialityList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'primaryLanguage', 'code', FPrimaryLanguage.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'mimeType', 'code', FMimeType.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'format', 'uri', FFormatList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'size', 'integer', FSize.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'hash', 'string', FHash.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'location', 'uri', FLocation.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'service', '', FService.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'context', '', FContext.Link.Link));{2}
end;

function TFhirDocumentReference.Link : TFhirDocumentReference;
begin
  result := TFhirDocumentReference(inherited Link);
end;

function TFhirDocumentReference.Clone : TFhirDocumentReference;
begin
  result := TFhirDocumentReference(inherited Clone);
end;

{ TFhirDocumentReference }

Procedure TFhirDocumentReference.SetMasterIdentifier(value : TFhirIdentifier);
begin
  FMasterIdentifier.free;
  FMasterIdentifier := value;
end;

Procedure TFhirDocumentReference.SetSubject(value : TFhirResourceReference{Resource});
begin
  FSubject.free;
  FSubject := value;
end;

Procedure TFhirDocumentReference.SetType_(value : TFhirCodeableConcept);
begin
  FType_.free;
  FType_ := value;
end;

Procedure TFhirDocumentReference.SetClass_(value : TFhirCodeableConcept);
begin
  FClass_.free;
  FClass_ := value;
end;

Procedure TFhirDocumentReference.SetCustodian(value : TFhirResourceReference{TFhirOrganization});
begin
  FCustodian.free;
  FCustodian := value;
end;

Procedure TFhirDocumentReference.SetPolicyManager(value : TFhirUri);
begin
  FPolicyManager.free;
  FPolicyManager := value;
end;

Function TFhirDocumentReference.GetPolicyManagerST : String;
begin
  if FPolicyManager = nil then
    result := ''
  else
    result := PolicyManager.value;
end;

Procedure TFhirDocumentReference.SetPolicyManagerST(value : String);
begin
  if value <> '' then
  begin
    if FPolicyManager = nil then
      FPolicyManager := TFhirUri.create;
    FPolicyManager.value := value
  end
  else if FPolicyManager <> nil then
    FPolicyManager.value := '';
end;

Procedure TFhirDocumentReference.SetAuthenticator(value : TFhirResourceReference{Resource});
begin
  FAuthenticator.free;
  FAuthenticator := value;
end;

Procedure TFhirDocumentReference.SetCreated(value : TFhirDateTime);
begin
  FCreated.free;
  FCreated := value;
end;

Function TFhirDocumentReference.GetCreatedST : TDateAndTime;
begin
  if FCreated = nil then
    result := nil
  else
    result := Created.value;
end;

Procedure TFhirDocumentReference.SetCreatedST(value : TDateAndTime);
begin
  if value <> nil then
  begin
    if FCreated = nil then
      FCreated := TFhirDateTime.create;
    FCreated.value := value
  end
  else if FCreated <> nil then
    FCreated.value := nil;
end;

Procedure TFhirDocumentReference.SetIndexed(value : TFhirInstant);
begin
  FIndexed.free;
  FIndexed := value;
end;

Function TFhirDocumentReference.GetIndexedST : TDateAndTime;
begin
  if FIndexed = nil then
    result := nil
  else
    result := Indexed.value;
end;

Procedure TFhirDocumentReference.SetIndexedST(value : TDateAndTime);
begin
  if value <> nil then
  begin
    if FIndexed = nil then
      FIndexed := TFhirInstant.create;
    FIndexed.value := value
  end
  else if FIndexed <> nil then
    FIndexed.value := nil;
end;

Procedure TFhirDocumentReference.SetStatus(value : TFhirEnum);
begin
  FStatus.free;
  FStatus := value;
end;

Function TFhirDocumentReference.GetStatusST : TFhirDocumentReferenceStatus;
begin
  if FStatus = nil then
    result := TFhirDocumentReferenceStatus(0)
  else
    result := TFhirDocumentReferenceStatus(StringArrayIndexOf(CODES_TFhirDocumentReferenceStatus, Status.value));
end;

Procedure TFhirDocumentReference.SetStatusST(value : TFhirDocumentReferenceStatus);
begin
  if ord(value) = 0 then
    Status := nil
  else
    Status := TFhirEnum.create(CODES_TFhirDocumentReferenceStatus[value]);
end;

Procedure TFhirDocumentReference.SetDocStatus(value : TFhirCodeableConcept);
begin
  FDocStatus.free;
  FDocStatus := value;
end;

Procedure TFhirDocumentReference.SetDescription(value : TFhirString);
begin
  FDescription.free;
  FDescription := value;
end;

Function TFhirDocumentReference.GetDescriptionST : String;
begin
  if FDescription = nil then
    result := ''
  else
    result := Description.value;
end;

Procedure TFhirDocumentReference.SetDescriptionST(value : String);
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

Procedure TFhirDocumentReference.SetPrimaryLanguage(value : TFhirCode);
begin
  FPrimaryLanguage.free;
  FPrimaryLanguage := value;
end;

Function TFhirDocumentReference.GetPrimaryLanguageST : String;
begin
  if FPrimaryLanguage = nil then
    result := ''
  else
    result := PrimaryLanguage.value;
end;

Procedure TFhirDocumentReference.SetPrimaryLanguageST(value : String);
begin
  if value <> '' then
  begin
    if FPrimaryLanguage = nil then
      FPrimaryLanguage := TFhirCode.create;
    FPrimaryLanguage.value := value
  end
  else if FPrimaryLanguage <> nil then
    FPrimaryLanguage.value := '';
end;

Procedure TFhirDocumentReference.SetMimeType(value : TFhirCode);
begin
  FMimeType.free;
  FMimeType := value;
end;

Function TFhirDocumentReference.GetMimeTypeST : String;
begin
  if FMimeType = nil then
    result := ''
  else
    result := MimeType.value;
end;

Procedure TFhirDocumentReference.SetMimeTypeST(value : String);
begin
  if value <> '' then
  begin
    if FMimeType = nil then
      FMimeType := TFhirCode.create;
    FMimeType.value := value
  end
  else if FMimeType <> nil then
    FMimeType.value := '';
end;

Procedure TFhirDocumentReference.SetSize(value : TFhirInteger);
begin
  FSize.free;
  FSize := value;
end;

Function TFhirDocumentReference.GetSizeST : String;
begin
  if FSize = nil then
    result := ''
  else
    result := Size.value;
end;

Procedure TFhirDocumentReference.SetSizeST(value : String);
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

Procedure TFhirDocumentReference.SetHash(value : TFhirString);
begin
  FHash.free;
  FHash := value;
end;

Function TFhirDocumentReference.GetHashST : String;
begin
  if FHash = nil then
    result := ''
  else
    result := Hash.value;
end;

Procedure TFhirDocumentReference.SetHashST(value : String);
begin
  if value <> '' then
  begin
    if FHash = nil then
      FHash := TFhirString.create;
    FHash.value := value
  end
  else if FHash <> nil then
    FHash.value := '';
end;

Procedure TFhirDocumentReference.SetLocation(value : TFhirUri);
begin
  FLocation.free;
  FLocation := value;
end;

Function TFhirDocumentReference.GetLocationST : String;
begin
  if FLocation = nil then
    result := ''
  else
    result := Location.value;
end;

Procedure TFhirDocumentReference.SetLocationST(value : String);
begin
  if value <> '' then
  begin
    if FLocation = nil then
      FLocation := TFhirUri.create;
    FLocation.value := value
  end
  else if FLocation <> nil then
    FLocation.value := '';
end;

Procedure TFhirDocumentReference.SetService(value : TFhirDocumentReferenceService);
begin
  FService.free;
  FService := value;
end;

Procedure TFhirDocumentReference.SetContext(value : TFhirDocumentReferenceContext);
begin
  FContext.free;
  FContext := value;
end;


{ TFhirEncounter }

constructor TFhirEncounter.Create;
begin
  inherited;
  FIdentifierList := TFhirIdentifierList.Create;
  FType_List := TFhirCodeableConceptList.Create;
  FParticipantList := TFhirEncounterParticipantList.Create;
  FLocationList := TFhirEncounterLocationList.Create;
end;

destructor TFhirEncounter.Destroy;
begin
  FIdentifierList.Free;
  FStatus.free;
  FClass_.free;
  FType_List.Free;
  FSubject.free;
  FParticipantList.Free;
  FFulfills.free;
  FPeriod.free;
  FLength.free;
  FReason.free;
  FIndication.free;
  FPriority.free;
  FHospitalization.free;
  FLocationList.Free;
  FServiceProvider.free;
  FPartOf.free;
  inherited;
end;

function TFhirEncounter.GetResourceType : TFhirResourceType;
begin
  result := frtEncounter;
end;

function TFhirEncounter.GetHasASummary : Boolean;
begin
  result := true;
end;

procedure TFhirEncounter.Assign(oSource : TAdvObject);
begin
  inherited;
  FIdentifierList.Assign(TFhirEncounter(oSource).FIdentifierList);
  FStatus := TFhirEncounter(oSource).FStatus.Link;
  FClass_ := TFhirEncounter(oSource).FClass_.Link;
  FType_List.Assign(TFhirEncounter(oSource).FType_List);
  subject := TFhirEncounter(oSource).subject.Clone;
  FParticipantList.Assign(TFhirEncounter(oSource).FParticipantList);
  fulfills := TFhirEncounter(oSource).fulfills.Clone;
  period := TFhirEncounter(oSource).period.Clone;
  length := TFhirEncounter(oSource).length.Clone;
  reason := TFhirEncounter(oSource).reason.Clone;
  indication := TFhirEncounter(oSource).indication.Clone;
  priority := TFhirEncounter(oSource).priority.Clone;
  hospitalization := TFhirEncounter(oSource).hospitalization.Clone;
  FLocationList.Assign(TFhirEncounter(oSource).FLocationList);
  serviceProvider := TFhirEncounter(oSource).serviceProvider.Clone;
  partOf := TFhirEncounter(oSource).partOf.Clone;
end;

procedure TFhirEncounter.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'identifier') Then
     list.addAll(FIdentifierList);
  if (child_name = 'status') Then
     list.add(FStatus.Link);
  if (child_name = 'class_') Then
     list.add(FClass_.Link);
  if (child_name = 'type_') Then
     list.addAll(FType_List);
  if (child_name = 'subject') Then
     list.add(Subject.Link);
  if (child_name = 'participant') Then
     list.addAll(FParticipantList);
  if (child_name = 'fulfills') Then
     list.add(Fulfills.Link);
  if (child_name = 'period') Then
     list.add(Period.Link);
  if (child_name = 'length') Then
     list.add(Length.Link);
  if (child_name = 'reason') Then
     list.add(Reason.Link);
  if (child_name = 'indication') Then
     list.add(Indication.Link);
  if (child_name = 'priority') Then
     list.add(Priority.Link);
  if (child_name = 'hospitalization') Then
     list.add(Hospitalization.Link);
  if (child_name = 'location') Then
     list.addAll(FLocationList);
  if (child_name = 'serviceProvider') Then
     list.add(ServiceProvider.Link);
  if (child_name = 'partOf') Then
     list.add(PartOf.Link);
end;

procedure TFhirEncounter.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'status', 'code', FStatus.Link));{1}
  oList.add(TFHIRProperty.create(self, 'class', 'code', FClass_.Link));{1}
  oList.add(TFHIRProperty.create(self, 'type', 'CodeableConcept', FType_List.Link)){3};
  oList.add(TFHIRProperty.create(self, 'subject', 'Resource(Patient)', FSubject.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'participant', '', FParticipantList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'fulfills', 'Resource(Appointment)', FFulfills.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'period', 'Period', FPeriod.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'length', 'Duration', FLength.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'reason', 'CodeableConcept', FReason.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'indication', 'Resource(Any)', FIndication.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'priority', 'CodeableConcept', FPriority.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'hospitalization', '', FHospitalization.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'location', '', FLocationList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'serviceProvider', 'Resource(Organization)', FServiceProvider.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'partOf', 'Resource(Encounter)', FPartOf.Link.Link));{2}
end;

function TFhirEncounter.Link : TFhirEncounter;
begin
  result := TFhirEncounter(inherited Link);
end;

function TFhirEncounter.Clone : TFhirEncounter;
begin
  result := TFhirEncounter(inherited Clone);
end;

{ TFhirEncounter }

Procedure TFhirEncounter.SetStatus(value : TFhirEnum);
begin
  FStatus.free;
  FStatus := value;
end;

Function TFhirEncounter.GetStatusST : TFhirEncounterState;
begin
  if FStatus = nil then
    result := TFhirEncounterState(0)
  else
    result := TFhirEncounterState(StringArrayIndexOf(CODES_TFhirEncounterState, Status.value));
end;

Procedure TFhirEncounter.SetStatusST(value : TFhirEncounterState);
begin
  if ord(value) = 0 then
    Status := nil
  else
    Status := TFhirEnum.create(CODES_TFhirEncounterState[value]);
end;

Procedure TFhirEncounter.SetClass_(value : TFhirEnum);
begin
  FClass_.free;
  FClass_ := value;
end;

Function TFhirEncounter.GetClass_ST : TFhirEncounterClass;
begin
  if FClass_ = nil then
    result := TFhirEncounterClass(0)
  else
    result := TFhirEncounterClass(StringArrayIndexOf(CODES_TFhirEncounterClass, Class_.value));
end;

Procedure TFhirEncounter.SetClass_ST(value : TFhirEncounterClass);
begin
  if ord(value) = 0 then
    Class_ := nil
  else
    Class_ := TFhirEnum.create(CODES_TFhirEncounterClass[value]);
end;

Procedure TFhirEncounter.SetSubject(value : TFhirResourceReference{TFhirPatient});
begin
  FSubject.free;
  FSubject := value;
end;

Procedure TFhirEncounter.SetFulfills(value : TFhirResourceReference{TFhirAppointment});
begin
  FFulfills.free;
  FFulfills := value;
end;

Procedure TFhirEncounter.SetPeriod(value : TFhirPeriod);
begin
  FPeriod.free;
  FPeriod := value;
end;

Procedure TFhirEncounter.SetLength(value : TFhirQuantity);
begin
  FLength.free;
  FLength := value;
end;

Procedure TFhirEncounter.SetReason(value : TFhirCodeableConcept);
begin
  FReason.free;
  FReason := value;
end;

Procedure TFhirEncounter.SetIndication(value : TFhirResourceReference{Resource});
begin
  FIndication.free;
  FIndication := value;
end;

Procedure TFhirEncounter.SetPriority(value : TFhirCodeableConcept);
begin
  FPriority.free;
  FPriority := value;
end;

Procedure TFhirEncounter.SetHospitalization(value : TFhirEncounterHospitalization);
begin
  FHospitalization.free;
  FHospitalization := value;
end;

Procedure TFhirEncounter.SetServiceProvider(value : TFhirResourceReference{TFhirOrganization});
begin
  FServiceProvider.free;
  FServiceProvider := value;
end;

Procedure TFhirEncounter.SetPartOf(value : TFhirResourceReference{TFhirEncounter});
begin
  FPartOf.free;
  FPartOf := value;
end;


{ TFhirFamilyHistory }

constructor TFhirFamilyHistory.Create;
begin
  inherited;
  FIdentifierList := TFhirIdentifierList.Create;
  FRelationList := TFhirFamilyHistoryRelationList.Create;
end;

destructor TFhirFamilyHistory.Destroy;
begin
  FIdentifierList.Free;
  FSubject.free;
  FDate.free;
  FNote.free;
  FRelationList.Free;
  inherited;
end;

function TFhirFamilyHistory.GetResourceType : TFhirResourceType;
begin
  result := frtFamilyHistory;
end;

function TFhirFamilyHistory.GetHasASummary : Boolean;
begin
  result := true;
end;

procedure TFhirFamilyHistory.Assign(oSource : TAdvObject);
begin
  inherited;
  FIdentifierList.Assign(TFhirFamilyHistory(oSource).FIdentifierList);
  subject := TFhirFamilyHistory(oSource).subject.Clone;
  date := TFhirFamilyHistory(oSource).date.Clone;
  note := TFhirFamilyHistory(oSource).note.Clone;
  FRelationList.Assign(TFhirFamilyHistory(oSource).FRelationList);
end;

procedure TFhirFamilyHistory.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'identifier') Then
     list.addAll(FIdentifierList);
  if (child_name = 'subject') Then
     list.add(Subject.Link);
  if (child_name = 'date') Then
     list.add(Date.Link);
  if (child_name = 'note') Then
     list.add(Note.Link);
  if (child_name = 'relation') Then
     list.addAll(FRelationList);
end;

procedure TFhirFamilyHistory.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'subject', 'Resource(Patient)', FSubject.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'date', 'dateTime', FDate.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'note', 'string', FNote.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'relation', '', FRelationList.Link)){3};
end;

function TFhirFamilyHistory.Link : TFhirFamilyHistory;
begin
  result := TFhirFamilyHistory(inherited Link);
end;

function TFhirFamilyHistory.Clone : TFhirFamilyHistory;
begin
  result := TFhirFamilyHistory(inherited Clone);
end;

{ TFhirFamilyHistory }

Procedure TFhirFamilyHistory.SetSubject(value : TFhirResourceReference{TFhirPatient});
begin
  FSubject.free;
  FSubject := value;
end;

Procedure TFhirFamilyHistory.SetDate(value : TFhirDateTime);
begin
  FDate.free;
  FDate := value;
end;

Function TFhirFamilyHistory.GetDateST : TDateAndTime;
begin
  if FDate = nil then
    result := nil
  else
    result := Date.value;
end;

Procedure TFhirFamilyHistory.SetDateST(value : TDateAndTime);
begin
  if value <> nil then
  begin
    if FDate = nil then
      FDate := TFhirDateTime.create;
    FDate.value := value
  end
  else if FDate <> nil then
    FDate.value := nil;
end;

Procedure TFhirFamilyHistory.SetNote(value : TFhirString);
begin
  FNote.free;
  FNote := value;
end;

Function TFhirFamilyHistory.GetNoteST : String;
begin
  if FNote = nil then
    result := ''
  else
    result := Note.value;
end;

Procedure TFhirFamilyHistory.SetNoteST(value : String);
begin
  if value <> '' then
  begin
    if FNote = nil then
      FNote := TFhirString.create;
    FNote.value := value
  end
  else if FNote <> nil then
    FNote.value := '';
end;


{ TFhirGroup }

constructor TFhirGroup.Create;
begin
  inherited;
  FCharacteristicList := TFhirGroupCharacteristicList.Create;
  FMemberList := TFhirResourceReferenceList{Resource}.Create;
end;

destructor TFhirGroup.Destroy;
begin
  FIdentifier.free;
  FType_.free;
  FActual.free;
  FCode.free;
  FName.free;
  FQuantity.free;
  FCharacteristicList.Free;
  FMemberList.Free;
  inherited;
end;

function TFhirGroup.GetResourceType : TFhirResourceType;
begin
  result := frtGroup;
end;

function TFhirGroup.GetHasASummary : Boolean;
begin
  result := true;
end;

procedure TFhirGroup.Assign(oSource : TAdvObject);
begin
  inherited;
  identifier := TFhirGroup(oSource).identifier.Clone;
  FType_ := TFhirGroup(oSource).FType_.Link;
  actual := TFhirGroup(oSource).actual.Clone;
  code := TFhirGroup(oSource).code.Clone;
  name := TFhirGroup(oSource).name.Clone;
  quantity := TFhirGroup(oSource).quantity.Clone;
  FCharacteristicList.Assign(TFhirGroup(oSource).FCharacteristicList);
  FMemberList.Assign(TFhirGroup(oSource).FMemberList);
end;

procedure TFhirGroup.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'identifier') Then
     list.add(Identifier.Link);
  if (child_name = 'type_') Then
     list.add(FType_.Link);
  if (child_name = 'actual') Then
     list.add(Actual.Link);
  if (child_name = 'code') Then
     list.add(Code.Link);
  if (child_name = 'name') Then
     list.add(Name.Link);
  if (child_name = 'quantity') Then
     list.add(Quantity.Link);
  if (child_name = 'characteristic') Then
     list.addAll(FCharacteristicList);
  if (child_name = 'member') Then
     list.addAll(FMemberList);
end;

procedure TFhirGroup.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifier.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'type', 'code', FType_.Link));{1}
  oList.add(TFHIRProperty.create(self, 'actual', 'boolean', FActual.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'code', 'CodeableConcept', FCode.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'name', 'string', FName.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'quantity', 'integer', FQuantity.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'characteristic', '', FCharacteristicList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'member', 'Resource(Patient|Practitioner|Device|Medication|Substance)', FMemberList.Link)){3};
end;

function TFhirGroup.Link : TFhirGroup;
begin
  result := TFhirGroup(inherited Link);
end;

function TFhirGroup.Clone : TFhirGroup;
begin
  result := TFhirGroup(inherited Clone);
end;

{ TFhirGroup }

Procedure TFhirGroup.SetIdentifier(value : TFhirIdentifier);
begin
  FIdentifier.free;
  FIdentifier := value;
end;

Procedure TFhirGroup.SetType_(value : TFhirEnum);
begin
  FType_.free;
  FType_ := value;
end;

Function TFhirGroup.GetType_ST : TFhirGroupType;
begin
  if FType_ = nil then
    result := TFhirGroupType(0)
  else
    result := TFhirGroupType(StringArrayIndexOf(CODES_TFhirGroupType, Type_.value));
end;

Procedure TFhirGroup.SetType_ST(value : TFhirGroupType);
begin
  if ord(value) = 0 then
    Type_ := nil
  else
    Type_ := TFhirEnum.create(CODES_TFhirGroupType[value]);
end;

Procedure TFhirGroup.SetActual(value : TFhirBoolean);
begin
  FActual.free;
  FActual := value;
end;

Function TFhirGroup.GetActualST : Boolean;
begin
  if FActual = nil then
    result := false
  else
    result := Actual.value;
end;

Procedure TFhirGroup.SetActualST(value : Boolean);
begin
  if FActual = nil then
    FActual := TFhirBoolean.create;
  FActual.value := value
end;

Procedure TFhirGroup.SetCode(value : TFhirCodeableConcept);
begin
  FCode.free;
  FCode := value;
end;

Procedure TFhirGroup.SetName(value : TFhirString);
begin
  FName.free;
  FName := value;
end;

Function TFhirGroup.GetNameST : String;
begin
  if FName = nil then
    result := ''
  else
    result := Name.value;
end;

Procedure TFhirGroup.SetNameST(value : String);
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

Procedure TFhirGroup.SetQuantity(value : TFhirInteger);
begin
  FQuantity.free;
  FQuantity := value;
end;

Function TFhirGroup.GetQuantityST : String;
begin
  if FQuantity = nil then
    result := ''
  else
    result := Quantity.value;
end;

Procedure TFhirGroup.SetQuantityST(value : String);
begin
  if value <> '' then
  begin
    if FQuantity = nil then
      FQuantity := TFhirInteger.create;
    FQuantity.value := value
  end
  else if FQuantity <> nil then
    FQuantity.value := '';
end;


{ TFhirImagingStudy }

constructor TFhirImagingStudy.Create;
begin
  inherited;
  FIdentifierList := TFhirIdentifierList.Create;
  FOrderList := TFhirResourceReferenceList{TFhirDiagnosticOrder}.Create;
  FModality := TFHIREnumList.Create;
  FProcedure_List := TFhirCodingList.Create;
  FSeriesList := TFhirImagingStudySeriesList.Create;
end;

destructor TFhirImagingStudy.Destroy;
begin
  FDateTime.free;
  FSubject.free;
  FUid.free;
  FAccessionNo.free;
  FIdentifierList.Free;
  FOrderList.Free;
  FModality.Free;
  FReferrer.free;
  FAvailability.free;
  FUrl.free;
  FNumberOfSeries.free;
  FNumberOfInstances.free;
  FClinicalInformation.free;
  FProcedure_List.Free;
  FInterpreter.free;
  FDescription.free;
  FSeriesList.Free;
  inherited;
end;

function TFhirImagingStudy.GetResourceType : TFhirResourceType;
begin
  result := frtImagingStudy;
end;

function TFhirImagingStudy.GetHasASummary : Boolean;
begin
  result := false;
end;

procedure TFhirImagingStudy.Assign(oSource : TAdvObject);
begin
  inherited;
  dateTime := TFhirImagingStudy(oSource).dateTime.Clone;
  subject := TFhirImagingStudy(oSource).subject.Clone;
  uid := TFhirImagingStudy(oSource).uid.Clone;
  accessionNo := TFhirImagingStudy(oSource).accessionNo.Clone;
  FIdentifierList.Assign(TFhirImagingStudy(oSource).FIdentifierList);
  FOrderList.Assign(TFhirImagingStudy(oSource).FOrderList);
  FModality.Assign(TFhirImagingStudy(oSource).FModality);
  referrer := TFhirImagingStudy(oSource).referrer.Clone;
  FAvailability := TFhirImagingStudy(oSource).FAvailability.Link;
  url := TFhirImagingStudy(oSource).url.Clone;
  numberOfSeries := TFhirImagingStudy(oSource).numberOfSeries.Clone;
  numberOfInstances := TFhirImagingStudy(oSource).numberOfInstances.Clone;
  clinicalInformation := TFhirImagingStudy(oSource).clinicalInformation.Clone;
  FProcedure_List.Assign(TFhirImagingStudy(oSource).FProcedure_List);
  interpreter := TFhirImagingStudy(oSource).interpreter.Clone;
  description := TFhirImagingStudy(oSource).description.Clone;
  FSeriesList.Assign(TFhirImagingStudy(oSource).FSeriesList);
end;

procedure TFhirImagingStudy.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'dateTime') Then
     list.add(DateTime.Link);
  if (child_name = 'subject') Then
     list.add(Subject.Link);
  if (child_name = 'uid') Then
     list.add(Uid.Link);
  if (child_name = 'accessionNo') Then
     list.add(AccessionNo.Link);
  if (child_name = 'identifier') Then
     list.addAll(FIdentifierList);
  if (child_name = 'order') Then
     list.addAll(FOrderList);
  if (child_name = 'modality') Then
     list.addAll(FModality);
  if (child_name = 'referrer') Then
     list.add(Referrer.Link);
  if (child_name = 'availability') Then
     list.add(FAvailability.Link);
  if (child_name = 'url') Then
     list.add(Url.Link);
  if (child_name = 'numberOfSeries') Then
     list.add(NumberOfSeries.Link);
  if (child_name = 'numberOfInstances') Then
     list.add(NumberOfInstances.Link);
  if (child_name = 'clinicalInformation') Then
     list.add(ClinicalInformation.Link);
  if (child_name = 'procedure_') Then
     list.addAll(FProcedure_List);
  if (child_name = 'interpreter') Then
     list.add(Interpreter.Link);
  if (child_name = 'description') Then
     list.add(Description.Link);
  if (child_name = 'series') Then
     list.addAll(FSeriesList);
end;

procedure TFhirImagingStudy.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'dateTime', 'dateTime', FDateTime.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'subject', 'Resource(Patient)', FSubject.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'uid', 'oid', FUid.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'accessionNo', 'Identifier', FAccessionNo.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'order', 'Resource(DiagnosticOrder)', FOrderList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'modality', 'code', FModality.Link)){3};
  oList.add(TFHIRProperty.create(self, 'referrer', 'Resource(Practitioner)', FReferrer.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'availability', 'code', FAvailability.Link));{1}
  oList.add(TFHIRProperty.create(self, 'url', 'uri', FUrl.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'numberOfSeries', 'integer', FNumberOfSeries.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'numberOfInstances', 'integer', FNumberOfInstances.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'clinicalInformation', 'string', FClinicalInformation.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'procedure', 'Coding', FProcedure_List.Link)){3};
  oList.add(TFHIRProperty.create(self, 'interpreter', 'Resource(Practitioner)', FInterpreter.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'description', 'string', FDescription.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'series', '', FSeriesList.Link)){3};
end;

function TFhirImagingStudy.Link : TFhirImagingStudy;
begin
  result := TFhirImagingStudy(inherited Link);
end;

function TFhirImagingStudy.Clone : TFhirImagingStudy;
begin
  result := TFhirImagingStudy(inherited Clone);
end;

{ TFhirImagingStudy }

Procedure TFhirImagingStudy.SetDateTime(value : TFhirDateTime);
begin
  FDateTime.free;
  FDateTime := value;
end;

Function TFhirImagingStudy.GetDateTimeST : TDateAndTime;
begin
  if FDateTime = nil then
    result := nil
  else
    result := DateTime.value;
end;

Procedure TFhirImagingStudy.SetDateTimeST(value : TDateAndTime);
begin
  if value <> nil then
  begin
    if FDateTime = nil then
      FDateTime := TFhirDateTime.create;
    FDateTime.value := value
  end
  else if FDateTime <> nil then
    FDateTime.value := nil;
end;

Procedure TFhirImagingStudy.SetSubject(value : TFhirResourceReference{TFhirPatient});
begin
  FSubject.free;
  FSubject := value;
end;

Procedure TFhirImagingStudy.SetUid(value : TFhirOid);
begin
  FUid.free;
  FUid := value;
end;

Function TFhirImagingStudy.GetUidST : String;
begin
  if FUid = nil then
    result := ''
  else
    result := Uid.value;
end;

Procedure TFhirImagingStudy.SetUidST(value : String);
begin
  if value <> '' then
  begin
    if FUid = nil then
      FUid := TFhirOid.create;
    FUid.value := value
  end
  else if FUid <> nil then
    FUid.value := '';
end;

Procedure TFhirImagingStudy.SetAccessionNo(value : TFhirIdentifier);
begin
  FAccessionNo.free;
  FAccessionNo := value;
end;

Procedure TFhirImagingStudy.SetReferrer(value : TFhirResourceReference{TFhirPractitioner});
begin
  FReferrer.free;
  FReferrer := value;
end;

Procedure TFhirImagingStudy.SetAvailability(value : TFhirEnum);
begin
  FAvailability.free;
  FAvailability := value;
end;

Function TFhirImagingStudy.GetAvailabilityST : TFhirInstanceAvailability;
begin
  if FAvailability = nil then
    result := TFhirInstanceAvailability(0)
  else
    result := TFhirInstanceAvailability(StringArrayIndexOf(CODES_TFhirInstanceAvailability, Availability.value));
end;

Procedure TFhirImagingStudy.SetAvailabilityST(value : TFhirInstanceAvailability);
begin
  if ord(value) = 0 then
    Availability := nil
  else
    Availability := TFhirEnum.create(CODES_TFhirInstanceAvailability[value]);
end;

Procedure TFhirImagingStudy.SetUrl(value : TFhirUri);
begin
  FUrl.free;
  FUrl := value;
end;

Function TFhirImagingStudy.GetUrlST : String;
begin
  if FUrl = nil then
    result := ''
  else
    result := Url.value;
end;

Procedure TFhirImagingStudy.SetUrlST(value : String);
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

Procedure TFhirImagingStudy.SetNumberOfSeries(value : TFhirInteger);
begin
  FNumberOfSeries.free;
  FNumberOfSeries := value;
end;

Function TFhirImagingStudy.GetNumberOfSeriesST : String;
begin
  if FNumberOfSeries = nil then
    result := ''
  else
    result := NumberOfSeries.value;
end;

Procedure TFhirImagingStudy.SetNumberOfSeriesST(value : String);
begin
  if value <> '' then
  begin
    if FNumberOfSeries = nil then
      FNumberOfSeries := TFhirInteger.create;
    FNumberOfSeries.value := value
  end
  else if FNumberOfSeries <> nil then
    FNumberOfSeries.value := '';
end;

Procedure TFhirImagingStudy.SetNumberOfInstances(value : TFhirInteger);
begin
  FNumberOfInstances.free;
  FNumberOfInstances := value;
end;

Function TFhirImagingStudy.GetNumberOfInstancesST : String;
begin
  if FNumberOfInstances = nil then
    result := ''
  else
    result := NumberOfInstances.value;
end;

Procedure TFhirImagingStudy.SetNumberOfInstancesST(value : String);
begin
  if value <> '' then
  begin
    if FNumberOfInstances = nil then
      FNumberOfInstances := TFhirInteger.create;
    FNumberOfInstances.value := value
  end
  else if FNumberOfInstances <> nil then
    FNumberOfInstances.value := '';
end;

Procedure TFhirImagingStudy.SetClinicalInformation(value : TFhirString);
begin
  FClinicalInformation.free;
  FClinicalInformation := value;
end;

Function TFhirImagingStudy.GetClinicalInformationST : String;
begin
  if FClinicalInformation = nil then
    result := ''
  else
    result := ClinicalInformation.value;
end;

Procedure TFhirImagingStudy.SetClinicalInformationST(value : String);
begin
  if value <> '' then
  begin
    if FClinicalInformation = nil then
      FClinicalInformation := TFhirString.create;
    FClinicalInformation.value := value
  end
  else if FClinicalInformation <> nil then
    FClinicalInformation.value := '';
end;

Procedure TFhirImagingStudy.SetInterpreter(value : TFhirResourceReference{TFhirPractitioner});
begin
  FInterpreter.free;
  FInterpreter := value;
end;

Procedure TFhirImagingStudy.SetDescription(value : TFhirString);
begin
  FDescription.free;
  FDescription := value;
end;

Function TFhirImagingStudy.GetDescriptionST : String;
begin
  if FDescription = nil then
    result := ''
  else
    result := Description.value;
end;

Procedure TFhirImagingStudy.SetDescriptionST(value : String);
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


{ TFhirImmunization }

constructor TFhirImmunization.Create;
begin
  inherited;
  FIdentifierList := TFhirIdentifierList.Create;
  FReactionList := TFhirImmunizationReactionList.Create;
  FVaccinationProtocolList := TFhirImmunizationVaccinationProtocolList.Create;
end;

destructor TFhirImmunization.Destroy;
begin
  FIdentifierList.Free;
  FDate.free;
  FVaccineType.free;
  FSubject.free;
  FRefusedIndicator.free;
  FReported.free;
  FPerformer.free;
  FRequester.free;
  FManufacturer.free;
  FLocation.free;
  FLotNumber.free;
  FExpirationDate.free;
  FSite.free;
  FRoute.free;
  FDoseQuantity.free;
  FExplanation.free;
  FReactionList.Free;
  FVaccinationProtocolList.Free;
  inherited;
end;

function TFhirImmunization.GetResourceType : TFhirResourceType;
begin
  result := frtImmunization;
end;

function TFhirImmunization.GetHasASummary : Boolean;
begin
  result := false;
end;

procedure TFhirImmunization.Assign(oSource : TAdvObject);
begin
  inherited;
  FIdentifierList.Assign(TFhirImmunization(oSource).FIdentifierList);
  date := TFhirImmunization(oSource).date.Clone;
  vaccineType := TFhirImmunization(oSource).vaccineType.Clone;
  subject := TFhirImmunization(oSource).subject.Clone;
  refusedIndicator := TFhirImmunization(oSource).refusedIndicator.Clone;
  reported := TFhirImmunization(oSource).reported.Clone;
  performer := TFhirImmunization(oSource).performer.Clone;
  requester := TFhirImmunization(oSource).requester.Clone;
  manufacturer := TFhirImmunization(oSource).manufacturer.Clone;
  location := TFhirImmunization(oSource).location.Clone;
  lotNumber := TFhirImmunization(oSource).lotNumber.Clone;
  expirationDate := TFhirImmunization(oSource).expirationDate.Clone;
  site := TFhirImmunization(oSource).site.Clone;
  route := TFhirImmunization(oSource).route.Clone;
  doseQuantity := TFhirImmunization(oSource).doseQuantity.Clone;
  explanation := TFhirImmunization(oSource).explanation.Clone;
  FReactionList.Assign(TFhirImmunization(oSource).FReactionList);
  FVaccinationProtocolList.Assign(TFhirImmunization(oSource).FVaccinationProtocolList);
end;

procedure TFhirImmunization.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'identifier') Then
     list.addAll(FIdentifierList);
  if (child_name = 'date') Then
     list.add(Date.Link);
  if (child_name = 'vaccineType') Then
     list.add(VaccineType.Link);
  if (child_name = 'subject') Then
     list.add(Subject.Link);
  if (child_name = 'refusedIndicator') Then
     list.add(RefusedIndicator.Link);
  if (child_name = 'reported') Then
     list.add(Reported.Link);
  if (child_name = 'performer') Then
     list.add(Performer.Link);
  if (child_name = 'requester') Then
     list.add(Requester.Link);
  if (child_name = 'manufacturer') Then
     list.add(Manufacturer.Link);
  if (child_name = 'location') Then
     list.add(Location.Link);
  if (child_name = 'lotNumber') Then
     list.add(LotNumber.Link);
  if (child_name = 'expirationDate') Then
     list.add(ExpirationDate.Link);
  if (child_name = 'site') Then
     list.add(Site.Link);
  if (child_name = 'route') Then
     list.add(Route.Link);
  if (child_name = 'doseQuantity') Then
     list.add(DoseQuantity.Link);
  if (child_name = 'explanation') Then
     list.add(Explanation.Link);
  if (child_name = 'reaction') Then
     list.addAll(FReactionList);
  if (child_name = 'vaccinationProtocol') Then
     list.addAll(FVaccinationProtocolList);
end;

procedure TFhirImmunization.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'date', 'dateTime', FDate.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'vaccineType', 'CodeableConcept', FVaccineType.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'subject', 'Resource(Patient)', FSubject.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'refusedIndicator', 'boolean', FRefusedIndicator.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'reported', 'boolean', FReported.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'performer', 'Resource(Practitioner)', FPerformer.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'requester', 'Resource(Practitioner)', FRequester.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'manufacturer', 'Resource(Organization)', FManufacturer.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'location', 'Resource(Location)', FLocation.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'lotNumber', 'string', FLotNumber.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'expirationDate', 'date', FExpirationDate.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'site', 'CodeableConcept', FSite.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'route', 'CodeableConcept', FRoute.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'doseQuantity', 'Quantity', FDoseQuantity.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'explanation', '', FExplanation.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'reaction', '', FReactionList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'vaccinationProtocol', '', FVaccinationProtocolList.Link)){3};
end;

function TFhirImmunization.Link : TFhirImmunization;
begin
  result := TFhirImmunization(inherited Link);
end;

function TFhirImmunization.Clone : TFhirImmunization;
begin
  result := TFhirImmunization(inherited Clone);
end;

{ TFhirImmunization }

Procedure TFhirImmunization.SetDate(value : TFhirDateTime);
begin
  FDate.free;
  FDate := value;
end;

Function TFhirImmunization.GetDateST : TDateAndTime;
begin
  if FDate = nil then
    result := nil
  else
    result := Date.value;
end;

Procedure TFhirImmunization.SetDateST(value : TDateAndTime);
begin
  if value <> nil then
  begin
    if FDate = nil then
      FDate := TFhirDateTime.create;
    FDate.value := value
  end
  else if FDate <> nil then
    FDate.value := nil;
end;

Procedure TFhirImmunization.SetVaccineType(value : TFhirCodeableConcept);
begin
  FVaccineType.free;
  FVaccineType := value;
end;

Procedure TFhirImmunization.SetSubject(value : TFhirResourceReference{TFhirPatient});
begin
  FSubject.free;
  FSubject := value;
end;

Procedure TFhirImmunization.SetRefusedIndicator(value : TFhirBoolean);
begin
  FRefusedIndicator.free;
  FRefusedIndicator := value;
end;

Function TFhirImmunization.GetRefusedIndicatorST : Boolean;
begin
  if FRefusedIndicator = nil then
    result := false
  else
    result := RefusedIndicator.value;
end;

Procedure TFhirImmunization.SetRefusedIndicatorST(value : Boolean);
begin
  if FRefusedIndicator = nil then
    FRefusedIndicator := TFhirBoolean.create;
  FRefusedIndicator.value := value
end;

Procedure TFhirImmunization.SetReported(value : TFhirBoolean);
begin
  FReported.free;
  FReported := value;
end;

Function TFhirImmunization.GetReportedST : Boolean;
begin
  if FReported = nil then
    result := false
  else
    result := Reported.value;
end;

Procedure TFhirImmunization.SetReportedST(value : Boolean);
begin
  if FReported = nil then
    FReported := TFhirBoolean.create;
  FReported.value := value
end;

Procedure TFhirImmunization.SetPerformer(value : TFhirResourceReference{TFhirPractitioner});
begin
  FPerformer.free;
  FPerformer := value;
end;

Procedure TFhirImmunization.SetRequester(value : TFhirResourceReference{TFhirPractitioner});
begin
  FRequester.free;
  FRequester := value;
end;

Procedure TFhirImmunization.SetManufacturer(value : TFhirResourceReference{TFhirOrganization});
begin
  FManufacturer.free;
  FManufacturer := value;
end;

Procedure TFhirImmunization.SetLocation(value : TFhirResourceReference{TFhirLocation});
begin
  FLocation.free;
  FLocation := value;
end;

Procedure TFhirImmunization.SetLotNumber(value : TFhirString);
begin
  FLotNumber.free;
  FLotNumber := value;
end;

Function TFhirImmunization.GetLotNumberST : String;
begin
  if FLotNumber = nil then
    result := ''
  else
    result := LotNumber.value;
end;

Procedure TFhirImmunization.SetLotNumberST(value : String);
begin
  if value <> '' then
  begin
    if FLotNumber = nil then
      FLotNumber := TFhirString.create;
    FLotNumber.value := value
  end
  else if FLotNumber <> nil then
    FLotNumber.value := '';
end;

Procedure TFhirImmunization.SetExpirationDate(value : TFhirDate);
begin
  FExpirationDate.free;
  FExpirationDate := value;
end;

Function TFhirImmunization.GetExpirationDateST : TDateAndTime;
begin
  if FExpirationDate = nil then
    result := nil
  else
    result := ExpirationDate.value;
end;

Procedure TFhirImmunization.SetExpirationDateST(value : TDateAndTime);
begin
  if value <> nil then
  begin
    if FExpirationDate = nil then
      FExpirationDate := TFhirDate.create;
    FExpirationDate.value := value
  end
  else if FExpirationDate <> nil then
    FExpirationDate.value := nil;
end;

Procedure TFhirImmunization.SetSite(value : TFhirCodeableConcept);
begin
  FSite.free;
  FSite := value;
end;

Procedure TFhirImmunization.SetRoute(value : TFhirCodeableConcept);
begin
  FRoute.free;
  FRoute := value;
end;

Procedure TFhirImmunization.SetDoseQuantity(value : TFhirQuantity);
begin
  FDoseQuantity.free;
  FDoseQuantity := value;
end;

Procedure TFhirImmunization.SetExplanation(value : TFhirImmunizationExplanation);
begin
  FExplanation.free;
  FExplanation := value;
end;


{ TFhirImmunizationRecommendation }

constructor TFhirImmunizationRecommendation.Create;
begin
  inherited;
  FIdentifierList := TFhirIdentifierList.Create;
  FRecommendationList := TFhirImmunizationRecommendationRecommendationList.Create;
end;

destructor TFhirImmunizationRecommendation.Destroy;
begin
  FIdentifierList.Free;
  FSubject.free;
  FRecommendationList.Free;
  inherited;
end;

function TFhirImmunizationRecommendation.GetResourceType : TFhirResourceType;
begin
  result := frtImmunizationRecommendation;
end;

function TFhirImmunizationRecommendation.GetHasASummary : Boolean;
begin
  result := false;
end;

procedure TFhirImmunizationRecommendation.Assign(oSource : TAdvObject);
begin
  inherited;
  FIdentifierList.Assign(TFhirImmunizationRecommendation(oSource).FIdentifierList);
  subject := TFhirImmunizationRecommendation(oSource).subject.Clone;
  FRecommendationList.Assign(TFhirImmunizationRecommendation(oSource).FRecommendationList);
end;

procedure TFhirImmunizationRecommendation.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'identifier') Then
     list.addAll(FIdentifierList);
  if (child_name = 'subject') Then
     list.add(Subject.Link);
  if (child_name = 'recommendation') Then
     list.addAll(FRecommendationList);
end;

procedure TFhirImmunizationRecommendation.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'subject', 'Resource(Patient)', FSubject.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'recommendation', '', FRecommendationList.Link)){3};
end;

function TFhirImmunizationRecommendation.Link : TFhirImmunizationRecommendation;
begin
  result := TFhirImmunizationRecommendation(inherited Link);
end;

function TFhirImmunizationRecommendation.Clone : TFhirImmunizationRecommendation;
begin
  result := TFhirImmunizationRecommendation(inherited Clone);
end;

{ TFhirImmunizationRecommendation }

Procedure TFhirImmunizationRecommendation.SetSubject(value : TFhirResourceReference{TFhirPatient});
begin
  FSubject.free;
  FSubject := value;
end;


{ TFhirList }

constructor TFhirList.Create;
begin
  inherited;
  FIdentifierList := TFhirIdentifierList.Create;
  FEntryList := TFhirListEntryList.Create;
end;

destructor TFhirList.Destroy;
begin
  FIdentifierList.Free;
  FCode.free;
  FSubject.free;
  FSource.free;
  FDate.free;
  FOrdered.free;
  FMode.free;
  FEntryList.Free;
  FEmptyReason.free;
  inherited;
end;

function TFhirList.GetResourceType : TFhirResourceType;
begin
  result := frtList;
end;

function TFhirList.GetHasASummary : Boolean;
begin
  result := false;
end;

procedure TFhirList.Assign(oSource : TAdvObject);
begin
  inherited;
  FIdentifierList.Assign(TFhirList(oSource).FIdentifierList);
  code := TFhirList(oSource).code.Clone;
  subject := TFhirList(oSource).subject.Clone;
  source := TFhirList(oSource).source.Clone;
  date := TFhirList(oSource).date.Clone;
  ordered := TFhirList(oSource).ordered.Clone;
  FMode := TFhirList(oSource).FMode.Link;
  FEntryList.Assign(TFhirList(oSource).FEntryList);
  emptyReason := TFhirList(oSource).emptyReason.Clone;
end;

procedure TFhirList.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'identifier') Then
     list.addAll(FIdentifierList);
  if (child_name = 'code') Then
     list.add(Code.Link);
  if (child_name = 'subject') Then
     list.add(Subject.Link);
  if (child_name = 'source') Then
     list.add(Source.Link);
  if (child_name = 'date') Then
     list.add(Date.Link);
  if (child_name = 'ordered') Then
     list.add(Ordered.Link);
  if (child_name = 'mode') Then
     list.add(FMode.Link);
  if (child_name = 'entry') Then
     list.addAll(FEntryList);
  if (child_name = 'emptyReason') Then
     list.add(EmptyReason.Link);
end;

procedure TFhirList.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'code', 'CodeableConcept', FCode.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'subject', 'Resource(Patient|Group|Device|Location)', FSubject.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'source', 'Resource(Practitioner|Patient|Device)', FSource.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'date', 'dateTime', FDate.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'ordered', 'boolean', FOrdered.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'mode', 'code', FMode.Link));{1}
  oList.add(TFHIRProperty.create(self, 'entry', '', FEntryList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'emptyReason', 'CodeableConcept', FEmptyReason.Link.Link));{2}
end;

function TFhirList.Link : TFhirList;
begin
  result := TFhirList(inherited Link);
end;

function TFhirList.Clone : TFhirList;
begin
  result := TFhirList(inherited Clone);
end;

{ TFhirList }

Procedure TFhirList.SetCode(value : TFhirCodeableConcept);
begin
  FCode.free;
  FCode := value;
end;

Procedure TFhirList.SetSubject(value : TFhirResourceReference{Resource});
begin
  FSubject.free;
  FSubject := value;
end;

Procedure TFhirList.SetSource(value : TFhirResourceReference{Resource});
begin
  FSource.free;
  FSource := value;
end;

Procedure TFhirList.SetDate(value : TFhirDateTime);
begin
  FDate.free;
  FDate := value;
end;

Function TFhirList.GetDateST : TDateAndTime;
begin
  if FDate = nil then
    result := nil
  else
    result := Date.value;
end;

Procedure TFhirList.SetDateST(value : TDateAndTime);
begin
  if value <> nil then
  begin
    if FDate = nil then
      FDate := TFhirDateTime.create;
    FDate.value := value
  end
  else if FDate <> nil then
    FDate.value := nil;
end;

Procedure TFhirList.SetOrdered(value : TFhirBoolean);
begin
  FOrdered.free;
  FOrdered := value;
end;

Function TFhirList.GetOrderedST : Boolean;
begin
  if FOrdered = nil then
    result := false
  else
    result := Ordered.value;
end;

Procedure TFhirList.SetOrderedST(value : Boolean);
begin
  if FOrdered = nil then
    FOrdered := TFhirBoolean.create;
  FOrdered.value := value
end;

Procedure TFhirList.SetMode(value : TFhirEnum);
begin
  FMode.free;
  FMode := value;
end;

Function TFhirList.GetModeST : TFhirListMode;
begin
  if FMode = nil then
    result := TFhirListMode(0)
  else
    result := TFhirListMode(StringArrayIndexOf(CODES_TFhirListMode, Mode.value));
end;

Procedure TFhirList.SetModeST(value : TFhirListMode);
begin
  if ord(value) = 0 then
    Mode := nil
  else
    Mode := TFhirEnum.create(CODES_TFhirListMode[value]);
end;

Procedure TFhirList.SetEmptyReason(value : TFhirCodeableConcept);
begin
  FEmptyReason.free;
  FEmptyReason := value;
end;


{ TFhirLocation }

constructor TFhirLocation.Create;
begin
  inherited;
  FIdentifierList := TFhirIdentifierList.Create;
  FTelecomList := TFhirContactList.Create;
end;

destructor TFhirLocation.Destroy;
begin
  FIdentifierList.Free;
  FName.free;
  FDescription.free;
  FType_.free;
  FTelecomList.Free;
  FAddress.free;
  FPhysicalType.free;
  FPosition.free;
  FManagingOrganization.free;
  FStatus.free;
  FPartOf.free;
  FMode.free;
  inherited;
end;

function TFhirLocation.GetResourceType : TFhirResourceType;
begin
  result := frtLocation;
end;

function TFhirLocation.GetHasASummary : Boolean;
begin
  result := false;
end;

procedure TFhirLocation.Assign(oSource : TAdvObject);
begin
  inherited;
  FIdentifierList.Assign(TFhirLocation(oSource).FIdentifierList);
  name := TFhirLocation(oSource).name.Clone;
  description := TFhirLocation(oSource).description.Clone;
  type_ := TFhirLocation(oSource).type_.Clone;
  FTelecomList.Assign(TFhirLocation(oSource).FTelecomList);
  address := TFhirLocation(oSource).address.Clone;
  physicalType := TFhirLocation(oSource).physicalType.Clone;
  position := TFhirLocation(oSource).position.Clone;
  managingOrganization := TFhirLocation(oSource).managingOrganization.Clone;
  FStatus := TFhirLocation(oSource).FStatus.Link;
  partOf := TFhirLocation(oSource).partOf.Clone;
  FMode := TFhirLocation(oSource).FMode.Link;
end;

procedure TFhirLocation.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'identifier') Then
     list.addAll(FIdentifierList);
  if (child_name = 'name') Then
     list.add(Name.Link);
  if (child_name = 'description') Then
     list.add(Description.Link);
  if (child_name = 'type_') Then
     list.add(Type_.Link);
  if (child_name = 'telecom') Then
     list.addAll(FTelecomList);
  if (child_name = 'address') Then
     list.add(Address.Link);
  if (child_name = 'physicalType') Then
     list.add(PhysicalType.Link);
  if (child_name = 'position') Then
     list.add(Position.Link);
  if (child_name = 'managingOrganization') Then
     list.add(ManagingOrganization.Link);
  if (child_name = 'status') Then
     list.add(FStatus.Link);
  if (child_name = 'partOf') Then
     list.add(PartOf.Link);
  if (child_name = 'mode') Then
     list.add(FMode.Link);
end;

procedure TFhirLocation.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'name', 'string', FName.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'description', 'string', FDescription.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'type', 'CodeableConcept', FType_.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'telecom', 'Contact', FTelecomList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'address', 'Address', FAddress.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'physicalType', 'CodeableConcept', FPhysicalType.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'position', '', FPosition.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'managingOrganization', 'Resource(Organization)', FManagingOrganization.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'status', 'code', FStatus.Link));{1}
  oList.add(TFHIRProperty.create(self, 'partOf', 'Resource(Location)', FPartOf.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'mode', 'code', FMode.Link));{1}
end;

function TFhirLocation.Link : TFhirLocation;
begin
  result := TFhirLocation(inherited Link);
end;

function TFhirLocation.Clone : TFhirLocation;
begin
  result := TFhirLocation(inherited Clone);
end;

{ TFhirLocation }

Procedure TFhirLocation.SetName(value : TFhirString);
begin
  FName.free;
  FName := value;
end;

Function TFhirLocation.GetNameST : String;
begin
  if FName = nil then
    result := ''
  else
    result := Name.value;
end;

Procedure TFhirLocation.SetNameST(value : String);
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

Procedure TFhirLocation.SetDescription(value : TFhirString);
begin
  FDescription.free;
  FDescription := value;
end;

Function TFhirLocation.GetDescriptionST : String;
begin
  if FDescription = nil then
    result := ''
  else
    result := Description.value;
end;

Procedure TFhirLocation.SetDescriptionST(value : String);
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

Procedure TFhirLocation.SetType_(value : TFhirCodeableConcept);
begin
  FType_.free;
  FType_ := value;
end;

Procedure TFhirLocation.SetAddress(value : TFhirAddress);
begin
  FAddress.free;
  FAddress := value;
end;

Procedure TFhirLocation.SetPhysicalType(value : TFhirCodeableConcept);
begin
  FPhysicalType.free;
  FPhysicalType := value;
end;

Procedure TFhirLocation.SetPosition(value : TFhirLocationPosition);
begin
  FPosition.free;
  FPosition := value;
end;

Procedure TFhirLocation.SetManagingOrganization(value : TFhirResourceReference{TFhirOrganization});
begin
  FManagingOrganization.free;
  FManagingOrganization := value;
end;

Procedure TFhirLocation.SetStatus(value : TFhirEnum);
begin
  FStatus.free;
  FStatus := value;
end;

Function TFhirLocation.GetStatusST : TFhirLocationStatus;
begin
  if FStatus = nil then
    result := TFhirLocationStatus(0)
  else
    result := TFhirLocationStatus(StringArrayIndexOf(CODES_TFhirLocationStatus, Status.value));
end;

Procedure TFhirLocation.SetStatusST(value : TFhirLocationStatus);
begin
  if ord(value) = 0 then
    Status := nil
  else
    Status := TFhirEnum.create(CODES_TFhirLocationStatus[value]);
end;

Procedure TFhirLocation.SetPartOf(value : TFhirResourceReference{TFhirLocation});
begin
  FPartOf.free;
  FPartOf := value;
end;

Procedure TFhirLocation.SetMode(value : TFhirEnum);
begin
  FMode.free;
  FMode := value;
end;

Function TFhirLocation.GetModeST : TFhirLocationMode;
begin
  if FMode = nil then
    result := TFhirLocationMode(0)
  else
    result := TFhirLocationMode(StringArrayIndexOf(CODES_TFhirLocationMode, Mode.value));
end;

Procedure TFhirLocation.SetModeST(value : TFhirLocationMode);
begin
  if ord(value) = 0 then
    Mode := nil
  else
    Mode := TFhirEnum.create(CODES_TFhirLocationMode[value]);
end;


{ TFhirMedia }

constructor TFhirMedia.Create;
begin
  inherited;
  FIdentifierList := TFhirIdentifierList.Create;
end;

destructor TFhirMedia.Destroy;
begin
  FType_.free;
  FSubtype.free;
  FIdentifierList.Free;
  FDateTime.free;
  FSubject.free;
  FOperator.free;
  FView.free;
  FDeviceName.free;
  FHeight.free;
  FWidth.free;
  FFrames.free;
  FLength.free;
  FContent.free;
  inherited;
end;

function TFhirMedia.GetResourceType : TFhirResourceType;
begin
  result := frtMedia;
end;

function TFhirMedia.GetHasASummary : Boolean;
begin
  result := true;
end;

procedure TFhirMedia.Assign(oSource : TAdvObject);
begin
  inherited;
  FType_ := TFhirMedia(oSource).FType_.Link;
  subtype := TFhirMedia(oSource).subtype.Clone;
  FIdentifierList.Assign(TFhirMedia(oSource).FIdentifierList);
  dateTime := TFhirMedia(oSource).dateTime.Clone;
  subject := TFhirMedia(oSource).subject.Clone;
  operator := TFhirMedia(oSource).operator.Clone;
  view := TFhirMedia(oSource).view.Clone;
  deviceName := TFhirMedia(oSource).deviceName.Clone;
  height := TFhirMedia(oSource).height.Clone;
  width := TFhirMedia(oSource).width.Clone;
  frames := TFhirMedia(oSource).frames.Clone;
  length := TFhirMedia(oSource).length.Clone;
  content := TFhirMedia(oSource).content.Clone;
end;

procedure TFhirMedia.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'type_') Then
     list.add(FType_.Link);
  if (child_name = 'subtype') Then
     list.add(Subtype.Link);
  if (child_name = 'identifier') Then
     list.addAll(FIdentifierList);
  if (child_name = 'dateTime') Then
     list.add(DateTime.Link);
  if (child_name = 'subject') Then
     list.add(Subject.Link);
  if (child_name = 'operator') Then
     list.add(Operator.Link);
  if (child_name = 'view') Then
     list.add(View.Link);
  if (child_name = 'deviceName') Then
     list.add(DeviceName.Link);
  if (child_name = 'height') Then
     list.add(Height.Link);
  if (child_name = 'width') Then
     list.add(Width.Link);
  if (child_name = 'frames') Then
     list.add(Frames.Link);
  if (child_name = 'length') Then
     list.add(Length.Link);
  if (child_name = 'content') Then
     list.add(Content.Link);
end;

procedure TFhirMedia.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'type', 'code', FType_.Link));{1}
  oList.add(TFHIRProperty.create(self, 'subtype', 'CodeableConcept', FSubtype.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'dateTime', 'dateTime', FDateTime.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'subject', 'Resource(Patient|Practitioner|Group|Device|Specimen)', FSubject.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'operator', 'Resource(Practitioner)', FOperator.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'view', 'CodeableConcept', FView.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'deviceName', 'string', FDeviceName.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'height', 'integer', FHeight.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'width', 'integer', FWidth.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'frames', 'integer', FFrames.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'length', 'integer', FLength.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'content', 'Attachment', FContent.Link.Link));{2}
end;

function TFhirMedia.Link : TFhirMedia;
begin
  result := TFhirMedia(inherited Link);
end;

function TFhirMedia.Clone : TFhirMedia;
begin
  result := TFhirMedia(inherited Clone);
end;

{ TFhirMedia }

Procedure TFhirMedia.SetType_(value : TFhirEnum);
begin
  FType_.free;
  FType_ := value;
end;

Function TFhirMedia.GetType_ST : TFhirMediaType;
begin
  if FType_ = nil then
    result := TFhirMediaType(0)
  else
    result := TFhirMediaType(StringArrayIndexOf(CODES_TFhirMediaType, Type_.value));
end;

Procedure TFhirMedia.SetType_ST(value : TFhirMediaType);
begin
  if ord(value) = 0 then
    Type_ := nil
  else
    Type_ := TFhirEnum.create(CODES_TFhirMediaType[value]);
end;

Procedure TFhirMedia.SetSubtype(value : TFhirCodeableConcept);
begin
  FSubtype.free;
  FSubtype := value;
end;

Procedure TFhirMedia.SetDateTime(value : TFhirDateTime);
begin
  FDateTime.free;
  FDateTime := value;
end;

Function TFhirMedia.GetDateTimeST : TDateAndTime;
begin
  if FDateTime = nil then
    result := nil
  else
    result := DateTime.value;
end;

Procedure TFhirMedia.SetDateTimeST(value : TDateAndTime);
begin
  if value <> nil then
  begin
    if FDateTime = nil then
      FDateTime := TFhirDateTime.create;
    FDateTime.value := value
  end
  else if FDateTime <> nil then
    FDateTime.value := nil;
end;

Procedure TFhirMedia.SetSubject(value : TFhirResourceReference{Resource});
begin
  FSubject.free;
  FSubject := value;
end;

Procedure TFhirMedia.SetOperator(value : TFhirResourceReference{TFhirPractitioner});
begin
  FOperator.free;
  FOperator := value;
end;

Procedure TFhirMedia.SetView(value : TFhirCodeableConcept);
begin
  FView.free;
  FView := value;
end;

Procedure TFhirMedia.SetDeviceName(value : TFhirString);
begin
  FDeviceName.free;
  FDeviceName := value;
end;

Function TFhirMedia.GetDeviceNameST : String;
begin
  if FDeviceName = nil then
    result := ''
  else
    result := DeviceName.value;
end;

Procedure TFhirMedia.SetDeviceNameST(value : String);
begin
  if value <> '' then
  begin
    if FDeviceName = nil then
      FDeviceName := TFhirString.create;
    FDeviceName.value := value
  end
  else if FDeviceName <> nil then
    FDeviceName.value := '';
end;

Procedure TFhirMedia.SetHeight(value : TFhirInteger);
begin
  FHeight.free;
  FHeight := value;
end;

Function TFhirMedia.GetHeightST : String;
begin
  if FHeight = nil then
    result := ''
  else
    result := Height.value;
end;

Procedure TFhirMedia.SetHeightST(value : String);
begin
  if value <> '' then
  begin
    if FHeight = nil then
      FHeight := TFhirInteger.create;
    FHeight.value := value
  end
  else if FHeight <> nil then
    FHeight.value := '';
end;

Procedure TFhirMedia.SetWidth(value : TFhirInteger);
begin
  FWidth.free;
  FWidth := value;
end;

Function TFhirMedia.GetWidthST : String;
begin
  if FWidth = nil then
    result := ''
  else
    result := Width.value;
end;

Procedure TFhirMedia.SetWidthST(value : String);
begin
  if value <> '' then
  begin
    if FWidth = nil then
      FWidth := TFhirInteger.create;
    FWidth.value := value
  end
  else if FWidth <> nil then
    FWidth.value := '';
end;

Procedure TFhirMedia.SetFrames(value : TFhirInteger);
begin
  FFrames.free;
  FFrames := value;
end;

Function TFhirMedia.GetFramesST : String;
begin
  if FFrames = nil then
    result := ''
  else
    result := Frames.value;
end;

Procedure TFhirMedia.SetFramesST(value : String);
begin
  if value <> '' then
  begin
    if FFrames = nil then
      FFrames := TFhirInteger.create;
    FFrames.value := value
  end
  else if FFrames <> nil then
    FFrames.value := '';
end;

Procedure TFhirMedia.SetLength(value : TFhirInteger);
begin
  FLength.free;
  FLength := value;
end;

Function TFhirMedia.GetLengthST : String;
begin
  if FLength = nil then
    result := ''
  else
    result := Length.value;
end;

Procedure TFhirMedia.SetLengthST(value : String);
begin
  if value <> '' then
  begin
    if FLength = nil then
      FLength := TFhirInteger.create;
    FLength.value := value
  end
  else if FLength <> nil then
    FLength.value := '';
end;

Procedure TFhirMedia.SetContent(value : TFhirAttachment);
begin
  FContent.free;
  FContent := value;
end;


{ TFhirMedication }

constructor TFhirMedication.Create;
begin
  inherited;
end;

destructor TFhirMedication.Destroy;
begin
  FName.free;
  FCode.free;
  FIsBrand.free;
  FManufacturer.free;
  FKind.free;
  FProduct.free;
  FPackage.free;
  inherited;
end;

function TFhirMedication.GetResourceType : TFhirResourceType;
begin
  result := frtMedication;
end;

function TFhirMedication.GetHasASummary : Boolean;
begin
  result := true;
end;

procedure TFhirMedication.Assign(oSource : TAdvObject);
begin
  inherited;
  name := TFhirMedication(oSource).name.Clone;
  code := TFhirMedication(oSource).code.Clone;
  isBrand := TFhirMedication(oSource).isBrand.Clone;
  manufacturer := TFhirMedication(oSource).manufacturer.Clone;
  FKind := TFhirMedication(oSource).FKind.Link;
  product := TFhirMedication(oSource).product.Clone;
  package := TFhirMedication(oSource).package.Clone;
end;

procedure TFhirMedication.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'name') Then
     list.add(Name.Link);
  if (child_name = 'code') Then
     list.add(Code.Link);
  if (child_name = 'isBrand') Then
     list.add(IsBrand.Link);
  if (child_name = 'manufacturer') Then
     list.add(Manufacturer.Link);
  if (child_name = 'kind') Then
     list.add(FKind.Link);
  if (child_name = 'product') Then
     list.add(Product.Link);
  if (child_name = 'package') Then
     list.add(Package.Link);
end;

procedure TFhirMedication.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'name', 'string', FName.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'code', 'CodeableConcept', FCode.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'isBrand', 'boolean', FIsBrand.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'manufacturer', 'Resource(Organization)', FManufacturer.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'kind', 'code', FKind.Link));{1}
  oList.add(TFHIRProperty.create(self, 'product', '', FProduct.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'package', '', FPackage.Link.Link));{2}
end;

function TFhirMedication.Link : TFhirMedication;
begin
  result := TFhirMedication(inherited Link);
end;

function TFhirMedication.Clone : TFhirMedication;
begin
  result := TFhirMedication(inherited Clone);
end;

{ TFhirMedication }

Procedure TFhirMedication.SetName(value : TFhirString);
begin
  FName.free;
  FName := value;
end;

Function TFhirMedication.GetNameST : String;
begin
  if FName = nil then
    result := ''
  else
    result := Name.value;
end;

Procedure TFhirMedication.SetNameST(value : String);
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

Procedure TFhirMedication.SetCode(value : TFhirCodeableConcept);
begin
  FCode.free;
  FCode := value;
end;

Procedure TFhirMedication.SetIsBrand(value : TFhirBoolean);
begin
  FIsBrand.free;
  FIsBrand := value;
end;

Function TFhirMedication.GetIsBrandST : Boolean;
begin
  if FIsBrand = nil then
    result := false
  else
    result := IsBrand.value;
end;

Procedure TFhirMedication.SetIsBrandST(value : Boolean);
begin
  if FIsBrand = nil then
    FIsBrand := TFhirBoolean.create;
  FIsBrand.value := value
end;

Procedure TFhirMedication.SetManufacturer(value : TFhirResourceReference{TFhirOrganization});
begin
  FManufacturer.free;
  FManufacturer := value;
end;

Procedure TFhirMedication.SetKind(value : TFhirEnum);
begin
  FKind.free;
  FKind := value;
end;

Function TFhirMedication.GetKindST : TFhirMedicationKind;
begin
  if FKind = nil then
    result := TFhirMedicationKind(0)
  else
    result := TFhirMedicationKind(StringArrayIndexOf(CODES_TFhirMedicationKind, Kind.value));
end;

Procedure TFhirMedication.SetKindST(value : TFhirMedicationKind);
begin
  if ord(value) = 0 then
    Kind := nil
  else
    Kind := TFhirEnum.create(CODES_TFhirMedicationKind[value]);
end;

Procedure TFhirMedication.SetProduct(value : TFhirMedicationProduct);
begin
  FProduct.free;
  FProduct := value;
end;

Procedure TFhirMedication.SetPackage(value : TFhirMedicationPackage);
begin
  FPackage.free;
  FPackage := value;
end;


{ TFhirMedicationAdministration }

constructor TFhirMedicationAdministration.Create;
begin
  inherited;
  FIdentifierList := TFhirIdentifierList.Create;
  FReasonNotGivenList := TFhirCodeableConceptList.Create;
  FDeviceList := TFhirResourceReferenceList{TFhirDevice}.Create;
  FDosageList := TFhirMedicationAdministrationDosageList.Create;
end;

destructor TFhirMedicationAdministration.Destroy;
begin
  FIdentifierList.Free;
  FStatus.free;
  FPatient.free;
  FPractitioner.free;
  FEncounter.free;
  FPrescription.free;
  FWasNotGiven.free;
  FReasonNotGivenList.Free;
  FWhenGiven.free;
  FMedication.free;
  FDeviceList.Free;
  FDosageList.Free;
  inherited;
end;

function TFhirMedicationAdministration.GetResourceType : TFhirResourceType;
begin
  result := frtMedicationAdministration;
end;

function TFhirMedicationAdministration.GetHasASummary : Boolean;
begin
  result := false;
end;

procedure TFhirMedicationAdministration.Assign(oSource : TAdvObject);
begin
  inherited;
  FIdentifierList.Assign(TFhirMedicationAdministration(oSource).FIdentifierList);
  FStatus := TFhirMedicationAdministration(oSource).FStatus.Link;
  patient := TFhirMedicationAdministration(oSource).patient.Clone;
  practitioner := TFhirMedicationAdministration(oSource).practitioner.Clone;
  encounter := TFhirMedicationAdministration(oSource).encounter.Clone;
  prescription := TFhirMedicationAdministration(oSource).prescription.Clone;
  wasNotGiven := TFhirMedicationAdministration(oSource).wasNotGiven.Clone;
  FReasonNotGivenList.Assign(TFhirMedicationAdministration(oSource).FReasonNotGivenList);
  whenGiven := TFhirMedicationAdministration(oSource).whenGiven.Clone;
  medication := TFhirMedicationAdministration(oSource).medication.Clone;
  FDeviceList.Assign(TFhirMedicationAdministration(oSource).FDeviceList);
  FDosageList.Assign(TFhirMedicationAdministration(oSource).FDosageList);
end;

procedure TFhirMedicationAdministration.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'identifier') Then
     list.addAll(FIdentifierList);
  if (child_name = 'status') Then
     list.add(FStatus.Link);
  if (child_name = 'patient') Then
     list.add(Patient.Link);
  if (child_name = 'practitioner') Then
     list.add(Practitioner.Link);
  if (child_name = 'encounter') Then
     list.add(Encounter.Link);
  if (child_name = 'prescription') Then
     list.add(Prescription.Link);
  if (child_name = 'wasNotGiven') Then
     list.add(WasNotGiven.Link);
  if (child_name = 'reasonNotGiven') Then
     list.addAll(FReasonNotGivenList);
  if (child_name = 'whenGiven') Then
     list.add(WhenGiven.Link);
  if (child_name = 'medication') Then
     list.add(Medication.Link);
  if (child_name = 'device') Then
     list.addAll(FDeviceList);
  if (child_name = 'dosage') Then
     list.addAll(FDosageList);
end;

procedure TFhirMedicationAdministration.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'status', 'code', FStatus.Link));{1}
  oList.add(TFHIRProperty.create(self, 'patient', 'Resource(Patient)', FPatient.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'practitioner', 'Resource(Practitioner)', FPractitioner.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'encounter', 'Resource(Encounter)', FEncounter.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'prescription', 'Resource(MedicationPrescription)', FPrescription.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'wasNotGiven', 'boolean', FWasNotGiven.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'reasonNotGiven', 'CodeableConcept', FReasonNotGivenList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'whenGiven', 'Period', FWhenGiven.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'medication', 'Resource(Medication)', FMedication.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'device', 'Resource(Device)', FDeviceList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'dosage', '', FDosageList.Link)){3};
end;

function TFhirMedicationAdministration.Link : TFhirMedicationAdministration;
begin
  result := TFhirMedicationAdministration(inherited Link);
end;

function TFhirMedicationAdministration.Clone : TFhirMedicationAdministration;
begin
  result := TFhirMedicationAdministration(inherited Clone);
end;

{ TFhirMedicationAdministration }

Procedure TFhirMedicationAdministration.SetStatus(value : TFhirEnum);
begin
  FStatus.free;
  FStatus := value;
end;

Function TFhirMedicationAdministration.GetStatusST : TFhirMedicationAdminStatus;
begin
  if FStatus = nil then
    result := TFhirMedicationAdminStatus(0)
  else
    result := TFhirMedicationAdminStatus(StringArrayIndexOf(CODES_TFhirMedicationAdminStatus, Status.value));
end;

Procedure TFhirMedicationAdministration.SetStatusST(value : TFhirMedicationAdminStatus);
begin
  if ord(value) = 0 then
    Status := nil
  else
    Status := TFhirEnum.create(CODES_TFhirMedicationAdminStatus[value]);
end;

Procedure TFhirMedicationAdministration.SetPatient(value : TFhirResourceReference{TFhirPatient});
begin
  FPatient.free;
  FPatient := value;
end;

Procedure TFhirMedicationAdministration.SetPractitioner(value : TFhirResourceReference{TFhirPractitioner});
begin
  FPractitioner.free;
  FPractitioner := value;
end;

Procedure TFhirMedicationAdministration.SetEncounter(value : TFhirResourceReference{TFhirEncounter});
begin
  FEncounter.free;
  FEncounter := value;
end;

Procedure TFhirMedicationAdministration.SetPrescription(value : TFhirResourceReference{TFhirMedicationPrescription});
begin
  FPrescription.free;
  FPrescription := value;
end;

Procedure TFhirMedicationAdministration.SetWasNotGiven(value : TFhirBoolean);
begin
  FWasNotGiven.free;
  FWasNotGiven := value;
end;

Function TFhirMedicationAdministration.GetWasNotGivenST : Boolean;
begin
  if FWasNotGiven = nil then
    result := false
  else
    result := WasNotGiven.value;
end;

Procedure TFhirMedicationAdministration.SetWasNotGivenST(value : Boolean);
begin
  if FWasNotGiven = nil then
    FWasNotGiven := TFhirBoolean.create;
  FWasNotGiven.value := value
end;

Procedure TFhirMedicationAdministration.SetWhenGiven(value : TFhirPeriod);
begin
  FWhenGiven.free;
  FWhenGiven := value;
end;

Procedure TFhirMedicationAdministration.SetMedication(value : TFhirResourceReference{TFhirMedication});
begin
  FMedication.free;
  FMedication := value;
end;


{ TFhirMedicationDispense }

constructor TFhirMedicationDispense.Create;
begin
  inherited;
  FAuthorizingPrescriptionList := TFhirResourceReferenceList{TFhirMedicationPrescription}.Create;
  FDispenseList := TFhirMedicationDispenseDispenseList.Create;
end;

destructor TFhirMedicationDispense.Destroy;
begin
  FIdentifier.free;
  FStatus.free;
  FPatient.free;
  FDispenser.free;
  FAuthorizingPrescriptionList.Free;
  FDispenseList.Free;
  FSubstitution.free;
  inherited;
end;

function TFhirMedicationDispense.GetResourceType : TFhirResourceType;
begin
  result := frtMedicationDispense;
end;

function TFhirMedicationDispense.GetHasASummary : Boolean;
begin
  result := false;
end;

procedure TFhirMedicationDispense.Assign(oSource : TAdvObject);
begin
  inherited;
  identifier := TFhirMedicationDispense(oSource).identifier.Clone;
  FStatus := TFhirMedicationDispense(oSource).FStatus.Link;
  patient := TFhirMedicationDispense(oSource).patient.Clone;
  dispenser := TFhirMedicationDispense(oSource).dispenser.Clone;
  FAuthorizingPrescriptionList.Assign(TFhirMedicationDispense(oSource).FAuthorizingPrescriptionList);
  FDispenseList.Assign(TFhirMedicationDispense(oSource).FDispenseList);
  substitution := TFhirMedicationDispense(oSource).substitution.Clone;
end;

procedure TFhirMedicationDispense.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'identifier') Then
     list.add(Identifier.Link);
  if (child_name = 'status') Then
     list.add(FStatus.Link);
  if (child_name = 'patient') Then
     list.add(Patient.Link);
  if (child_name = 'dispenser') Then
     list.add(Dispenser.Link);
  if (child_name = 'authorizingPrescription') Then
     list.addAll(FAuthorizingPrescriptionList);
  if (child_name = 'dispense') Then
     list.addAll(FDispenseList);
  if (child_name = 'substitution') Then
     list.add(Substitution.Link);
end;

procedure TFhirMedicationDispense.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifier.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'status', 'code', FStatus.Link));{1}
  oList.add(TFHIRProperty.create(self, 'patient', 'Resource(Patient)', FPatient.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'dispenser', 'Resource(Practitioner)', FDispenser.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'authorizingPrescription', 'Resource(MedicationPrescription)', FAuthorizingPrescriptionList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'dispense', '', FDispenseList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'substitution', '', FSubstitution.Link.Link));{2}
end;

function TFhirMedicationDispense.Link : TFhirMedicationDispense;
begin
  result := TFhirMedicationDispense(inherited Link);
end;

function TFhirMedicationDispense.Clone : TFhirMedicationDispense;
begin
  result := TFhirMedicationDispense(inherited Clone);
end;

{ TFhirMedicationDispense }

Procedure TFhirMedicationDispense.SetIdentifier(value : TFhirIdentifier);
begin
  FIdentifier.free;
  FIdentifier := value;
end;

Procedure TFhirMedicationDispense.SetStatus(value : TFhirEnum);
begin
  FStatus.free;
  FStatus := value;
end;

Function TFhirMedicationDispense.GetStatusST : TFhirMedicationDispenseStatus;
begin
  if FStatus = nil then
    result := TFhirMedicationDispenseStatus(0)
  else
    result := TFhirMedicationDispenseStatus(StringArrayIndexOf(CODES_TFhirMedicationDispenseStatus, Status.value));
end;

Procedure TFhirMedicationDispense.SetStatusST(value : TFhirMedicationDispenseStatus);
begin
  if ord(value) = 0 then
    Status := nil
  else
    Status := TFhirEnum.create(CODES_TFhirMedicationDispenseStatus[value]);
end;

Procedure TFhirMedicationDispense.SetPatient(value : TFhirResourceReference{TFhirPatient});
begin
  FPatient.free;
  FPatient := value;
end;

Procedure TFhirMedicationDispense.SetDispenser(value : TFhirResourceReference{TFhirPractitioner});
begin
  FDispenser.free;
  FDispenser := value;
end;

Procedure TFhirMedicationDispense.SetSubstitution(value : TFhirMedicationDispenseSubstitution);
begin
  FSubstitution.free;
  FSubstitution := value;
end;


{ TFhirMedicationPrescription }

constructor TFhirMedicationPrescription.Create;
begin
  inherited;
  FIdentifierList := TFhirIdentifierList.Create;
  FDosageInstructionList := TFhirMedicationPrescriptionDosageInstructionList.Create;
end;

destructor TFhirMedicationPrescription.Destroy;
begin
  FIdentifierList.Free;
  FDateWritten.free;
  FStatus.free;
  FPatient.free;
  FPrescriber.free;
  FEncounter.free;
  FReason.free;
  FMedication.free;
  FDosageInstructionList.Free;
  FDispense.free;
  FSubstitution.free;
  inherited;
end;

function TFhirMedicationPrescription.GetResourceType : TFhirResourceType;
begin
  result := frtMedicationPrescription;
end;

function TFhirMedicationPrescription.GetHasASummary : Boolean;
begin
  result := false;
end;

procedure TFhirMedicationPrescription.Assign(oSource : TAdvObject);
begin
  inherited;
  FIdentifierList.Assign(TFhirMedicationPrescription(oSource).FIdentifierList);
  dateWritten := TFhirMedicationPrescription(oSource).dateWritten.Clone;
  FStatus := TFhirMedicationPrescription(oSource).FStatus.Link;
  patient := TFhirMedicationPrescription(oSource).patient.Clone;
  prescriber := TFhirMedicationPrescription(oSource).prescriber.Clone;
  encounter := TFhirMedicationPrescription(oSource).encounter.Clone;
  reason := TFhirMedicationPrescription(oSource).reason.Clone;
  medication := TFhirMedicationPrescription(oSource).medication.Clone;
  FDosageInstructionList.Assign(TFhirMedicationPrescription(oSource).FDosageInstructionList);
  dispense := TFhirMedicationPrescription(oSource).dispense.Clone;
  substitution := TFhirMedicationPrescription(oSource).substitution.Clone;
end;

procedure TFhirMedicationPrescription.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'identifier') Then
     list.addAll(FIdentifierList);
  if (child_name = 'dateWritten') Then
     list.add(DateWritten.Link);
  if (child_name = 'status') Then
     list.add(FStatus.Link);
  if (child_name = 'patient') Then
     list.add(Patient.Link);
  if (child_name = 'prescriber') Then
     list.add(Prescriber.Link);
  if (child_name = 'encounter') Then
     list.add(Encounter.Link);
  if (child_name = 'reason') Then
     list.add(Reason.Link);
  if (child_name = 'medication') Then
     list.add(Medication.Link);
  if (child_name = 'dosageInstruction') Then
     list.addAll(FDosageInstructionList);
  if (child_name = 'dispense') Then
     list.add(Dispense.Link);
  if (child_name = 'substitution') Then
     list.add(Substitution.Link);
end;

procedure TFhirMedicationPrescription.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'dateWritten', 'dateTime', FDateWritten.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'status', 'code', FStatus.Link));{1}
  oList.add(TFHIRProperty.create(self, 'patient', 'Resource(Patient)', FPatient.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'prescriber', 'Resource(Practitioner)', FPrescriber.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'encounter', 'Resource(Encounter)', FEncounter.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'reason[x]', 'CodeableConcept|Resource(Condition)', FReason.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'medication', 'Resource(Medication)', FMedication.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'dosageInstruction', '', FDosageInstructionList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'dispense', '', FDispense.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'substitution', '', FSubstitution.Link.Link));{2}
end;

function TFhirMedicationPrescription.Link : TFhirMedicationPrescription;
begin
  result := TFhirMedicationPrescription(inherited Link);
end;

function TFhirMedicationPrescription.Clone : TFhirMedicationPrescription;
begin
  result := TFhirMedicationPrescription(inherited Clone);
end;

{ TFhirMedicationPrescription }

Procedure TFhirMedicationPrescription.SetDateWritten(value : TFhirDateTime);
begin
  FDateWritten.free;
  FDateWritten := value;
end;

Function TFhirMedicationPrescription.GetDateWrittenST : TDateAndTime;
begin
  if FDateWritten = nil then
    result := nil
  else
    result := DateWritten.value;
end;

Procedure TFhirMedicationPrescription.SetDateWrittenST(value : TDateAndTime);
begin
  if value <> nil then
  begin
    if FDateWritten = nil then
      FDateWritten := TFhirDateTime.create;
    FDateWritten.value := value
  end
  else if FDateWritten <> nil then
    FDateWritten.value := nil;
end;

Procedure TFhirMedicationPrescription.SetStatus(value : TFhirEnum);
begin
  FStatus.free;
  FStatus := value;
end;

Function TFhirMedicationPrescription.GetStatusST : TFhirMedicationPrescriptionStatus;
begin
  if FStatus = nil then
    result := TFhirMedicationPrescriptionStatus(0)
  else
    result := TFhirMedicationPrescriptionStatus(StringArrayIndexOf(CODES_TFhirMedicationPrescriptionStatus, Status.value));
end;

Procedure TFhirMedicationPrescription.SetStatusST(value : TFhirMedicationPrescriptionStatus);
begin
  if ord(value) = 0 then
    Status := nil
  else
    Status := TFhirEnum.create(CODES_TFhirMedicationPrescriptionStatus[value]);
end;

Procedure TFhirMedicationPrescription.SetPatient(value : TFhirResourceReference{TFhirPatient});
begin
  FPatient.free;
  FPatient := value;
end;

Procedure TFhirMedicationPrescription.SetPrescriber(value : TFhirResourceReference{TFhirPractitioner});
begin
  FPrescriber.free;
  FPrescriber := value;
end;

Procedure TFhirMedicationPrescription.SetEncounter(value : TFhirResourceReference{TFhirEncounter});
begin
  FEncounter.free;
  FEncounter := value;
end;

Procedure TFhirMedicationPrescription.SetReason(value : TFhirType);
begin
  FReason.free;
  FReason := value;
end;

Procedure TFhirMedicationPrescription.SetMedication(value : TFhirResourceReference{TFhirMedication});
begin
  FMedication.free;
  FMedication := value;
end;

Procedure TFhirMedicationPrescription.SetDispense(value : TFhirMedicationPrescriptionDispense);
begin
  FDispense.free;
  FDispense := value;
end;

Procedure TFhirMedicationPrescription.SetSubstitution(value : TFhirMedicationPrescriptionSubstitution);
begin
  FSubstitution.free;
  FSubstitution := value;
end;


{ TFhirMedicationStatement }

constructor TFhirMedicationStatement.Create;
begin
  inherited;
  FIdentifierList := TFhirIdentifierList.Create;
  FReasonNotGivenList := TFhirCodeableConceptList.Create;
  FDeviceList := TFhirResourceReferenceList{TFhirDevice}.Create;
  FDosageList := TFhirMedicationStatementDosageList.Create;
end;

destructor TFhirMedicationStatement.Destroy;
begin
  FIdentifierList.Free;
  FPatient.free;
  FWasNotGiven.free;
  FReasonNotGivenList.Free;
  FWhenGiven.free;
  FMedication.free;
  FDeviceList.Free;
  FDosageList.Free;
  inherited;
end;

function TFhirMedicationStatement.GetResourceType : TFhirResourceType;
begin
  result := frtMedicationStatement;
end;

function TFhirMedicationStatement.GetHasASummary : Boolean;
begin
  result := false;
end;

procedure TFhirMedicationStatement.Assign(oSource : TAdvObject);
begin
  inherited;
  FIdentifierList.Assign(TFhirMedicationStatement(oSource).FIdentifierList);
  patient := TFhirMedicationStatement(oSource).patient.Clone;
  wasNotGiven := TFhirMedicationStatement(oSource).wasNotGiven.Clone;
  FReasonNotGivenList.Assign(TFhirMedicationStatement(oSource).FReasonNotGivenList);
  whenGiven := TFhirMedicationStatement(oSource).whenGiven.Clone;
  medication := TFhirMedicationStatement(oSource).medication.Clone;
  FDeviceList.Assign(TFhirMedicationStatement(oSource).FDeviceList);
  FDosageList.Assign(TFhirMedicationStatement(oSource).FDosageList);
end;

procedure TFhirMedicationStatement.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'identifier') Then
     list.addAll(FIdentifierList);
  if (child_name = 'patient') Then
     list.add(Patient.Link);
  if (child_name = 'wasNotGiven') Then
     list.add(WasNotGiven.Link);
  if (child_name = 'reasonNotGiven') Then
     list.addAll(FReasonNotGivenList);
  if (child_name = 'whenGiven') Then
     list.add(WhenGiven.Link);
  if (child_name = 'medication') Then
     list.add(Medication.Link);
  if (child_name = 'device') Then
     list.addAll(FDeviceList);
  if (child_name = 'dosage') Then
     list.addAll(FDosageList);
end;

procedure TFhirMedicationStatement.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'patient', 'Resource(Patient)', FPatient.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'wasNotGiven', 'boolean', FWasNotGiven.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'reasonNotGiven', 'CodeableConcept', FReasonNotGivenList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'whenGiven', 'Period', FWhenGiven.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'medication', 'Resource(Medication)', FMedication.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'device', 'Resource(Device)', FDeviceList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'dosage', '', FDosageList.Link)){3};
end;

function TFhirMedicationStatement.Link : TFhirMedicationStatement;
begin
  result := TFhirMedicationStatement(inherited Link);
end;

function TFhirMedicationStatement.Clone : TFhirMedicationStatement;
begin
  result := TFhirMedicationStatement(inherited Clone);
end;

{ TFhirMedicationStatement }

Procedure TFhirMedicationStatement.SetPatient(value : TFhirResourceReference{TFhirPatient});
begin
  FPatient.free;
  FPatient := value;
end;

Procedure TFhirMedicationStatement.SetWasNotGiven(value : TFhirBoolean);
begin
  FWasNotGiven.free;
  FWasNotGiven := value;
end;

Function TFhirMedicationStatement.GetWasNotGivenST : Boolean;
begin
  if FWasNotGiven = nil then
    result := false
  else
    result := WasNotGiven.value;
end;

Procedure TFhirMedicationStatement.SetWasNotGivenST(value : Boolean);
begin
  if FWasNotGiven = nil then
    FWasNotGiven := TFhirBoolean.create;
  FWasNotGiven.value := value
end;

Procedure TFhirMedicationStatement.SetWhenGiven(value : TFhirPeriod);
begin
  FWhenGiven.free;
  FWhenGiven := value;
end;

Procedure TFhirMedicationStatement.SetMedication(value : TFhirResourceReference{TFhirMedication});
begin
  FMedication.free;
  FMedication := value;
end;


{ TFhirMessageHeader }

constructor TFhirMessageHeader.Create;
begin
  inherited;
  FDestinationList := TFhirMessageHeaderDestinationList.Create;
  FDataList := TFhirResourceReferenceList{Resource}.Create;
end;

destructor TFhirMessageHeader.Destroy;
begin
  FIdentifier.free;
  FTimestamp.free;
  FEvent.free;
  FResponse.free;
  FSource.free;
  FDestinationList.Free;
  FEnterer.free;
  FAuthor.free;
  FReceiver.free;
  FResponsible.free;
  FReason.free;
  FDataList.Free;
  inherited;
end;

function TFhirMessageHeader.GetResourceType : TFhirResourceType;
begin
  result := frtMessageHeader;
end;

function TFhirMessageHeader.GetHasASummary : Boolean;
begin
  result := false;
end;

procedure TFhirMessageHeader.Assign(oSource : TAdvObject);
begin
  inherited;
  identifier := TFhirMessageHeader(oSource).identifier.Clone;
  timestamp := TFhirMessageHeader(oSource).timestamp.Clone;
  event := TFhirMessageHeader(oSource).event.Clone;
  response := TFhirMessageHeader(oSource).response.Clone;
  source := TFhirMessageHeader(oSource).source.Clone;
  FDestinationList.Assign(TFhirMessageHeader(oSource).FDestinationList);
  enterer := TFhirMessageHeader(oSource).enterer.Clone;
  author := TFhirMessageHeader(oSource).author.Clone;
  receiver := TFhirMessageHeader(oSource).receiver.Clone;
  responsible := TFhirMessageHeader(oSource).responsible.Clone;
  reason := TFhirMessageHeader(oSource).reason.Clone;
  FDataList.Assign(TFhirMessageHeader(oSource).FDataList);
end;

procedure TFhirMessageHeader.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'identifier') Then
     list.add(Identifier.Link);
  if (child_name = 'timestamp') Then
     list.add(Timestamp.Link);
  if (child_name = 'event') Then
     list.add(Event.Link);
  if (child_name = 'response') Then
     list.add(Response.Link);
  if (child_name = 'source') Then
     list.add(Source.Link);
  if (child_name = 'destination') Then
     list.addAll(FDestinationList);
  if (child_name = 'enterer') Then
     list.add(Enterer.Link);
  if (child_name = 'author') Then
     list.add(Author.Link);
  if (child_name = 'receiver') Then
     list.add(Receiver.Link);
  if (child_name = 'responsible') Then
     list.add(Responsible.Link);
  if (child_name = 'reason') Then
     list.add(Reason.Link);
  if (child_name = 'data') Then
     list.addAll(FDataList);
end;

procedure TFhirMessageHeader.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'id', FIdentifier.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'timestamp', 'instant', FTimestamp.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'event', 'Coding', FEvent.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'response', '', FResponse.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'source', '', FSource.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'destination', '', FDestinationList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'enterer', 'Resource(Practitioner)', FEnterer.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'author', 'Resource(Practitioner)', FAuthor.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'receiver', 'Resource(Practitioner|Organization)', FReceiver.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'responsible', 'Resource(Practitioner|Organization)', FResponsible.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'reason', 'CodeableConcept', FReason.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'data', 'Resource(Any)', FDataList.Link)){3};
end;

function TFhirMessageHeader.Link : TFhirMessageHeader;
begin
  result := TFhirMessageHeader(inherited Link);
end;

function TFhirMessageHeader.Clone : TFhirMessageHeader;
begin
  result := TFhirMessageHeader(inherited Clone);
end;

{ TFhirMessageHeader }

Procedure TFhirMessageHeader.SetIdentifier(value : TFhirId);
begin
  FIdentifier.free;
  FIdentifier := value;
end;

Function TFhirMessageHeader.GetIdentifierST : String;
begin
  if FIdentifier = nil then
    result := ''
  else
    result := Identifier.value;
end;

Procedure TFhirMessageHeader.SetIdentifierST(value : String);
begin
  if value <> '' then
  begin
    if FIdentifier = nil then
      FIdentifier := TFhirId.create;
    FIdentifier.value := value
  end
  else if FIdentifier <> nil then
    FIdentifier.value := '';
end;

Procedure TFhirMessageHeader.SetTimestamp(value : TFhirInstant);
begin
  FTimestamp.free;
  FTimestamp := value;
end;

Function TFhirMessageHeader.GetTimestampST : TDateAndTime;
begin
  if FTimestamp = nil then
    result := nil
  else
    result := Timestamp.value;
end;

Procedure TFhirMessageHeader.SetTimestampST(value : TDateAndTime);
begin
  if value <> nil then
  begin
    if FTimestamp = nil then
      FTimestamp := TFhirInstant.create;
    FTimestamp.value := value
  end
  else if FTimestamp <> nil then
    FTimestamp.value := nil;
end;

Procedure TFhirMessageHeader.SetEvent(value : TFhirCoding);
begin
  FEvent.free;
  FEvent := value;
end;

Procedure TFhirMessageHeader.SetResponse(value : TFhirMessageHeaderResponse);
begin
  FResponse.free;
  FResponse := value;
end;

Procedure TFhirMessageHeader.SetSource(value : TFhirMessageHeaderSource);
begin
  FSource.free;
  FSource := value;
end;

Procedure TFhirMessageHeader.SetEnterer(value : TFhirResourceReference{TFhirPractitioner});
begin
  FEnterer.free;
  FEnterer := value;
end;

Procedure TFhirMessageHeader.SetAuthor(value : TFhirResourceReference{TFhirPractitioner});
begin
  FAuthor.free;
  FAuthor := value;
end;

Procedure TFhirMessageHeader.SetReceiver(value : TFhirResourceReference{Resource});
begin
  FReceiver.free;
  FReceiver := value;
end;

Procedure TFhirMessageHeader.SetResponsible(value : TFhirResourceReference{Resource});
begin
  FResponsible.free;
  FResponsible := value;
end;

Procedure TFhirMessageHeader.SetReason(value : TFhirCodeableConcept);
begin
  FReason.free;
  FReason := value;
end;


{ TFhirNamespace }

constructor TFhirNamespace.Create;
begin
  inherited;
  FUniqueIdList := TFhirNamespaceUniqueIdList.Create;
end;

destructor TFhirNamespace.Destroy;
begin
  FType_.free;
  FName.free;
  FStatus.free;
  FCountry.free;
  FCategory.free;
  FResponsible.free;
  FDescription.free;
  FUsage.free;
  FUniqueIdList.Free;
  FContact.free;
  FReplacedBy.free;
  inherited;
end;

function TFhirNamespace.GetResourceType : TFhirResourceType;
begin
  result := frtNamespace;
end;

function TFhirNamespace.GetHasASummary : Boolean;
begin
  result := false;
end;

procedure TFhirNamespace.Assign(oSource : TAdvObject);
begin
  inherited;
  FType_ := TFhirNamespace(oSource).FType_.Link;
  name := TFhirNamespace(oSource).name.Clone;
  FStatus := TFhirNamespace(oSource).FStatus.Link;
  country := TFhirNamespace(oSource).country.Clone;
  category := TFhirNamespace(oSource).category.Clone;
  responsible := TFhirNamespace(oSource).responsible.Clone;
  description := TFhirNamespace(oSource).description.Clone;
  usage := TFhirNamespace(oSource).usage.Clone;
  FUniqueIdList.Assign(TFhirNamespace(oSource).FUniqueIdList);
  contact := TFhirNamespace(oSource).contact.Clone;
  replacedBy := TFhirNamespace(oSource).replacedBy.Clone;
end;

procedure TFhirNamespace.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'type_') Then
     list.add(FType_.Link);
  if (child_name = 'name') Then
     list.add(Name.Link);
  if (child_name = 'status') Then
     list.add(FStatus.Link);
  if (child_name = 'country') Then
     list.add(Country.Link);
  if (child_name = 'category') Then
     list.add(Category.Link);
  if (child_name = 'responsible') Then
     list.add(Responsible.Link);
  if (child_name = 'description') Then
     list.add(Description.Link);
  if (child_name = 'usage') Then
     list.add(Usage.Link);
  if (child_name = 'uniqueId') Then
     list.addAll(FUniqueIdList);
  if (child_name = 'contact') Then
     list.add(Contact.Link);
  if (child_name = 'replacedBy') Then
     list.add(ReplacedBy.Link);
end;

procedure TFhirNamespace.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'type', 'code', FType_.Link));{1}
  oList.add(TFHIRProperty.create(self, 'name', 'string', FName.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'status', 'code', FStatus.Link));{1}
  oList.add(TFHIRProperty.create(self, 'country', 'code', FCountry.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'category', 'CodeableConcept', FCategory.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'responsible', 'string', FResponsible.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'description', 'string', FDescription.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'usage', 'string', FUsage.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'uniqueId', '', FUniqueIdList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'contact', '', FContact.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'replacedBy', 'Resource(Namespace)', FReplacedBy.Link.Link));{2}
end;

function TFhirNamespace.Link : TFhirNamespace;
begin
  result := TFhirNamespace(inherited Link);
end;

function TFhirNamespace.Clone : TFhirNamespace;
begin
  result := TFhirNamespace(inherited Clone);
end;

{ TFhirNamespace }

Procedure TFhirNamespace.SetType_(value : TFhirEnum);
begin
  FType_.free;
  FType_ := value;
end;

Function TFhirNamespace.GetType_ST : TFhirNamespaceType;
begin
  if FType_ = nil then
    result := TFhirNamespaceType(0)
  else
    result := TFhirNamespaceType(StringArrayIndexOf(CODES_TFhirNamespaceType, Type_.value));
end;

Procedure TFhirNamespace.SetType_ST(value : TFhirNamespaceType);
begin
  if ord(value) = 0 then
    Type_ := nil
  else
    Type_ := TFhirEnum.create(CODES_TFhirNamespaceType[value]);
end;

Procedure TFhirNamespace.SetName(value : TFhirString);
begin
  FName.free;
  FName := value;
end;

Function TFhirNamespace.GetNameST : String;
begin
  if FName = nil then
    result := ''
  else
    result := Name.value;
end;

Procedure TFhirNamespace.SetNameST(value : String);
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

Procedure TFhirNamespace.SetStatus(value : TFhirEnum);
begin
  FStatus.free;
  FStatus := value;
end;

Function TFhirNamespace.GetStatusST : TFhirNamespaceStatus;
begin
  if FStatus = nil then
    result := TFhirNamespaceStatus(0)
  else
    result := TFhirNamespaceStatus(StringArrayIndexOf(CODES_TFhirNamespaceStatus, Status.value));
end;

Procedure TFhirNamespace.SetStatusST(value : TFhirNamespaceStatus);
begin
  if ord(value) = 0 then
    Status := nil
  else
    Status := TFhirEnum.create(CODES_TFhirNamespaceStatus[value]);
end;

Procedure TFhirNamespace.SetCountry(value : TFhirCode);
begin
  FCountry.free;
  FCountry := value;
end;

Function TFhirNamespace.GetCountryST : String;
begin
  if FCountry = nil then
    result := ''
  else
    result := Country.value;
end;

Procedure TFhirNamespace.SetCountryST(value : String);
begin
  if value <> '' then
  begin
    if FCountry = nil then
      FCountry := TFhirCode.create;
    FCountry.value := value
  end
  else if FCountry <> nil then
    FCountry.value := '';
end;

Procedure TFhirNamespace.SetCategory(value : TFhirCodeableConcept);
begin
  FCategory.free;
  FCategory := value;
end;

Procedure TFhirNamespace.SetResponsible(value : TFhirString);
begin
  FResponsible.free;
  FResponsible := value;
end;

Function TFhirNamespace.GetResponsibleST : String;
begin
  if FResponsible = nil then
    result := ''
  else
    result := Responsible.value;
end;

Procedure TFhirNamespace.SetResponsibleST(value : String);
begin
  if value <> '' then
  begin
    if FResponsible = nil then
      FResponsible := TFhirString.create;
    FResponsible.value := value
  end
  else if FResponsible <> nil then
    FResponsible.value := '';
end;

Procedure TFhirNamespace.SetDescription(value : TFhirString);
begin
  FDescription.free;
  FDescription := value;
end;

Function TFhirNamespace.GetDescriptionST : String;
begin
  if FDescription = nil then
    result := ''
  else
    result := Description.value;
end;

Procedure TFhirNamespace.SetDescriptionST(value : String);
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

Procedure TFhirNamespace.SetUsage(value : TFhirString);
begin
  FUsage.free;
  FUsage := value;
end;

Function TFhirNamespace.GetUsageST : String;
begin
  if FUsage = nil then
    result := ''
  else
    result := Usage.value;
end;

Procedure TFhirNamespace.SetUsageST(value : String);
begin
  if value <> '' then
  begin
    if FUsage = nil then
      FUsage := TFhirString.create;
    FUsage.value := value
  end
  else if FUsage <> nil then
    FUsage.value := '';
end;

Procedure TFhirNamespace.SetContact(value : TFhirNamespaceContact);
begin
  FContact.free;
  FContact := value;
end;

Procedure TFhirNamespace.SetReplacedBy(value : TFhirResourceReference{TFhirNamespace});
begin
  FReplacedBy.free;
  FReplacedBy := value;
end;


{ TFhirObservation }

constructor TFhirObservation.Create;
begin
  inherited;
  FPerformerList := TFhirResourceReferenceList{Resource}.Create;
  FReferenceRangeList := TFhirObservationReferenceRangeList.Create;
  FRelatedList := TFhirObservationRelatedList.Create;
end;

destructor TFhirObservation.Destroy;
begin
  FName.free;
  FValue.free;
  FInterpretation.free;
  FComments.free;
  FApplies.free;
  FIssued.free;
  FStatus.free;
  FReliability.free;
  FBodySite.free;
  FMethod.free;
  FIdentifier.free;
  FSubject.free;
  FSpecimen.free;
  FPerformerList.Free;
  FEncounter.free;
  FReferenceRangeList.Free;
  FRelatedList.Free;
  inherited;
end;

function TFhirObservation.GetResourceType : TFhirResourceType;
begin
  result := frtObservation;
end;

function TFhirObservation.GetHasASummary : Boolean;
begin
  result := false;
end;

procedure TFhirObservation.Assign(oSource : TAdvObject);
begin
  inherited;
  name := TFhirObservation(oSource).name.Clone;
  value := TFhirObservation(oSource).value.Clone;
  interpretation := TFhirObservation(oSource).interpretation.Clone;
  comments := TFhirObservation(oSource).comments.Clone;
  applies := TFhirObservation(oSource).applies.Clone;
  issued := TFhirObservation(oSource).issued.Clone;
  FStatus := TFhirObservation(oSource).FStatus.Link;
  FReliability := TFhirObservation(oSource).FReliability.Link;
  bodySite := TFhirObservation(oSource).bodySite.Clone;
  method := TFhirObservation(oSource).method.Clone;
  identifier := TFhirObservation(oSource).identifier.Clone;
  subject := TFhirObservation(oSource).subject.Clone;
  specimen := TFhirObservation(oSource).specimen.Clone;
  FPerformerList.Assign(TFhirObservation(oSource).FPerformerList);
  encounter := TFhirObservation(oSource).encounter.Clone;
  FReferenceRangeList.Assign(TFhirObservation(oSource).FReferenceRangeList);
  FRelatedList.Assign(TFhirObservation(oSource).FRelatedList);
end;

procedure TFhirObservation.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'name') Then
     list.add(Name.Link);
  if (child_name = 'value') Then
     list.add(Value.Link);
  if (child_name = 'interpretation') Then
     list.add(Interpretation.Link);
  if (child_name = 'comments') Then
     list.add(Comments.Link);
  if (child_name = 'applies') Then
     list.add(Applies.Link);
  if (child_name = 'issued') Then
     list.add(Issued.Link);
  if (child_name = 'status') Then
     list.add(FStatus.Link);
  if (child_name = 'reliability') Then
     list.add(FReliability.Link);
  if (child_name = 'bodySite') Then
     list.add(BodySite.Link);
  if (child_name = 'method') Then
     list.add(Method.Link);
  if (child_name = 'identifier') Then
     list.add(Identifier.Link);
  if (child_name = 'subject') Then
     list.add(Subject.Link);
  if (child_name = 'specimen') Then
     list.add(Specimen.Link);
  if (child_name = 'performer') Then
     list.addAll(FPerformerList);
  if (child_name = 'encounter') Then
     list.add(Encounter.Link);
  if (child_name = 'referenceRange') Then
     list.addAll(FReferenceRangeList);
  if (child_name = 'related') Then
     list.addAll(FRelatedList);
end;

procedure TFhirObservation.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'name', 'CodeableConcept', FName.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'value[x]', 'Quantity|CodeableConcept|Attachment|Ratio|dateTime|Period|SampledData|string|time', FValue.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'interpretation', 'CodeableConcept', FInterpretation.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'comments', 'string', FComments.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'applies[x]', 'dateTime|Period', FApplies.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'issued', 'instant', FIssued.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'status', 'code', FStatus.Link));{1}
  oList.add(TFHIRProperty.create(self, 'reliability', 'code', FReliability.Link));{1}
  oList.add(TFHIRProperty.create(self, 'bodySite', 'CodeableConcept', FBodySite.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'method', 'CodeableConcept', FMethod.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifier.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'subject', 'Resource(Patient|Group|Device|Location)', FSubject.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'specimen', 'Resource(Specimen)', FSpecimen.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'performer', 'Resource(Practitioner|Device|Organization|Patient)', FPerformerList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'encounter', 'Resource(Encounter)', FEncounter.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'referenceRange', '', FReferenceRangeList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'related', '', FRelatedList.Link)){3};
end;

function TFhirObservation.Link : TFhirObservation;
begin
  result := TFhirObservation(inherited Link);
end;

function TFhirObservation.Clone : TFhirObservation;
begin
  result := TFhirObservation(inherited Clone);
end;

{ TFhirObservation }

Procedure TFhirObservation.SetName(value : TFhirCodeableConcept);
begin
  FName.free;
  FName := value;
end;

Procedure TFhirObservation.SetValue(value : TFhirType);
begin
  FValue.free;
  FValue := value;
end;

Procedure TFhirObservation.SetInterpretation(value : TFhirCodeableConcept);
begin
  FInterpretation.free;
  FInterpretation := value;
end;

Procedure TFhirObservation.SetComments(value : TFhirString);
begin
  FComments.free;
  FComments := value;
end;

Function TFhirObservation.GetCommentsST : String;
begin
  if FComments = nil then
    result := ''
  else
    result := Comments.value;
end;

Procedure TFhirObservation.SetCommentsST(value : String);
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

Procedure TFhirObservation.SetApplies(value : TFhirType);
begin
  FApplies.free;
  FApplies := value;
end;

Procedure TFhirObservation.SetIssued(value : TFhirInstant);
begin
  FIssued.free;
  FIssued := value;
end;

Function TFhirObservation.GetIssuedST : TDateAndTime;
begin
  if FIssued = nil then
    result := nil
  else
    result := Issued.value;
end;

Procedure TFhirObservation.SetIssuedST(value : TDateAndTime);
begin
  if value <> nil then
  begin
    if FIssued = nil then
      FIssued := TFhirInstant.create;
    FIssued.value := value
  end
  else if FIssued <> nil then
    FIssued.value := nil;
end;

Procedure TFhirObservation.SetStatus(value : TFhirEnum);
begin
  FStatus.free;
  FStatus := value;
end;

Function TFhirObservation.GetStatusST : TFhirObservationStatus;
begin
  if FStatus = nil then
    result := TFhirObservationStatus(0)
  else
    result := TFhirObservationStatus(StringArrayIndexOf(CODES_TFhirObservationStatus, Status.value));
end;

Procedure TFhirObservation.SetStatusST(value : TFhirObservationStatus);
begin
  if ord(value) = 0 then
    Status := nil
  else
    Status := TFhirEnum.create(CODES_TFhirObservationStatus[value]);
end;

Procedure TFhirObservation.SetReliability(value : TFhirEnum);
begin
  FReliability.free;
  FReliability := value;
end;

Function TFhirObservation.GetReliabilityST : TFhirObservationReliability;
begin
  if FReliability = nil then
    result := TFhirObservationReliability(0)
  else
    result := TFhirObservationReliability(StringArrayIndexOf(CODES_TFhirObservationReliability, Reliability.value));
end;

Procedure TFhirObservation.SetReliabilityST(value : TFhirObservationReliability);
begin
  if ord(value) = 0 then
    Reliability := nil
  else
    Reliability := TFhirEnum.create(CODES_TFhirObservationReliability[value]);
end;

Procedure TFhirObservation.SetBodySite(value : TFhirCodeableConcept);
begin
  FBodySite.free;
  FBodySite := value;
end;

Procedure TFhirObservation.SetMethod(value : TFhirCodeableConcept);
begin
  FMethod.free;
  FMethod := value;
end;

Procedure TFhirObservation.SetIdentifier(value : TFhirIdentifier);
begin
  FIdentifier.free;
  FIdentifier := value;
end;

Procedure TFhirObservation.SetSubject(value : TFhirResourceReference{Resource});
begin
  FSubject.free;
  FSubject := value;
end;

Procedure TFhirObservation.SetSpecimen(value : TFhirResourceReference{TFhirSpecimen});
begin
  FSpecimen.free;
  FSpecimen := value;
end;

Procedure TFhirObservation.SetEncounter(value : TFhirResourceReference{TFhirEncounter});
begin
  FEncounter.free;
  FEncounter := value;
end;


{ TFhirOperationDefinition }

constructor TFhirOperationDefinition.Create;
begin
  inherited;
  FTelecomList := TFhirContactList.Create;
  FCodeList := TFhirCodingList.Create;
  FType_List := TFhirCodeList.Create;
  FParameterList := TFhirOperationDefinitionParameterList.Create;
end;

destructor TFhirOperationDefinition.Destroy;
begin
  FIdentifier.free;
  FVersion.free;
  FTitle.free;
  FPublisher.free;
  FTelecomList.Free;
  FDescription.free;
  FCodeList.Free;
  FStatus.free;
  FExperimental.free;
  FDate.free;
  FKind.free;
  FName.free;
  FNotes.free;
  FBase.free;
  FSystem.free;
  FType_List.Free;
  FInstance.free;
  FParameterList.Free;
  inherited;
end;

function TFhirOperationDefinition.GetResourceType : TFhirResourceType;
begin
  result := frtOperationDefinition;
end;

function TFhirOperationDefinition.GetHasASummary : Boolean;
begin
  result := false;
end;

procedure TFhirOperationDefinition.Assign(oSource : TAdvObject);
begin
  inherited;
  identifier := TFhirOperationDefinition(oSource).identifier.Clone;
  version := TFhirOperationDefinition(oSource).version.Clone;
  title := TFhirOperationDefinition(oSource).title.Clone;
  publisher := TFhirOperationDefinition(oSource).publisher.Clone;
  FTelecomList.Assign(TFhirOperationDefinition(oSource).FTelecomList);
  description := TFhirOperationDefinition(oSource).description.Clone;
  FCodeList.Assign(TFhirOperationDefinition(oSource).FCodeList);
  FStatus := TFhirOperationDefinition(oSource).FStatus.Link;
  experimental := TFhirOperationDefinition(oSource).experimental.Clone;
  date := TFhirOperationDefinition(oSource).date.Clone;
  FKind := TFhirOperationDefinition(oSource).FKind.Link;
  name := TFhirOperationDefinition(oSource).name.Clone;
  notes := TFhirOperationDefinition(oSource).notes.Clone;
  base := TFhirOperationDefinition(oSource).base.Clone;
  system := TFhirOperationDefinition(oSource).system.Clone;
  FType_List.Assign(TFhirOperationDefinition(oSource).FType_List);
  instance := TFhirOperationDefinition(oSource).instance.Clone;
  FParameterList.Assign(TFhirOperationDefinition(oSource).FParameterList);
end;

procedure TFhirOperationDefinition.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'identifier') Then
     list.add(Identifier.Link);
  if (child_name = 'version') Then
     list.add(Version.Link);
  if (child_name = 'title') Then
     list.add(Title.Link);
  if (child_name = 'publisher') Then
     list.add(Publisher.Link);
  if (child_name = 'telecom') Then
     list.addAll(FTelecomList);
  if (child_name = 'description') Then
     list.add(Description.Link);
  if (child_name = 'code') Then
     list.addAll(FCodeList);
  if (child_name = 'status') Then
     list.add(FStatus.Link);
  if (child_name = 'experimental') Then
     list.add(Experimental.Link);
  if (child_name = 'date') Then
     list.add(Date.Link);
  if (child_name = 'kind') Then
     list.add(FKind.Link);
  if (child_name = 'name') Then
     list.add(Name.Link);
  if (child_name = 'notes') Then
     list.add(Notes.Link);
  if (child_name = 'base') Then
     list.add(Base.Link);
  if (child_name = 'system') Then
     list.add(System.Link);
  if (child_name = 'type_') Then
     list.addAll(FType_List);
  if (child_name = 'instance') Then
     list.add(Instance.Link);
  if (child_name = 'parameter') Then
     list.addAll(FParameterList);
end;

procedure TFhirOperationDefinition.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'uri', FIdentifier.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'version', 'string', FVersion.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'title', 'string', FTitle.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'publisher', 'string', FPublisher.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'telecom', 'Contact', FTelecomList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'description', 'string', FDescription.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'code', 'Coding', FCodeList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'status', 'code', FStatus.Link));{1}
  oList.add(TFHIRProperty.create(self, 'experimental', 'boolean', FExperimental.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'date', 'dateTime', FDate.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'kind', 'code', FKind.Link));{1}
  oList.add(TFHIRProperty.create(self, 'name', 'code', FName.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'notes', 'string', FNotes.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'base', 'Resource(OperationDefinition)', FBase.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'system', 'boolean', FSystem.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'type', 'code', FType_List.Link)){3};
  oList.add(TFHIRProperty.create(self, 'instance', 'boolean', FInstance.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'parameter', '', FParameterList.Link)){3};
end;

function TFhirOperationDefinition.Link : TFhirOperationDefinition;
begin
  result := TFhirOperationDefinition(inherited Link);
end;

function TFhirOperationDefinition.Clone : TFhirOperationDefinition;
begin
  result := TFhirOperationDefinition(inherited Clone);
end;

{ TFhirOperationDefinition }

Procedure TFhirOperationDefinition.SetIdentifier(value : TFhirUri);
begin
  FIdentifier.free;
  FIdentifier := value;
end;

Function TFhirOperationDefinition.GetIdentifierST : String;
begin
  if FIdentifier = nil then
    result := ''
  else
    result := Identifier.value;
end;

Procedure TFhirOperationDefinition.SetIdentifierST(value : String);
begin
  if value <> '' then
  begin
    if FIdentifier = nil then
      FIdentifier := TFhirUri.create;
    FIdentifier.value := value
  end
  else if FIdentifier <> nil then
    FIdentifier.value := '';
end;

Procedure TFhirOperationDefinition.SetVersion(value : TFhirString);
begin
  FVersion.free;
  FVersion := value;
end;

Function TFhirOperationDefinition.GetVersionST : String;
begin
  if FVersion = nil then
    result := ''
  else
    result := Version.value;
end;

Procedure TFhirOperationDefinition.SetVersionST(value : String);
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

Procedure TFhirOperationDefinition.SetTitle(value : TFhirString);
begin
  FTitle.free;
  FTitle := value;
end;

Function TFhirOperationDefinition.GetTitleST : String;
begin
  if FTitle = nil then
    result := ''
  else
    result := Title.value;
end;

Procedure TFhirOperationDefinition.SetTitleST(value : String);
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

Procedure TFhirOperationDefinition.SetPublisher(value : TFhirString);
begin
  FPublisher.free;
  FPublisher := value;
end;

Function TFhirOperationDefinition.GetPublisherST : String;
begin
  if FPublisher = nil then
    result := ''
  else
    result := Publisher.value;
end;

Procedure TFhirOperationDefinition.SetPublisherST(value : String);
begin
  if value <> '' then
  begin
    if FPublisher = nil then
      FPublisher := TFhirString.create;
    FPublisher.value := value
  end
  else if FPublisher <> nil then
    FPublisher.value := '';
end;

Procedure TFhirOperationDefinition.SetDescription(value : TFhirString);
begin
  FDescription.free;
  FDescription := value;
end;

Function TFhirOperationDefinition.GetDescriptionST : String;
begin
  if FDescription = nil then
    result := ''
  else
    result := Description.value;
end;

Procedure TFhirOperationDefinition.SetDescriptionST(value : String);
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

Procedure TFhirOperationDefinition.SetStatus(value : TFhirEnum);
begin
  FStatus.free;
  FStatus := value;
end;

Function TFhirOperationDefinition.GetStatusST : TFhirResourceProfileStatus;
begin
  if FStatus = nil then
    result := TFhirResourceProfileStatus(0)
  else
    result := TFhirResourceProfileStatus(StringArrayIndexOf(CODES_TFhirResourceProfileStatus, Status.value));
end;

Procedure TFhirOperationDefinition.SetStatusST(value : TFhirResourceProfileStatus);
begin
  if ord(value) = 0 then
    Status := nil
  else
    Status := TFhirEnum.create(CODES_TFhirResourceProfileStatus[value]);
end;

Procedure TFhirOperationDefinition.SetExperimental(value : TFhirBoolean);
begin
  FExperimental.free;
  FExperimental := value;
end;

Function TFhirOperationDefinition.GetExperimentalST : Boolean;
begin
  if FExperimental = nil then
    result := false
  else
    result := Experimental.value;
end;

Procedure TFhirOperationDefinition.SetExperimentalST(value : Boolean);
begin
  if FExperimental = nil then
    FExperimental := TFhirBoolean.create;
  FExperimental.value := value
end;

Procedure TFhirOperationDefinition.SetDate(value : TFhirDateTime);
begin
  FDate.free;
  FDate := value;
end;

Function TFhirOperationDefinition.GetDateST : TDateAndTime;
begin
  if FDate = nil then
    result := nil
  else
    result := Date.value;
end;

Procedure TFhirOperationDefinition.SetDateST(value : TDateAndTime);
begin
  if value <> nil then
  begin
    if FDate = nil then
      FDate := TFhirDateTime.create;
    FDate.value := value
  end
  else if FDate <> nil then
    FDate.value := nil;
end;

Procedure TFhirOperationDefinition.SetKind(value : TFhirEnum);
begin
  FKind.free;
  FKind := value;
end;

Function TFhirOperationDefinition.GetKindST : TFhirOperationKind;
begin
  if FKind = nil then
    result := TFhirOperationKind(0)
  else
    result := TFhirOperationKind(StringArrayIndexOf(CODES_TFhirOperationKind, Kind.value));
end;

Procedure TFhirOperationDefinition.SetKindST(value : TFhirOperationKind);
begin
  if ord(value) = 0 then
    Kind := nil
  else
    Kind := TFhirEnum.create(CODES_TFhirOperationKind[value]);
end;

Procedure TFhirOperationDefinition.SetName(value : TFhirCode);
begin
  FName.free;
  FName := value;
end;

Function TFhirOperationDefinition.GetNameST : String;
begin
  if FName = nil then
    result := ''
  else
    result := Name.value;
end;

Procedure TFhirOperationDefinition.SetNameST(value : String);
begin
  if value <> '' then
  begin
    if FName = nil then
      FName := TFhirCode.create;
    FName.value := value
  end
  else if FName <> nil then
    FName.value := '';
end;

Procedure TFhirOperationDefinition.SetNotes(value : TFhirString);
begin
  FNotes.free;
  FNotes := value;
end;

Function TFhirOperationDefinition.GetNotesST : String;
begin
  if FNotes = nil then
    result := ''
  else
    result := Notes.value;
end;

Procedure TFhirOperationDefinition.SetNotesST(value : String);
begin
  if value <> '' then
  begin
    if FNotes = nil then
      FNotes := TFhirString.create;
    FNotes.value := value
  end
  else if FNotes <> nil then
    FNotes.value := '';
end;

Procedure TFhirOperationDefinition.SetBase(value : TFhirResourceReference{TFhirOperationDefinition});
begin
  FBase.free;
  FBase := value;
end;

Procedure TFhirOperationDefinition.SetSystem(value : TFhirBoolean);
begin
  FSystem.free;
  FSystem := value;
end;

Function TFhirOperationDefinition.GetSystemST : Boolean;
begin
  if FSystem = nil then
    result := false
  else
    result := System.value;
end;

Procedure TFhirOperationDefinition.SetSystemST(value : Boolean);
begin
  if FSystem = nil then
    FSystem := TFhirBoolean.create;
  FSystem.value := value
end;

Procedure TFhirOperationDefinition.SetInstance(value : TFhirBoolean);
begin
  FInstance.free;
  FInstance := value;
end;

Function TFhirOperationDefinition.GetInstanceST : Boolean;
begin
  if FInstance = nil then
    result := false
  else
    result := Instance.value;
end;

Procedure TFhirOperationDefinition.SetInstanceST(value : Boolean);
begin
  if FInstance = nil then
    FInstance := TFhirBoolean.create;
  FInstance.value := value
end;


{ TFhirOperationOutcome }

constructor TFhirOperationOutcome.Create;
begin
  inherited;
  FIssueList := TFhirOperationOutcomeIssueList.Create;
end;

destructor TFhirOperationOutcome.Destroy;
begin
  FIssueList.Free;
  inherited;
end;

function TFhirOperationOutcome.GetResourceType : TFhirResourceType;
begin
  result := frtOperationOutcome;
end;

function TFhirOperationOutcome.GetHasASummary : Boolean;
begin
  result := false;
end;

procedure TFhirOperationOutcome.Assign(oSource : TAdvObject);
begin
  inherited;
  FIssueList.Assign(TFhirOperationOutcome(oSource).FIssueList);
end;

procedure TFhirOperationOutcome.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'issue') Then
     list.addAll(FIssueList);
end;

procedure TFhirOperationOutcome.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'issue', '', FIssueList.Link)){3};
end;

function TFhirOperationOutcome.Link : TFhirOperationOutcome;
begin
  result := TFhirOperationOutcome(inherited Link);
end;

function TFhirOperationOutcome.Clone : TFhirOperationOutcome;
begin
  result := TFhirOperationOutcome(inherited Clone);
end;

{ TFhirOperationOutcome }


{ TFhirOrder }

constructor TFhirOrder.Create;
begin
  inherited;
  FIdentifierList := TFhirIdentifierList.Create;
  FDetailList := TFhirResourceReferenceList{Resource}.Create;
end;

destructor TFhirOrder.Destroy;
begin
  FIdentifierList.Free;
  FDate.free;
  FSubject.free;
  FSource.free;
  FTarget.free;
  FReason.free;
  FAuthority.free;
  FWhen.free;
  FDetailList.Free;
  inherited;
end;

function TFhirOrder.GetResourceType : TFhirResourceType;
begin
  result := frtOrder;
end;

function TFhirOrder.GetHasASummary : Boolean;
begin
  result := false;
end;

procedure TFhirOrder.Assign(oSource : TAdvObject);
begin
  inherited;
  FIdentifierList.Assign(TFhirOrder(oSource).FIdentifierList);
  date := TFhirOrder(oSource).date.Clone;
  subject := TFhirOrder(oSource).subject.Clone;
  source := TFhirOrder(oSource).source.Clone;
  target := TFhirOrder(oSource).target.Clone;
  reason := TFhirOrder(oSource).reason.Clone;
  authority := TFhirOrder(oSource).authority.Clone;
  when := TFhirOrder(oSource).when.Clone;
  FDetailList.Assign(TFhirOrder(oSource).FDetailList);
end;

procedure TFhirOrder.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'identifier') Then
     list.addAll(FIdentifierList);
  if (child_name = 'date') Then
     list.add(Date.Link);
  if (child_name = 'subject') Then
     list.add(Subject.Link);
  if (child_name = 'source') Then
     list.add(Source.Link);
  if (child_name = 'target') Then
     list.add(Target.Link);
  if (child_name = 'reason') Then
     list.add(Reason.Link);
  if (child_name = 'authority') Then
     list.add(Authority.Link);
  if (child_name = 'when') Then
     list.add(When.Link);
  if (child_name = 'detail') Then
     list.addAll(FDetailList);
end;

procedure TFhirOrder.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'date', 'dateTime', FDate.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'subject', 'Resource(Patient)', FSubject.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'source', 'Resource(Practitioner)', FSource.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'target', 'Resource(Organization|Device|Practitioner)', FTarget.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'reason[x]', 'CodeableConcept|Resource(Any)', FReason.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'authority', 'Resource(Any)', FAuthority.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'when', '', FWhen.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'detail', 'Resource(Any)', FDetailList.Link)){3};
end;

function TFhirOrder.Link : TFhirOrder;
begin
  result := TFhirOrder(inherited Link);
end;

function TFhirOrder.Clone : TFhirOrder;
begin
  result := TFhirOrder(inherited Clone);
end;

{ TFhirOrder }

Procedure TFhirOrder.SetDate(value : TFhirDateTime);
begin
  FDate.free;
  FDate := value;
end;

Function TFhirOrder.GetDateST : TDateAndTime;
begin
  if FDate = nil then
    result := nil
  else
    result := Date.value;
end;

Procedure TFhirOrder.SetDateST(value : TDateAndTime);
begin
  if value <> nil then
  begin
    if FDate = nil then
      FDate := TFhirDateTime.create;
    FDate.value := value
  end
  else if FDate <> nil then
    FDate.value := nil;
end;

Procedure TFhirOrder.SetSubject(value : TFhirResourceReference{TFhirPatient});
begin
  FSubject.free;
  FSubject := value;
end;

Procedure TFhirOrder.SetSource(value : TFhirResourceReference{TFhirPractitioner});
begin
  FSource.free;
  FSource := value;
end;

Procedure TFhirOrder.SetTarget(value : TFhirResourceReference{Resource});
begin
  FTarget.free;
  FTarget := value;
end;

Procedure TFhirOrder.SetReason(value : TFhirType);
begin
  FReason.free;
  FReason := value;
end;

Procedure TFhirOrder.SetAuthority(value : TFhirResourceReference{Resource});
begin
  FAuthority.free;
  FAuthority := value;
end;

Procedure TFhirOrder.SetWhen(value : TFhirOrderWhen);
begin
  FWhen.free;
  FWhen := value;
end;


{ TFhirOrderResponse }

constructor TFhirOrderResponse.Create;
begin
  inherited;
  FIdentifierList := TFhirIdentifierList.Create;
  FFulfillmentList := TFhirResourceReferenceList{Resource}.Create;
end;

destructor TFhirOrderResponse.Destroy;
begin
  FIdentifierList.Free;
  FRequest.free;
  FDate.free;
  FWho.free;
  FAuthority.free;
  FCode.free;
  FDescription.free;
  FFulfillmentList.Free;
  inherited;
end;

function TFhirOrderResponse.GetResourceType : TFhirResourceType;
begin
  result := frtOrderResponse;
end;

function TFhirOrderResponse.GetHasASummary : Boolean;
begin
  result := false;
end;

procedure TFhirOrderResponse.Assign(oSource : TAdvObject);
begin
  inherited;
  FIdentifierList.Assign(TFhirOrderResponse(oSource).FIdentifierList);
  request := TFhirOrderResponse(oSource).request.Clone;
  date := TFhirOrderResponse(oSource).date.Clone;
  who := TFhirOrderResponse(oSource).who.Clone;
  authority := TFhirOrderResponse(oSource).authority.Clone;
  FCode := TFhirOrderResponse(oSource).FCode.Link;
  description := TFhirOrderResponse(oSource).description.Clone;
  FFulfillmentList.Assign(TFhirOrderResponse(oSource).FFulfillmentList);
end;

procedure TFhirOrderResponse.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'identifier') Then
     list.addAll(FIdentifierList);
  if (child_name = 'request') Then
     list.add(Request.Link);
  if (child_name = 'date') Then
     list.add(Date.Link);
  if (child_name = 'who') Then
     list.add(Who.Link);
  if (child_name = 'authority') Then
     list.add(Authority.Link);
  if (child_name = 'code') Then
     list.add(FCode.Link);
  if (child_name = 'description') Then
     list.add(Description.Link);
  if (child_name = 'fulfillment') Then
     list.addAll(FFulfillmentList);
end;

procedure TFhirOrderResponse.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'request', 'Resource(Order)', FRequest.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'date', 'dateTime', FDate.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'who', 'Resource(Practitioner|Organization|Device)', FWho.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'authority[x]', 'CodeableConcept|Resource(Any)', FAuthority.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'code', 'code', FCode.Link));{1}
  oList.add(TFHIRProperty.create(self, 'description', 'string', FDescription.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'fulfillment', 'Resource(Any)', FFulfillmentList.Link)){3};
end;

function TFhirOrderResponse.Link : TFhirOrderResponse;
begin
  result := TFhirOrderResponse(inherited Link);
end;

function TFhirOrderResponse.Clone : TFhirOrderResponse;
begin
  result := TFhirOrderResponse(inherited Clone);
end;

{ TFhirOrderResponse }

Procedure TFhirOrderResponse.SetRequest(value : TFhirResourceReference{TFhirOrder});
begin
  FRequest.free;
  FRequest := value;
end;

Procedure TFhirOrderResponse.SetDate(value : TFhirDateTime);
begin
  FDate.free;
  FDate := value;
end;

Function TFhirOrderResponse.GetDateST : TDateAndTime;
begin
  if FDate = nil then
    result := nil
  else
    result := Date.value;
end;

Procedure TFhirOrderResponse.SetDateST(value : TDateAndTime);
begin
  if value <> nil then
  begin
    if FDate = nil then
      FDate := TFhirDateTime.create;
    FDate.value := value
  end
  else if FDate <> nil then
    FDate.value := nil;
end;

Procedure TFhirOrderResponse.SetWho(value : TFhirResourceReference{Resource});
begin
  FWho.free;
  FWho := value;
end;

Procedure TFhirOrderResponse.SetAuthority(value : TFhirType);
begin
  FAuthority.free;
  FAuthority := value;
end;

Procedure TFhirOrderResponse.SetCode(value : TFhirEnum);
begin
  FCode.free;
  FCode := value;
end;

Function TFhirOrderResponse.GetCodeST : TFhirOrderOutcomeCode;
begin
  if FCode = nil then
    result := TFhirOrderOutcomeCode(0)
  else
    result := TFhirOrderOutcomeCode(StringArrayIndexOf(CODES_TFhirOrderOutcomeCode, Code.value));
end;

Procedure TFhirOrderResponse.SetCodeST(value : TFhirOrderOutcomeCode);
begin
  if ord(value) = 0 then
    Code := nil
  else
    Code := TFhirEnum.create(CODES_TFhirOrderOutcomeCode[value]);
end;

Procedure TFhirOrderResponse.SetDescription(value : TFhirString);
begin
  FDescription.free;
  FDescription := value;
end;

Function TFhirOrderResponse.GetDescriptionST : String;
begin
  if FDescription = nil then
    result := ''
  else
    result := Description.value;
end;

Procedure TFhirOrderResponse.SetDescriptionST(value : String);
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


{ TFhirOrganization }

constructor TFhirOrganization.Create;
begin
  inherited;
  FIdentifierList := TFhirIdentifierList.Create;
  FTelecomList := TFhirContactList.Create;
  FAddressList := TFhirAddressList.Create;
  FContactList := TFhirOrganizationContactList.Create;
  FLocationList := TFhirResourceReferenceList{TFhirLocation}.Create;
end;

destructor TFhirOrganization.Destroy;
begin
  FIdentifierList.Free;
  FName.free;
  FType_.free;
  FTelecomList.Free;
  FAddressList.Free;
  FPartOf.free;
  FContactList.Free;
  FLocationList.Free;
  FActive.free;
  inherited;
end;

function TFhirOrganization.GetResourceType : TFhirResourceType;
begin
  result := frtOrganization;
end;

function TFhirOrganization.GetHasASummary : Boolean;
begin
  result := false;
end;

procedure TFhirOrganization.Assign(oSource : TAdvObject);
begin
  inherited;
  FIdentifierList.Assign(TFhirOrganization(oSource).FIdentifierList);
  name := TFhirOrganization(oSource).name.Clone;
  type_ := TFhirOrganization(oSource).type_.Clone;
  FTelecomList.Assign(TFhirOrganization(oSource).FTelecomList);
  FAddressList.Assign(TFhirOrganization(oSource).FAddressList);
  partOf := TFhirOrganization(oSource).partOf.Clone;
  FContactList.Assign(TFhirOrganization(oSource).FContactList);
  FLocationList.Assign(TFhirOrganization(oSource).FLocationList);
  active := TFhirOrganization(oSource).active.Clone;
end;

procedure TFhirOrganization.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'identifier') Then
     list.addAll(FIdentifierList);
  if (child_name = 'name') Then
     list.add(Name.Link);
  if (child_name = 'type_') Then
     list.add(Type_.Link);
  if (child_name = 'telecom') Then
     list.addAll(FTelecomList);
  if (child_name = 'address') Then
     list.addAll(FAddressList);
  if (child_name = 'partOf') Then
     list.add(PartOf.Link);
  if (child_name = 'contact') Then
     list.addAll(FContactList);
  if (child_name = 'location') Then
     list.addAll(FLocationList);
  if (child_name = 'active') Then
     list.add(Active.Link);
end;

procedure TFhirOrganization.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'name', 'string', FName.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'type', 'CodeableConcept', FType_.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'telecom', 'Contact', FTelecomList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'address', 'Address', FAddressList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'partOf', 'Resource(Organization)', FPartOf.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'contact', '', FContactList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'location', 'Resource(Location)', FLocationList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'active', 'boolean', FActive.Link.Link));{2}
end;

function TFhirOrganization.Link : TFhirOrganization;
begin
  result := TFhirOrganization(inherited Link);
end;

function TFhirOrganization.Clone : TFhirOrganization;
begin
  result := TFhirOrganization(inherited Clone);
end;

{ TFhirOrganization }

Procedure TFhirOrganization.SetName(value : TFhirString);
begin
  FName.free;
  FName := value;
end;

Function TFhirOrganization.GetNameST : String;
begin
  if FName = nil then
    result := ''
  else
    result := Name.value;
end;

Procedure TFhirOrganization.SetNameST(value : String);
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

Procedure TFhirOrganization.SetType_(value : TFhirCodeableConcept);
begin
  FType_.free;
  FType_ := value;
end;

Procedure TFhirOrganization.SetPartOf(value : TFhirResourceReference{TFhirOrganization});
begin
  FPartOf.free;
  FPartOf := value;
end;

Procedure TFhirOrganization.SetActive(value : TFhirBoolean);
begin
  FActive.free;
  FActive := value;
end;

Function TFhirOrganization.GetActiveST : Boolean;
begin
  if FActive = nil then
    result := false
  else
    result := Active.value;
end;

Procedure TFhirOrganization.SetActiveST(value : Boolean);
begin
  if FActive = nil then
    FActive := TFhirBoolean.create;
  FActive.value := value
end;


{ TFhirOther }

constructor TFhirOther.Create;
begin
  inherited;
  FIdentifierList := TFhirIdentifierList.Create;
end;

destructor TFhirOther.Destroy;
begin
  FIdentifierList.Free;
  FCode.free;
  FSubject.free;
  FAuthor.free;
  FCreated.free;
  inherited;
end;

function TFhirOther.GetResourceType : TFhirResourceType;
begin
  result := frtOther;
end;

function TFhirOther.GetHasASummary : Boolean;
begin
  result := false;
end;

procedure TFhirOther.Assign(oSource : TAdvObject);
begin
  inherited;
  FIdentifierList.Assign(TFhirOther(oSource).FIdentifierList);
  code := TFhirOther(oSource).code.Clone;
  subject := TFhirOther(oSource).subject.Clone;
  author := TFhirOther(oSource).author.Clone;
  created := TFhirOther(oSource).created.Clone;
end;

procedure TFhirOther.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'identifier') Then
     list.addAll(FIdentifierList);
  if (child_name = 'code') Then
     list.add(Code.Link);
  if (child_name = 'subject') Then
     list.add(Subject.Link);
  if (child_name = 'author') Then
     list.add(Author.Link);
  if (child_name = 'created') Then
     list.add(Created.Link);
end;

procedure TFhirOther.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'code', 'CodeableConcept', FCode.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'subject', 'Resource(Any)', FSubject.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'author', 'Resource(Practitioner|Patient|RelatedPerson)', FAuthor.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'created', 'date', FCreated.Link.Link));{2}
end;

function TFhirOther.Link : TFhirOther;
begin
  result := TFhirOther(inherited Link);
end;

function TFhirOther.Clone : TFhirOther;
begin
  result := TFhirOther(inherited Clone);
end;

{ TFhirOther }

Procedure TFhirOther.SetCode(value : TFhirCodeableConcept);
begin
  FCode.free;
  FCode := value;
end;

Procedure TFhirOther.SetSubject(value : TFhirResourceReference{Resource});
begin
  FSubject.free;
  FSubject := value;
end;

Procedure TFhirOther.SetAuthor(value : TFhirResourceReference{Resource});
begin
  FAuthor.free;
  FAuthor := value;
end;

Procedure TFhirOther.SetCreated(value : TFhirDate);
begin
  FCreated.free;
  FCreated := value;
end;

Function TFhirOther.GetCreatedST : TDateAndTime;
begin
  if FCreated = nil then
    result := nil
  else
    result := Created.value;
end;

Procedure TFhirOther.SetCreatedST(value : TDateAndTime);
begin
  if value <> nil then
  begin
    if FCreated = nil then
      FCreated := TFhirDate.create;
    FCreated.value := value
  end
  else if FCreated <> nil then
    FCreated.value := nil;
end;


{ TFhirPatient }

constructor TFhirPatient.Create;
begin
  inherited;
  FIdentifierList := TFhirIdentifierList.Create;
  FNameList := TFhirHumanNameList.Create;
  FTelecomList := TFhirContactList.Create;
  FAddressList := TFhirAddressList.Create;
  FPhotoList := TFhirAttachmentList.Create;
  FContactList := TFhirPatientContactList.Create;
  FCommunicationList := TFhirCodeableConceptList.Create;
  FCareProviderList := TFhirResourceReferenceList{Resource}.Create;
  FLink_List := TFhirPatientLinkList.Create;
end;

destructor TFhirPatient.Destroy;
begin
  FIdentifierList.Free;
  FNameList.Free;
  FTelecomList.Free;
  FGender.free;
  FBirthDate.free;
  FDeceased.free;
  FAddressList.Free;
  FMaritalStatus.free;
  FMultipleBirth.free;
  FPhotoList.Free;
  FContactList.Free;
  FAnimal.free;
  FCommunicationList.Free;
  FCareProviderList.Free;
  FManagingOrganization.free;
  FLink_List.Free;
  FActive.free;
  inherited;
end;

function TFhirPatient.GetResourceType : TFhirResourceType;
begin
  result := frtPatient;
end;

function TFhirPatient.GetHasASummary : Boolean;
begin
  result := true;
end;

procedure TFhirPatient.Assign(oSource : TAdvObject);
begin
  inherited;
  FIdentifierList.Assign(TFhirPatient(oSource).FIdentifierList);
  FNameList.Assign(TFhirPatient(oSource).FNameList);
  FTelecomList.Assign(TFhirPatient(oSource).FTelecomList);
  FGender := TFhirPatient(oSource).FGender.Link;
  birthDate := TFhirPatient(oSource).birthDate.Clone;
  deceased := TFhirPatient(oSource).deceased.Clone;
  FAddressList.Assign(TFhirPatient(oSource).FAddressList);
  maritalStatus := TFhirPatient(oSource).maritalStatus.Clone;
  multipleBirth := TFhirPatient(oSource).multipleBirth.Clone;
  FPhotoList.Assign(TFhirPatient(oSource).FPhotoList);
  FContactList.Assign(TFhirPatient(oSource).FContactList);
  animal := TFhirPatient(oSource).animal.Clone;
  FCommunicationList.Assign(TFhirPatient(oSource).FCommunicationList);
  FCareProviderList.Assign(TFhirPatient(oSource).FCareProviderList);
  managingOrganization := TFhirPatient(oSource).managingOrganization.Clone;
  FLink_List.Assign(TFhirPatient(oSource).FLink_List);
  active := TFhirPatient(oSource).active.Clone;
end;

procedure TFhirPatient.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'identifier') Then
     list.addAll(FIdentifierList);
  if (child_name = 'name') Then
     list.addAll(FNameList);
  if (child_name = 'telecom') Then
     list.addAll(FTelecomList);
  if (child_name = 'gender') Then
     list.add(FGender.Link);
  if (child_name = 'birthDate') Then
     list.add(BirthDate.Link);
  if (child_name = 'deceased') Then
     list.add(Deceased.Link);
  if (child_name = 'address') Then
     list.addAll(FAddressList);
  if (child_name = 'maritalStatus') Then
     list.add(MaritalStatus.Link);
  if (child_name = 'multipleBirth') Then
     list.add(MultipleBirth.Link);
  if (child_name = 'photo') Then
     list.addAll(FPhotoList);
  if (child_name = 'contact') Then
     list.addAll(FContactList);
  if (child_name = 'animal') Then
     list.add(Animal.Link);
  if (child_name = 'communication') Then
     list.addAll(FCommunicationList);
  if (child_name = 'careProvider') Then
     list.addAll(FCareProviderList);
  if (child_name = 'managingOrganization') Then
     list.add(ManagingOrganization.Link);
  if (child_name = 'link_') Then
     list.addAll(FLink_List);
  if (child_name = 'active') Then
     list.add(Active.Link);
end;

procedure TFhirPatient.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'name', 'HumanName', FNameList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'telecom', 'Contact', FTelecomList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'gender', 'code', FGender.Link));{1}
  oList.add(TFHIRProperty.create(self, 'birthDate', 'dateTime', FBirthDate.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'deceased[x]', 'boolean|dateTime', FDeceased.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'address', 'Address', FAddressList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'maritalStatus', 'CodeableConcept', FMaritalStatus.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'multipleBirth[x]', 'boolean|integer', FMultipleBirth.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'photo', 'Attachment', FPhotoList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'contact', '', FContactList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'animal', '', FAnimal.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'communication', 'CodeableConcept', FCommunicationList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'careProvider', 'Resource(Organization|Practitioner)', FCareProviderList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'managingOrganization', 'Resource(Organization)', FManagingOrganization.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'link', '', FLink_List.Link)){3};
  oList.add(TFHIRProperty.create(self, 'active', 'boolean', FActive.Link.Link));{2}
end;

function TFhirPatient.Link : TFhirPatient;
begin
  result := TFhirPatient(inherited Link);
end;

function TFhirPatient.Clone : TFhirPatient;
begin
  result := TFhirPatient(inherited Clone);
end;

{ TFhirPatient }

Procedure TFhirPatient.SetGender(value : TFhirEnum);
begin
  FGender.free;
  FGender := value;
end;

Function TFhirPatient.GetGenderST : TFhirAdministrativeGender;
begin
  if FGender = nil then
    result := TFhirAdministrativeGender(0)
  else
    result := TFhirAdministrativeGender(StringArrayIndexOf(CODES_TFhirAdministrativeGender, Gender.value));
end;

Procedure TFhirPatient.SetGenderST(value : TFhirAdministrativeGender);
begin
  if ord(value) = 0 then
    Gender := nil
  else
    Gender := TFhirEnum.create(CODES_TFhirAdministrativeGender[value]);
end;

Procedure TFhirPatient.SetBirthDate(value : TFhirDateTime);
begin
  FBirthDate.free;
  FBirthDate := value;
end;

Function TFhirPatient.GetBirthDateST : TDateAndTime;
begin
  if FBirthDate = nil then
    result := nil
  else
    result := BirthDate.value;
end;

Procedure TFhirPatient.SetBirthDateST(value : TDateAndTime);
begin
  if value <> nil then
  begin
    if FBirthDate = nil then
      FBirthDate := TFhirDateTime.create;
    FBirthDate.value := value
  end
  else if FBirthDate <> nil then
    FBirthDate.value := nil;
end;

Procedure TFhirPatient.SetDeceased(value : TFhirType);
begin
  FDeceased.free;
  FDeceased := value;
end;

Procedure TFhirPatient.SetMaritalStatus(value : TFhirCodeableConcept);
begin
  FMaritalStatus.free;
  FMaritalStatus := value;
end;

Procedure TFhirPatient.SetMultipleBirth(value : TFhirType);
begin
  FMultipleBirth.free;
  FMultipleBirth := value;
end;

Procedure TFhirPatient.SetAnimal(value : TFhirPatientAnimal);
begin
  FAnimal.free;
  FAnimal := value;
end;

Procedure TFhirPatient.SetManagingOrganization(value : TFhirResourceReference{TFhirOrganization});
begin
  FManagingOrganization.free;
  FManagingOrganization := value;
end;

Procedure TFhirPatient.SetActive(value : TFhirBoolean);
begin
  FActive.free;
  FActive := value;
end;

Function TFhirPatient.GetActiveST : Boolean;
begin
  if FActive = nil then
    result := false
  else
    result := Active.value;
end;

Procedure TFhirPatient.SetActiveST(value : Boolean);
begin
  if FActive = nil then
    FActive := TFhirBoolean.create;
  FActive.value := value
end;


{ TFhirPractitioner }

constructor TFhirPractitioner.Create;
begin
  inherited;
  FIdentifierList := TFhirIdentifierList.Create;
  FTelecomList := TFhirContactList.Create;
  FAddressList := TFhirAddressList.Create;
  FPhotoList := TFhirAttachmentList.Create;
  FRoleList := TFhirCodeableConceptList.Create;
  FSpecialtyList := TFhirCodeableConceptList.Create;
  FLocationList := TFhirResourceReferenceList{TFhirLocation}.Create;
  FQualificationList := TFhirPractitionerQualificationList.Create;
  FCommunicationList := TFhirCodeableConceptList.Create;
end;

destructor TFhirPractitioner.Destroy;
begin
  FIdentifierList.Free;
  FName.free;
  FTelecomList.Free;
  FAddressList.Free;
  FGender.free;
  FBirthDate.free;
  FPhotoList.Free;
  FOrganization.free;
  FRoleList.Free;
  FSpecialtyList.Free;
  FPeriod.free;
  FLocationList.Free;
  FQualificationList.Free;
  FCommunicationList.Free;
  inherited;
end;

function TFhirPractitioner.GetResourceType : TFhirResourceType;
begin
  result := frtPractitioner;
end;

function TFhirPractitioner.GetHasASummary : Boolean;
begin
  result := true;
end;

procedure TFhirPractitioner.Assign(oSource : TAdvObject);
begin
  inherited;
  FIdentifierList.Assign(TFhirPractitioner(oSource).FIdentifierList);
  name := TFhirPractitioner(oSource).name.Clone;
  FTelecomList.Assign(TFhirPractitioner(oSource).FTelecomList);
  FAddressList.Assign(TFhirPractitioner(oSource).FAddressList);
  FGender := TFhirPractitioner(oSource).FGender.Link;
  birthDate := TFhirPractitioner(oSource).birthDate.Clone;
  FPhotoList.Assign(TFhirPractitioner(oSource).FPhotoList);
  organization := TFhirPractitioner(oSource).organization.Clone;
  FRoleList.Assign(TFhirPractitioner(oSource).FRoleList);
  FSpecialtyList.Assign(TFhirPractitioner(oSource).FSpecialtyList);
  period := TFhirPractitioner(oSource).period.Clone;
  FLocationList.Assign(TFhirPractitioner(oSource).FLocationList);
  FQualificationList.Assign(TFhirPractitioner(oSource).FQualificationList);
  FCommunicationList.Assign(TFhirPractitioner(oSource).FCommunicationList);
end;

procedure TFhirPractitioner.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'identifier') Then
     list.addAll(FIdentifierList);
  if (child_name = 'name') Then
     list.add(Name.Link);
  if (child_name = 'telecom') Then
     list.addAll(FTelecomList);
  if (child_name = 'address') Then
     list.addAll(FAddressList);
  if (child_name = 'gender') Then
     list.add(FGender.Link);
  if (child_name = 'birthDate') Then
     list.add(BirthDate.Link);
  if (child_name = 'photo') Then
     list.addAll(FPhotoList);
  if (child_name = 'organization') Then
     list.add(Organization.Link);
  if (child_name = 'role') Then
     list.addAll(FRoleList);
  if (child_name = 'specialty') Then
     list.addAll(FSpecialtyList);
  if (child_name = 'period') Then
     list.add(Period.Link);
  if (child_name = 'location') Then
     list.addAll(FLocationList);
  if (child_name = 'qualification') Then
     list.addAll(FQualificationList);
  if (child_name = 'communication') Then
     list.addAll(FCommunicationList);
end;

procedure TFhirPractitioner.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'name', 'HumanName', FName.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'telecom', 'Contact', FTelecomList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'address', 'Address', FAddressList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'gender', 'code', FGender.Link));{1}
  oList.add(TFHIRProperty.create(self, 'birthDate', 'dateTime', FBirthDate.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'photo', 'Attachment', FPhotoList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'organization', 'Resource(Organization)', FOrganization.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'role', 'CodeableConcept', FRoleList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'specialty', 'CodeableConcept', FSpecialtyList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'period', 'Period', FPeriod.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'location', 'Resource(Location)', FLocationList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'qualification', '', FQualificationList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'communication', 'CodeableConcept', FCommunicationList.Link)){3};
end;

function TFhirPractitioner.Link : TFhirPractitioner;
begin
  result := TFhirPractitioner(inherited Link);
end;

function TFhirPractitioner.Clone : TFhirPractitioner;
begin
  result := TFhirPractitioner(inherited Clone);
end;

{ TFhirPractitioner }

Procedure TFhirPractitioner.SetName(value : TFhirHumanName);
begin
  FName.free;
  FName := value;
end;

Procedure TFhirPractitioner.SetGender(value : TFhirEnum);
begin
  FGender.free;
  FGender := value;
end;

Function TFhirPractitioner.GetGenderST : TFhirAdministrativeGender;
begin
  if FGender = nil then
    result := TFhirAdministrativeGender(0)
  else
    result := TFhirAdministrativeGender(StringArrayIndexOf(CODES_TFhirAdministrativeGender, Gender.value));
end;

Procedure TFhirPractitioner.SetGenderST(value : TFhirAdministrativeGender);
begin
  if ord(value) = 0 then
    Gender := nil
  else
    Gender := TFhirEnum.create(CODES_TFhirAdministrativeGender[value]);
end;

Procedure TFhirPractitioner.SetBirthDate(value : TFhirDateTime);
begin
  FBirthDate.free;
  FBirthDate := value;
end;

Function TFhirPractitioner.GetBirthDateST : TDateAndTime;
begin
  if FBirthDate = nil then
    result := nil
  else
    result := BirthDate.value;
end;

Procedure TFhirPractitioner.SetBirthDateST(value : TDateAndTime);
begin
  if value <> nil then
  begin
    if FBirthDate = nil then
      FBirthDate := TFhirDateTime.create;
    FBirthDate.value := value
  end
  else if FBirthDate <> nil then
    FBirthDate.value := nil;
end;

Procedure TFhirPractitioner.SetOrganization(value : TFhirResourceReference{TFhirOrganization});
begin
  FOrganization.free;
  FOrganization := value;
end;

Procedure TFhirPractitioner.SetPeriod(value : TFhirPeriod);
begin
  FPeriod.free;
  FPeriod := value;
end;


{ TFhirProcedure }

constructor TFhirProcedure.Create;
begin
  inherited;
  FIdentifierList := TFhirIdentifierList.Create;
  FBodySiteList := TFhirCodeableConceptList.Create;
  FIndicationList := TFhirCodeableConceptList.Create;
  FPerformerList := TFhirProcedurePerformerList.Create;
  FReportList := TFhirResourceReferenceList{TFhirDiagnosticReport}.Create;
  FComplicationList := TFhirCodeableConceptList.Create;
  FRelatedItemList := TFhirProcedureRelatedItemList.Create;
end;

destructor TFhirProcedure.Destroy;
begin
  FIdentifierList.Free;
  FSubject.free;
  FType_.free;
  FBodySiteList.Free;
  FIndicationList.Free;
  FPerformerList.Free;
  FDate.free;
  FEncounter.free;
  FOutcome.free;
  FReportList.Free;
  FComplicationList.Free;
  FFollowUp.free;
  FRelatedItemList.Free;
  FNotes.free;
  inherited;
end;

function TFhirProcedure.GetResourceType : TFhirResourceType;
begin
  result := frtProcedure;
end;

function TFhirProcedure.GetHasASummary : Boolean;
begin
  result := true;
end;

procedure TFhirProcedure.Assign(oSource : TAdvObject);
begin
  inherited;
  FIdentifierList.Assign(TFhirProcedure(oSource).FIdentifierList);
  subject := TFhirProcedure(oSource).subject.Clone;
  type_ := TFhirProcedure(oSource).type_.Clone;
  FBodySiteList.Assign(TFhirProcedure(oSource).FBodySiteList);
  FIndicationList.Assign(TFhirProcedure(oSource).FIndicationList);
  FPerformerList.Assign(TFhirProcedure(oSource).FPerformerList);
  date := TFhirProcedure(oSource).date.Clone;
  encounter := TFhirProcedure(oSource).encounter.Clone;
  outcome := TFhirProcedure(oSource).outcome.Clone;
  FReportList.Assign(TFhirProcedure(oSource).FReportList);
  FComplicationList.Assign(TFhirProcedure(oSource).FComplicationList);
  followUp := TFhirProcedure(oSource).followUp.Clone;
  FRelatedItemList.Assign(TFhirProcedure(oSource).FRelatedItemList);
  notes := TFhirProcedure(oSource).notes.Clone;
end;

procedure TFhirProcedure.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'identifier') Then
     list.addAll(FIdentifierList);
  if (child_name = 'subject') Then
     list.add(Subject.Link);
  if (child_name = 'type_') Then
     list.add(Type_.Link);
  if (child_name = 'bodySite') Then
     list.addAll(FBodySiteList);
  if (child_name = 'indication') Then
     list.addAll(FIndicationList);
  if (child_name = 'performer') Then
     list.addAll(FPerformerList);
  if (child_name = 'date') Then
     list.add(Date.Link);
  if (child_name = 'encounter') Then
     list.add(Encounter.Link);
  if (child_name = 'outcome') Then
     list.add(Outcome.Link);
  if (child_name = 'report') Then
     list.addAll(FReportList);
  if (child_name = 'complication') Then
     list.addAll(FComplicationList);
  if (child_name = 'followUp') Then
     list.add(FollowUp.Link);
  if (child_name = 'relatedItem') Then
     list.addAll(FRelatedItemList);
  if (child_name = 'notes') Then
     list.add(Notes.Link);
end;

procedure TFhirProcedure.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'subject', 'Resource(Patient)', FSubject.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'type', 'CodeableConcept', FType_.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'bodySite', 'CodeableConcept', FBodySiteList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'indication', 'CodeableConcept', FIndicationList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'performer', '', FPerformerList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'date', 'Period', FDate.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'encounter', 'Resource(Encounter)', FEncounter.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'outcome', 'string', FOutcome.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'report', 'Resource(DiagnosticReport)', FReportList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'complication', 'CodeableConcept', FComplicationList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'followUp', 'string', FFollowUp.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'relatedItem', '', FRelatedItemList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'notes', 'string', FNotes.Link.Link));{2}
end;

function TFhirProcedure.Link : TFhirProcedure;
begin
  result := TFhirProcedure(inherited Link);
end;

function TFhirProcedure.Clone : TFhirProcedure;
begin
  result := TFhirProcedure(inherited Clone);
end;

{ TFhirProcedure }

Procedure TFhirProcedure.SetSubject(value : TFhirResourceReference{TFhirPatient});
begin
  FSubject.free;
  FSubject := value;
end;

Procedure TFhirProcedure.SetType_(value : TFhirCodeableConcept);
begin
  FType_.free;
  FType_ := value;
end;

Procedure TFhirProcedure.SetDate(value : TFhirPeriod);
begin
  FDate.free;
  FDate := value;
end;

Procedure TFhirProcedure.SetEncounter(value : TFhirResourceReference{TFhirEncounter});
begin
  FEncounter.free;
  FEncounter := value;
end;

Procedure TFhirProcedure.SetOutcome(value : TFhirString);
begin
  FOutcome.free;
  FOutcome := value;
end;

Function TFhirProcedure.GetOutcomeST : String;
begin
  if FOutcome = nil then
    result := ''
  else
    result := Outcome.value;
end;

Procedure TFhirProcedure.SetOutcomeST(value : String);
begin
  if value <> '' then
  begin
    if FOutcome = nil then
      FOutcome := TFhirString.create;
    FOutcome.value := value
  end
  else if FOutcome <> nil then
    FOutcome.value := '';
end;

Procedure TFhirProcedure.SetFollowUp(value : TFhirString);
begin
  FFollowUp.free;
  FFollowUp := value;
end;

Function TFhirProcedure.GetFollowUpST : String;
begin
  if FFollowUp = nil then
    result := ''
  else
    result := FollowUp.value;
end;

Procedure TFhirProcedure.SetFollowUpST(value : String);
begin
  if value <> '' then
  begin
    if FFollowUp = nil then
      FFollowUp := TFhirString.create;
    FFollowUp.value := value
  end
  else if FFollowUp <> nil then
    FFollowUp.value := '';
end;

Procedure TFhirProcedure.SetNotes(value : TFhirString);
begin
  FNotes.free;
  FNotes := value;
end;

Function TFhirProcedure.GetNotesST : String;
begin
  if FNotes = nil then
    result := ''
  else
    result := Notes.value;
end;

Procedure TFhirProcedure.SetNotesST(value : String);
begin
  if value <> '' then
  begin
    if FNotes = nil then
      FNotes := TFhirString.create;
    FNotes.value := value
  end
  else if FNotes <> nil then
    FNotes.value := '';
end;


{ TFhirProfile }

constructor TFhirProfile.Create;
begin
  inherited;
  FIdentifierList := TFhirIdentifierList.Create;
  FTelecomList := TFhirContactList.Create;
  FCodeList := TFhirCodingList.Create;
  FMappingList := TFhirProfileMappingList.Create;
  FStructureList := TFhirProfileStructureList.Create;
  FExtensionDefnList := TFhirProfileExtensionDefnList.Create;
end;

destructor TFhirProfile.Destroy;
begin
  FUrl.free;
  FIdentifierList.Free;
  FVersion.free;
  FName.free;
  FPublisher.free;
  FTelecomList.Free;
  FDescription.free;
  FCodeList.Free;
  FStatus.free;
  FExperimental.free;
  FDate.free;
  FRequirements.free;
  FFhirVersion.free;
  FMappingList.Free;
  FStructureList.Free;
  FExtensionDefnList.Free;
  inherited;
end;

function TFhirProfile.GetResourceType : TFhirResourceType;
begin
  result := frtProfile;
end;

function TFhirProfile.GetHasASummary : Boolean;
begin
  result := true;
end;

procedure TFhirProfile.Assign(oSource : TAdvObject);
begin
  inherited;
  url := TFhirProfile(oSource).url.Clone;
  FIdentifierList.Assign(TFhirProfile(oSource).FIdentifierList);
  version := TFhirProfile(oSource).version.Clone;
  name := TFhirProfile(oSource).name.Clone;
  publisher := TFhirProfile(oSource).publisher.Clone;
  FTelecomList.Assign(TFhirProfile(oSource).FTelecomList);
  description := TFhirProfile(oSource).description.Clone;
  FCodeList.Assign(TFhirProfile(oSource).FCodeList);
  FStatus := TFhirProfile(oSource).FStatus.Link;
  experimental := TFhirProfile(oSource).experimental.Clone;
  date := TFhirProfile(oSource).date.Clone;
  requirements := TFhirProfile(oSource).requirements.Clone;
  fhirVersion := TFhirProfile(oSource).fhirVersion.Clone;
  FMappingList.Assign(TFhirProfile(oSource).FMappingList);
  FStructureList.Assign(TFhirProfile(oSource).FStructureList);
  FExtensionDefnList.Assign(TFhirProfile(oSource).FExtensionDefnList);
end;

procedure TFhirProfile.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'url') Then
     list.add(Url.Link);
  if (child_name = 'identifier') Then
     list.addAll(FIdentifierList);
  if (child_name = 'version') Then
     list.add(Version.Link);
  if (child_name = 'name') Then
     list.add(Name.Link);
  if (child_name = 'publisher') Then
     list.add(Publisher.Link);
  if (child_name = 'telecom') Then
     list.addAll(FTelecomList);
  if (child_name = 'description') Then
     list.add(Description.Link);
  if (child_name = 'code') Then
     list.addAll(FCodeList);
  if (child_name = 'status') Then
     list.add(FStatus.Link);
  if (child_name = 'experimental') Then
     list.add(Experimental.Link);
  if (child_name = 'date') Then
     list.add(Date.Link);
  if (child_name = 'requirements') Then
     list.add(Requirements.Link);
  if (child_name = 'fhirVersion') Then
     list.add(FhirVersion.Link);
  if (child_name = 'mapping') Then
     list.addAll(FMappingList);
  if (child_name = 'structure') Then
     list.addAll(FStructureList);
  if (child_name = 'extensionDefn') Then
     list.addAll(FExtensionDefnList);
end;

procedure TFhirProfile.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'url', 'uri', FUrl.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'version', 'string', FVersion.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'name', 'string', FName.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'publisher', 'string', FPublisher.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'telecom', 'Contact', FTelecomList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'description', 'string', FDescription.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'code', 'Coding', FCodeList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'status', 'code', FStatus.Link));{1}
  oList.add(TFHIRProperty.create(self, 'experimental', 'boolean', FExperimental.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'date', 'dateTime', FDate.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'requirements', 'string', FRequirements.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'fhirVersion', 'id', FFhirVersion.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'mapping', '', FMappingList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'structure', '', FStructureList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'extensionDefn', '', FExtensionDefnList.Link)){3};
end;

function TFhirProfile.Link : TFhirProfile;
begin
  result := TFhirProfile(inherited Link);
end;

function TFhirProfile.Clone : TFhirProfile;
begin
  result := TFhirProfile(inherited Clone);
end;

{ TFhirProfile }

Procedure TFhirProfile.SetUrl(value : TFhirUri);
begin
  FUrl.free;
  FUrl := value;
end;

Function TFhirProfile.GetUrlST : String;
begin
  if FUrl = nil then
    result := ''
  else
    result := Url.value;
end;

Procedure TFhirProfile.SetUrlST(value : String);
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

Procedure TFhirProfile.SetVersion(value : TFhirString);
begin
  FVersion.free;
  FVersion := value;
end;

Function TFhirProfile.GetVersionST : String;
begin
  if FVersion = nil then
    result := ''
  else
    result := Version.value;
end;

Procedure TFhirProfile.SetVersionST(value : String);
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

Procedure TFhirProfile.SetName(value : TFhirString);
begin
  FName.free;
  FName := value;
end;

Function TFhirProfile.GetNameST : String;
begin
  if FName = nil then
    result := ''
  else
    result := Name.value;
end;

Procedure TFhirProfile.SetNameST(value : String);
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

Procedure TFhirProfile.SetPublisher(value : TFhirString);
begin
  FPublisher.free;
  FPublisher := value;
end;

Function TFhirProfile.GetPublisherST : String;
begin
  if FPublisher = nil then
    result := ''
  else
    result := Publisher.value;
end;

Procedure TFhirProfile.SetPublisherST(value : String);
begin
  if value <> '' then
  begin
    if FPublisher = nil then
      FPublisher := TFhirString.create;
    FPublisher.value := value
  end
  else if FPublisher <> nil then
    FPublisher.value := '';
end;

Procedure TFhirProfile.SetDescription(value : TFhirString);
begin
  FDescription.free;
  FDescription := value;
end;

Function TFhirProfile.GetDescriptionST : String;
begin
  if FDescription = nil then
    result := ''
  else
    result := Description.value;
end;

Procedure TFhirProfile.SetDescriptionST(value : String);
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

Procedure TFhirProfile.SetStatus(value : TFhirEnum);
begin
  FStatus.free;
  FStatus := value;
end;

Function TFhirProfile.GetStatusST : TFhirResourceProfileStatus;
begin
  if FStatus = nil then
    result := TFhirResourceProfileStatus(0)
  else
    result := TFhirResourceProfileStatus(StringArrayIndexOf(CODES_TFhirResourceProfileStatus, Status.value));
end;

Procedure TFhirProfile.SetStatusST(value : TFhirResourceProfileStatus);
begin
  if ord(value) = 0 then
    Status := nil
  else
    Status := TFhirEnum.create(CODES_TFhirResourceProfileStatus[value]);
end;

Procedure TFhirProfile.SetExperimental(value : TFhirBoolean);
begin
  FExperimental.free;
  FExperimental := value;
end;

Function TFhirProfile.GetExperimentalST : Boolean;
begin
  if FExperimental = nil then
    result := false
  else
    result := Experimental.value;
end;

Procedure TFhirProfile.SetExperimentalST(value : Boolean);
begin
  if FExperimental = nil then
    FExperimental := TFhirBoolean.create;
  FExperimental.value := value
end;

Procedure TFhirProfile.SetDate(value : TFhirDateTime);
begin
  FDate.free;
  FDate := value;
end;

Function TFhirProfile.GetDateST : TDateAndTime;
begin
  if FDate = nil then
    result := nil
  else
    result := Date.value;
end;

Procedure TFhirProfile.SetDateST(value : TDateAndTime);
begin
  if value <> nil then
  begin
    if FDate = nil then
      FDate := TFhirDateTime.create;
    FDate.value := value
  end
  else if FDate <> nil then
    FDate.value := nil;
end;

Procedure TFhirProfile.SetRequirements(value : TFhirString);
begin
  FRequirements.free;
  FRequirements := value;
end;

Function TFhirProfile.GetRequirementsST : String;
begin
  if FRequirements = nil then
    result := ''
  else
    result := Requirements.value;
end;

Procedure TFhirProfile.SetRequirementsST(value : String);
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

Procedure TFhirProfile.SetFhirVersion(value : TFhirId);
begin
  FFhirVersion.free;
  FFhirVersion := value;
end;

Function TFhirProfile.GetFhirVersionST : String;
begin
  if FFhirVersion = nil then
    result := ''
  else
    result := FhirVersion.value;
end;

Procedure TFhirProfile.SetFhirVersionST(value : String);
begin
  if value <> '' then
  begin
    if FFhirVersion = nil then
      FFhirVersion := TFhirId.create;
    FFhirVersion.value := value
  end
  else if FFhirVersion <> nil then
    FFhirVersion.value := '';
end;


{ TFhirProvenance }

constructor TFhirProvenance.Create;
begin
  inherited;
  FTargetList := TFhirResourceReferenceList{Resource}.Create;
  FPolicyList := TFhirUriList.Create;
  FAgentList := TFhirProvenanceAgentList.Create;
  FEntityList := TFhirProvenanceEntityList.Create;
end;

destructor TFhirProvenance.Destroy;
begin
  FTargetList.Free;
  FPeriod.free;
  FRecorded.free;
  FReason.free;
  FLocation.free;
  FPolicyList.Free;
  FAgentList.Free;
  FEntityList.Free;
  FIntegritySignature.free;
  inherited;
end;

function TFhirProvenance.GetResourceType : TFhirResourceType;
begin
  result := frtProvenance;
end;

function TFhirProvenance.GetHasASummary : Boolean;
begin
  result := false;
end;

procedure TFhirProvenance.Assign(oSource : TAdvObject);
begin
  inherited;
  FTargetList.Assign(TFhirProvenance(oSource).FTargetList);
  period := TFhirProvenance(oSource).period.Clone;
  recorded := TFhirProvenance(oSource).recorded.Clone;
  reason := TFhirProvenance(oSource).reason.Clone;
  location := TFhirProvenance(oSource).location.Clone;
  FPolicyList.Assign(TFhirProvenance(oSource).FPolicyList);
  FAgentList.Assign(TFhirProvenance(oSource).FAgentList);
  FEntityList.Assign(TFhirProvenance(oSource).FEntityList);
  integritySignature := TFhirProvenance(oSource).integritySignature.Clone;
end;

procedure TFhirProvenance.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'target') Then
     list.addAll(FTargetList);
  if (child_name = 'period') Then
     list.add(Period.Link);
  if (child_name = 'recorded') Then
     list.add(Recorded.Link);
  if (child_name = 'reason') Then
     list.add(Reason.Link);
  if (child_name = 'location') Then
     list.add(Location.Link);
  if (child_name = 'policy') Then
     list.addAll(FPolicyList);
  if (child_name = 'agent') Then
     list.addAll(FAgentList);
  if (child_name = 'entity') Then
     list.addAll(FEntityList);
  if (child_name = 'integritySignature') Then
     list.add(IntegritySignature.Link);
end;

procedure TFhirProvenance.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'target', 'Resource(Any)', FTargetList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'period', 'Period', FPeriod.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'recorded', 'instant', FRecorded.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'reason', 'CodeableConcept', FReason.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'location', 'Resource(Location)', FLocation.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'policy', 'uri', FPolicyList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'agent', '', FAgentList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'entity', '', FEntityList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'integritySignature', 'string', FIntegritySignature.Link.Link));{2}
end;

function TFhirProvenance.Link : TFhirProvenance;
begin
  result := TFhirProvenance(inherited Link);
end;

function TFhirProvenance.Clone : TFhirProvenance;
begin
  result := TFhirProvenance(inherited Clone);
end;

{ TFhirProvenance }

Procedure TFhirProvenance.SetPeriod(value : TFhirPeriod);
begin
  FPeriod.free;
  FPeriod := value;
end;

Procedure TFhirProvenance.SetRecorded(value : TFhirInstant);
begin
  FRecorded.free;
  FRecorded := value;
end;

Function TFhirProvenance.GetRecordedST : TDateAndTime;
begin
  if FRecorded = nil then
    result := nil
  else
    result := Recorded.value;
end;

Procedure TFhirProvenance.SetRecordedST(value : TDateAndTime);
begin
  if value <> nil then
  begin
    if FRecorded = nil then
      FRecorded := TFhirInstant.create;
    FRecorded.value := value
  end
  else if FRecorded <> nil then
    FRecorded.value := nil;
end;

Procedure TFhirProvenance.SetReason(value : TFhirCodeableConcept);
begin
  FReason.free;
  FReason := value;
end;

Procedure TFhirProvenance.SetLocation(value : TFhirResourceReference{TFhirLocation});
begin
  FLocation.free;
  FLocation := value;
end;

Procedure TFhirProvenance.SetIntegritySignature(value : TFhirString);
begin
  FIntegritySignature.free;
  FIntegritySignature := value;
end;

Function TFhirProvenance.GetIntegritySignatureST : String;
begin
  if FIntegritySignature = nil then
    result := ''
  else
    result := IntegritySignature.value;
end;

Procedure TFhirProvenance.SetIntegritySignatureST(value : String);
begin
  if value <> '' then
  begin
    if FIntegritySignature = nil then
      FIntegritySignature := TFhirString.create;
    FIntegritySignature.value := value
  end
  else if FIntegritySignature <> nil then
    FIntegritySignature.value := '';
end;


{ TFhirQuery }

constructor TFhirQuery.Create;
begin
  inherited;
  FParameterList := TFhirExtensionList.Create;
end;

destructor TFhirQuery.Destroy;
begin
  FIdentifier.free;
  FParameterList.Free;
  FResponse.free;
  inherited;
end;

function TFhirQuery.GetResourceType : TFhirResourceType;
begin
  result := frtQuery;
end;

function TFhirQuery.GetHasASummary : Boolean;
begin
  result := false;
end;

procedure TFhirQuery.Assign(oSource : TAdvObject);
begin
  inherited;
  identifier := TFhirQuery(oSource).identifier.Clone;
  FParameterList.Assign(TFhirQuery(oSource).FParameterList);
  response := TFhirQuery(oSource).response.Clone;
end;

procedure TFhirQuery.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'identifier') Then
     list.add(Identifier.Link);
  if (child_name = 'parameter') Then
     list.addAll(FParameterList);
  if (child_name = 'response') Then
     list.add(Response.Link);
end;

procedure TFhirQuery.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'uri', FIdentifier.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'parameter', 'Extension', FParameterList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'response', '', FResponse.Link.Link));{2}
end;

function TFhirQuery.Link : TFhirQuery;
begin
  result := TFhirQuery(inherited Link);
end;

function TFhirQuery.Clone : TFhirQuery;
begin
  result := TFhirQuery(inherited Clone);
end;

{ TFhirQuery }

Procedure TFhirQuery.SetIdentifier(value : TFhirUri);
begin
  FIdentifier.free;
  FIdentifier := value;
end;

Function TFhirQuery.GetIdentifierST : String;
begin
  if FIdentifier = nil then
    result := ''
  else
    result := Identifier.value;
end;

Procedure TFhirQuery.SetIdentifierST(value : String);
begin
  if value <> '' then
  begin
    if FIdentifier = nil then
      FIdentifier := TFhirUri.create;
    FIdentifier.value := value
  end
  else if FIdentifier <> nil then
    FIdentifier.value := '';
end;

Procedure TFhirQuery.SetResponse(value : TFhirQueryResponse);
begin
  FResponse.free;
  FResponse := value;
end;


{ TFhirQuestionnaire }

constructor TFhirQuestionnaire.Create;
begin
  inherited;
  FIdentifierList := TFhirIdentifierList.Create;
end;

destructor TFhirQuestionnaire.Destroy;
begin
  FIdentifierList.Free;
  FVersion.free;
  FStatus.free;
  FDate.free;
  FPublisher.free;
  FGroup.free;
  inherited;
end;

function TFhirQuestionnaire.GetResourceType : TFhirResourceType;
begin
  result := frtQuestionnaire;
end;

function TFhirQuestionnaire.GetHasASummary : Boolean;
begin
  result := true;
end;

procedure TFhirQuestionnaire.Assign(oSource : TAdvObject);
begin
  inherited;
  FIdentifierList.Assign(TFhirQuestionnaire(oSource).FIdentifierList);
  version := TFhirQuestionnaire(oSource).version.Clone;
  FStatus := TFhirQuestionnaire(oSource).FStatus.Link;
  date := TFhirQuestionnaire(oSource).date.Clone;
  publisher := TFhirQuestionnaire(oSource).publisher.Clone;
  group := TFhirQuestionnaire(oSource).group.Clone;
end;

procedure TFhirQuestionnaire.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'identifier') Then
     list.addAll(FIdentifierList);
  if (child_name = 'version') Then
     list.add(Version.Link);
  if (child_name = 'status') Then
     list.add(FStatus.Link);
  if (child_name = 'date') Then
     list.add(Date.Link);
  if (child_name = 'publisher') Then
     list.add(Publisher.Link);
  if (child_name = 'group') Then
     list.add(Group.Link);
end;

procedure TFhirQuestionnaire.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'version', 'string', FVersion.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'status', 'code', FStatus.Link));{1}
  oList.add(TFHIRProperty.create(self, 'date', 'dateTime', FDate.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'publisher', 'string', FPublisher.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'group', '', FGroup.Link.Link));{2}
end;

function TFhirQuestionnaire.Link : TFhirQuestionnaire;
begin
  result := TFhirQuestionnaire(inherited Link);
end;

function TFhirQuestionnaire.Clone : TFhirQuestionnaire;
begin
  result := TFhirQuestionnaire(inherited Clone);
end;

{ TFhirQuestionnaire }

Procedure TFhirQuestionnaire.SetVersion(value : TFhirString);
begin
  FVersion.free;
  FVersion := value;
end;

Function TFhirQuestionnaire.GetVersionST : String;
begin
  if FVersion = nil then
    result := ''
  else
    result := Version.value;
end;

Procedure TFhirQuestionnaire.SetVersionST(value : String);
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

Procedure TFhirQuestionnaire.SetStatus(value : TFhirEnum);
begin
  FStatus.free;
  FStatus := value;
end;

Function TFhirQuestionnaire.GetStatusST : TFhirQuestionnaireStatus;
begin
  if FStatus = nil then
    result := TFhirQuestionnaireStatus(0)
  else
    result := TFhirQuestionnaireStatus(StringArrayIndexOf(CODES_TFhirQuestionnaireStatus, Status.value));
end;

Procedure TFhirQuestionnaire.SetStatusST(value : TFhirQuestionnaireStatus);
begin
  if ord(value) = 0 then
    Status := nil
  else
    Status := TFhirEnum.create(CODES_TFhirQuestionnaireStatus[value]);
end;

Procedure TFhirQuestionnaire.SetDate(value : TFhirDateTime);
begin
  FDate.free;
  FDate := value;
end;

Function TFhirQuestionnaire.GetDateST : TDateAndTime;
begin
  if FDate = nil then
    result := nil
  else
    result := Date.value;
end;

Procedure TFhirQuestionnaire.SetDateST(value : TDateAndTime);
begin
  if value <> nil then
  begin
    if FDate = nil then
      FDate := TFhirDateTime.create;
    FDate.value := value
  end
  else if FDate <> nil then
    FDate.value := nil;
end;

Procedure TFhirQuestionnaire.SetPublisher(value : TFhirString);
begin
  FPublisher.free;
  FPublisher := value;
end;

Function TFhirQuestionnaire.GetPublisherST : String;
begin
  if FPublisher = nil then
    result := ''
  else
    result := Publisher.value;
end;

Procedure TFhirQuestionnaire.SetPublisherST(value : String);
begin
  if value <> '' then
  begin
    if FPublisher = nil then
      FPublisher := TFhirString.create;
    FPublisher.value := value
  end
  else if FPublisher <> nil then
    FPublisher.value := '';
end;

Procedure TFhirQuestionnaire.SetGroup(value : TFhirQuestionnaireGroup);
begin
  FGroup.free;
  FGroup := value;
end;


{ TFhirQuestionnaireAnswers }

constructor TFhirQuestionnaireAnswers.Create;
begin
  inherited;
end;

destructor TFhirQuestionnaireAnswers.Destroy;
begin
  FIdentifier.free;
  FQuestionnaire.free;
  FStatus.free;
  FSubject.free;
  FAuthor.free;
  FAuthored.free;
  FSource.free;
  FEncounter.free;
  FGroup.free;
  inherited;
end;

function TFhirQuestionnaireAnswers.GetResourceType : TFhirResourceType;
begin
  result := frtQuestionnaireAnswers;
end;

function TFhirQuestionnaireAnswers.GetHasASummary : Boolean;
begin
  result := true;
end;

procedure TFhirQuestionnaireAnswers.Assign(oSource : TAdvObject);
begin
  inherited;
  identifier := TFhirQuestionnaireAnswers(oSource).identifier.Clone;
  questionnaire := TFhirQuestionnaireAnswers(oSource).questionnaire.Clone;
  FStatus := TFhirQuestionnaireAnswers(oSource).FStatus.Link;
  subject := TFhirQuestionnaireAnswers(oSource).subject.Clone;
  author := TFhirQuestionnaireAnswers(oSource).author.Clone;
  authored := TFhirQuestionnaireAnswers(oSource).authored.Clone;
  source := TFhirQuestionnaireAnswers(oSource).source.Clone;
  encounter := TFhirQuestionnaireAnswers(oSource).encounter.Clone;
  group := TFhirQuestionnaireAnswers(oSource).group.Clone;
end;

procedure TFhirQuestionnaireAnswers.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'identifier') Then
     list.add(Identifier.Link);
  if (child_name = 'questionnaire') Then
     list.add(Questionnaire.Link);
  if (child_name = 'status') Then
     list.add(FStatus.Link);
  if (child_name = 'subject') Then
     list.add(Subject.Link);
  if (child_name = 'author') Then
     list.add(Author.Link);
  if (child_name = 'authored') Then
     list.add(Authored.Link);
  if (child_name = 'source') Then
     list.add(Source.Link);
  if (child_name = 'encounter') Then
     list.add(Encounter.Link);
  if (child_name = 'group') Then
     list.add(Group.Link);
end;

procedure TFhirQuestionnaireAnswers.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifier.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'questionnaire', 'Resource(Questionnaire)', FQuestionnaire.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'status', 'code', FStatus.Link));{1}
  oList.add(TFHIRProperty.create(self, 'subject', 'Resource(Any)', FSubject.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'author', 'Resource(Practitioner|Patient|RelatedPerson)', FAuthor.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'authored', 'dateTime', FAuthored.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'source', 'Resource(Patient|Practitioner|RelatedPerson)', FSource.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'encounter', 'Resource(Encounter)', FEncounter.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'group', '', FGroup.Link.Link));{2}
end;

function TFhirQuestionnaireAnswers.Link : TFhirQuestionnaireAnswers;
begin
  result := TFhirQuestionnaireAnswers(inherited Link);
end;

function TFhirQuestionnaireAnswers.Clone : TFhirQuestionnaireAnswers;
begin
  result := TFhirQuestionnaireAnswers(inherited Clone);
end;

{ TFhirQuestionnaireAnswers }

Procedure TFhirQuestionnaireAnswers.SetIdentifier(value : TFhirIdentifier);
begin
  FIdentifier.free;
  FIdentifier := value;
end;

Procedure TFhirQuestionnaireAnswers.SetQuestionnaire(value : TFhirResourceReference{TFhirQuestionnaire});
begin
  FQuestionnaire.free;
  FQuestionnaire := value;
end;

Procedure TFhirQuestionnaireAnswers.SetStatus(value : TFhirEnum);
begin
  FStatus.free;
  FStatus := value;
end;

Function TFhirQuestionnaireAnswers.GetStatusST : TFhirQuestionnaireAnswersStatus;
begin
  if FStatus = nil then
    result := TFhirQuestionnaireAnswersStatus(0)
  else
    result := TFhirQuestionnaireAnswersStatus(StringArrayIndexOf(CODES_TFhirQuestionnaireAnswersStatus, Status.value));
end;

Procedure TFhirQuestionnaireAnswers.SetStatusST(value : TFhirQuestionnaireAnswersStatus);
begin
  if ord(value) = 0 then
    Status := nil
  else
    Status := TFhirEnum.create(CODES_TFhirQuestionnaireAnswersStatus[value]);
end;

Procedure TFhirQuestionnaireAnswers.SetSubject(value : TFhirResourceReference{Resource});
begin
  FSubject.free;
  FSubject := value;
end;

Procedure TFhirQuestionnaireAnswers.SetAuthor(value : TFhirResourceReference{Resource});
begin
  FAuthor.free;
  FAuthor := value;
end;

Procedure TFhirQuestionnaireAnswers.SetAuthored(value : TFhirDateTime);
begin
  FAuthored.free;
  FAuthored := value;
end;

Function TFhirQuestionnaireAnswers.GetAuthoredST : TDateAndTime;
begin
  if FAuthored = nil then
    result := nil
  else
    result := Authored.value;
end;

Procedure TFhirQuestionnaireAnswers.SetAuthoredST(value : TDateAndTime);
begin
  if value <> nil then
  begin
    if FAuthored = nil then
      FAuthored := TFhirDateTime.create;
    FAuthored.value := value
  end
  else if FAuthored <> nil then
    FAuthored.value := nil;
end;

Procedure TFhirQuestionnaireAnswers.SetSource(value : TFhirResourceReference{Resource});
begin
  FSource.free;
  FSource := value;
end;

Procedure TFhirQuestionnaireAnswers.SetEncounter(value : TFhirResourceReference{TFhirEncounter});
begin
  FEncounter.free;
  FEncounter := value;
end;

Procedure TFhirQuestionnaireAnswers.SetGroup(value : TFhirQuestionnaireAnswersGroup);
begin
  FGroup.free;
  FGroup := value;
end;


{ TFhirReferralRequest }

constructor TFhirReferralRequest.Create;
begin
  inherited;
  FIdentifierList := TFhirIdentifierList.Create;
  FRecipientList := TFhirResourceReferenceList{Resource}.Create;
  FServiceRequestedList := TFhirCodeableConceptList.Create;
  FSupportingInformationList := TFhirResourceReferenceList{Resource}.Create;
end;

destructor TFhirReferralRequest.Destroy;
begin
  FStatus.free;
  FIdentifierList.Free;
  FType_.free;
  FSpecialty.free;
  FPriority.free;
  FSubject.free;
  FRequester.free;
  FRecipientList.Free;
  FEncounter.free;
  FDateSent.free;
  FReason.free;
  FDescription.free;
  FServiceRequestedList.Free;
  FSupportingInformationList.Free;
  FFulfillmentTime.free;
  inherited;
end;

function TFhirReferralRequest.GetResourceType : TFhirResourceType;
begin
  result := frtReferralRequest;
end;

function TFhirReferralRequest.GetHasASummary : Boolean;
begin
  result := true;
end;

procedure TFhirReferralRequest.Assign(oSource : TAdvObject);
begin
  inherited;
  FStatus := TFhirReferralRequest(oSource).FStatus.Link;
  FIdentifierList.Assign(TFhirReferralRequest(oSource).FIdentifierList);
  type_ := TFhirReferralRequest(oSource).type_.Clone;
  specialty := TFhirReferralRequest(oSource).specialty.Clone;
  priority := TFhirReferralRequest(oSource).priority.Clone;
  subject := TFhirReferralRequest(oSource).subject.Clone;
  requester := TFhirReferralRequest(oSource).requester.Clone;
  FRecipientList.Assign(TFhirReferralRequest(oSource).FRecipientList);
  encounter := TFhirReferralRequest(oSource).encounter.Clone;
  dateSent := TFhirReferralRequest(oSource).dateSent.Clone;
  reason := TFhirReferralRequest(oSource).reason.Clone;
  description := TFhirReferralRequest(oSource).description.Clone;
  FServiceRequestedList.Assign(TFhirReferralRequest(oSource).FServiceRequestedList);
  FSupportingInformationList.Assign(TFhirReferralRequest(oSource).FSupportingInformationList);
  fulfillmentTime := TFhirReferralRequest(oSource).fulfillmentTime.Clone;
end;

procedure TFhirReferralRequest.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'status') Then
     list.add(FStatus.Link);
  if (child_name = 'identifier') Then
     list.addAll(FIdentifierList);
  if (child_name = 'type_') Then
     list.add(Type_.Link);
  if (child_name = 'specialty') Then
     list.add(Specialty.Link);
  if (child_name = 'priority') Then
     list.add(Priority.Link);
  if (child_name = 'subject') Then
     list.add(Subject.Link);
  if (child_name = 'requester') Then
     list.add(Requester.Link);
  if (child_name = 'recipient') Then
     list.addAll(FRecipientList);
  if (child_name = 'encounter') Then
     list.add(Encounter.Link);
  if (child_name = 'dateSent') Then
     list.add(DateSent.Link);
  if (child_name = 'reason') Then
     list.add(Reason.Link);
  if (child_name = 'description') Then
     list.add(Description.Link);
  if (child_name = 'serviceRequested') Then
     list.addAll(FServiceRequestedList);
  if (child_name = 'supportingInformation') Then
     list.addAll(FSupportingInformationList);
  if (child_name = 'fulfillmentTime') Then
     list.add(FulfillmentTime.Link);
end;

procedure TFhirReferralRequest.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'status', 'code', FStatus.Link));{1}
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'type', 'CodeableConcept', FType_.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'specialty', 'CodeableConcept', FSpecialty.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'priority', 'CodeableConcept', FPriority.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'subject', 'Resource(Patient)', FSubject.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'requester', 'Resource(Practitioner|Organization|Patient)', FRequester.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'recipient', 'Resource(Practitioner|Organization)', FRecipientList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'encounter', 'Resource(Encounter)', FEncounter.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'dateSent', 'dateTime', FDateSent.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'reason', 'CodeableConcept', FReason.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'description', 'string', FDescription.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'serviceRequested', 'CodeableConcept', FServiceRequestedList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'supportingInformation', 'Resource(Any)', FSupportingInformationList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'fulfillmentTime', 'Period', FFulfillmentTime.Link.Link));{2}
end;

function TFhirReferralRequest.Link : TFhirReferralRequest;
begin
  result := TFhirReferralRequest(inherited Link);
end;

function TFhirReferralRequest.Clone : TFhirReferralRequest;
begin
  result := TFhirReferralRequest(inherited Clone);
end;

{ TFhirReferralRequest }

Procedure TFhirReferralRequest.SetStatus(value : TFhirEnum);
begin
  FStatus.free;
  FStatus := value;
end;

Function TFhirReferralRequest.GetStatusST : TFhirReferralstatus;
begin
  if FStatus = nil then
    result := TFhirReferralstatus(0)
  else
    result := TFhirReferralstatus(StringArrayIndexOf(CODES_TFhirReferralstatus, Status.value));
end;

Procedure TFhirReferralRequest.SetStatusST(value : TFhirReferralstatus);
begin
  if ord(value) = 0 then
    Status := nil
  else
    Status := TFhirEnum.create(CODES_TFhirReferralstatus[value]);
end;

Procedure TFhirReferralRequest.SetType_(value : TFhirCodeableConcept);
begin
  FType_.free;
  FType_ := value;
end;

Procedure TFhirReferralRequest.SetSpecialty(value : TFhirCodeableConcept);
begin
  FSpecialty.free;
  FSpecialty := value;
end;

Procedure TFhirReferralRequest.SetPriority(value : TFhirCodeableConcept);
begin
  FPriority.free;
  FPriority := value;
end;

Procedure TFhirReferralRequest.SetSubject(value : TFhirResourceReference{TFhirPatient});
begin
  FSubject.free;
  FSubject := value;
end;

Procedure TFhirReferralRequest.SetRequester(value : TFhirResourceReference{Resource});
begin
  FRequester.free;
  FRequester := value;
end;

Procedure TFhirReferralRequest.SetEncounter(value : TFhirResourceReference{TFhirEncounter});
begin
  FEncounter.free;
  FEncounter := value;
end;

Procedure TFhirReferralRequest.SetDateSent(value : TFhirDateTime);
begin
  FDateSent.free;
  FDateSent := value;
end;

Function TFhirReferralRequest.GetDateSentST : TDateAndTime;
begin
  if FDateSent = nil then
    result := nil
  else
    result := DateSent.value;
end;

Procedure TFhirReferralRequest.SetDateSentST(value : TDateAndTime);
begin
  if value <> nil then
  begin
    if FDateSent = nil then
      FDateSent := TFhirDateTime.create;
    FDateSent.value := value
  end
  else if FDateSent <> nil then
    FDateSent.value := nil;
end;

Procedure TFhirReferralRequest.SetReason(value : TFhirCodeableConcept);
begin
  FReason.free;
  FReason := value;
end;

Procedure TFhirReferralRequest.SetDescription(value : TFhirString);
begin
  FDescription.free;
  FDescription := value;
end;

Function TFhirReferralRequest.GetDescriptionST : String;
begin
  if FDescription = nil then
    result := ''
  else
    result := Description.value;
end;

Procedure TFhirReferralRequest.SetDescriptionST(value : String);
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

Procedure TFhirReferralRequest.SetFulfillmentTime(value : TFhirPeriod);
begin
  FFulfillmentTime.free;
  FFulfillmentTime := value;
end;


{ TFhirRelatedPerson }

constructor TFhirRelatedPerson.Create;
begin
  inherited;
  FIdentifierList := TFhirIdentifierList.Create;
  FTelecomList := TFhirContactList.Create;
  FPhotoList := TFhirAttachmentList.Create;
end;

destructor TFhirRelatedPerson.Destroy;
begin
  FIdentifierList.Free;
  FPatient.free;
  FRelationship.free;
  FName.free;
  FTelecomList.Free;
  FGender.free;
  FAddress.free;
  FPhotoList.Free;
  inherited;
end;

function TFhirRelatedPerson.GetResourceType : TFhirResourceType;
begin
  result := frtRelatedPerson;
end;

function TFhirRelatedPerson.GetHasASummary : Boolean;
begin
  result := true;
end;

procedure TFhirRelatedPerson.Assign(oSource : TAdvObject);
begin
  inherited;
  FIdentifierList.Assign(TFhirRelatedPerson(oSource).FIdentifierList);
  patient := TFhirRelatedPerson(oSource).patient.Clone;
  relationship := TFhirRelatedPerson(oSource).relationship.Clone;
  name := TFhirRelatedPerson(oSource).name.Clone;
  FTelecomList.Assign(TFhirRelatedPerson(oSource).FTelecomList);
  FGender := TFhirRelatedPerson(oSource).FGender.Link;
  address := TFhirRelatedPerson(oSource).address.Clone;
  FPhotoList.Assign(TFhirRelatedPerson(oSource).FPhotoList);
end;

procedure TFhirRelatedPerson.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'identifier') Then
     list.addAll(FIdentifierList);
  if (child_name = 'patient') Then
     list.add(Patient.Link);
  if (child_name = 'relationship') Then
     list.add(Relationship.Link);
  if (child_name = 'name') Then
     list.add(Name.Link);
  if (child_name = 'telecom') Then
     list.addAll(FTelecomList);
  if (child_name = 'gender') Then
     list.add(FGender.Link);
  if (child_name = 'address') Then
     list.add(Address.Link);
  if (child_name = 'photo') Then
     list.addAll(FPhotoList);
end;

procedure TFhirRelatedPerson.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'patient', 'Resource(Patient)', FPatient.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'relationship', 'CodeableConcept', FRelationship.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'name', 'HumanName', FName.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'telecom', 'Contact', FTelecomList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'gender', 'code', FGender.Link));{1}
  oList.add(TFHIRProperty.create(self, 'address', 'Address', FAddress.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'photo', 'Attachment', FPhotoList.Link)){3};
end;

function TFhirRelatedPerson.Link : TFhirRelatedPerson;
begin
  result := TFhirRelatedPerson(inherited Link);
end;

function TFhirRelatedPerson.Clone : TFhirRelatedPerson;
begin
  result := TFhirRelatedPerson(inherited Clone);
end;

{ TFhirRelatedPerson }

Procedure TFhirRelatedPerson.SetPatient(value : TFhirResourceReference{TFhirPatient});
begin
  FPatient.free;
  FPatient := value;
end;

Procedure TFhirRelatedPerson.SetRelationship(value : TFhirCodeableConcept);
begin
  FRelationship.free;
  FRelationship := value;
end;

Procedure TFhirRelatedPerson.SetName(value : TFhirHumanName);
begin
  FName.free;
  FName := value;
end;

Procedure TFhirRelatedPerson.SetGender(value : TFhirEnum);
begin
  FGender.free;
  FGender := value;
end;

Function TFhirRelatedPerson.GetGenderST : TFhirAdministrativeGender;
begin
  if FGender = nil then
    result := TFhirAdministrativeGender(0)
  else
    result := TFhirAdministrativeGender(StringArrayIndexOf(CODES_TFhirAdministrativeGender, Gender.value));
end;

Procedure TFhirRelatedPerson.SetGenderST(value : TFhirAdministrativeGender);
begin
  if ord(value) = 0 then
    Gender := nil
  else
    Gender := TFhirEnum.create(CODES_TFhirAdministrativeGender[value]);
end;

Procedure TFhirRelatedPerson.SetAddress(value : TFhirAddress);
begin
  FAddress.free;
  FAddress := value;
end;


{ TFhirRiskAssessment }

constructor TFhirRiskAssessment.Create;
begin
  inherited;
  FBasisList := TFhirResourceReferenceList{Resource}.Create;
  FPredictionList := TFhirRiskAssessmentPredictionList.Create;
end;

destructor TFhirRiskAssessment.Destroy;
begin
  FSubject.free;
  FDate.free;
  FCondition.free;
  FPerformer.free;
  FIdentifier.free;
  FMethod.free;
  FBasisList.Free;
  FPredictionList.Free;
  FMitigation.free;
  inherited;
end;

function TFhirRiskAssessment.GetResourceType : TFhirResourceType;
begin
  result := frtRiskAssessment;
end;

function TFhirRiskAssessment.GetHasASummary : Boolean;
begin
  result := true;
end;

procedure TFhirRiskAssessment.Assign(oSource : TAdvObject);
begin
  inherited;
  subject := TFhirRiskAssessment(oSource).subject.Clone;
  date := TFhirRiskAssessment(oSource).date.Clone;
  condition := TFhirRiskAssessment(oSource).condition.Clone;
  performer := TFhirRiskAssessment(oSource).performer.Clone;
  identifier := TFhirRiskAssessment(oSource).identifier.Clone;
  method := TFhirRiskAssessment(oSource).method.Clone;
  FBasisList.Assign(TFhirRiskAssessment(oSource).FBasisList);
  FPredictionList.Assign(TFhirRiskAssessment(oSource).FPredictionList);
  mitigation := TFhirRiskAssessment(oSource).mitigation.Clone;
end;

procedure TFhirRiskAssessment.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'subject') Then
     list.add(Subject.Link);
  if (child_name = 'date') Then
     list.add(Date.Link);
  if (child_name = 'condition') Then
     list.add(Condition.Link);
  if (child_name = 'performer') Then
     list.add(Performer.Link);
  if (child_name = 'identifier') Then
     list.add(Identifier.Link);
  if (child_name = 'method') Then
     list.add(Method.Link);
  if (child_name = 'basis') Then
     list.addAll(FBasisList);
  if (child_name = 'prediction') Then
     list.addAll(FPredictionList);
  if (child_name = 'mitigation') Then
     list.add(Mitigation.Link);
end;

procedure TFhirRiskAssessment.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'subject', 'Resource(Patient|Group)', FSubject.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'date', 'dateTime', FDate.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'condition', 'Resource(Condition)', FCondition.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'performer', 'Resource(Practitioner|Device)', FPerformer.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifier.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'method', 'CodeableConcept', FMethod.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'basis', 'Resource(Any)', FBasisList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'prediction', '', FPredictionList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'mitigation', 'string', FMitigation.Link.Link));{2}
end;

function TFhirRiskAssessment.Link : TFhirRiskAssessment;
begin
  result := TFhirRiskAssessment(inherited Link);
end;

function TFhirRiskAssessment.Clone : TFhirRiskAssessment;
begin
  result := TFhirRiskAssessment(inherited Clone);
end;

{ TFhirRiskAssessment }

Procedure TFhirRiskAssessment.SetSubject(value : TFhirResourceReference{Resource});
begin
  FSubject.free;
  FSubject := value;
end;

Procedure TFhirRiskAssessment.SetDate(value : TFhirDateTime);
begin
  FDate.free;
  FDate := value;
end;

Function TFhirRiskAssessment.GetDateST : TDateAndTime;
begin
  if FDate = nil then
    result := nil
  else
    result := Date.value;
end;

Procedure TFhirRiskAssessment.SetDateST(value : TDateAndTime);
begin
  if value <> nil then
  begin
    if FDate = nil then
      FDate := TFhirDateTime.create;
    FDate.value := value
  end
  else if FDate <> nil then
    FDate.value := nil;
end;

Procedure TFhirRiskAssessment.SetCondition(value : TFhirResourceReference{TFhirCondition});
begin
  FCondition.free;
  FCondition := value;
end;

Procedure TFhirRiskAssessment.SetPerformer(value : TFhirResourceReference{Resource});
begin
  FPerformer.free;
  FPerformer := value;
end;

Procedure TFhirRiskAssessment.SetIdentifier(value : TFhirIdentifier);
begin
  FIdentifier.free;
  FIdentifier := value;
end;

Procedure TFhirRiskAssessment.SetMethod(value : TFhirCodeableConcept);
begin
  FMethod.free;
  FMethod := value;
end;

Procedure TFhirRiskAssessment.SetMitigation(value : TFhirString);
begin
  FMitigation.free;
  FMitigation := value;
end;

Function TFhirRiskAssessment.GetMitigationST : String;
begin
  if FMitigation = nil then
    result := ''
  else
    result := Mitigation.value;
end;

Procedure TFhirRiskAssessment.SetMitigationST(value : String);
begin
  if value <> '' then
  begin
    if FMitigation = nil then
      FMitigation := TFhirString.create;
    FMitigation.value := value
  end
  else if FMitigation <> nil then
    FMitigation.value := '';
end;


{ TFhirSecurityEvent }

constructor TFhirSecurityEvent.Create;
begin
  inherited;
  FParticipantList := TFhirSecurityEventParticipantList.Create;
  FObject_List := TFhirSecurityEventObjectList.Create;
end;

destructor TFhirSecurityEvent.Destroy;
begin
  FEvent.free;
  FParticipantList.Free;
  FSource.free;
  FObject_List.Free;
  inherited;
end;

function TFhirSecurityEvent.GetResourceType : TFhirResourceType;
begin
  result := frtSecurityEvent;
end;

function TFhirSecurityEvent.GetHasASummary : Boolean;
begin
  result := false;
end;

procedure TFhirSecurityEvent.Assign(oSource : TAdvObject);
begin
  inherited;
  event := TFhirSecurityEvent(oSource).event.Clone;
  FParticipantList.Assign(TFhirSecurityEvent(oSource).FParticipantList);
  source := TFhirSecurityEvent(oSource).source.Clone;
  FObject_List.Assign(TFhirSecurityEvent(oSource).FObject_List);
end;

procedure TFhirSecurityEvent.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'event') Then
     list.add(Event.Link);
  if (child_name = 'participant') Then
     list.addAll(FParticipantList);
  if (child_name = 'source') Then
     list.add(Source.Link);
  if (child_name = 'object_') Then
     list.addAll(FObject_List);
end;

procedure TFhirSecurityEvent.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'event', '', FEvent.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'participant', '', FParticipantList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'source', '', FSource.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'object', '', FObject_List.Link)){3};
end;

function TFhirSecurityEvent.Link : TFhirSecurityEvent;
begin
  result := TFhirSecurityEvent(inherited Link);
end;

function TFhirSecurityEvent.Clone : TFhirSecurityEvent;
begin
  result := TFhirSecurityEvent(inherited Clone);
end;

{ TFhirSecurityEvent }

Procedure TFhirSecurityEvent.SetEvent(value : TFhirSecurityEventEvent);
begin
  FEvent.free;
  FEvent := value;
end;

Procedure TFhirSecurityEvent.SetSource(value : TFhirSecurityEventSource);
begin
  FSource.free;
  FSource := value;
end;


{ TFhirSlot }

constructor TFhirSlot.Create;
begin
  inherited;
  FIdentifierList := TFhirIdentifierList.Create;
end;

destructor TFhirSlot.Destroy;
begin
  FIdentifierList.Free;
  FType_.free;
  FAvailability.free;
  FFreeBusyType.free;
  FStart.free;
  FEnd_.free;
  FOverbooked.free;
  FComment.free;
  FLastModified.free;
  inherited;
end;

function TFhirSlot.GetResourceType : TFhirResourceType;
begin
  result := frtSlot;
end;

function TFhirSlot.GetHasASummary : Boolean;
begin
  result := false;
end;

procedure TFhirSlot.Assign(oSource : TAdvObject);
begin
  inherited;
  FIdentifierList.Assign(TFhirSlot(oSource).FIdentifierList);
  type_ := TFhirSlot(oSource).type_.Clone;
  availability := TFhirSlot(oSource).availability.Clone;
  FFreeBusyType := TFhirSlot(oSource).FFreeBusyType.Link;
  start := TFhirSlot(oSource).start.Clone;
  end_ := TFhirSlot(oSource).end_.Clone;
  overbooked := TFhirSlot(oSource).overbooked.Clone;
  comment := TFhirSlot(oSource).comment.Clone;
  lastModified := TFhirSlot(oSource).lastModified.Clone;
end;

procedure TFhirSlot.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'identifier') Then
     list.addAll(FIdentifierList);
  if (child_name = 'type_') Then
     list.add(Type_.Link);
  if (child_name = 'availability') Then
     list.add(Availability.Link);
  if (child_name = 'freeBusyType') Then
     list.add(FFreeBusyType.Link);
  if (child_name = 'start') Then
     list.add(Start.Link);
  if (child_name = 'end_') Then
     list.add(End_.Link);
  if (child_name = 'overbooked') Then
     list.add(Overbooked.Link);
  if (child_name = 'comment') Then
     list.add(Comment.Link);
  if (child_name = 'lastModified') Then
     list.add(LastModified.Link);
end;

procedure TFhirSlot.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'type', 'CodeableConcept', FType_.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'availability', 'Resource(Availability)', FAvailability.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'freeBusyType', 'code', FFreeBusyType.Link));{1}
  oList.add(TFHIRProperty.create(self, 'start', 'instant', FStart.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'end', 'instant', FEnd_.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'overbooked', 'boolean', FOverbooked.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'comment', 'string', FComment.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'lastModified', 'dateTime', FLastModified.Link.Link));{2}
end;

function TFhirSlot.Link : TFhirSlot;
begin
  result := TFhirSlot(inherited Link);
end;

function TFhirSlot.Clone : TFhirSlot;
begin
  result := TFhirSlot(inherited Clone);
end;

{ TFhirSlot }

Procedure TFhirSlot.SetType_(value : TFhirCodeableConcept);
begin
  FType_.free;
  FType_ := value;
end;

Procedure TFhirSlot.SetAvailability(value : TFhirResourceReference{TFhirAvailability});
begin
  FAvailability.free;
  FAvailability := value;
end;

Procedure TFhirSlot.SetFreeBusyType(value : TFhirEnum);
begin
  FFreeBusyType.free;
  FFreeBusyType := value;
end;

Function TFhirSlot.GetFreeBusyTypeST : TFhirSlotstatus;
begin
  if FFreeBusyType = nil then
    result := TFhirSlotstatus(0)
  else
    result := TFhirSlotstatus(StringArrayIndexOf(CODES_TFhirSlotstatus, FreeBusyType.value));
end;

Procedure TFhirSlot.SetFreeBusyTypeST(value : TFhirSlotstatus);
begin
  if ord(value) = 0 then
    FreeBusyType := nil
  else
    FreeBusyType := TFhirEnum.create(CODES_TFhirSlotstatus[value]);
end;

Procedure TFhirSlot.SetStart(value : TFhirInstant);
begin
  FStart.free;
  FStart := value;
end;

Function TFhirSlot.GetStartST : TDateAndTime;
begin
  if FStart = nil then
    result := nil
  else
    result := Start.value;
end;

Procedure TFhirSlot.SetStartST(value : TDateAndTime);
begin
  if value <> nil then
  begin
    if FStart = nil then
      FStart := TFhirInstant.create;
    FStart.value := value
  end
  else if FStart <> nil then
    FStart.value := nil;
end;

Procedure TFhirSlot.SetEnd_(value : TFhirInstant);
begin
  FEnd_.free;
  FEnd_ := value;
end;

Function TFhirSlot.GetEnd_ST : TDateAndTime;
begin
  if FEnd_ = nil then
    result := nil
  else
    result := End_.value;
end;

Procedure TFhirSlot.SetEnd_ST(value : TDateAndTime);
begin
  if value <> nil then
  begin
    if FEnd_ = nil then
      FEnd_ := TFhirInstant.create;
    FEnd_.value := value
  end
  else if FEnd_ <> nil then
    FEnd_.value := nil;
end;

Procedure TFhirSlot.SetOverbooked(value : TFhirBoolean);
begin
  FOverbooked.free;
  FOverbooked := value;
end;

Function TFhirSlot.GetOverbookedST : Boolean;
begin
  if FOverbooked = nil then
    result := false
  else
    result := Overbooked.value;
end;

Procedure TFhirSlot.SetOverbookedST(value : Boolean);
begin
  if FOverbooked = nil then
    FOverbooked := TFhirBoolean.create;
  FOverbooked.value := value
end;

Procedure TFhirSlot.SetComment(value : TFhirString);
begin
  FComment.free;
  FComment := value;
end;

Function TFhirSlot.GetCommentST : String;
begin
  if FComment = nil then
    result := ''
  else
    result := Comment.value;
end;

Procedure TFhirSlot.SetCommentST(value : String);
begin
  if value <> '' then
  begin
    if FComment = nil then
      FComment := TFhirString.create;
    FComment.value := value
  end
  else if FComment <> nil then
    FComment.value := '';
end;

Procedure TFhirSlot.SetLastModified(value : TFhirDateTime);
begin
  FLastModified.free;
  FLastModified := value;
end;

Function TFhirSlot.GetLastModifiedST : TDateAndTime;
begin
  if FLastModified = nil then
    result := nil
  else
    result := LastModified.value;
end;

Procedure TFhirSlot.SetLastModifiedST(value : TDateAndTime);
begin
  if value <> nil then
  begin
    if FLastModified = nil then
      FLastModified := TFhirDateTime.create;
    FLastModified.value := value
  end
  else if FLastModified <> nil then
    FLastModified.value := nil;
end;


{ TFhirSpecimen }

constructor TFhirSpecimen.Create;
begin
  inherited;
  FIdentifierList := TFhirIdentifierList.Create;
  FSourceList := TFhirSpecimenSourceList.Create;
  FTreatmentList := TFhirSpecimenTreatmentList.Create;
  FContainerList := TFhirSpecimenContainerList.Create;
end;

destructor TFhirSpecimen.Destroy;
begin
  FIdentifierList.Free;
  FType_.free;
  FSourceList.Free;
  FSubject.free;
  FAccessionIdentifier.free;
  FReceivedTime.free;
  FCollection.free;
  FTreatmentList.Free;
  FContainerList.Free;
  inherited;
end;

function TFhirSpecimen.GetResourceType : TFhirResourceType;
begin
  result := frtSpecimen;
end;

function TFhirSpecimen.GetHasASummary : Boolean;
begin
  result := false;
end;

procedure TFhirSpecimen.Assign(oSource : TAdvObject);
begin
  inherited;
  FIdentifierList.Assign(TFhirSpecimen(oSource).FIdentifierList);
  type_ := TFhirSpecimen(oSource).type_.Clone;
  FSourceList.Assign(TFhirSpecimen(oSource).FSourceList);
  subject := TFhirSpecimen(oSource).subject.Clone;
  accessionIdentifier := TFhirSpecimen(oSource).accessionIdentifier.Clone;
  receivedTime := TFhirSpecimen(oSource).receivedTime.Clone;
  collection := TFhirSpecimen(oSource).collection.Clone;
  FTreatmentList.Assign(TFhirSpecimen(oSource).FTreatmentList);
  FContainerList.Assign(TFhirSpecimen(oSource).FContainerList);
end;

procedure TFhirSpecimen.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'identifier') Then
     list.addAll(FIdentifierList);
  if (child_name = 'type_') Then
     list.add(Type_.Link);
  if (child_name = 'source') Then
     list.addAll(FSourceList);
  if (child_name = 'subject') Then
     list.add(Subject.Link);
  if (child_name = 'accessionIdentifier') Then
     list.add(AccessionIdentifier.Link);
  if (child_name = 'receivedTime') Then
     list.add(ReceivedTime.Link);
  if (child_name = 'collection') Then
     list.add(Collection.Link);
  if (child_name = 'treatment') Then
     list.addAll(FTreatmentList);
  if (child_name = 'container') Then
     list.addAll(FContainerList);
end;

procedure TFhirSpecimen.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'type', 'CodeableConcept', FType_.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'source', '', FSourceList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'subject', 'Resource(Patient|Group|Device|Substance)', FSubject.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'accessionIdentifier', 'Identifier', FAccessionIdentifier.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'receivedTime', 'dateTime', FReceivedTime.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'collection', '', FCollection.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'treatment', '', FTreatmentList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'container', '', FContainerList.Link)){3};
end;

function TFhirSpecimen.Link : TFhirSpecimen;
begin
  result := TFhirSpecimen(inherited Link);
end;

function TFhirSpecimen.Clone : TFhirSpecimen;
begin
  result := TFhirSpecimen(inherited Clone);
end;

{ TFhirSpecimen }

Procedure TFhirSpecimen.SetType_(value : TFhirCodeableConcept);
begin
  FType_.free;
  FType_ := value;
end;

Procedure TFhirSpecimen.SetSubject(value : TFhirResourceReference{Resource});
begin
  FSubject.free;
  FSubject := value;
end;

Procedure TFhirSpecimen.SetAccessionIdentifier(value : TFhirIdentifier);
begin
  FAccessionIdentifier.free;
  FAccessionIdentifier := value;
end;

Procedure TFhirSpecimen.SetReceivedTime(value : TFhirDateTime);
begin
  FReceivedTime.free;
  FReceivedTime := value;
end;

Function TFhirSpecimen.GetReceivedTimeST : TDateAndTime;
begin
  if FReceivedTime = nil then
    result := nil
  else
    result := ReceivedTime.value;
end;

Procedure TFhirSpecimen.SetReceivedTimeST(value : TDateAndTime);
begin
  if value <> nil then
  begin
    if FReceivedTime = nil then
      FReceivedTime := TFhirDateTime.create;
    FReceivedTime.value := value
  end
  else if FReceivedTime <> nil then
    FReceivedTime.value := nil;
end;

Procedure TFhirSpecimen.SetCollection(value : TFhirSpecimenCollection);
begin
  FCollection.free;
  FCollection := value;
end;


{ TFhirSubscription }

constructor TFhirSubscription.Create;
begin
  inherited;
  FContactList := TFhirContactList.Create;
  FTagList := TFhirSubscriptionTagList.Create;
end;

destructor TFhirSubscription.Destroy;
begin
  FCriteria.free;
  FContactList.Free;
  FReason.free;
  FStatus.free;
  FError.free;
  FChannel.free;
  FEnd_.free;
  FTagList.Free;
  inherited;
end;

function TFhirSubscription.GetResourceType : TFhirResourceType;
begin
  result := frtSubscription;
end;

function TFhirSubscription.GetHasASummary : Boolean;
begin
  result := false;
end;

procedure TFhirSubscription.Assign(oSource : TAdvObject);
begin
  inherited;
  criteria := TFhirSubscription(oSource).criteria.Clone;
  FContactList.Assign(TFhirSubscription(oSource).FContactList);
  reason := TFhirSubscription(oSource).reason.Clone;
  FStatus := TFhirSubscription(oSource).FStatus.Link;
  error := TFhirSubscription(oSource).error.Clone;
  channel := TFhirSubscription(oSource).channel.Clone;
  end_ := TFhirSubscription(oSource).end_.Clone;
  FTagList.Assign(TFhirSubscription(oSource).FTagList);
end;

procedure TFhirSubscription.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'criteria') Then
     list.add(Criteria.Link);
  if (child_name = 'contact') Then
     list.addAll(FContactList);
  if (child_name = 'reason') Then
     list.add(Reason.Link);
  if (child_name = 'status') Then
     list.add(FStatus.Link);
  if (child_name = 'error') Then
     list.add(Error.Link);
  if (child_name = 'channel') Then
     list.add(Channel.Link);
  if (child_name = 'end_') Then
     list.add(End_.Link);
  if (child_name = 'tag') Then
     list.addAll(FTagList);
end;

procedure TFhirSubscription.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'criteria', 'string', FCriteria.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'contact', 'Contact', FContactList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'reason', 'string', FReason.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'status', 'code', FStatus.Link));{1}
  oList.add(TFHIRProperty.create(self, 'error', 'string', FError.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'channel', '', FChannel.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'end', 'instant', FEnd_.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'tag', '', FTagList.Link)){3};
end;

function TFhirSubscription.Link : TFhirSubscription;
begin
  result := TFhirSubscription(inherited Link);
end;

function TFhirSubscription.Clone : TFhirSubscription;
begin
  result := TFhirSubscription(inherited Clone);
end;

{ TFhirSubscription }

Procedure TFhirSubscription.SetCriteria(value : TFhirString);
begin
  FCriteria.free;
  FCriteria := value;
end;

Function TFhirSubscription.GetCriteriaST : String;
begin
  if FCriteria = nil then
    result := ''
  else
    result := Criteria.value;
end;

Procedure TFhirSubscription.SetCriteriaST(value : String);
begin
  if value <> '' then
  begin
    if FCriteria = nil then
      FCriteria := TFhirString.create;
    FCriteria.value := value
  end
  else if FCriteria <> nil then
    FCriteria.value := '';
end;

Procedure TFhirSubscription.SetReason(value : TFhirString);
begin
  FReason.free;
  FReason := value;
end;

Function TFhirSubscription.GetReasonST : String;
begin
  if FReason = nil then
    result := ''
  else
    result := Reason.value;
end;

Procedure TFhirSubscription.SetReasonST(value : String);
begin
  if value <> '' then
  begin
    if FReason = nil then
      FReason := TFhirString.create;
    FReason.value := value
  end
  else if FReason <> nil then
    FReason.value := '';
end;

Procedure TFhirSubscription.SetStatus(value : TFhirEnum);
begin
  FStatus.free;
  FStatus := value;
end;

Function TFhirSubscription.GetStatusST : TFhirSubscriptionStatus;
begin
  if FStatus = nil then
    result := TFhirSubscriptionStatus(0)
  else
    result := TFhirSubscriptionStatus(StringArrayIndexOf(CODES_TFhirSubscriptionStatus, Status.value));
end;

Procedure TFhirSubscription.SetStatusST(value : TFhirSubscriptionStatus);
begin
  if ord(value) = 0 then
    Status := nil
  else
    Status := TFhirEnum.create(CODES_TFhirSubscriptionStatus[value]);
end;

Procedure TFhirSubscription.SetError(value : TFhirString);
begin
  FError.free;
  FError := value;
end;

Function TFhirSubscription.GetErrorST : String;
begin
  if FError = nil then
    result := ''
  else
    result := Error.value;
end;

Procedure TFhirSubscription.SetErrorST(value : String);
begin
  if value <> '' then
  begin
    if FError = nil then
      FError := TFhirString.create;
    FError.value := value
  end
  else if FError <> nil then
    FError.value := '';
end;

Procedure TFhirSubscription.SetChannel(value : TFhirSubscriptionChannel);
begin
  FChannel.free;
  FChannel := value;
end;

Procedure TFhirSubscription.SetEnd_(value : TFhirInstant);
begin
  FEnd_.free;
  FEnd_ := value;
end;

Function TFhirSubscription.GetEnd_ST : TDateAndTime;
begin
  if FEnd_ = nil then
    result := nil
  else
    result := End_.value;
end;

Procedure TFhirSubscription.SetEnd_ST(value : TDateAndTime);
begin
  if value <> nil then
  begin
    if FEnd_ = nil then
      FEnd_ := TFhirInstant.create;
    FEnd_.value := value
  end
  else if FEnd_ <> nil then
    FEnd_.value := nil;
end;


{ TFhirSubstance }

constructor TFhirSubstance.Create;
begin
  inherited;
  FIngredientList := TFhirSubstanceIngredientList.Create;
end;

destructor TFhirSubstance.Destroy;
begin
  FType_.free;
  FDescription.free;
  FInstance.free;
  FIngredientList.Free;
  inherited;
end;

function TFhirSubstance.GetResourceType : TFhirResourceType;
begin
  result := frtSubstance;
end;

function TFhirSubstance.GetHasASummary : Boolean;
begin
  result := false;
end;

procedure TFhirSubstance.Assign(oSource : TAdvObject);
begin
  inherited;
  type_ := TFhirSubstance(oSource).type_.Clone;
  description := TFhirSubstance(oSource).description.Clone;
  instance := TFhirSubstance(oSource).instance.Clone;
  FIngredientList.Assign(TFhirSubstance(oSource).FIngredientList);
end;

procedure TFhirSubstance.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'type_') Then
     list.add(Type_.Link);
  if (child_name = 'description') Then
     list.add(Description.Link);
  if (child_name = 'instance') Then
     list.add(Instance.Link);
  if (child_name = 'ingredient') Then
     list.addAll(FIngredientList);
end;

procedure TFhirSubstance.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'type', 'CodeableConcept', FType_.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'description', 'string', FDescription.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'instance', '', FInstance.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'ingredient', '', FIngredientList.Link)){3};
end;

function TFhirSubstance.Link : TFhirSubstance;
begin
  result := TFhirSubstance(inherited Link);
end;

function TFhirSubstance.Clone : TFhirSubstance;
begin
  result := TFhirSubstance(inherited Clone);
end;

{ TFhirSubstance }

Procedure TFhirSubstance.SetType_(value : TFhirCodeableConcept);
begin
  FType_.free;
  FType_ := value;
end;

Procedure TFhirSubstance.SetDescription(value : TFhirString);
begin
  FDescription.free;
  FDescription := value;
end;

Function TFhirSubstance.GetDescriptionST : String;
begin
  if FDescription = nil then
    result := ''
  else
    result := Description.value;
end;

Procedure TFhirSubstance.SetDescriptionST(value : String);
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

Procedure TFhirSubstance.SetInstance(value : TFhirSubstanceInstance);
begin
  FInstance.free;
  FInstance := value;
end;


{ TFhirSupply }

constructor TFhirSupply.Create;
begin
  inherited;
  FDispenseList := TFhirSupplyDispenseList.Create;
end;

destructor TFhirSupply.Destroy;
begin
  FKind.free;
  FIdentifier.free;
  FStatus.free;
  FOrderedItem.free;
  FPatient.free;
  FDispenseList.Free;
  inherited;
end;

function TFhirSupply.GetResourceType : TFhirResourceType;
begin
  result := frtSupply;
end;

function TFhirSupply.GetHasASummary : Boolean;
begin
  result := false;
end;

procedure TFhirSupply.Assign(oSource : TAdvObject);
begin
  inherited;
  kind := TFhirSupply(oSource).kind.Clone;
  identifier := TFhirSupply(oSource).identifier.Clone;
  FStatus := TFhirSupply(oSource).FStatus.Link;
  orderedItem := TFhirSupply(oSource).orderedItem.Clone;
  patient := TFhirSupply(oSource).patient.Clone;
  FDispenseList.Assign(TFhirSupply(oSource).FDispenseList);
end;

procedure TFhirSupply.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'kind') Then
     list.add(Kind.Link);
  if (child_name = 'identifier') Then
     list.add(Identifier.Link);
  if (child_name = 'status') Then
     list.add(FStatus.Link);
  if (child_name = 'orderedItem') Then
     list.add(OrderedItem.Link);
  if (child_name = 'patient') Then
     list.add(Patient.Link);
  if (child_name = 'dispense') Then
     list.addAll(FDispenseList);
end;

procedure TFhirSupply.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'kind', 'CodeableConcept', FKind.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifier.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'status', 'code', FStatus.Link));{1}
  oList.add(TFHIRProperty.create(self, 'orderedItem', 'Resource(Medication|Substance|Device)', FOrderedItem.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'patient', 'Resource(Patient)', FPatient.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'dispense', '', FDispenseList.Link)){3};
end;

function TFhirSupply.Link : TFhirSupply;
begin
  result := TFhirSupply(inherited Link);
end;

function TFhirSupply.Clone : TFhirSupply;
begin
  result := TFhirSupply(inherited Clone);
end;

{ TFhirSupply }

Procedure TFhirSupply.SetKind(value : TFhirCodeableConcept);
begin
  FKind.free;
  FKind := value;
end;

Procedure TFhirSupply.SetIdentifier(value : TFhirIdentifier);
begin
  FIdentifier.free;
  FIdentifier := value;
end;

Procedure TFhirSupply.SetStatus(value : TFhirEnum);
begin
  FStatus.free;
  FStatus := value;
end;

Function TFhirSupply.GetStatusST : TFhirValuesetSupplyStatus;
begin
  if FStatus = nil then
    result := TFhirValuesetSupplyStatus(0)
  else
    result := TFhirValuesetSupplyStatus(StringArrayIndexOf(CODES_TFhirValuesetSupplyStatus, Status.value));
end;

Procedure TFhirSupply.SetStatusST(value : TFhirValuesetSupplyStatus);
begin
  if ord(value) = 0 then
    Status := nil
  else
    Status := TFhirEnum.create(CODES_TFhirValuesetSupplyStatus[value]);
end;

Procedure TFhirSupply.SetOrderedItem(value : TFhirResourceReference{Resource});
begin
  FOrderedItem.free;
  FOrderedItem := value;
end;

Procedure TFhirSupply.SetPatient(value : TFhirResourceReference{TFhirPatient});
begin
  FPatient.free;
  FPatient := value;
end;


{ TFhirValueSet }

constructor TFhirValueSet.Create;
begin
  inherited;
  FTelecomList := TFhirContactList.Create;
end;

destructor TFhirValueSet.Destroy;
begin
  FIdentifier.free;
  FVersion.free;
  FName.free;
  FPurpose.free;
  FImmutable.free;
  FPublisher.free;
  FTelecomList.Free;
  FDescription.free;
  FCopyright.free;
  FStatus.free;
  FExperimental.free;
  FExtensible.free;
  FDate.free;
  FStableDate.free;
  FDefine.free;
  FCompose.free;
  FExpansion.free;
  inherited;
end;

function TFhirValueSet.GetResourceType : TFhirResourceType;
begin
  result := frtValueSet;
end;

function TFhirValueSet.GetHasASummary : Boolean;
begin
  result := true;
end;

procedure TFhirValueSet.Assign(oSource : TAdvObject);
begin
  inherited;
  identifier := TFhirValueSet(oSource).identifier.Clone;
  version := TFhirValueSet(oSource).version.Clone;
  name := TFhirValueSet(oSource).name.Clone;
  purpose := TFhirValueSet(oSource).purpose.Clone;
  immutable := TFhirValueSet(oSource).immutable.Clone;
  publisher := TFhirValueSet(oSource).publisher.Clone;
  FTelecomList.Assign(TFhirValueSet(oSource).FTelecomList);
  description := TFhirValueSet(oSource).description.Clone;
  copyright := TFhirValueSet(oSource).copyright.Clone;
  FStatus := TFhirValueSet(oSource).FStatus.Link;
  experimental := TFhirValueSet(oSource).experimental.Clone;
  extensible := TFhirValueSet(oSource).extensible.Clone;
  date := TFhirValueSet(oSource).date.Clone;
  stableDate := TFhirValueSet(oSource).stableDate.Clone;
  define := TFhirValueSet(oSource).define.Clone;
  compose := TFhirValueSet(oSource).compose.Clone;
  expansion := TFhirValueSet(oSource).expansion.Clone;
end;

procedure TFhirValueSet.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'identifier') Then
     list.add(Identifier.Link);
  if (child_name = 'version') Then
     list.add(Version.Link);
  if (child_name = 'name') Then
     list.add(Name.Link);
  if (child_name = 'purpose') Then
     list.add(Purpose.Link);
  if (child_name = 'immutable') Then
     list.add(Immutable.Link);
  if (child_name = 'publisher') Then
     list.add(Publisher.Link);
  if (child_name = 'telecom') Then
     list.addAll(FTelecomList);
  if (child_name = 'description') Then
     list.add(Description.Link);
  if (child_name = 'copyright') Then
     list.add(Copyright.Link);
  if (child_name = 'status') Then
     list.add(FStatus.Link);
  if (child_name = 'experimental') Then
     list.add(Experimental.Link);
  if (child_name = 'extensible') Then
     list.add(Extensible.Link);
  if (child_name = 'date') Then
     list.add(Date.Link);
  if (child_name = 'stableDate') Then
     list.add(StableDate.Link);
  if (child_name = 'define') Then
     list.add(Define.Link);
  if (child_name = 'compose') Then
     list.add(Compose.Link);
  if (child_name = 'expansion') Then
     list.add(Expansion.Link);
end;

procedure TFhirValueSet.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'string', FIdentifier.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'version', 'string', FVersion.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'name', 'string', FName.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'purpose', 'string', FPurpose.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'immutable', 'boolean', FImmutable.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'publisher', 'string', FPublisher.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'telecom', 'Contact', FTelecomList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'description', 'string', FDescription.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'copyright', 'string', FCopyright.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'status', 'code', FStatus.Link));{1}
  oList.add(TFHIRProperty.create(self, 'experimental', 'boolean', FExperimental.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'extensible', 'boolean', FExtensible.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'date', 'dateTime', FDate.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'stableDate', 'date', FStableDate.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'define', '', FDefine.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'compose', '', FCompose.Link.Link));{2}
  oList.add(TFHIRProperty.create(self, 'expansion', '', FExpansion.Link.Link));{2}
end;

function TFhirValueSet.Link : TFhirValueSet;
begin
  result := TFhirValueSet(inherited Link);
end;

function TFhirValueSet.Clone : TFhirValueSet;
begin
  result := TFhirValueSet(inherited Clone);
end;

{ TFhirValueSet }

Procedure TFhirValueSet.SetIdentifier(value : TFhirString);
begin
  FIdentifier.free;
  FIdentifier := value;
end;

Function TFhirValueSet.GetIdentifierST : String;
begin
  if FIdentifier = nil then
    result := ''
  else
    result := Identifier.value;
end;

Procedure TFhirValueSet.SetIdentifierST(value : String);
begin
  if value <> '' then
  begin
    if FIdentifier = nil then
      FIdentifier := TFhirString.create;
    FIdentifier.value := value
  end
  else if FIdentifier <> nil then
    FIdentifier.value := '';
end;

Procedure TFhirValueSet.SetVersion(value : TFhirString);
begin
  FVersion.free;
  FVersion := value;
end;

Function TFhirValueSet.GetVersionST : String;
begin
  if FVersion = nil then
    result := ''
  else
    result := Version.value;
end;

Procedure TFhirValueSet.SetVersionST(value : String);
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

Procedure TFhirValueSet.SetName(value : TFhirString);
begin
  FName.free;
  FName := value;
end;

Function TFhirValueSet.GetNameST : String;
begin
  if FName = nil then
    result := ''
  else
    result := Name.value;
end;

Procedure TFhirValueSet.SetNameST(value : String);
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

Procedure TFhirValueSet.SetPurpose(value : TFhirString);
begin
  FPurpose.free;
  FPurpose := value;
end;

Function TFhirValueSet.GetPurposeST : String;
begin
  if FPurpose = nil then
    result := ''
  else
    result := Purpose.value;
end;

Procedure TFhirValueSet.SetPurposeST(value : String);
begin
  if value <> '' then
  begin
    if FPurpose = nil then
      FPurpose := TFhirString.create;
    FPurpose.value := value
  end
  else if FPurpose <> nil then
    FPurpose.value := '';
end;

Procedure TFhirValueSet.SetImmutable(value : TFhirBoolean);
begin
  FImmutable.free;
  FImmutable := value;
end;

Function TFhirValueSet.GetImmutableST : Boolean;
begin
  if FImmutable = nil then
    result := false
  else
    result := Immutable.value;
end;

Procedure TFhirValueSet.SetImmutableST(value : Boolean);
begin
  if FImmutable = nil then
    FImmutable := TFhirBoolean.create;
  FImmutable.value := value
end;

Procedure TFhirValueSet.SetPublisher(value : TFhirString);
begin
  FPublisher.free;
  FPublisher := value;
end;

Function TFhirValueSet.GetPublisherST : String;
begin
  if FPublisher = nil then
    result := ''
  else
    result := Publisher.value;
end;

Procedure TFhirValueSet.SetPublisherST(value : String);
begin
  if value <> '' then
  begin
    if FPublisher = nil then
      FPublisher := TFhirString.create;
    FPublisher.value := value
  end
  else if FPublisher <> nil then
    FPublisher.value := '';
end;

Procedure TFhirValueSet.SetDescription(value : TFhirString);
begin
  FDescription.free;
  FDescription := value;
end;

Function TFhirValueSet.GetDescriptionST : String;
begin
  if FDescription = nil then
    result := ''
  else
    result := Description.value;
end;

Procedure TFhirValueSet.SetDescriptionST(value : String);
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

Procedure TFhirValueSet.SetCopyright(value : TFhirString);
begin
  FCopyright.free;
  FCopyright := value;
end;

Function TFhirValueSet.GetCopyrightST : String;
begin
  if FCopyright = nil then
    result := ''
  else
    result := Copyright.value;
end;

Procedure TFhirValueSet.SetCopyrightST(value : String);
begin
  if value <> '' then
  begin
    if FCopyright = nil then
      FCopyright := TFhirString.create;
    FCopyright.value := value
  end
  else if FCopyright <> nil then
    FCopyright.value := '';
end;

Procedure TFhirValueSet.SetStatus(value : TFhirEnum);
begin
  FStatus.free;
  FStatus := value;
end;

Function TFhirValueSet.GetStatusST : TFhirValuesetStatus;
begin
  if FStatus = nil then
    result := TFhirValuesetStatus(0)
  else
    result := TFhirValuesetStatus(StringArrayIndexOf(CODES_TFhirValuesetStatus, Status.value));
end;

Procedure TFhirValueSet.SetStatusST(value : TFhirValuesetStatus);
begin
  if ord(value) = 0 then
    Status := nil
  else
    Status := TFhirEnum.create(CODES_TFhirValuesetStatus[value]);
end;

Procedure TFhirValueSet.SetExperimental(value : TFhirBoolean);
begin
  FExperimental.free;
  FExperimental := value;
end;

Function TFhirValueSet.GetExperimentalST : Boolean;
begin
  if FExperimental = nil then
    result := false
  else
    result := Experimental.value;
end;

Procedure TFhirValueSet.SetExperimentalST(value : Boolean);
begin
  if FExperimental = nil then
    FExperimental := TFhirBoolean.create;
  FExperimental.value := value
end;

Procedure TFhirValueSet.SetExtensible(value : TFhirBoolean);
begin
  FExtensible.free;
  FExtensible := value;
end;

Function TFhirValueSet.GetExtensibleST : Boolean;
begin
  if FExtensible = nil then
    result := false
  else
    result := Extensible.value;
end;

Procedure TFhirValueSet.SetExtensibleST(value : Boolean);
begin
  if FExtensible = nil then
    FExtensible := TFhirBoolean.create;
  FExtensible.value := value
end;

Procedure TFhirValueSet.SetDate(value : TFhirDateTime);
begin
  FDate.free;
  FDate := value;
end;

Function TFhirValueSet.GetDateST : TDateAndTime;
begin
  if FDate = nil then
    result := nil
  else
    result := Date.value;
end;

Procedure TFhirValueSet.SetDateST(value : TDateAndTime);
begin
  if value <> nil then
  begin
    if FDate = nil then
      FDate := TFhirDateTime.create;
    FDate.value := value
  end
  else if FDate <> nil then
    FDate.value := nil;
end;

Procedure TFhirValueSet.SetStableDate(value : TFhirDate);
begin
  FStableDate.free;
  FStableDate := value;
end;

Function TFhirValueSet.GetStableDateST : TDateAndTime;
begin
  if FStableDate = nil then
    result := nil
  else
    result := StableDate.value;
end;

Procedure TFhirValueSet.SetStableDateST(value : TDateAndTime);
begin
  if value <> nil then
  begin
    if FStableDate = nil then
      FStableDate := TFhirDate.create;
    FStableDate.value := value
  end
  else if FStableDate <> nil then
    FStableDate.value := nil;
end;

Procedure TFhirValueSet.SetDefine(value : TFhirValueSetDefine);
begin
  FDefine.free;
  FDefine := value;
end;

Procedure TFhirValueSet.SetCompose(value : TFhirValueSetCompose);
begin
  FCompose.free;
  FCompose := value;
end;

Procedure TFhirValueSet.SetExpansion(value : TFhirValueSetExpansion);
begin
  FExpansion.free;
  FExpansion := value;
end;


function TFhirResourceFactory.newEnum : TFhirEnum;
begin
  result := TFhirEnum.create;
end;

function TFhirResourceFactory.makeEnum(value : String) : TFhirEnum;
begin
  result := TFhirEnum.create;
  result.value := value;
end;

function TFhirResourceFactory.newInteger : TFhirInteger;
begin
  result := TFhirInteger.create;
end;

function TFhirResourceFactory.makeInteger(value : String) : TFhirInteger;
begin
  result := TFhirInteger.create;
  result.value := value;
end;

function TFhirResourceFactory.newDateTime : TFhirDateTime;
begin
  result := TFhirDateTime.create;
end;

function TFhirResourceFactory.makeDateTime(value : TDateAndTime) : TFhirDateTime;
begin
  result := TFhirDateTime.create;
  result.value := value;
end;

function TFhirResourceFactory.newDate : TFhirDate;
begin
  result := TFhirDate.create;
end;

function TFhirResourceFactory.makeDate(value : TDateAndTime) : TFhirDate;
begin
  result := TFhirDate.create;
  result.value := value;
end;

function TFhirResourceFactory.newDecimal : TFhirDecimal;
begin
  result := TFhirDecimal.create;
end;

function TFhirResourceFactory.makeDecimal(value : String) : TFhirDecimal;
begin
  result := TFhirDecimal.create;
  result.value := value;
end;

function TFhirResourceFactory.newUri : TFhirUri;
begin
  result := TFhirUri.create;
end;

function TFhirResourceFactory.makeUri(value : String) : TFhirUri;
begin
  result := TFhirUri.create;
  result.value := value;
end;

function TFhirResourceFactory.newBase64Binary : TFhirBase64Binary;
begin
  result := TFhirBase64Binary.create;
end;

function TFhirResourceFactory.makeBase64Binary(value : String) : TFhirBase64Binary;
begin
  result := TFhirBase64Binary.create;
  result.value := value;
end;

function TFhirResourceFactory.newTime : TFhirTime;
begin
  result := TFhirTime.create;
end;

function TFhirResourceFactory.makeTime(value : String) : TFhirTime;
begin
  result := TFhirTime.create;
  result.value := value;
end;

function TFhirResourceFactory.newString : TFhirString;
begin
  result := TFhirString.create;
end;

function TFhirResourceFactory.makeString(value : String) : TFhirString;
begin
  result := TFhirString.create;
  result.value := value;
end;

function TFhirResourceFactory.newBoolean : TFhirBoolean;
begin
  result := TFhirBoolean.create;
end;

function TFhirResourceFactory.makeBoolean(value : Boolean) : TFhirBoolean;
begin
  result := TFhirBoolean.create;
  result.value := value;
end;

function TFhirResourceFactory.newInstant : TFhirInstant;
begin
  result := TFhirInstant.create;
end;

function TFhirResourceFactory.makeInstant(value : TDateAndTime) : TFhirInstant;
begin
  result := TFhirInstant.create;
  result.value := value;
end;

function TFhirResourceFactory.newCode : TFhirCode;
begin
  result := TFhirCode.create;
end;

function TFhirResourceFactory.makeCode(value : String) : TFhirCode;
begin
  result := TFhirCode.create;
  result.value := value;
end;

function TFhirResourceFactory.newId : TFhirId;
begin
  result := TFhirId.create;
end;

function TFhirResourceFactory.makeId(value : String) : TFhirId;
begin
  result := TFhirId.create;
  result.value := value;
end;

function TFhirResourceFactory.newOid : TFhirOid;
begin
  result := TFhirOid.create;
end;

function TFhirResourceFactory.makeOid(value : String) : TFhirOid;
begin
  result := TFhirOid.create;
  result.value := value;
end;

function TFhirResourceFactory.newUuid : TFhirUuid;
begin
  result := TFhirUuid.create;
end;

function TFhirResourceFactory.makeUuid(value : String) : TFhirUuid;
begin
  result := TFhirUuid.create;
  result.value := value;
end;

function TFhirResourceFactory.newExtension : TFhirExtension;
begin
  result := TFhirExtension.create;
end;

function TFhirResourceFactory.newNarrative : TFhirNarrative;
begin
  result := TFhirNarrative.create;
end;

function TFhirResourceFactory.newPeriod : TFhirPeriod;
begin
  result := TFhirPeriod.create;
end;

function TFhirResourceFactory.newCoding : TFhirCoding;
begin
  result := TFhirCoding.create;
end;

function TFhirResourceFactory.newRange : TFhirRange;
begin
  result := TFhirRange.create;
end;

function TFhirResourceFactory.newQuantity : TFhirQuantity;
begin
  result := TFhirQuantity.create;
end;

function TFhirResourceFactory.newAttachment : TFhirAttachment;
begin
  result := TFhirAttachment.create;
end;

function TFhirResourceFactory.newRatio : TFhirRatio;
begin
  result := TFhirRatio.create;
end;

function TFhirResourceFactory.newSampledData : TFhirSampledData;
begin
  result := TFhirSampledData.create;
end;

function TFhirResourceFactory.newResourceReference : TFhirResourceReference;
begin
  result := TFhirResourceReference.create;
end;

function TFhirResourceFactory.newCodeableConcept : TFhirCodeableConcept;
begin
  result := TFhirCodeableConcept.create;
end;

function TFhirResourceFactory.newIdentifier : TFhirIdentifier;
begin
  result := TFhirIdentifier.create;
end;

function TFhirResourceFactory.newScheduleRepeat : TFhirScheduleRepeat;
begin
  result := TFhirScheduleRepeat.create;
end;

function TFhirResourceFactory.newSchedule : TFhirSchedule;
begin
  result := TFhirSchedule.create;
end;

function TFhirResourceFactory.newContact : TFhirContact;
begin
  result := TFhirContact.create;
end;

function TFhirResourceFactory.newAddress : TFhirAddress;
begin
  result := TFhirAddress.create;
end;

function TFhirResourceFactory.newHumanName : TFhirHumanName;
begin
  result := TFhirHumanName.create;
end;

function TFhirResourceFactory.newAdverseReactionSymptom : TFhirAdverseReactionSymptom;
begin
  result := TFhirAdverseReactionSymptom.create;
end;

function TFhirResourceFactory.newAdverseReactionExposure : TFhirAdverseReactionExposure;
begin
  result := TFhirAdverseReactionExposure.create;
end;

function TFhirResourceFactory.newAdverseReaction : TFhirAdverseReaction;
begin
  result := TFhirAdverseReaction.create;
end;

function TFhirResourceFactory.newAlert : TFhirAlert;
begin
  result := TFhirAlert.create;
end;

function TFhirResourceFactory.newAllergyIntolerance : TFhirAllergyIntolerance;
begin
  result := TFhirAllergyIntolerance.create;
end;

function TFhirResourceFactory.newAppointmentParticipant : TFhirAppointmentParticipant;
begin
  result := TFhirAppointmentParticipant.create;
end;

function TFhirResourceFactory.newAppointment : TFhirAppointment;
begin
  result := TFhirAppointment.create;
end;

function TFhirResourceFactory.newAppointmentResponse : TFhirAppointmentResponse;
begin
  result := TFhirAppointmentResponse.create;
end;

function TFhirResourceFactory.newAvailability : TFhirAvailability;
begin
  result := TFhirAvailability.create;
end;

function TFhirResourceFactory.newCarePlanParticipant : TFhirCarePlanParticipant;
begin
  result := TFhirCarePlanParticipant.create;
end;

function TFhirResourceFactory.newCarePlanGoal : TFhirCarePlanGoal;
begin
  result := TFhirCarePlanGoal.create;
end;

function TFhirResourceFactory.newCarePlanActivity : TFhirCarePlanActivity;
begin
  result := TFhirCarePlanActivity.create;
end;

function TFhirResourceFactory.newCarePlanActivitySimple : TFhirCarePlanActivitySimple;
begin
  result := TFhirCarePlanActivitySimple.create;
end;

function TFhirResourceFactory.newCarePlan : TFhirCarePlan;
begin
  result := TFhirCarePlan.create;
end;

function TFhirResourceFactory.newCompositionAttester : TFhirCompositionAttester;
begin
  result := TFhirCompositionAttester.create;
end;

function TFhirResourceFactory.newCompositionEvent : TFhirCompositionEvent;
begin
  result := TFhirCompositionEvent.create;
end;

function TFhirResourceFactory.newCompositionSection : TFhirCompositionSection;
begin
  result := TFhirCompositionSection.create;
end;

function TFhirResourceFactory.newComposition : TFhirComposition;
begin
  result := TFhirComposition.create;
end;

function TFhirResourceFactory.newConceptMapElement : TFhirConceptMapElement;
begin
  result := TFhirConceptMapElement.create;
end;

function TFhirResourceFactory.newConceptMapElementDependsOn : TFhirConceptMapElementDependsOn;
begin
  result := TFhirConceptMapElementDependsOn.create;
end;

function TFhirResourceFactory.newConceptMapElementMap : TFhirConceptMapElementMap;
begin
  result := TFhirConceptMapElementMap.create;
end;

function TFhirResourceFactory.newConceptMap : TFhirConceptMap;
begin
  result := TFhirConceptMap.create;
end;

function TFhirResourceFactory.newConditionStage : TFhirConditionStage;
begin
  result := TFhirConditionStage.create;
end;

function TFhirResourceFactory.newConditionEvidence : TFhirConditionEvidence;
begin
  result := TFhirConditionEvidence.create;
end;

function TFhirResourceFactory.newConditionLocation : TFhirConditionLocation;
begin
  result := TFhirConditionLocation.create;
end;

function TFhirResourceFactory.newConditionRelatedItem : TFhirConditionRelatedItem;
begin
  result := TFhirConditionRelatedItem.create;
end;

function TFhirResourceFactory.newCondition : TFhirCondition;
begin
  result := TFhirCondition.create;
end;

function TFhirResourceFactory.newConformanceSoftware : TFhirConformanceSoftware;
begin
  result := TFhirConformanceSoftware.create;
end;

function TFhirResourceFactory.newConformanceImplementation : TFhirConformanceImplementation;
begin
  result := TFhirConformanceImplementation.create;
end;

function TFhirResourceFactory.newConformanceRest : TFhirConformanceRest;
begin
  result := TFhirConformanceRest.create;
end;

function TFhirResourceFactory.newConformanceRestSecurity : TFhirConformanceRestSecurity;
begin
  result := TFhirConformanceRestSecurity.create;
end;

function TFhirResourceFactory.newConformanceRestSecurityCertificate : TFhirConformanceRestSecurityCertificate;
begin
  result := TFhirConformanceRestSecurityCertificate.create;
end;

function TFhirResourceFactory.newConformanceRestResource : TFhirConformanceRestResource;
begin
  result := TFhirConformanceRestResource.create;
end;

function TFhirResourceFactory.newConformanceRestResourceInteraction : TFhirConformanceRestResourceInteraction;
begin
  result := TFhirConformanceRestResourceInteraction.create;
end;

function TFhirResourceFactory.newConformanceRestResourceSearchParam : TFhirConformanceRestResourceSearchParam;
begin
  result := TFhirConformanceRestResourceSearchParam.create;
end;

function TFhirResourceFactory.newConformanceRestInteraction : TFhirConformanceRestInteraction;
begin
  result := TFhirConformanceRestInteraction.create;
end;

function TFhirResourceFactory.newConformanceRestOperation : TFhirConformanceRestOperation;
begin
  result := TFhirConformanceRestOperation.create;
end;

function TFhirResourceFactory.newConformanceMessaging : TFhirConformanceMessaging;
begin
  result := TFhirConformanceMessaging.create;
end;

function TFhirResourceFactory.newConformanceMessagingEvent : TFhirConformanceMessagingEvent;
begin
  result := TFhirConformanceMessagingEvent.create;
end;

function TFhirResourceFactory.newConformanceDocument : TFhirConformanceDocument;
begin
  result := TFhirConformanceDocument.create;
end;

function TFhirResourceFactory.newConformance : TFhirConformance;
begin
  result := TFhirConformance.create;
end;

function TFhirResourceFactory.newContraindicationMitigation : TFhirContraindicationMitigation;
begin
  result := TFhirContraindicationMitigation.create;
end;

function TFhirResourceFactory.newContraindication : TFhirContraindication;
begin
  result := TFhirContraindication.create;
end;

function TFhirResourceFactory.newDataElementBinding : TFhirDataElementBinding;
begin
  result := TFhirDataElementBinding.create;
end;

function TFhirResourceFactory.newDataElementMapping : TFhirDataElementMapping;
begin
  result := TFhirDataElementMapping.create;
end;

function TFhirResourceFactory.newDataElement : TFhirDataElement;
begin
  result := TFhirDataElement.create;
end;

function TFhirResourceFactory.newDevice : TFhirDevice;
begin
  result := TFhirDevice.create;
end;

function TFhirResourceFactory.newDeviceObservationReportVirtualDevice : TFhirDeviceObservationReportVirtualDevice;
begin
  result := TFhirDeviceObservationReportVirtualDevice.create;
end;

function TFhirResourceFactory.newDeviceObservationReportVirtualDeviceChannel : TFhirDeviceObservationReportVirtualDeviceChannel;
begin
  result := TFhirDeviceObservationReportVirtualDeviceChannel.create;
end;

function TFhirResourceFactory.newDeviceObservationReportVirtualDeviceChannelMetric : TFhirDeviceObservationReportVirtualDeviceChannelMetric;
begin
  result := TFhirDeviceObservationReportVirtualDeviceChannelMetric.create;
end;

function TFhirResourceFactory.newDeviceObservationReport : TFhirDeviceObservationReport;
begin
  result := TFhirDeviceObservationReport.create;
end;

function TFhirResourceFactory.newDiagnosticOrderEvent : TFhirDiagnosticOrderEvent;
begin
  result := TFhirDiagnosticOrderEvent.create;
end;

function TFhirResourceFactory.newDiagnosticOrderItem : TFhirDiagnosticOrderItem;
begin
  result := TFhirDiagnosticOrderItem.create;
end;

function TFhirResourceFactory.newDiagnosticOrder : TFhirDiagnosticOrder;
begin
  result := TFhirDiagnosticOrder.create;
end;

function TFhirResourceFactory.newDiagnosticReportImage : TFhirDiagnosticReportImage;
begin
  result := TFhirDiagnosticReportImage.create;
end;

function TFhirResourceFactory.newDiagnosticReport : TFhirDiagnosticReport;
begin
  result := TFhirDiagnosticReport.create;
end;

function TFhirResourceFactory.newDocumentManifest : TFhirDocumentManifest;
begin
  result := TFhirDocumentManifest.create;
end;

function TFhirResourceFactory.newDocumentReferenceRelatesTo : TFhirDocumentReferenceRelatesTo;
begin
  result := TFhirDocumentReferenceRelatesTo.create;
end;

function TFhirResourceFactory.newDocumentReferenceService : TFhirDocumentReferenceService;
begin
  result := TFhirDocumentReferenceService.create;
end;

function TFhirResourceFactory.newDocumentReferenceServiceParameter : TFhirDocumentReferenceServiceParameter;
begin
  result := TFhirDocumentReferenceServiceParameter.create;
end;

function TFhirResourceFactory.newDocumentReferenceContext : TFhirDocumentReferenceContext;
begin
  result := TFhirDocumentReferenceContext.create;
end;

function TFhirResourceFactory.newDocumentReference : TFhirDocumentReference;
begin
  result := TFhirDocumentReference.create;
end;

function TFhirResourceFactory.newEncounterParticipant : TFhirEncounterParticipant;
begin
  result := TFhirEncounterParticipant.create;
end;

function TFhirResourceFactory.newEncounterHospitalization : TFhirEncounterHospitalization;
begin
  result := TFhirEncounterHospitalization.create;
end;

function TFhirResourceFactory.newEncounterHospitalizationAccomodation : TFhirEncounterHospitalizationAccomodation;
begin
  result := TFhirEncounterHospitalizationAccomodation.create;
end;

function TFhirResourceFactory.newEncounterLocation : TFhirEncounterLocation;
begin
  result := TFhirEncounterLocation.create;
end;

function TFhirResourceFactory.newEncounter : TFhirEncounter;
begin
  result := TFhirEncounter.create;
end;

function TFhirResourceFactory.newFamilyHistoryRelation : TFhirFamilyHistoryRelation;
begin
  result := TFhirFamilyHistoryRelation.create;
end;

function TFhirResourceFactory.newFamilyHistoryRelationCondition : TFhirFamilyHistoryRelationCondition;
begin
  result := TFhirFamilyHistoryRelationCondition.create;
end;

function TFhirResourceFactory.newFamilyHistory : TFhirFamilyHistory;
begin
  result := TFhirFamilyHistory.create;
end;

function TFhirResourceFactory.newGroupCharacteristic : TFhirGroupCharacteristic;
begin
  result := TFhirGroupCharacteristic.create;
end;

function TFhirResourceFactory.newGroup : TFhirGroup;
begin
  result := TFhirGroup.create;
end;

function TFhirResourceFactory.newImagingStudySeries : TFhirImagingStudySeries;
begin
  result := TFhirImagingStudySeries.create;
end;

function TFhirResourceFactory.newImagingStudySeriesInstance : TFhirImagingStudySeriesInstance;
begin
  result := TFhirImagingStudySeriesInstance.create;
end;

function TFhirResourceFactory.newImagingStudy : TFhirImagingStudy;
begin
  result := TFhirImagingStudy.create;
end;

function TFhirResourceFactory.newImmunizationExplanation : TFhirImmunizationExplanation;
begin
  result := TFhirImmunizationExplanation.create;
end;

function TFhirResourceFactory.newImmunizationReaction : TFhirImmunizationReaction;
begin
  result := TFhirImmunizationReaction.create;
end;

function TFhirResourceFactory.newImmunizationVaccinationProtocol : TFhirImmunizationVaccinationProtocol;
begin
  result := TFhirImmunizationVaccinationProtocol.create;
end;

function TFhirResourceFactory.newImmunization : TFhirImmunization;
begin
  result := TFhirImmunization.create;
end;

function TFhirResourceFactory.newImmunizationRecommendationRecommendation : TFhirImmunizationRecommendationRecommendation;
begin
  result := TFhirImmunizationRecommendationRecommendation.create;
end;

function TFhirResourceFactory.newImmunizationRecommendationRecommendationDateCriterion : TFhirImmunizationRecommendationRecommendationDateCriterion;
begin
  result := TFhirImmunizationRecommendationRecommendationDateCriterion.create;
end;

function TFhirResourceFactory.newImmunizationRecommendationRecommendationProtocol : TFhirImmunizationRecommendationRecommendationProtocol;
begin
  result := TFhirImmunizationRecommendationRecommendationProtocol.create;
end;

function TFhirResourceFactory.newImmunizationRecommendation : TFhirImmunizationRecommendation;
begin
  result := TFhirImmunizationRecommendation.create;
end;

function TFhirResourceFactory.newListEntry : TFhirListEntry;
begin
  result := TFhirListEntry.create;
end;

function TFhirResourceFactory.newList : TFhirList;
begin
  result := TFhirList.create;
end;

function TFhirResourceFactory.newLocationPosition : TFhirLocationPosition;
begin
  result := TFhirLocationPosition.create;
end;

function TFhirResourceFactory.newLocation : TFhirLocation;
begin
  result := TFhirLocation.create;
end;

function TFhirResourceFactory.newMedia : TFhirMedia;
begin
  result := TFhirMedia.create;
end;

function TFhirResourceFactory.newMedicationProduct : TFhirMedicationProduct;
begin
  result := TFhirMedicationProduct.create;
end;

function TFhirResourceFactory.newMedicationProductIngredient : TFhirMedicationProductIngredient;
begin
  result := TFhirMedicationProductIngredient.create;
end;

function TFhirResourceFactory.newMedicationPackage : TFhirMedicationPackage;
begin
  result := TFhirMedicationPackage.create;
end;

function TFhirResourceFactory.newMedicationPackageContent : TFhirMedicationPackageContent;
begin
  result := TFhirMedicationPackageContent.create;
end;

function TFhirResourceFactory.newMedication : TFhirMedication;
begin
  result := TFhirMedication.create;
end;

function TFhirResourceFactory.newMedicationAdministrationDosage : TFhirMedicationAdministrationDosage;
begin
  result := TFhirMedicationAdministrationDosage.create;
end;

function TFhirResourceFactory.newMedicationAdministration : TFhirMedicationAdministration;
begin
  result := TFhirMedicationAdministration.create;
end;

function TFhirResourceFactory.newMedicationDispenseDispense : TFhirMedicationDispenseDispense;
begin
  result := TFhirMedicationDispenseDispense.create;
end;

function TFhirResourceFactory.newMedicationDispenseDispenseDosage : TFhirMedicationDispenseDispenseDosage;
begin
  result := TFhirMedicationDispenseDispenseDosage.create;
end;

function TFhirResourceFactory.newMedicationDispenseSubstitution : TFhirMedicationDispenseSubstitution;
begin
  result := TFhirMedicationDispenseSubstitution.create;
end;

function TFhirResourceFactory.newMedicationDispense : TFhirMedicationDispense;
begin
  result := TFhirMedicationDispense.create;
end;

function TFhirResourceFactory.newMedicationPrescriptionDosageInstruction : TFhirMedicationPrescriptionDosageInstruction;
begin
  result := TFhirMedicationPrescriptionDosageInstruction.create;
end;

function TFhirResourceFactory.newMedicationPrescriptionDispense : TFhirMedicationPrescriptionDispense;
begin
  result := TFhirMedicationPrescriptionDispense.create;
end;

function TFhirResourceFactory.newMedicationPrescriptionSubstitution : TFhirMedicationPrescriptionSubstitution;
begin
  result := TFhirMedicationPrescriptionSubstitution.create;
end;

function TFhirResourceFactory.newMedicationPrescription : TFhirMedicationPrescription;
begin
  result := TFhirMedicationPrescription.create;
end;

function TFhirResourceFactory.newMedicationStatementDosage : TFhirMedicationStatementDosage;
begin
  result := TFhirMedicationStatementDosage.create;
end;

function TFhirResourceFactory.newMedicationStatement : TFhirMedicationStatement;
begin
  result := TFhirMedicationStatement.create;
end;

function TFhirResourceFactory.newMessageHeaderResponse : TFhirMessageHeaderResponse;
begin
  result := TFhirMessageHeaderResponse.create;
end;

function TFhirResourceFactory.newMessageHeaderSource : TFhirMessageHeaderSource;
begin
  result := TFhirMessageHeaderSource.create;
end;

function TFhirResourceFactory.newMessageHeaderDestination : TFhirMessageHeaderDestination;
begin
  result := TFhirMessageHeaderDestination.create;
end;

function TFhirResourceFactory.newMessageHeader : TFhirMessageHeader;
begin
  result := TFhirMessageHeader.create;
end;

function TFhirResourceFactory.newNamespaceUniqueId : TFhirNamespaceUniqueId;
begin
  result := TFhirNamespaceUniqueId.create;
end;

function TFhirResourceFactory.newNamespaceContact : TFhirNamespaceContact;
begin
  result := TFhirNamespaceContact.create;
end;

function TFhirResourceFactory.newNamespace : TFhirNamespace;
begin
  result := TFhirNamespace.create;
end;

function TFhirResourceFactory.newObservationReferenceRange : TFhirObservationReferenceRange;
begin
  result := TFhirObservationReferenceRange.create;
end;

function TFhirResourceFactory.newObservationRelated : TFhirObservationRelated;
begin
  result := TFhirObservationRelated.create;
end;

function TFhirResourceFactory.newObservation : TFhirObservation;
begin
  result := TFhirObservation.create;
end;

function TFhirResourceFactory.newOperationDefinitionParameter : TFhirOperationDefinitionParameter;
begin
  result := TFhirOperationDefinitionParameter.create;
end;

function TFhirResourceFactory.newOperationDefinition : TFhirOperationDefinition;
begin
  result := TFhirOperationDefinition.create;
end;

function TFhirResourceFactory.newOperationOutcomeIssue : TFhirOperationOutcomeIssue;
begin
  result := TFhirOperationOutcomeIssue.create;
end;

function TFhirResourceFactory.newOperationOutcome : TFhirOperationOutcome;
begin
  result := TFhirOperationOutcome.create;
end;

function TFhirResourceFactory.newOrderWhen : TFhirOrderWhen;
begin
  result := TFhirOrderWhen.create;
end;

function TFhirResourceFactory.newOrder : TFhirOrder;
begin
  result := TFhirOrder.create;
end;

function TFhirResourceFactory.newOrderResponse : TFhirOrderResponse;
begin
  result := TFhirOrderResponse.create;
end;

function TFhirResourceFactory.newOrganizationContact : TFhirOrganizationContact;
begin
  result := TFhirOrganizationContact.create;
end;

function TFhirResourceFactory.newOrganization : TFhirOrganization;
begin
  result := TFhirOrganization.create;
end;

function TFhirResourceFactory.newOther : TFhirOther;
begin
  result := TFhirOther.create;
end;

function TFhirResourceFactory.newPatientContact : TFhirPatientContact;
begin
  result := TFhirPatientContact.create;
end;

function TFhirResourceFactory.newPatientAnimal : TFhirPatientAnimal;
begin
  result := TFhirPatientAnimal.create;
end;

function TFhirResourceFactory.newPatientLink : TFhirPatientLink;
begin
  result := TFhirPatientLink.create;
end;

function TFhirResourceFactory.newPatient : TFhirPatient;
begin
  result := TFhirPatient.create;
end;

function TFhirResourceFactory.newPractitionerQualification : TFhirPractitionerQualification;
begin
  result := TFhirPractitionerQualification.create;
end;

function TFhirResourceFactory.newPractitioner : TFhirPractitioner;
begin
  result := TFhirPractitioner.create;
end;

function TFhirResourceFactory.newProcedurePerformer : TFhirProcedurePerformer;
begin
  result := TFhirProcedurePerformer.create;
end;

function TFhirResourceFactory.newProcedureRelatedItem : TFhirProcedureRelatedItem;
begin
  result := TFhirProcedureRelatedItem.create;
end;

function TFhirResourceFactory.newProcedure : TFhirProcedure;
begin
  result := TFhirProcedure.create;
end;

function TFhirResourceFactory.newProfileMapping : TFhirProfileMapping;
begin
  result := TFhirProfileMapping.create;
end;

function TFhirResourceFactory.newProfileStructure : TFhirProfileStructure;
begin
  result := TFhirProfileStructure.create;
end;

function TFhirResourceFactory.newProfileStructureSnapshot : TFhirProfileStructureSnapshot;
begin
  result := TFhirProfileStructureSnapshot.create;
end;

function TFhirResourceFactory.newProfileStructureSnapshotElement : TFhirProfileStructureSnapshotElement;
begin
  result := TFhirProfileStructureSnapshotElement.create;
end;

function TFhirResourceFactory.newProfileStructureSnapshotElementSlicing : TFhirProfileStructureSnapshotElementSlicing;
begin
  result := TFhirProfileStructureSnapshotElementSlicing.create;
end;

function TFhirResourceFactory.newProfileStructureSnapshotElementDefinition : TFhirProfileStructureSnapshotElementDefinition;
begin
  result := TFhirProfileStructureSnapshotElementDefinition.create;
end;

function TFhirResourceFactory.newProfileStructureSnapshotElementDefinitionType : TFhirProfileStructureSnapshotElementDefinitionType;
begin
  result := TFhirProfileStructureSnapshotElementDefinitionType.create;
end;

function TFhirResourceFactory.newProfileStructureSnapshotElementDefinitionConstraint : TFhirProfileStructureSnapshotElementDefinitionConstraint;
begin
  result := TFhirProfileStructureSnapshotElementDefinitionConstraint.create;
end;

function TFhirResourceFactory.newProfileStructureSnapshotElementDefinitionBinding : TFhirProfileStructureSnapshotElementDefinitionBinding;
begin
  result := TFhirProfileStructureSnapshotElementDefinitionBinding.create;
end;

function TFhirResourceFactory.newProfileStructureSnapshotElementDefinitionMapping : TFhirProfileStructureSnapshotElementDefinitionMapping;
begin
  result := TFhirProfileStructureSnapshotElementDefinitionMapping.create;
end;

function TFhirResourceFactory.newProfileStructureSearchParam : TFhirProfileStructureSearchParam;
begin
  result := TFhirProfileStructureSearchParam.create;
end;

function TFhirResourceFactory.newProfileExtensionDefn : TFhirProfileExtensionDefn;
begin
  result := TFhirProfileExtensionDefn.create;
end;

function TFhirResourceFactory.newProfile : TFhirProfile;
begin
  result := TFhirProfile.create;
end;

function TFhirResourceFactory.newProvenanceAgent : TFhirProvenanceAgent;
begin
  result := TFhirProvenanceAgent.create;
end;

function TFhirResourceFactory.newProvenanceEntity : TFhirProvenanceEntity;
begin
  result := TFhirProvenanceEntity.create;
end;

function TFhirResourceFactory.newProvenance : TFhirProvenance;
begin
  result := TFhirProvenance.create;
end;

function TFhirResourceFactory.newQueryResponse : TFhirQueryResponse;
begin
  result := TFhirQueryResponse.create;
end;

function TFhirResourceFactory.newQuery : TFhirQuery;
begin
  result := TFhirQuery.create;
end;

function TFhirResourceFactory.newQuestionnaireGroup : TFhirQuestionnaireGroup;
begin
  result := TFhirQuestionnaireGroup.create;
end;

function TFhirResourceFactory.newQuestionnaireGroupQuestion : TFhirQuestionnaireGroupQuestion;
begin
  result := TFhirQuestionnaireGroupQuestion.create;
end;

function TFhirResourceFactory.newQuestionnaire : TFhirQuestionnaire;
begin
  result := TFhirQuestionnaire.create;
end;

function TFhirResourceFactory.newQuestionnaireAnswersGroup : TFhirQuestionnaireAnswersGroup;
begin
  result := TFhirQuestionnaireAnswersGroup.create;
end;

function TFhirResourceFactory.newQuestionnaireAnswersGroupQuestion : TFhirQuestionnaireAnswersGroupQuestion;
begin
  result := TFhirQuestionnaireAnswersGroupQuestion.create;
end;

function TFhirResourceFactory.newQuestionnaireAnswersGroupQuestionAnswer : TFhirQuestionnaireAnswersGroupQuestionAnswer;
begin
  result := TFhirQuestionnaireAnswersGroupQuestionAnswer.create;
end;

function TFhirResourceFactory.newQuestionnaireAnswers : TFhirQuestionnaireAnswers;
begin
  result := TFhirQuestionnaireAnswers.create;
end;

function TFhirResourceFactory.newReferralRequest : TFhirReferralRequest;
begin
  result := TFhirReferralRequest.create;
end;

function TFhirResourceFactory.newRelatedPerson : TFhirRelatedPerson;
begin
  result := TFhirRelatedPerson.create;
end;

function TFhirResourceFactory.newRiskAssessmentPrediction : TFhirRiskAssessmentPrediction;
begin
  result := TFhirRiskAssessmentPrediction.create;
end;

function TFhirResourceFactory.newRiskAssessment : TFhirRiskAssessment;
begin
  result := TFhirRiskAssessment.create;
end;

function TFhirResourceFactory.newSecurityEventEvent : TFhirSecurityEventEvent;
begin
  result := TFhirSecurityEventEvent.create;
end;

function TFhirResourceFactory.newSecurityEventParticipant : TFhirSecurityEventParticipant;
begin
  result := TFhirSecurityEventParticipant.create;
end;

function TFhirResourceFactory.newSecurityEventParticipantNetwork : TFhirSecurityEventParticipantNetwork;
begin
  result := TFhirSecurityEventParticipantNetwork.create;
end;

function TFhirResourceFactory.newSecurityEventSource : TFhirSecurityEventSource;
begin
  result := TFhirSecurityEventSource.create;
end;

function TFhirResourceFactory.newSecurityEventObject : TFhirSecurityEventObject;
begin
  result := TFhirSecurityEventObject.create;
end;

function TFhirResourceFactory.newSecurityEventObjectDetail : TFhirSecurityEventObjectDetail;
begin
  result := TFhirSecurityEventObjectDetail.create;
end;

function TFhirResourceFactory.newSecurityEvent : TFhirSecurityEvent;
begin
  result := TFhirSecurityEvent.create;
end;

function TFhirResourceFactory.newSlot : TFhirSlot;
begin
  result := TFhirSlot.create;
end;

function TFhirResourceFactory.newSpecimenSource : TFhirSpecimenSource;
begin
  result := TFhirSpecimenSource.create;
end;

function TFhirResourceFactory.newSpecimenCollection : TFhirSpecimenCollection;
begin
  result := TFhirSpecimenCollection.create;
end;

function TFhirResourceFactory.newSpecimenTreatment : TFhirSpecimenTreatment;
begin
  result := TFhirSpecimenTreatment.create;
end;

function TFhirResourceFactory.newSpecimenContainer : TFhirSpecimenContainer;
begin
  result := TFhirSpecimenContainer.create;
end;

function TFhirResourceFactory.newSpecimen : TFhirSpecimen;
begin
  result := TFhirSpecimen.create;
end;

function TFhirResourceFactory.newSubscriptionChannel : TFhirSubscriptionChannel;
begin
  result := TFhirSubscriptionChannel.create;
end;

function TFhirResourceFactory.newSubscriptionTag : TFhirSubscriptionTag;
begin
  result := TFhirSubscriptionTag.create;
end;

function TFhirResourceFactory.newSubscription : TFhirSubscription;
begin
  result := TFhirSubscription.create;
end;

function TFhirResourceFactory.newSubstanceInstance : TFhirSubstanceInstance;
begin
  result := TFhirSubstanceInstance.create;
end;

function TFhirResourceFactory.newSubstanceIngredient : TFhirSubstanceIngredient;
begin
  result := TFhirSubstanceIngredient.create;
end;

function TFhirResourceFactory.newSubstance : TFhirSubstance;
begin
  result := TFhirSubstance.create;
end;

function TFhirResourceFactory.newSupplyDispense : TFhirSupplyDispense;
begin
  result := TFhirSupplyDispense.create;
end;

function TFhirResourceFactory.newSupply : TFhirSupply;
begin
  result := TFhirSupply.create;
end;

function TFhirResourceFactory.newValueSetDefine : TFhirValueSetDefine;
begin
  result := TFhirValueSetDefine.create;
end;

function TFhirResourceFactory.newValueSetDefineConcept : TFhirValueSetDefineConcept;
begin
  result := TFhirValueSetDefineConcept.create;
end;

function TFhirResourceFactory.newValueSetCompose : TFhirValueSetCompose;
begin
  result := TFhirValueSetCompose.create;
end;

function TFhirResourceFactory.newValueSetComposeInclude : TFhirValueSetComposeInclude;
begin
  result := TFhirValueSetComposeInclude.create;
end;

function TFhirResourceFactory.newValueSetComposeIncludeFilter : TFhirValueSetComposeIncludeFilter;
begin
  result := TFhirValueSetComposeIncludeFilter.create;
end;

function TFhirResourceFactory.newValueSetExpansion : TFhirValueSetExpansion;
begin
  result := TFhirValueSetExpansion.create;
end;

function TFhirResourceFactory.newValueSetExpansionContains : TFhirValueSetExpansionContains;
begin
  result := TFhirValueSetExpansionContains.create;
end;

function TFhirResourceFactory.newValueSet : TFhirValueSet;
begin
  result := TFhirValueSet.create;
end;


end.

