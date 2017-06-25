  {@Class TFhirResourceFactory : TFHIRBaseFactory
     FHIR factory: class constructors and general useful builders
  }
{!Wrapper uses FHIRBase, FHIRBase_Wrapper, FHIRTypes, FHIRTypes_Wrapper, FHIRComponents, FHIRComponents_Wrapper,  DateAndTime_Wrapper}

unit FHIRResources;

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
This is the dstu1 version of the FHIR code
{$ENDIF}

interface

// FHIR v0.0.82 generated Tue, Sep 30, 2014 18:08+1000

uses
  SysUtils, Classes, StringSupport, DecimalSupport, AdvBuffers,  FHIRBase, FHIRTypes, FHIRComponents;

Type
  {@Enum TFhirResourceType
    Enumeration of known resource types
  }
  TFhirResourceType = (
    frtNull, {@enum.value Resource type not known / not Specified }
    frtAdverseReaction, {@enum.value Records an unexpected reaction suspected to be related to the exposure of the reaction subject to a substance. }
    frtAlert, {@enum.value Prospective warnings of potential issues when providing care to the patient. }
    frtAllergyIntolerance, {@enum.value Indicates the patient has a susceptibility to an adverse reaction upon exposure to a specified substance. }
    frtCarePlan, {@enum.value Describes the intention of how one or more practitioners intend to deliver care for a particular patient for a period of time, possibly limited to care for a specific condition or set of conditions. }
    frtComposition, {@enum.value A set of healthcare-related information that is assembled together into a single logical document that provides a single coherent statement of meaning, establishes its own context and that has clinical attestation with regard to who is making the statement. }
    frtConceptMap, {@enum.value A statement of relationships from one set of concepts to one or more other concept systems. }
    frtCondition, {@enum.value Use to record detailed information about conditions, problems or diagnoses recognized by a clinician. There are many uses including: recording a Diagnosis during an Encounter; populating a problem List or a Summary Statement, such as a Discharge Summary. }
    frtConformance, {@enum.value A conformance statement is a set of requirements for a desired implementation or a description of how a target application fulfills those requirements in a particular implementation. }
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
    frtObservation, {@enum.value Measurements and simple assertions made about a patient, device or other subject. }
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
    frtQuestionnaire, {@enum.value A structured set of questions and their answers. The Questionnaire may contain questions, answers or both. The questions are ordered and grouped into coherent subsets, corresponding to the structure of the grouping of the underlying questions. }
    frtRelatedPerson, {@enum.value Information about a person that is involved in the care for a patient, but who is not the target of healthcare, nor has a formal responsibility in the care process. }
    frtSecurityEvent, {@enum.value A record of an event made for purposes of maintaining a security log. Typical uses include detection of intrusion attempts and monitoring for inappropriate usage. }
    frtSpecimen, {@enum.value Sample for analysis. }
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
    spAdverseReaction__language, {@enum.value spAdverseReaction__language The language of the resource }
    spAdverseReaction_Date, {@enum.value spAdverseReaction_Date The date of the reaction }
    spAdverseReaction_Subject, {@enum.value spAdverseReaction_Subject The subject that the sensitivity is about }
    spAdverseReaction_Substance, {@enum.value spAdverseReaction_Substance The name or code of the substance that produces the sensitivity }
    spAdverseReaction_Symptom); {@enum.value spAdverseReaction_Symptom One of the symptoms of the reaction }

  {@Enum TSearchParamsAlert
    Search Parameters for Alert
  }
  TSearchParamsAlert = (
    spAlert__id, {@enum.value spAlert__id The logical resource id associated with the resource (must be supported by all servers) }
    spAlert__language, {@enum.value spAlert__language The language of the resource }
    spAlert_Subject); {@enum.value spAlert_Subject The identity of a subject to list alerts for }

  {@Enum TSearchParamsAllergyIntolerance
    Search Parameters for AllergyIntolerance
  }
  TSearchParamsAllergyIntolerance = (
    spAllergyIntolerance__id, {@enum.value spAllergyIntolerance__id The logical resource id associated with the resource (must be supported by all servers) }
    spAllergyIntolerance__language, {@enum.value spAllergyIntolerance__language The language of the resource }
    spAllergyIntolerance_Date, {@enum.value spAllergyIntolerance_Date Recorded date/time. }
    spAllergyIntolerance_Recorder, {@enum.value spAllergyIntolerance_Recorder Who recorded the sensitivity }
    spAllergyIntolerance_Status, {@enum.value spAllergyIntolerance_Status The status of the sensitivity }
    spAllergyIntolerance_Subject, {@enum.value spAllergyIntolerance_Subject The subject that the sensitivity is about }
    spAllergyIntolerance_Substance, {@enum.value spAllergyIntolerance_Substance The name or code of the substance that produces the sensitivity }
    spAllergyIntolerance_Type); {@enum.value spAllergyIntolerance_Type The type of sensitivity }

  {@Enum TSearchParamsCarePlan
    Search Parameters for CarePlan
  }
  TSearchParamsCarePlan = (
    spCarePlan__id, {@enum.value spCarePlan__id The logical resource id associated with the resource (must be supported by all servers) }
    spCarePlan__language, {@enum.value spCarePlan__language The language of the resource }
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
    spComposition__language, {@enum.value spComposition__language The language of the resource }
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
    spConceptMap__language, {@enum.value spConceptMap__language The language of the resource }
    spConceptMap_Date, {@enum.value spConceptMap_Date The concept map publication date }
    spConceptMap_Dependson, {@enum.value spConceptMap_Dependson Reference to element/field/valueset provides the context }
    spConceptMap_Description, {@enum.value spConceptMap_Description Text search in the description of the concept map }
    spConceptMap_Identifier, {@enum.value spConceptMap_Identifier The identifier of the concept map }
    spConceptMap_Name, {@enum.value spConceptMap_Name Name of the concept map }
    spConceptMap_Product, {@enum.value spConceptMap_Product Reference to element/field/valueset provides the context }
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
    spCondition__language, {@enum.value spCondition__language The language of the resource }
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
    spConformance__language, {@enum.value spConformance__language The language of the resource }
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

  {@Enum TSearchParamsDevice
    Search Parameters for Device
  }
  TSearchParamsDevice = (
    spDevice__id, {@enum.value spDevice__id The logical resource id associated with the resource (must be supported by all servers) }
    spDevice__language, {@enum.value spDevice__language The language of the resource }
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
    spDeviceObservationReport__language, {@enum.value spDeviceObservationReport__language The language of the resource }
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
    spDiagnosticOrder__language, {@enum.value spDiagnosticOrder__language The language of the resource }
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
    spDiagnosticReport__language, {@enum.value spDiagnosticReport__language The language of the resource }
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
    spDocumentManifest__language, {@enum.value spDocumentManifest__language The language of the resource }
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
    spDocumentReference__language, {@enum.value spDocumentReference__language The language of the resource }
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
    spEncounter__language, {@enum.value spEncounter__language The language of the resource }
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
    spFamilyHistory__language, {@enum.value spFamilyHistory__language The language of the resource }
    spFamilyHistory_Subject); {@enum.value spFamilyHistory_Subject The identity of a subject to list family history items for }

  {@Enum TSearchParamsGroup
    Search Parameters for Group
  }
  TSearchParamsGroup = (
    spGroup__id, {@enum.value spGroup__id The logical resource id associated with the resource (must be supported by all servers) }
    spGroup__language, {@enum.value spGroup__language The language of the resource }
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
    spImagingStudy__language, {@enum.value spImagingStudy__language The language of the resource }
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
    spImmunization__language, {@enum.value spImmunization__language The language of the resource }
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
    spImmunizationRecommendation__language, {@enum.value spImmunizationRecommendation__language The language of the resource }
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
    spList__language, {@enum.value spList__language The language of the resource }
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
    spLocation__language, {@enum.value spLocation__language The language of the resource }
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
    spMedia__language, {@enum.value spMedia__language The language of the resource }
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
    spMedication__language, {@enum.value spMedication__language The language of the resource }
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
    spMedicationAdministration__language, {@enum.value spMedicationAdministration__language The language of the resource }
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
    spMedicationDispense__language, {@enum.value spMedicationDispense__language The language of the resource }
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
    spMedicationPrescription__language, {@enum.value spMedicationPrescription__language The language of the resource }
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
    spMedicationStatement__language, {@enum.value spMedicationStatement__language The language of the resource }
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
    spMessageHeader__language); {@enum.value spMessageHeader__language The language of the resource }

  {@Enum TSearchParamsObservation
    Search Parameters for Observation
  }
  TSearchParamsObservation = (
    spObservation__id, {@enum.value spObservation__id The logical resource id associated with the resource (must be supported by all servers) }
    spObservation__language, {@enum.value spObservation__language The language of the resource }
    spObservation_Date, {@enum.value spObservation_Date Obtained date/time. If the obtained element is a period, a date that falls in the period }
    spObservation_Name, {@enum.value spObservation_Name The name of the observation type }
    spObservation_Name_value_x, {@enum.value spObservation_Name_value_x Both name and one of the value parameters }
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

  {@Enum TSearchParamsOperationOutcome
    Search Parameters for OperationOutcome
  }
  TSearchParamsOperationOutcome = (
    spOperationOutcome__id, {@enum.value spOperationOutcome__id The logical resource id associated with the resource (must be supported by all servers) }
    spOperationOutcome__language); {@enum.value spOperationOutcome__language The language of the resource }

  {@Enum TSearchParamsOrder
    Search Parameters for Order
  }
  TSearchParamsOrder = (
    spOrder__id, {@enum.value spOrder__id The logical resource id associated with the resource (must be supported by all servers) }
    spOrder__language, {@enum.value spOrder__language The language of the resource }
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
    spOrderResponse__language, {@enum.value spOrderResponse__language The language of the resource }
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
    spOrganization__language, {@enum.value spOrganization__language The language of the resource }
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
    spOther__language, {@enum.value spOther__language The language of the resource }
    spOther_Code, {@enum.value spOther_Code Kind of Resource }
    spOther_Created, {@enum.value spOther_Created When created }
    spOther_Subject); {@enum.value spOther_Subject Identifies the }

  {@Enum TSearchParamsPatient
    Search Parameters for Patient
  }
  TSearchParamsPatient = (
    spPatient__id, {@enum.value spPatient__id The logical resource id associated with the resource (must be supported by all servers) }
    spPatient__language, {@enum.value spPatient__language The language of the resource }
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
    spPractitioner__language, {@enum.value spPractitioner__language The language of the resource }
    spPractitioner_Address, {@enum.value spPractitioner_Address An address in any kind of address/part }
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
    spProcedure__language, {@enum.value spProcedure__language The language of the resource }
    spProcedure_Date, {@enum.value spProcedure_Date The date the procedure was performed on }
    spProcedure_Subject, {@enum.value spProcedure_Subject The identity of a patient to list procedures  for }
    spProcedure_Type); {@enum.value spProcedure_Type Type of procedure }

  {@Enum TSearchParamsProfile
    Search Parameters for Profile
  }
  TSearchParamsProfile = (
    spProfile__id, {@enum.value spProfile__id The logical resource id associated with the resource (must be supported by all servers) }
    spProfile__language, {@enum.value spProfile__language The language of the resource }
    spProfile_Code, {@enum.value spProfile_Code A code for the profile in the format uri::code (server may choose to do subsumption) }
    spProfile_Date, {@enum.value spProfile_Date The profile publication date }
    spProfile_Description, {@enum.value spProfile_Description Text search in the description of the profile }
    spProfile_Extension, {@enum.value spProfile_Extension An extension code (use or definition) }
    spProfile_Identifier, {@enum.value spProfile_Identifier The identifier of the profile }
    spProfile_Name, {@enum.value spProfile_Name Name of the profile }
    spProfile_Publisher, {@enum.value spProfile_Publisher Name of the publisher of the profile }
    spProfile_Status, {@enum.value spProfile_Status The current status of the profile }
    spProfile_Type, {@enum.value spProfile_Type Type of resource that is constrained in the profile }
    spProfile_Valueset, {@enum.value spProfile_Valueset A vocabulary binding code }
    spProfile_Version); {@enum.value spProfile_Version The version identifier of the profile }

  {@Enum TSearchParamsProvenance
    Search Parameters for Provenance
  }
  TSearchParamsProvenance = (
    spProvenance__id, {@enum.value spProvenance__id The logical resource id associated with the resource (must be supported by all servers) }
    spProvenance__language, {@enum.value spProvenance__language The language of the resource }
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
    spQuery__language, {@enum.value spQuery__language The language of the resource }
    spQuery_Identifier, {@enum.value spQuery_Identifier Links query and its response(s) }
    spQuery_Response); {@enum.value spQuery_Response Links response to source query }

  {@Enum TSearchParamsQuestionnaire
    Search Parameters for Questionnaire
  }
  TSearchParamsQuestionnaire = (
    spQuestionnaire__id, {@enum.value spQuestionnaire__id The logical resource id associated with the resource (must be supported by all servers) }
    spQuestionnaire__language, {@enum.value spQuestionnaire__language The language of the resource }
    spQuestionnaire_Author, {@enum.value spQuestionnaire_Author The author of the questionnaire }
    spQuestionnaire_Authored, {@enum.value spQuestionnaire_Authored When the questionnaire was authored }
    spQuestionnaire_Encounter, {@enum.value spQuestionnaire_Encounter Encounter during which questionnaire was authored }
    spQuestionnaire_Identifier, {@enum.value spQuestionnaire_Identifier An identifier for the questionnaire }
    spQuestionnaire_Name, {@enum.value spQuestionnaire_Name Name of the questionnaire }
    spQuestionnaire_Status, {@enum.value spQuestionnaire_Status The status of the questionnaire }
    spQuestionnaire_Subject); {@enum.value spQuestionnaire_Subject The subject of the questionnaire }

  {@Enum TSearchParamsRelatedPerson
    Search Parameters for RelatedPerson
  }
  TSearchParamsRelatedPerson = (
    spRelatedPerson__id, {@enum.value spRelatedPerson__id The logical resource id associated with the resource (must be supported by all servers) }
    spRelatedPerson__language, {@enum.value spRelatedPerson__language The language of the resource }
    spRelatedPerson_Address, {@enum.value spRelatedPerson_Address An address in any kind of address/part }
    spRelatedPerson_Gender, {@enum.value spRelatedPerson_Gender Gender of the person }
    spRelatedPerson_Identifier, {@enum.value spRelatedPerson_Identifier A patient Identifier }
    spRelatedPerson_Name, {@enum.value spRelatedPerson_Name A portion of name in any name part }
    spRelatedPerson_Patient, {@enum.value spRelatedPerson_Patient The patient this person is related to }
    spRelatedPerson_Phonetic, {@enum.value spRelatedPerson_Phonetic A portion of name using some kind of phonetic matching algorithm }
    spRelatedPerson_Telecom); {@enum.value spRelatedPerson_Telecom The value in any kind of contact }

  {@Enum TSearchParamsSecurityEvent
    Search Parameters for SecurityEvent
  }
  TSearchParamsSecurityEvent = (
    spSecurityEvent__id, {@enum.value spSecurityEvent__id The logical resource id associated with the resource (must be supported by all servers) }
    spSecurityEvent__language, {@enum.value spSecurityEvent__language The language of the resource }
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

  {@Enum TSearchParamsSpecimen
    Search Parameters for Specimen
  }
  TSearchParamsSpecimen = (
    spSpecimen__id, {@enum.value spSpecimen__id The logical resource id associated with the resource (must be supported by all servers) }
    spSpecimen__language, {@enum.value spSpecimen__language The language of the resource }
    spSpecimen_Subject); {@enum.value spSpecimen_Subject The subject of the specimen }

  {@Enum TSearchParamsSubstance
    Search Parameters for Substance
  }
  TSearchParamsSubstance = (
    spSubstance__id, {@enum.value spSubstance__id The logical resource id associated with the resource (must be supported by all servers) }
    spSubstance__language, {@enum.value spSubstance__language The language of the resource }
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
    spSupply__language, {@enum.value spSupply__language The language of the resource }
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
    spValueSet__language, {@enum.value spValueSet__language The language of the resource }
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
  TFhirCarePlan = class;
  TFhirComposition = class;
  TFhirConceptMap = class;
  TFhirCondition = class;
  TFhirConformance = class;
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
  TFhirObservation = class;
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
  TFhirRelatedPerson = class;
  TFhirSecurityEvent = class;
  TFhirSpecimen = class;
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
  


  TFhirResourceListEnumerator = class (TAdvObject)
  private
    FIndex : integer;
    FList : TFhirResourceList;
    function GetCurrent : TFhirResource;
  public
    Constructor Create(list : TFhirResourceList);
    Destructor Destroy; override;
    function MoveNext : boolean;
    property Current : TFhirResource read GetCurrent;
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
    function GetEnumerator : TFhirResourceListEnumerator;
    {!script show}
    

    
    {@member AddItem
      Add an already existing FhirResource to the end of the list.
    }
    procedure AddItem(value : TFhirResource); overload;

    
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
    Function GetDateST : TDateTimeEx;
    Procedure SetDateST(value : TDateTimeEx);
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
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member identifierList
      This records identifiers associated with this reaction that are defined by business processed and/ or used to refer to it when a direct URL reference to the resource itself is not appropriate (e.g. in CDA documents, or in written / printed documentation).
    }
    property identifierList : TFhirIdentifierList read FIdentifierList;

    {@member date
      The date (and possibly time) when the reaction began.
    }
    {@member date
      Typed access to The date (and possibly time) when the reaction began.
    }
    property date : TDateTimeEx read GetDateST write SetDateST;
    property dateObject : TFhirDateTime read FDate write SetDate;

    {@member subject
      The subject of the adverse reaction.
    }
    property subject : TFhirResourceReference{TFhirPatient} read FSubject write SetSubject;
    property subjectObject : TFhirResourceReference{TFhirPatient} read FSubject write SetSubject;

    {@member didNotOccurFlag
      If true, indicates that no reaction occurred.
    }
    {@member didNotOccurFlag
      Typed access to If true, indicates that no reaction occurred.
    }
    property didNotOccurFlag : Boolean read GetDidNotOccurFlagST write SetDidNotOccurFlagST;
    property didNotOccurFlagObject : TFhirBoolean read FDidNotOccurFlag write SetDidNotOccurFlag;

    {@member recorder
      Identifies the individual responsible for the information in the reaction record.
    }
    property recorder : TFhirResourceReference{Resource} read FRecorder write SetRecorder;
    property recorderObject : TFhirResourceReference{Resource} read FRecorder write SetRecorder;

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
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
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
    property categoryObject : TFhirCodeableConcept read FCategory write SetCategory;

    {@member status
      Supports basic workflow.
    }
    property status : TFhirAlertStatus read GetStatusST write SetStatusST;
    property statusObject : TFhirEnum read FStatus write SetStatus;

    {@member subject
      The person who this alert concerns.
    }
    property subject : TFhirResourceReference{TFhirPatient} read FSubject write SetSubject;
    property subjectObject : TFhirResourceReference{TFhirPatient} read FSubject write SetSubject;

    {@member author
      The person or device that created the alert.
    }
    property author : TFhirResourceReference{Resource} read FAuthor write SetAuthor;
    property authorObject : TFhirResourceReference{Resource} read FAuthor write SetAuthor;

    {@member note
      The textual component of the alert to display to the user.
    }
    {@member note
      Typed access to The textual component of the alert to display to the user.
    }
    property note : String read GetNoteST write SetNoteST;
    property noteObject : TFhirString read FNote write SetNote;

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
    Function GetRecordedDateST : TDateTimeEx;
    Procedure SetRecordedDateST(value : TDateTimeEx);
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
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member identifierList
      This records identifiers associated with this allergy/intolerance concern that are defined by business processed and/ or used to refer to it when a direct URL reference to the resource itself is not appropriate (e.g. in CDA documents, or in written / printed documentation).
    }
    property identifierList : TFhirIdentifierList read FIdentifierList;

    {@member criticality
      Criticality of the sensitivity.
    }
    property criticality : TFhirCriticality read GetCriticalityST write SetCriticalityST;
    property criticalityObject : TFhirEnum read FCriticality write SetCriticality;

    {@member sensitivityType
      Type of the sensitivity.
    }
    property sensitivityType : TFhirSensitivitytype read GetSensitivityTypeST write SetSensitivityTypeST;
    property sensitivityTypeObject : TFhirEnum read FSensitivityType write SetSensitivityType;

    {@member recordedDate
      Date when the sensitivity was recorded.
    }
    {@member recordedDate
      Typed access to Date when the sensitivity was recorded.
    }
    property recordedDate : TDateTimeEx read GetRecordedDateST write SetRecordedDateST;
    property recordedDateObject : TFhirDateTime read FRecordedDate write SetRecordedDate;

    {@member status
      Status of the sensitivity.
    }
    property status : TFhirSensitivitystatus read GetStatusST write SetStatusST;
    property statusObject : TFhirEnum read FStatus write SetStatus;

    {@member subject
      The patient who has the allergy or intolerance.
    }
    property subject : TFhirResourceReference{TFhirPatient} read FSubject write SetSubject;
    property subjectObject : TFhirResourceReference{TFhirPatient} read FSubject write SetSubject;

    {@member recorder
      Indicates who has responsibility for the record.
    }
    property recorder : TFhirResourceReference{Resource} read FRecorder write SetRecorder;
    property recorderObject : TFhirResourceReference{Resource} read FRecorder write SetRecorder;

    {@member substance
      The substance that causes the sensitivity.
    }
    property substance : TFhirResourceReference{TFhirSubstance} read FSubstance write SetSubstance;
    property substanceObject : TFhirResourceReference{TFhirSubstance} read FSubstance write SetSubstance;

    {@member reactionList
      Reactions associated with the sensitivity.
    }
    property reactionList : TFhirResourceReferenceList{TFhirAdverseReaction} read FReactionList;

    {@member sensitivityTestList
      Observations that confirm or refute the sensitivity.
    }
    property sensitivityTestList : TFhirResourceReferenceList{TFhirObservation} read FSensitivityTestList;

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
    Function GetModifiedST : TDateTimeEx;
    Procedure SetModifiedST(value : TDateTimeEx);
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
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
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
    property patientObject : TFhirResourceReference{TFhirPatient} read FPatient write SetPatient;

    {@member status
      Indicates whether the plan is currently being acted upon, represents future intentions or is now just historical record.
    }
    property status : TFhirCarePlanStatus read GetStatusST write SetStatusST;
    property statusObject : TFhirEnum read FStatus write SetStatus;

    {@member period
      Indicates when the plan did (or is intended to) come into effect and end.
    }
    property period : TFhirPeriod read FPeriod write SetPeriod;
    property periodObject : TFhirPeriod read FPeriod write SetPeriod;

    {@member modified
      Identifies the most recent date on which the plan has been revised.
    }
    {@member modified
      Typed access to Identifies the most recent date on which the plan has been revised.
    }
    property modified : TDateTimeEx read GetModifiedST write SetModifiedST;
    property modifiedObject : TFhirDateTime read FModified write SetModified;

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
    {@member notes
      Typed access to General notes about the care plan not covered elsewhere.
    }
    property notes : String read GetNotesST write SetNotesST;
    property notesObject : TFhirString read FNotes write SetNotes;

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
    Function GetDateST : TDateTimeEx;
    Procedure SetDateST(value : TDateTimeEx);
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
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member identifier
      Logical Identifier for the composition, assigned when created. This identifier stays constant as the composition is changed over time.
    }
    property identifier : TFhirIdentifier read FIdentifier write SetIdentifier;
    property identifierObject : TFhirIdentifier read FIdentifier write SetIdentifier;

    {@member date
      The composition editing time, when the composition was last logically changed by the author.
    }
    {@member date
      Typed access to The composition editing time, when the composition was last logically changed by the author.
    }
    property date : TDateTimeEx read GetDateST write SetDateST;
    property dateObject : TFhirDateTime read FDate write SetDate;

    {@member type_
      Specifies the particular kind of composition (e.g. History and Physical, Discharge Summary, Progress Note). This usually equates to the purpose of making the composition.
    }
    property type_ : TFhirCodeableConcept read FType_ write SetType_;
    property type_Object : TFhirCodeableConcept read FType_ write SetType_;

    {@member class_
      A categorization for the type of the composition. This may be implied by or derived from the code specified in the Composition Type.
    }
    property class_ : TFhirCodeableConcept read FClass_ write SetClass_;
    property class_Object : TFhirCodeableConcept read FClass_ write SetClass_;

    {@member title
      Official human-readable label for the composition.
    }
    {@member title
      Typed access to Official human-readable label for the composition.
    }
    property title : String read GetTitleST write SetTitleST;
    property titleObject : TFhirString read FTitle write SetTitle;

    {@member status
      The workflow/clinical status of this composition. The status is a marker for the clinical standing of the document.
    }
    property status : TFhirCompositionStatus read GetStatusST write SetStatusST;
    property statusObject : TFhirEnum read FStatus write SetStatus;

    {@member confidentiality
      The code specifying the level of confidentiality of the Composition.
    }
    property confidentiality : TFhirCoding read FConfidentiality write SetConfidentiality;
    property confidentialityObject : TFhirCoding read FConfidentiality write SetConfidentiality;

    {@member subject
      Who or what the composition is about. The composition can be about a person, (patient or healthcare practitioner), a device (I.e. machine) or even a group of subjects (such as a document about a herd of livestock, or a set of patients that share a common exposure).
    }
    property subject : TFhirResourceReference{Resource} read FSubject write SetSubject;
    property subjectObject : TFhirResourceReference{Resource} read FSubject write SetSubject;

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
    property custodianObject : TFhirResourceReference{TFhirOrganization} read FCustodian write SetCustodian;

    {@member event
      The main event/act/item, such as a colonoscopy or an appendectomy, being documented.
    }
    property event : TFhirCompositionEvent read FEvent write SetEvent;
    property eventObject : TFhirCompositionEvent read FEvent write SetEvent;

    {@member encounter
      Describes the clinical encounter or type of care this documentation is associated with.
    }
    property encounter : TFhirResourceReference{TFhirEncounter} read FEncounter write SetEncounter;
    property encounterObject : TFhirResourceReference{TFhirEncounter} read FEncounter write SetEncounter;

    {@member sectionList
      The root of the sections that make up the composition.
    }
    property sectionList : TFhirCompositionSectionList read FSectionList;

  end;


  {@Class TFhirConceptMap : TFhirResource
    A statement of relationships from one set of concepts to one or more other concept systems.
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
    FSource : TFhirResourceReference{TFhirValueSet};
    FTarget : TFhirResourceReference{TFhirValueSet};
    FconceptList : TFhirConceptMapConceptList;
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
    Function GetDateST : TDateTimeEx;
    Procedure SetDateST(value : TDateTimeEx);
    Procedure SetSource(value : TFhirResourceReference{TFhirValueSet});
    Procedure SetTarget(value : TFhirResourceReference{TFhirValueSet});
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
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member identifier
      The identifier that is used to identify this concept map when it is referenced in a specification, model, design or an instance (should be globally unique OID, UUID, or URI).
    }
    {@member identifier
      Typed access to The identifier that is used to identify this concept map when it is referenced in a specification, model, design or an instance (should be globally unique OID, UUID, or URI).
    }
    property identifier : String read GetIdentifierST write SetIdentifierST;
    property identifierObject : TFhirString read FIdentifier write SetIdentifier;

    {@member version
      The identifier that is used to identify this version of the concept map when it is referenced in a specification, model, design or instance. This is an arbitrary value managed by the profile author manually and the value should be a timestamp.
    }
    {@member version
      Typed access to The identifier that is used to identify this version of the concept map when it is referenced in a specification, model, design or instance. This is an arbitrary value managed by the profile author manually and the value should be a timestamp.
    }
    property version : String read GetVersionST write SetVersionST;
    property versionObject : TFhirString read FVersion write SetVersion;

    {@member name
      A free text natural language name describing the concept map.
    }
    {@member name
      Typed access to A free text natural language name describing the concept map.
    }
    property name : String read GetNameST write SetNameST;
    property nameObject : TFhirString read FName write SetName;

    {@member publisher
      The name of the individual or organization that published the concept map.
    }
    {@member publisher
      Typed access to The name of the individual or organization that published the concept map.
    }
    property publisher : String read GetPublisherST write SetPublisherST;
    property publisherObject : TFhirString read FPublisher write SetPublisher;

    {@member telecomList
      Contacts of the publisher to assist a user in finding and communicating with the publisher.
    }
    property telecomList : TFhirContactList read FTelecomList;

    {@member description
      A free text natural language description of the use of the concept map - reason for definition, conditions of use, etc.
    }
    {@member description
      Typed access to A free text natural language description of the use of the concept map - reason for definition, conditions of use, etc.
    }
    property description : String read GetDescriptionST write SetDescriptionST;
    property descriptionObject : TFhirString read FDescription write SetDescription;

    {@member copyright
      A copyright statement relating to the concept map and/or its contents.
    }
    {@member copyright
      Typed access to A copyright statement relating to the concept map and/or its contents.
    }
    property copyright : String read GetCopyrightST write SetCopyrightST;
    property copyrightObject : TFhirString read FCopyright write SetCopyright;

    {@member status
      The status of the concept map.
    }
    property status : TFhirValuesetStatus read GetStatusST write SetStatusST;
    property statusObject : TFhirEnum read FStatus write SetStatus;

    {@member experimental
      This ConceptMap was authored for testing purposes (or education/evaluation/marketing), and is not intended to be used for genuine usage.
    }
    {@member experimental
      Typed access to This ConceptMap was authored for testing purposes (or education/evaluation/marketing), and is not intended to be used for genuine usage.
    }
    property experimental : Boolean read GetExperimentalST write SetExperimentalST;
    property experimentalObject : TFhirBoolean read FExperimental write SetExperimental;

    {@member date
      The date that the concept map status was last changed.
    }
    {@member date
      Typed access to The date that the concept map status was last changed.
    }
    property date : TDateTimeEx read GetDateST write SetDateST;
    property dateObject : TFhirDateTime read FDate write SetDate;

    {@member source
      The source value set that specifies the concepts that are being mapped.
    }
    property source : TFhirResourceReference{TFhirValueSet} read FSource write SetSource;
    property sourceObject : TFhirResourceReference{TFhirValueSet} read FSource write SetSource;

    {@member target
      The target value set provides context to the mappings. Note that the mapping is made between concepts, not between value sets, but the value set provides important context about how the concept mapping choices are made.
    }
    property target : TFhirResourceReference{TFhirValueSet} read FTarget write SetTarget;
    property targetObject : TFhirResourceReference{TFhirValueSet} read FTarget write SetTarget;

    {@member conceptList
      Mappings for a concept from the source valueset.
    }
    property conceptList : TFhirConceptMapConceptList read FConceptList;

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
    Function GetDateAssertedST : TDateTimeEx;
    Procedure SetDateAssertedST(value : TDateTimeEx);
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
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
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
    property subjectObject : TFhirResourceReference{TFhirPatient} read FSubject write SetSubject;

    {@member encounter
      Encounter during which the condition was first asserted.
    }
    property encounter : TFhirResourceReference{TFhirEncounter} read FEncounter write SetEncounter;
    property encounterObject : TFhirResourceReference{TFhirEncounter} read FEncounter write SetEncounter;

    {@member asserter
      Person who takes responsibility for asserting the existence of the condition as part of the electronic record.
    }
    property asserter : TFhirResourceReference{Resource} read FAsserter write SetAsserter;
    property asserterObject : TFhirResourceReference{Resource} read FAsserter write SetAsserter;

    {@member dateAsserted
      Estimated or actual date the condition/problem/diagnosis was first detected/suspected.
    }
    {@member dateAsserted
      Typed access to Estimated or actual date the condition/problem/diagnosis was first detected/suspected.
    }
    property dateAsserted : TDateTimeEx read GetDateAssertedST write SetDateAssertedST;
    property dateAssertedObject : TFhirDate read FDateAsserted write SetDateAsserted;

    {@member code
      Identification of the condition, problem or diagnosis.
    }
    property code : TFhirCodeableConcept read FCode write SetCode;
    property codeObject : TFhirCodeableConcept read FCode write SetCode;

    {@member category
      A category assigned to the condition. E.g. complaint | symptom | finding | diagnosis.
    }
    property category : TFhirCodeableConcept read FCategory write SetCategory;
    property categoryObject : TFhirCodeableConcept read FCategory write SetCategory;

    {@member status
      The clinical status of the condition.
    }
    property status : TFhirConditionStatus read GetStatusST write SetStatusST;
    property statusObject : TFhirEnum read FStatus write SetStatus;

    {@member certainty
      The degree of confidence that this condition is correct.
    }
    property certainty : TFhirCodeableConcept read FCertainty write SetCertainty;
    property certaintyObject : TFhirCodeableConcept read FCertainty write SetCertainty;

    {@member severity
      A subjective assessment of the severity of the condition as evaluated by the clinician.
    }
    property severity : TFhirCodeableConcept read FSeverity write SetSeverity;
    property severityObject : TFhirCodeableConcept read FSeverity write SetSeverity;

    {@member onset
      Estimated or actual date the condition began, in the opinion of the clinician.
    }
    property onset : TFhirType read FOnset write SetOnset;
    property onsetObject : TFhirType read FOnset write SetOnset;

    {@member abatement
      The date or estimated date that the condition resolved or went into remission. This is called "abatement" because of the many overloaded connotations associated with "remission" or "resolution" - Conditions are never really resolved, but they can abate.
    }
    property abatement : TFhirType read FAbatement write SetAbatement;
    property abatementObject : TFhirType read FAbatement write SetAbatement;

    {@member stage
      Clinical stage or grade of a condition. May include formal severity assessments.
    }
    property stage : TFhirConditionStage read FStage write SetStage;
    property stageObject : TFhirConditionStage read FStage write SetStage;

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
    {@member notes
      Typed access to Additional information about the Condition. This is a general notes/comments entry  for description of the Condition, its diagnosis and prognosis.
    }
    property notes : String read GetNotesST write SetNotesST;
    property notesObject : TFhirString read FNotes write SetNotes;

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
    Function GetDateST : TDateTimeEx;
    Procedure SetDateST(value : TDateTimeEx);
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
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member identifier
      The identifier that is used to identify this conformance statement when it is referenced in a specification, model, design or an instance (should be globally unique OID, UUID, or URI).
    }
    {@member identifier
      Typed access to The identifier that is used to identify this conformance statement when it is referenced in a specification, model, design or an instance (should be globally unique OID, UUID, or URI).
    }
    property identifier : String read GetIdentifierST write SetIdentifierST;
    property identifierObject : TFhirString read FIdentifier write SetIdentifier;

    {@member version
      The identifier that is used to identify this version of the conformance statement when it is referenced in a specification, model, design or instance. This is an arbitrary value managed by the profile author manually and the value should be a timestamp.
    }
    {@member version
      Typed access to The identifier that is used to identify this version of the conformance statement when it is referenced in a specification, model, design or instance. This is an arbitrary value managed by the profile author manually and the value should be a timestamp.
    }
    property version : String read GetVersionST write SetVersionST;
    property versionObject : TFhirString read FVersion write SetVersion;

    {@member name
      A free text natural language name identifying the conformance statement.
    }
    {@member name
      Typed access to A free text natural language name identifying the conformance statement.
    }
    property name : String read GetNameST write SetNameST;
    property nameObject : TFhirString read FName write SetName;

    {@member publisher
      Name of Organization publishing this conformance statement.
    }
    {@member publisher
      Typed access to Name of Organization publishing this conformance statement.
    }
    property publisher : String read GetPublisherST write SetPublisherST;
    property publisherObject : TFhirString read FPublisher write SetPublisher;

    {@member telecomList
      Contacts for Organization relevant to this conformance statement.  The contacts may be a website, email, phone numbers, etc.
    }
    property telecomList : TFhirContactList read FTelecomList;

    {@member description
      A free text natural language description of the conformance statement and its use. Typically, this is used when the profile describes a desired rather than an actual solution, for example as a formal expression of requirements as part of an RFP.
    }
    {@member description
      Typed access to A free text natural language description of the conformance statement and its use. Typically, this is used when the profile describes a desired rather than an actual solution, for example as a formal expression of requirements as part of an RFP.
    }
    property description : String read GetDescriptionST write SetDescriptionST;
    property descriptionObject : TFhirString read FDescription write SetDescription;

    {@member status
      The status of this conformance statement.
    }
    property status : TFhirConformanceStatementStatus read GetStatusST write SetStatusST;
    property statusObject : TFhirEnum read FStatus write SetStatus;

    {@member experimental
      A flag to indicate that this conformance statement is authored for testing purposes (or education/evaluation/marketing), and is not intended to be used for genuine usage.
    }
    {@member experimental
      Typed access to A flag to indicate that this conformance statement is authored for testing purposes (or education/evaluation/marketing), and is not intended to be used for genuine usage.
    }
    property experimental : Boolean read GetExperimentalST write SetExperimentalST;
    property experimentalObject : TFhirBoolean read FExperimental write SetExperimental;

    {@member date
      The date when the conformance statement was published.
    }
    {@member date
      Typed access to The date when the conformance statement was published.
    }
    property date : TDateTimeEx read GetDateST write SetDateST;
    property dateObject : TFhirDateTime read FDate write SetDate;

    {@member software
      Software that is covered by this conformance statement.  It is used when the profile describes the capabilities of a particular software version, independent of an installation.
    }
    property software : TFhirConformanceSoftware read FSoftware write SetSoftware;
    property softwareObject : TFhirConformanceSoftware read FSoftware write SetSoftware;

    {@member implementation_
      Identifies a specific implementation instance that is described by the conformance statement - i.e. a particular installation, rather than the capabilities of a software program.
    }
    property implementation_ : TFhirConformanceImplementation read FImplementation_ write SetImplementation_;
    property implementation_Object : TFhirConformanceImplementation read FImplementation_ write SetImplementation_;

    {@member fhirVersion
      The version of the FHIR specification on which this conformance statement is based.
    }
    {@member fhirVersion
      Typed access to The version of the FHIR specification on which this conformance statement is based.
    }
    property fhirVersion : String read GetFhirVersionST write SetFhirVersionST;
    property fhirVersionObject : TFhirId read FFhirVersion write SetFhirVersion;

    {@member acceptUnknown
      A flag that indicates whether the application accepts unknown elements as part of a resource.
    }
    {@member acceptUnknown
      Typed access to A flag that indicates whether the application accepts unknown elements as part of a resource.
    }
    property acceptUnknown : Boolean read GetAcceptUnknownST write SetAcceptUnknownST;
    property acceptUnknownObject : TFhirBoolean read FAcceptUnknown write SetAcceptUnknown;

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
    Function GetExpiryST : TDateTimeEx;
    Procedure SetExpiryST(value : TDateTimeEx);
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
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
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
    property type_Object : TFhirCodeableConcept read FType_ write SetType_;

    {@member manufacturer
      A name of the manufacturer.
    }
    {@member manufacturer
      Typed access to A name of the manufacturer.
    }
    property manufacturer : String read GetManufacturerST write SetManufacturerST;
    property manufacturerObject : TFhirString read FManufacturer write SetManufacturer;

    {@member model
      The "model" - an identifier assigned by the manufacturer to identify the product by its type. This number is shared by the all devices sold as the same type.
    }
    {@member model
      Typed access to The "model" - an identifier assigned by the manufacturer to identify the product by its type. This number is shared by the all devices sold as the same type.
    }
    property model : String read GetModelST write SetModelST;
    property modelObject : TFhirString read FModel write SetModel;

    {@member version
      The version of the device, if the device has multiple releases under the same model, or if the device is software or carries firmware.
    }
    {@member version
      Typed access to The version of the device, if the device has multiple releases under the same model, or if the device is software or carries firmware.
    }
    property version : String read GetVersionST write SetVersionST;
    property versionObject : TFhirString read FVersion write SetVersion;

    {@member expiry
      Date of expiry of this device (if applicable).
    }
    {@member expiry
      Typed access to Date of expiry of this device (if applicable).
    }
    property expiry : TDateTimeEx read GetExpiryST write SetExpiryST;
    property expiryObject : TFhirDate read FExpiry write SetExpiry;

    {@member udi
      FDA Mandated Unique Device Identifier. Use the human readable information (the content that the user sees, which is sometimes different to the exact syntax represented in the barcode)  - see http://www.fda.gov/MedicalDevices/DeviceRegulationandGuidance/UniqueDeviceIdentification/default.htm.
    }
    {@member udi
      Typed access to FDA Mandated Unique Device Identifier. Use the human readable information (the content that the user sees, which is sometimes different to the exact syntax represented in the barcode)  - see http://www.fda.gov/MedicalDevices/DeviceRegulationandGuidance/UniqueDeviceIdentification/default.htm.
    }
    property udi : String read GetUdiST write SetUdiST;
    property udiObject : TFhirString read FUdi write SetUdi;

    {@member lotNumber
      Lot number assigned by the manufacturer.
    }
    {@member lotNumber
      Typed access to Lot number assigned by the manufacturer.
    }
    property lotNumber : String read GetLotNumberST write SetLotNumberST;
    property lotNumberObject : TFhirString read FLotNumber write SetLotNumber;

    {@member owner
      An organization that is responsible for the provision and ongoing maintenance of the device.
    }
    property owner : TFhirResourceReference{TFhirOrganization} read FOwner write SetOwner;
    property ownerObject : TFhirResourceReference{TFhirOrganization} read FOwner write SetOwner;

    {@member location
      The resource may be found in a literal location (i.e. GPS coordinates), a logical place (i.e. "in/with the patient"), or a coded location.
    }
    property location : TFhirResourceReference{TFhirLocation} read FLocation write SetLocation;
    property locationObject : TFhirResourceReference{TFhirLocation} read FLocation write SetLocation;

    {@member patient
      Patient information, if the resource is affixed to a person.
    }
    property patient : TFhirResourceReference{TFhirPatient} read FPatient write SetPatient;
    property patientObject : TFhirResourceReference{TFhirPatient} read FPatient write SetPatient;

    {@member contactList
      Contact details for an organization or a particular human that is responsible for the device.
    }
    property contactList : TFhirContactList read FContactList;

    {@member url
      A network address on which the device may be contacted directly.
    }
    {@member url
      Typed access to A network address on which the device may be contacted directly.
    }
    property url : String read GetUrlST write SetUrlST;
    property urlObject : TFhirUri read FUrl write SetUrl;

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
    Function GetInstantST : TDateTimeEx;
    Procedure SetInstantST(value : TDateTimeEx);
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
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member instant
      The point in time that the values are reported.
    }
    {@member instant
      Typed access to The point in time that the values are reported.
    }
    property instant : TDateTimeEx read GetInstantST write SetInstantST;
    property instantObject : TFhirInstant read FInstant write SetInstant;

    {@member identifier
      An identifier assigned to this observation bu the source device that made the observation.
    }
    property identifier : TFhirIdentifier read FIdentifier write SetIdentifier;
    property identifierObject : TFhirIdentifier read FIdentifier write SetIdentifier;

    {@member source
      Identification information for the device that is the source of the data.
    }
    property source : TFhirResourceReference{TFhirDevice} read FSource write SetSource;
    property sourceObject : TFhirResourceReference{TFhirDevice} read FSource write SetSource;

    {@member subject
      The subject of the measurement.
    }
    property subject : TFhirResourceReference{Resource} read FSubject write SetSubject;
    property subjectObject : TFhirResourceReference{Resource} read FSubject write SetSubject;

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
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member subject
      Who or what the investigation is to be performed on. This is usually a human patient, but diagnostic tests can also be requested on animals, groups of humans or animals, devices such as dialysis machines, or even locations (typically for environmental scans).
    }
    property subject : TFhirResourceReference{Resource} read FSubject write SetSubject;
    property subjectObject : TFhirResourceReference{Resource} read FSubject write SetSubject;

    {@member orderer
      The practitioner that holds legal responsibility for ordering the investigation.
    }
    property orderer : TFhirResourceReference{TFhirPractitioner} read FOrderer write SetOrderer;
    property ordererObject : TFhirResourceReference{TFhirPractitioner} read FOrderer write SetOrderer;

    {@member identifierList
      Identifiers assigned to this order by the order or by the receiver.
    }
    property identifierList : TFhirIdentifierList read FIdentifierList;

    {@member encounter
      An encounter that provides additional informaton about the healthcare context in which this request is made.
    }
    property encounter : TFhirResourceReference{TFhirEncounter} read FEncounter write SetEncounter;
    property encounterObject : TFhirResourceReference{TFhirEncounter} read FEncounter write SetEncounter;

    {@member clinicalNotes
      An explanation or justification for why this diagnostic investigation is being requested.
    }
    {@member clinicalNotes
      Typed access to An explanation or justification for why this diagnostic investigation is being requested.
    }
    property clinicalNotes : String read GetClinicalNotesST write SetClinicalNotesST;
    property clinicalNotesObject : TFhirString read FClinicalNotes write SetClinicalNotes;

    {@member specimenList
      One or more specimens that the diagnostic investigation is about.
    }
    property specimenList : TFhirResourceReferenceList{TFhirSpecimen} read FSpecimenList;

    {@member status
      The status of the order.
    }
    property status : TFhirDiagnosticOrderStatus read GetStatusST write SetStatusST;
    property statusObject : TFhirEnum read FStatus write SetStatus;

    {@member priority
      The clinical priority associated with this order.
    }
    property priority : TFhirDiagnosticOrderPriority read GetPriorityST write SetPriorityST;
    property priorityObject : TFhirEnum read FPriority write SetPriority;

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
    Function GetIssuedST : TDateTimeEx;
    Procedure SetIssuedST(value : TDateTimeEx);
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
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member name
      A code or name that describes this diagnostic report.
    }
    property name : TFhirCodeableConcept read FName write SetName;
    property nameObject : TFhirCodeableConcept read FName write SetName;

    {@member status
      The status of the diagnostic report as a whole.
    }
    property status : TFhirDiagnosticReportStatus read GetStatusST write SetStatusST;
    property statusObject : TFhirEnum read FStatus write SetStatus;

    {@member issued
      The date and/or time that this version of the report was released from the source diagnostic service.
    }
    {@member issued
      Typed access to The date and/or time that this version of the report was released from the source diagnostic service.
    }
    property issued : TDateTimeEx read GetIssuedST write SetIssuedST;
    property issuedObject : TFhirDateTime read FIssued write SetIssued;

    {@member subject
      The subject of the report. Usually, but not always, this is a patient. However diagnostic services also perform analyses on specimens collected from a variety of other sources.
    }
    property subject : TFhirResourceReference{Resource} read FSubject write SetSubject;
    property subjectObject : TFhirResourceReference{Resource} read FSubject write SetSubject;

    {@member performer
      The diagnostic service that is responsible for issuing the report.
    }
    property performer : TFhirResourceReference{Resource} read FPerformer write SetPerformer;
    property performerObject : TFhirResourceReference{Resource} read FPerformer write SetPerformer;

    {@member identifier
      The local ID assigned to the report by the order filler, usually by the Information System of the diagnostic service provider.
    }
    property identifier : TFhirIdentifier read FIdentifier write SetIdentifier;
    property identifierObject : TFhirIdentifier read FIdentifier write SetIdentifier;

    {@member requestDetailList
      Details concerning a test requested.
    }
    property requestDetailList : TFhirResourceReferenceList{TFhirDiagnosticOrder} read FRequestDetailList;

    {@member serviceCategory
      The section of the diagnostic service that performs the examination e.g. biochemistry, hematology, MRI.
    }
    property serviceCategory : TFhirCodeableConcept read FServiceCategory write SetServiceCategory;
    property serviceCategoryObject : TFhirCodeableConcept read FServiceCategory write SetServiceCategory;

    {@member diagnostic
      The time or time-period the observed values are related to. This is usually either the time of the procedure or of specimen collection(s), but very often the source of the date/time is not known, only the date/time itself.
    }
    property diagnostic : TFhirType read FDiagnostic write SetDiagnostic;
    property diagnosticObject : TFhirType read FDiagnostic write SetDiagnostic;

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
    {@member conclusion
      Typed access to Concise and clinically contextualized narrative interpretation of the diagnostic report.
    }
    property conclusion : String read GetConclusionST write SetConclusionST;
    property conclusionObject : TFhirString read FConclusion write SetConclusion;

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
    Function GetCreatedST : TDateTimeEx;
    Procedure SetCreatedST(value : TDateTimeEx);
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
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member masterIdentifier
      A single identifier that uniquely identifies this manifest. Principally used to refer to the manifest in non-FHIR contexts.
    }
    property masterIdentifier : TFhirIdentifier read FMasterIdentifier write SetMasterIdentifier;
    property masterIdentifierObject : TFhirIdentifier read FMasterIdentifier write SetMasterIdentifier;

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
    property type_Object : TFhirCodeableConcept read FType_ write SetType_;

    {@member authorList
      Identifies who is responsible for adding the information to the document.
    }
    property authorList : TFhirResourceReferenceList{Resource} read FAuthorList;

    {@member created
      When the document manifest was created for submission to the server (not necessarily the same thing as the actual resource last modified time, since it may be modified, replicated etc).
    }
    {@member created
      Typed access to When the document manifest was created for submission to the server (not necessarily the same thing as the actual resource last modified time, since it may be modified, replicated etc).
    }
    property created : TDateTimeEx read GetCreatedST write SetCreatedST;
    property createdObject : TFhirDateTime read FCreated write SetCreated;

    {@member source
      Identifies the source system, application, or software that produced the document manifest.
    }
    {@member source
      Typed access to Identifies the source system, application, or software that produced the document manifest.
    }
    property source : String read GetSourceST write SetSourceST;
    property sourceObject : TFhirUri read FSource write SetSource;

    {@member status
      The status of this document manifest.
    }
    property status : TFhirDocumentReferenceStatus read GetStatusST write SetStatusST;
    property statusObject : TFhirEnum read FStatus write SetStatus;

    {@member supercedes
      Whether this document manifest replaces another.
    }
    property supercedes : TFhirResourceReference{TFhirDocumentManifest} read FSupercedes write SetSupercedes;
    property supercedesObject : TFhirResourceReference{TFhirDocumentManifest} read FSupercedes write SetSupercedes;

    {@member description
      Human-readable description of the source document. This is sometimes known as the "title".
    }
    {@member description
      Typed access to Human-readable description of the source document. This is sometimes known as the "title".
    }
    property description : String read GetDescriptionST write SetDescriptionST;
    property descriptionObject : TFhirString read FDescription write SetDescription;

    {@member confidentiality
      A code specifying the level of confidentiality of this set of Documents.
    }
    property confidentiality : TFhirCodeableConcept read FConfidentiality write SetConfidentiality;
    property confidentialityObject : TFhirCodeableConcept read FConfidentiality write SetConfidentiality;

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
    Function GetCreatedST : TDateTimeEx;
    Procedure SetCreatedST(value : TDateTimeEx);
    Procedure SetIndexed(value : TFhirInstant);
    Function GetIndexedST : TDateTimeEx;
    Procedure SetIndexedST(value : TDateTimeEx);
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
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member masterIdentifier
      Document identifier as assigned by the source of the document. This identifier is specific to this version of the document. This unique identifier may be used elsewhere to identify this version of the document.
    }
    property masterIdentifier : TFhirIdentifier read FMasterIdentifier write SetMasterIdentifier;
    property masterIdentifierObject : TFhirIdentifier read FMasterIdentifier write SetMasterIdentifier;

    {@member identifierList
      Other identifiers associated with the document, including version independent, source record and workflow related identifiers.
    }
    property identifierList : TFhirIdentifierList read FIdentifierList;

    {@member subject
      Who or what the document is about. The document can be about a person, (patient or healthcare practitioner), a device (I.e. machine) or even a group of subjects (such as a document about a herd of farm animals, or a set of patients that share a common exposure).
    }
    property subject : TFhirResourceReference{Resource} read FSubject write SetSubject;
    property subjectObject : TFhirResourceReference{Resource} read FSubject write SetSubject;

    {@member type_
      Specifies the particular kind of document (e.g. Patient Summary, Discharge Summary, Prescription, etc.).
    }
    property type_ : TFhirCodeableConcept read FType_ write SetType_;
    property type_Object : TFhirCodeableConcept read FType_ write SetType_;

    {@member class_
      A categorization for the type of the document. This may be implied by or derived from the code specified in the Document Type.
    }
    property class_ : TFhirCodeableConcept read FClass_ write SetClass_;
    property class_Object : TFhirCodeableConcept read FClass_ write SetClass_;

    {@member authorList
      Identifies who is responsible for adding the information to the document.
    }
    property authorList : TFhirResourceReferenceList{Resource} read FAuthorList;

    {@member custodian
      Identifies the organization or group who is responsible for ongoing maintenance of and access to the document.
    }
    property custodian : TFhirResourceReference{TFhirOrganization} read FCustodian write SetCustodian;
    property custodianObject : TFhirResourceReference{TFhirOrganization} read FCustodian write SetCustodian;

    {@member policyManager
      A reference to a domain or server that manages policies under which the document is accessed and/or made available.
    }
    {@member policyManager
      Typed access to A reference to a domain or server that manages policies under which the document is accessed and/or made available.
    }
    property policyManager : String read GetPolicyManagerST write SetPolicyManagerST;
    property policyManagerObject : TFhirUri read FPolicyManager write SetPolicyManager;

    {@member authenticator
      Which person or organization authenticates that this document is valid.
    }
    property authenticator : TFhirResourceReference{Resource} read FAuthenticator write SetAuthenticator;
    property authenticatorObject : TFhirResourceReference{Resource} read FAuthenticator write SetAuthenticator;

    {@member created
      When the document was created.
    }
    {@member created
      Typed access to When the document was created.
    }
    property created : TDateTimeEx read GetCreatedST write SetCreatedST;
    property createdObject : TFhirDateTime read FCreated write SetCreated;

    {@member indexed
      When the document reference was created.
    }
    {@member indexed
      Typed access to When the document reference was created.
    }
    property indexed : TDateTimeEx read GetIndexedST write SetIndexedST;
    property indexedObject : TFhirInstant read FIndexed write SetIndexed;

    {@member status
      The status of this document reference.
    }
    property status : TFhirDocumentReferenceStatus read GetStatusST write SetStatusST;
    property statusObject : TFhirEnum read FStatus write SetStatus;

    {@member docStatus
      The status of the underlying document.
    }
    property docStatus : TFhirCodeableConcept read FDocStatus write SetDocStatus;
    property docStatusObject : TFhirCodeableConcept read FDocStatus write SetDocStatus;

    {@member relatesToList
      Relationships that this document has with other document references that already exist.
    }
    property relatesToList : TFhirDocumentReferenceRelatesToList read FRelatesToList;

    {@member description
      Human-readable description of the source document. This is sometimes known as the "title".
    }
    {@member description
      Typed access to Human-readable description of the source document. This is sometimes known as the "title".
    }
    property description : String read GetDescriptionST write SetDescriptionST;
    property descriptionObject : TFhirString read FDescription write SetDescription;

    {@member confidentialityList
      A code specifying the level of confidentiality of the XDS Document.
    }
    property confidentialityList : TFhirCodeableConceptList read FConfidentialityList;

    {@member primaryLanguage
      The primary language in which the source document is written.
    }
    {@member primaryLanguage
      Typed access to The primary language in which the source document is written.
    }
    property primaryLanguage : String read GetPrimaryLanguageST write SetPrimaryLanguageST;
    property primaryLanguageObject : TFhirCode read FPrimaryLanguage write SetPrimaryLanguage;

    {@member mimeType
      The mime type of the source document.
    }
    {@member mimeType
      Typed access to The mime type of the source document.
    }
    property mimeType : String read GetMimeTypeST write SetMimeTypeST;
    property mimeTypeObject : TFhirCode read FMimeType write SetMimeType;

    {@member formatList
      An identifier that identifies that the format and content of the document conforms to additional rules beyond the base format indicated in the mimeType.
    }
    property formatList : TFhirUriList read FFormatList;

    {@member size
      The size of the source document this reference refers to in bytes.
    }
    {@member size
      Typed access to The size of the source document this reference refers to in bytes.
    }
    property size : String read GetSizeST write SetSizeST;
    property sizeObject : TFhirInteger read FSize write SetSize;

    {@member hash
      A hash of the source document to ensure that changes have not occurred.
    }
    {@member hash
      Typed access to A hash of the source document to ensure that changes have not occurred.
    }
    property hash : String read GetHashST write SetHashST;
    property hashObject : TFhirString read FHash write SetHash;

    {@member location
      A url at which the document can be accessed.
    }
    {@member location
      Typed access to A url at which the document can be accessed.
    }
    property location : String read GetLocationST write SetLocationST;
    property locationObject : TFhirUri read FLocation write SetLocation;

    {@member service
      A description of a service call that can be used to retrieve the document.
    }
    property service : TFhirDocumentReferenceService read FService write SetService;
    property serviceObject : TFhirDocumentReferenceService read FService write SetService;

    {@member context
      The clinical context in which the document was prepared.
    }
    property context : TFhirDocumentReferenceContext read FContext write SetContext;
    property contextObject : TFhirDocumentReferenceContext read FContext write SetContext;

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
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member identifierList
      Identifier(s) by which this encounter is known.
    }
    property identifierList : TFhirIdentifierList read FIdentifierList;

    {@member status
      planned | in progress | onleave | finished | cancelled.
    }
    property status : TFhirEncounterState read GetStatusST write SetStatusST;
    property statusObject : TFhirEnum read FStatus write SetStatus;

    {@member class_
      inpatient | outpatient | ambulatory | emergency +.
    }
    property class_ : TFhirEncounterClass read GetClass_ST write SetClass_ST;
    property class_Object : TFhirEnum read FClass_ write SetClass_;

    {@member type_List
      Specific type of encounter (e.g. e-mail consultation, surgical day-care, skilled nursing, rehabilitation).
    }
    property type_List : TFhirCodeableConceptList read FType_List;

    {@member subject
      The patient present at the encounter.
    }
    property subject : TFhirResourceReference{TFhirPatient} read FSubject write SetSubject;
    property subjectObject : TFhirResourceReference{TFhirPatient} read FSubject write SetSubject;

    {@member participantList
      The main practitioner responsible for providing the service.
    }
    property participantList : TFhirEncounterParticipantList read FParticipantList;

    {@member period
      The start and end time of the encounter.
    }
    property period : TFhirPeriod read FPeriod write SetPeriod;
    property periodObject : TFhirPeriod read FPeriod write SetPeriod;

    {@member length
      Quantity of time the encounter lasted. This excludes the time during leaves of absence.
    }
    property length : TFhirQuantity read FLength write SetLength;
    property lengthObject : TFhirQuantity read FLength write SetLength;

    {@member reason
      Reason the encounter takes place, expressed as a code. For admissions, this can be used for a coded admission diagnosis.
    }
    property reason : TFhirCodeableConcept read FReason write SetReason;
    property reasonObject : TFhirCodeableConcept read FReason write SetReason;

    {@member indication
      Reason the encounter takes place, as specified using information from another resource. For admissions, this is the admission diagnosis.
    }
    property indication : TFhirResourceReference{Resource} read FIndication write SetIndication;
    property indicationObject : TFhirResourceReference{Resource} read FIndication write SetIndication;

    {@member priority
      Indicates the urgency of the encounter.
    }
    property priority : TFhirCodeableConcept read FPriority write SetPriority;
    property priorityObject : TFhirCodeableConcept read FPriority write SetPriority;

    {@member hospitalization
      Details about an admission to a clinic.
    }
    property hospitalization : TFhirEncounterHospitalization read FHospitalization write SetHospitalization;
    property hospitalizationObject : TFhirEncounterHospitalization read FHospitalization write SetHospitalization;

    {@member locationList
      List of locations at which the patient has been.
    }
    property locationList : TFhirEncounterLocationList read FLocationList;

    {@member serviceProvider
      Department or team providing care.
    }
    property serviceProvider : TFhirResourceReference{TFhirOrganization} read FServiceProvider write SetServiceProvider;
    property serviceProviderObject : TFhirResourceReference{TFhirOrganization} read FServiceProvider write SetServiceProvider;

    {@member partOf
      Another Encounter of which this encounter is a part of (administratively or in time).
    }
    property partOf : TFhirResourceReference{TFhirEncounter} read FPartOf write SetPartOf;
    property partOfObject : TFhirResourceReference{TFhirEncounter} read FPartOf write SetPartOf;

  end;


  {@Class TFhirFamilyHistory : TFhirResource
    Significant health events and conditions for people related to the subject relevant in the context of care for the subject.
  }
  {!.Net HL7Connect.Fhir.FamilyHistory}
  TFhirFamilyHistory = class (TFhirResource)
  private
    FidentifierList : TFhirIdentifierList;
    FSubject : TFhirResourceReference{TFhirPatient};
    FNote : TFhirString;
    FrelationList : TFhirFamilyHistoryRelationList;
    Procedure SetSubject(value : TFhirResourceReference{TFhirPatient});
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
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
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
    property subjectObject : TFhirResourceReference{TFhirPatient} read FSubject write SetSubject;

    {@member note
      Conveys information about family history not specific to individual relations.
    }
    {@member note
      Typed access to Conveys information about family history not specific to individual relations.
    }
    property note : String read GetNoteST write SetNoteST;
    property noteObject : TFhirString read FNote write SetNote;

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
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member identifier
      A unique business identifier for this group.
    }
    property identifier : TFhirIdentifier read FIdentifier write SetIdentifier;
    property identifierObject : TFhirIdentifier read FIdentifier write SetIdentifier;

    {@member type_
      Identifies the broad classification of the kind of resources the group includes.
    }
    property type_ : TFhirGroupType read GetType_ST write SetType_ST;
    property type_Object : TFhirEnum read FType_ write SetType_;

    {@member actual
      If true, indicates that the resource refers to a specific group of real individuals.  If false, the group defines a set of intended individuals.
    }
    {@member actual
      Typed access to If true, indicates that the resource refers to a specific group of real individuals.  If false, the group defines a set of intended individuals.
    }
    property actual : Boolean read GetActualST write SetActualST;
    property actualObject : TFhirBoolean read FActual write SetActual;

    {@member code
      Provides a specific type of resource the group includes.  E.g. "cow", "syringe", etc.
    }
    property code : TFhirCodeableConcept read FCode write SetCode;
    property codeObject : TFhirCodeableConcept read FCode write SetCode;

    {@member name
      A label assigned to the group for human identification and communication.
    }
    {@member name
      Typed access to A label assigned to the group for human identification and communication.
    }
    property name : String read GetNameST write SetNameST;
    property nameObject : TFhirString read FName write SetName;

    {@member quantity
      A count of the number of resource instances that are part of the group.
    }
    {@member quantity
      Typed access to A count of the number of resource instances that are part of the group.
    }
    property quantity : String read GetQuantityST write SetQuantityST;
    property quantityObject : TFhirInteger read FQuantity write SetQuantity;

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
    Function GetDateTimeST : TDateTimeEx;
    Procedure SetDateTimeST(value : TDateTimeEx);
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
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member dateTime
      Date and Time the study took place.
    }
    {@member dateTime
      Typed access to Date and Time the study took place.
    }
    property dateTime : TDateTimeEx read GetDateTimeST write SetDateTimeST;
    property dateTimeObject : TFhirDateTime read FDateTime write SetDateTime;

    {@member subject
      Who the images are of.
    }
    property subject : TFhirResourceReference{TFhirPatient} read FSubject write SetSubject;
    property subjectObject : TFhirResourceReference{TFhirPatient} read FSubject write SetSubject;

    {@member uid
      Formal identifier for the study.
    }
    {@member uid
      Typed access to Formal identifier for the study.
    }
    property uid : String read GetUidST write SetUidST;
    property uidObject : TFhirOid read FUid write SetUid;

    {@member accessionNo
      Accession Number.
    }
    property accessionNo : TFhirIdentifier read FAccessionNo write SetAccessionNo;
    property accessionNoObject : TFhirIdentifier read FAccessionNo write SetAccessionNo;

    {@member identifierList
      Other identifiers for the study.
    }
    property identifierList : TFhirIdentifierList read FIdentifierList;

    {@member orderList
      A list of the diagnostic orders that resulted in this imaging study being performed.
    }
    property orderList : TFhirResourceReferenceList{TFhirDiagnosticOrder} read FOrderList;

    property modality : TFhirEnumList read FModality;
    {@member referrer
      The requesting/referring physician.
    }
    property referrer : TFhirResourceReference{TFhirPractitioner} read FReferrer write SetReferrer;
    property referrerObject : TFhirResourceReference{TFhirPractitioner} read FReferrer write SetReferrer;

    {@member availability
      Availability of study (online, offline or nearline).
    }
    property availability : TFhirInstanceAvailability read GetAvailabilityST write SetAvailabilityST;
    property availabilityObject : TFhirEnum read FAvailability write SetAvailability;

    {@member url
      WADO-RS URI where Study is available.
    }
    {@member url
      Typed access to WADO-RS URI where Study is available.
    }
    property url : String read GetUrlST write SetUrlST;
    property urlObject : TFhirUri read FUrl write SetUrl;

    {@member numberOfSeries
      Number of Series in Study.
    }
    {@member numberOfSeries
      Typed access to Number of Series in Study.
    }
    property numberOfSeries : String read GetNumberOfSeriesST write SetNumberOfSeriesST;
    property numberOfSeriesObject : TFhirInteger read FNumberOfSeries write SetNumberOfSeries;

    {@member numberOfInstances
      Number of SOP Instances in Study.
    }
    {@member numberOfInstances
      Typed access to Number of SOP Instances in Study.
    }
    property numberOfInstances : String read GetNumberOfInstancesST write SetNumberOfInstancesST;
    property numberOfInstancesObject : TFhirInteger read FNumberOfInstances write SetNumberOfInstances;

    {@member clinicalInformation
      Diagnoses etc provided with request.
    }
    {@member clinicalInformation
      Typed access to Diagnoses etc provided with request.
    }
    property clinicalInformation : String read GetClinicalInformationST write SetClinicalInformationST;
    property clinicalInformationObject : TFhirString read FClinicalInformation write SetClinicalInformation;

    {@member procedure_List
      Type of procedure performed.
    }
    property procedure_List : TFhirCodingList read FProcedure_List;

    {@member interpreter
      Who read study and interpreted the images.
    }
    property interpreter : TFhirResourceReference{TFhirPractitioner} read FInterpreter write SetInterpreter;
    property interpreterObject : TFhirResourceReference{TFhirPractitioner} read FInterpreter write SetInterpreter;

    {@member description
      Institution-generated description or classification of the Study (component) performed.
    }
    {@member description
      Typed access to Institution-generated description or classification of the Study (component) performed.
    }
    property description : String read GetDescriptionST write SetDescriptionST;
    property descriptionObject : TFhirString read FDescription write SetDescription;

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
    Function GetDateST : TDateTimeEx;
    Procedure SetDateST(value : TDateTimeEx);
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
    Function GetExpirationDateST : TDateTimeEx;
    Procedure SetExpirationDateST(value : TDateTimeEx);
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
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member identifierList
      A unique identifier assigned to this adverse reaction record.
    }
    property identifierList : TFhirIdentifierList read FIdentifierList;

    {@member date
      Date vaccine administered or was to be administered.
    }
    {@member date
      Typed access to Date vaccine administered or was to be administered.
    }
    property date : TDateTimeEx read GetDateST write SetDateST;
    property dateObject : TFhirDateTime read FDate write SetDate;

    {@member vaccineType
      Vaccine that was administered or was to be administered.
    }
    property vaccineType : TFhirCodeableConcept read FVaccineType write SetVaccineType;
    property vaccineTypeObject : TFhirCodeableConcept read FVaccineType write SetVaccineType;

    {@member subject
      The patient to whom the vaccine was to be administered.
    }
    property subject : TFhirResourceReference{TFhirPatient} read FSubject write SetSubject;
    property subjectObject : TFhirResourceReference{TFhirPatient} read FSubject write SetSubject;

    {@member refusedIndicator
      Indicates if the vaccination was refused.
    }
    {@member refusedIndicator
      Typed access to Indicates if the vaccination was refused.
    }
    property refusedIndicator : Boolean read GetRefusedIndicatorST write SetRefusedIndicatorST;
    property refusedIndicatorObject : TFhirBoolean read FRefusedIndicator write SetRefusedIndicator;

    {@member reported
      True if this administration was reported rather than directly administered.
    }
    {@member reported
      Typed access to True if this administration was reported rather than directly administered.
    }
    property reported : Boolean read GetReportedST write SetReportedST;
    property reportedObject : TFhirBoolean read FReported write SetReported;

    {@member performer
      Clinician who administered the vaccine.
    }
    property performer : TFhirResourceReference{TFhirPractitioner} read FPerformer write SetPerformer;
    property performerObject : TFhirResourceReference{TFhirPractitioner} read FPerformer write SetPerformer;

    {@member requester
      Clinician who ordered the vaccination.
    }
    property requester : TFhirResourceReference{TFhirPractitioner} read FRequester write SetRequester;
    property requesterObject : TFhirResourceReference{TFhirPractitioner} read FRequester write SetRequester;

    {@member manufacturer
      Name of vaccine manufacturer.
    }
    property manufacturer : TFhirResourceReference{TFhirOrganization} read FManufacturer write SetManufacturer;
    property manufacturerObject : TFhirResourceReference{TFhirOrganization} read FManufacturer write SetManufacturer;

    {@member location
      The service delivery location where the vaccine administration occurred.
    }
    property location : TFhirResourceReference{TFhirLocation} read FLocation write SetLocation;
    property locationObject : TFhirResourceReference{TFhirLocation} read FLocation write SetLocation;

    {@member lotNumber
      Lot number of the  vaccine product.
    }
    {@member lotNumber
      Typed access to Lot number of the  vaccine product.
    }
    property lotNumber : String read GetLotNumberST write SetLotNumberST;
    property lotNumberObject : TFhirString read FLotNumber write SetLotNumber;

    {@member expirationDate
      Date vaccine batch expires.
    }
    {@member expirationDate
      Typed access to Date vaccine batch expires.
    }
    property expirationDate : TDateTimeEx read GetExpirationDateST write SetExpirationDateST;
    property expirationDateObject : TFhirDate read FExpirationDate write SetExpirationDate;

    {@member site
      Body site where vaccine was administered.
    }
    property site : TFhirCodeableConcept read FSite write SetSite;
    property siteObject : TFhirCodeableConcept read FSite write SetSite;

    {@member route
      The path by which the vaccine product is taken into the body.
    }
    property route : TFhirCodeableConcept read FRoute write SetRoute;
    property routeObject : TFhirCodeableConcept read FRoute write SetRoute;

    {@member doseQuantity
      The quantity of vaccine product that was administered.
    }
    property doseQuantity : TFhirQuantity read FDoseQuantity write SetDoseQuantity;
    property doseQuantityObject : TFhirQuantity read FDoseQuantity write SetDoseQuantity;

    {@member explanation
      Reasons why a vaccine was administered or refused.
    }
    property explanation : TFhirImmunizationExplanation read FExplanation write SetExplanation;
    property explanationObject : TFhirImmunizationExplanation read FExplanation write SetExplanation;

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
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
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
    property subjectObject : TFhirResourceReference{TFhirPatient} read FSubject write SetSubject;

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
    Function GetDateST : TDateTimeEx;
    Procedure SetDateST(value : TDateTimeEx);
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
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
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
    property codeObject : TFhirCodeableConcept read FCode write SetCode;

    {@member subject
      The common subject (or patient) of the resources that are in the list, if there is one.
    }
    property subject : TFhirResourceReference{Resource} read FSubject write SetSubject;
    property subjectObject : TFhirResourceReference{Resource} read FSubject write SetSubject;

    {@member source
      The entity responsible for deciding what the contents of the list were.
    }
    property source : TFhirResourceReference{Resource} read FSource write SetSource;
    property sourceObject : TFhirResourceReference{Resource} read FSource write SetSource;

    {@member date
      The date that the list was prepared.
    }
    {@member date
      Typed access to The date that the list was prepared.
    }
    property date : TDateTimeEx read GetDateST write SetDateST;
    property dateObject : TFhirDateTime read FDate write SetDate;

    {@member ordered
      Whether items in the list have a meaningful order.
    }
    {@member ordered
      Typed access to Whether items in the list have a meaningful order.
    }
    property ordered : Boolean read GetOrderedST write SetOrderedST;
    property orderedObject : TFhirBoolean read FOrdered write SetOrdered;

    {@member mode
      How this list was prepared - whether it is a working list that is suitable for being maintained on an ongoing basis, or if it represents a snapshot of a list of items from another source, or whether it is a prepared list where items may be marked as added, modified or deleted.
    }
    property mode : TFhirListMode read GetModeST write SetModeST;
    property modeObject : TFhirEnum read FMode write SetMode;

    {@member entryList
      Entries in this list.
    }
    property entryList : TFhirListEntryList read FEntryList;

    {@member emptyReason
      If the list is empty, why the list is empty.
    }
    property emptyReason : TFhirCodeableConcept read FEmptyReason write SetEmptyReason;
    property emptyReasonObject : TFhirCodeableConcept read FEmptyReason write SetEmptyReason;

  end;


  {@Class TFhirLocation : TFhirResource
    Details and position information for a physical place where services are provided  and resources and participants may be stored, found, contained or accommodated.
  }
  {!.Net HL7Connect.Fhir.Location}
  TFhirLocation = class (TFhirResource)
  private
    FIdentifier : TFhirIdentifier;
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
    Procedure SetIdentifier(value : TFhirIdentifier);
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
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member identifier
      Unique code or number identifying the location to its users.
    }
    property identifier : TFhirIdentifier read FIdentifier write SetIdentifier;
    property identifierObject : TFhirIdentifier read FIdentifier write SetIdentifier;

    {@member name
      Name of the location as used by humans. Does not need to be unique.
    }
    {@member name
      Typed access to Name of the location as used by humans. Does not need to be unique.
    }
    property name : String read GetNameST write SetNameST;
    property nameObject : TFhirString read FName write SetName;

    {@member description
      Description of the Location, which helps in finding or referencing the place.
    }
    {@member description
      Typed access to Description of the Location, which helps in finding or referencing the place.
    }
    property description : String read GetDescriptionST write SetDescriptionST;
    property descriptionObject : TFhirString read FDescription write SetDescription;

    {@member type_
      Indicates the type of function performed at the location.
    }
    property type_ : TFhirCodeableConcept read FType_ write SetType_;
    property type_Object : TFhirCodeableConcept read FType_ write SetType_;

    {@member telecomList
      The contact details of communication devices available at the location. This can include phone numbers, fax numbers, mobile numbers, email addresses and web sites.
    }
    property telecomList : TFhirContactList read FTelecomList;

    {@member address
      Physical location.
    }
    property address : TFhirAddress read FAddress write SetAddress;
    property addressObject : TFhirAddress read FAddress write SetAddress;

    {@member physicalType
      Physical form of the location, e.g. building, room, vehicle, road.
    }
    property physicalType : TFhirCodeableConcept read FPhysicalType write SetPhysicalType;
    property physicalTypeObject : TFhirCodeableConcept read FPhysicalType write SetPhysicalType;

    {@member position
      The absolute geographic location of the Location, expressed in a KML compatible manner (see notes below for KML).
    }
    property position : TFhirLocationPosition read FPosition write SetPosition;
    property positionObject : TFhirLocationPosition read FPosition write SetPosition;

    {@member managingOrganization
      The organization that is responsible for the provisioning and upkeep of the location.
    }
    property managingOrganization : TFhirResourceReference{TFhirOrganization} read FManagingOrganization write SetManagingOrganization;
    property managingOrganizationObject : TFhirResourceReference{TFhirOrganization} read FManagingOrganization write SetManagingOrganization;

    {@member status
      active | suspended | inactive.
    }
    property status : TFhirLocationStatus read GetStatusST write SetStatusST;
    property statusObject : TFhirEnum read FStatus write SetStatus;

    {@member partOf
      Another Location which this Location is physically part of.
    }
    property partOf : TFhirResourceReference{TFhirLocation} read FPartOf write SetPartOf;
    property partOfObject : TFhirResourceReference{TFhirLocation} read FPartOf write SetPartOf;

    {@member mode
      Indicates whether a resource instance represents a specific location or a class of locations.
    }
    property mode : TFhirLocationMode read GetModeST write SetModeST;
    property modeObject : TFhirEnum read FMode write SetMode;

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
    Function GetDateTimeST : TDateTimeEx;
    Procedure SetDateTimeST(value : TDateTimeEx);
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
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member type_
      Whether the media is a photo (still image), an audio recording, or a video recording.
    }
    property type_ : TFhirMediaType read GetType_ST write SetType_ST;
    property type_Object : TFhirEnum read FType_ write SetType_;

    {@member subtype
      Details of the type of the media - usually, how it was acquired (what type of device). If images sourced from a DICOM system, are wrapped in a Media resource, then this is the modality.
    }
    property subtype : TFhirCodeableConcept read FSubtype write SetSubtype;
    property subtypeObject : TFhirCodeableConcept read FSubtype write SetSubtype;

    {@member identifierList
      Identifiers associated with the image - these may include identifiers for the image itself, identifiers for the context of its collection (e.g. series ids) and context ids such as accession numbers or other workflow identifiers.
    }
    property identifierList : TFhirIdentifierList read FIdentifierList;

    {@member dateTime
      When the media was originally recorded. For video and audio, if the length of the recording is not insignificant, this is the end of the recording.
    }
    {@member dateTime
      Typed access to When the media was originally recorded. For video and audio, if the length of the recording is not insignificant, this is the end of the recording.
    }
    property dateTime : TDateTimeEx read GetDateTimeST write SetDateTimeST;
    property dateTimeObject : TFhirDateTime read FDateTime write SetDateTime;

    {@member subject
      Who/What this Media is a record of.
    }
    property subject : TFhirResourceReference{Resource} read FSubject write SetSubject;
    property subjectObject : TFhirResourceReference{Resource} read FSubject write SetSubject;

    {@member operator
      The person who administered the collection of the image.
    }
    property operator : TFhirResourceReference{TFhirPractitioner} read FOperator write SetOperator;
    property operatorObject : TFhirResourceReference{TFhirPractitioner} read FOperator write SetOperator;

    {@member view
      The name of the imaging view e.g Lateral or Antero-posterior (AP).
    }
    property view : TFhirCodeableConcept read FView write SetView;
    property viewObject : TFhirCodeableConcept read FView write SetView;

    {@member deviceName
      The name of the device / manufacturer of the device  that was used to make the recording.
    }
    {@member deviceName
      Typed access to The name of the device / manufacturer of the device  that was used to make the recording.
    }
    property deviceName : String read GetDeviceNameST write SetDeviceNameST;
    property deviceNameObject : TFhirString read FDeviceName write SetDeviceName;

    {@member height
      Height of the image in pixels(photo/video).
    }
    {@member height
      Typed access to Height of the image in pixels(photo/video).
    }
    property height : String read GetHeightST write SetHeightST;
    property heightObject : TFhirInteger read FHeight write SetHeight;

    {@member width
      Width of the image in pixels (photo/video).
    }
    {@member width
      Typed access to Width of the image in pixels (photo/video).
    }
    property width : String read GetWidthST write SetWidthST;
    property widthObject : TFhirInteger read FWidth write SetWidth;

    {@member frames
      The number of frames in a photo. This is used with a multi-page fax, or an imaging acquisition context that takes multiple slices in a single image, or an animated gif. If there is more than one frame, this SHALL have a value in order to alert interface software that a multi-frame capable rendering widget is required.
    }
    {@member frames
      Typed access to The number of frames in a photo. This is used with a multi-page fax, or an imaging acquisition context that takes multiple slices in a single image, or an animated gif. If there is more than one frame, this SHALL have a value in order to alert interface software that a multi-frame capable rendering widget is required.
    }
    property frames : String read GetFramesST write SetFramesST;
    property framesObject : TFhirInteger read FFrames write SetFrames;

    {@member length
      The length of the recording in seconds - for audio and video.
    }
    {@member length
      Typed access to The length of the recording in seconds - for audio and video.
    }
    property length : String read GetLengthST write SetLengthST;
    property lengthObject : TFhirInteger read FLength write SetLength;

    {@member content
      The actual content of the media - inline or by direct reference to the media source file.
    }
    property content : TFhirAttachment read FContent write SetContent;
    property contentObject : TFhirAttachment read FContent write SetContent;

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
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member name
      The common/commercial name of the medication absent information such as strength, form, etc.  E.g. Acetaminophen, Tylenol 3, etc.  The fully coordinated name is communicated as the display of Medication.code.
    }
    {@member name
      Typed access to The common/commercial name of the medication absent information such as strength, form, etc.  E.g. Acetaminophen, Tylenol 3, etc.  The fully coordinated name is communicated as the display of Medication.code.
    }
    property name : String read GetNameST write SetNameST;
    property nameObject : TFhirString read FName write SetName;

    {@member code
      A code (or set of codes) that identify this medication.   Usage note: This could be a standard drug code such as a drug regulator code, RxNorm code, SNOMED CT code, etc. It could also be a local formulary code, optionally with translations to the standard drug codes.
    }
    property code : TFhirCodeableConcept read FCode write SetCode;
    property codeObject : TFhirCodeableConcept read FCode write SetCode;

    {@member isBrand
      Set to true if the item is attributable to a specific manufacturer (even if we don't know who that is).
    }
    {@member isBrand
      Typed access to Set to true if the item is attributable to a specific manufacturer (even if we don't know who that is).
    }
    property isBrand : Boolean read GetIsBrandST write SetIsBrandST;
    property isBrandObject : TFhirBoolean read FIsBrand write SetIsBrand;

    {@member manufacturer
      Describes the details of the manufacturer.
    }
    property manufacturer : TFhirResourceReference{TFhirOrganization} read FManufacturer write SetManufacturer;
    property manufacturerObject : TFhirResourceReference{TFhirOrganization} read FManufacturer write SetManufacturer;

    {@member kind
      Medications are either a single administrable product or a package that contains one or more products.
    }
    property kind : TFhirMedicationKind read GetKindST write SetKindST;
    property kindObject : TFhirEnum read FKind write SetKind;

    {@member product
      Information that only applies to products (not packages).
    }
    property product : TFhirMedicationProduct read FProduct write SetProduct;
    property productObject : TFhirMedicationProduct read FProduct write SetProduct;

    {@member package
      Information that only applies to packages (not products).
    }
    property package : TFhirMedicationPackage read FPackage write SetPackage;
    property packageObject : TFhirMedicationPackage read FPackage write SetPackage;

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
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member identifierList
      External identifier - FHIR will generate its own internal IDs (probably URLs) which do not need to be explicitly managed by the resource.  The identifier here is one that would be used by another non-FHIR system - for example an automated medication pump would provide a record each time it operated; an administration while the patient was off the ward might be made with a different system and entered after the event.  Particularly important if these records have to be updated.
    }
    property identifierList : TFhirIdentifierList read FIdentifierList;

    {@member status
      Will generally be set to show that the administration has been completed.  For some long running administrations such as infusions it is possible for an administration to be started but not completed or it may be paused while some other process is under way.
    }
    property status : TFhirMedicationAdminStatus read GetStatusST write SetStatusST;
    property statusObject : TFhirEnum read FStatus write SetStatus;

    {@member patient
      The person or animal to whom the medication was given.
    }
    property patient : TFhirResourceReference{TFhirPatient} read FPatient write SetPatient;
    property patientObject : TFhirResourceReference{TFhirPatient} read FPatient write SetPatient;

    {@member practitioner
      The individual who was responsible for giving the medication to the patient.
    }
    property practitioner : TFhirResourceReference{TFhirPractitioner} read FPractitioner write SetPractitioner;
    property practitionerObject : TFhirResourceReference{TFhirPractitioner} read FPractitioner write SetPractitioner;

    {@member encounter
      The visit or admission the or other contact between patient and health care provider the medication administration was performed as part of.
    }
    property encounter : TFhirResourceReference{TFhirEncounter} read FEncounter write SetEncounter;
    property encounterObject : TFhirResourceReference{TFhirEncounter} read FEncounter write SetEncounter;

    {@member prescription
      The original request, instruction or authority to perform the administration.
    }
    property prescription : TFhirResourceReference{TFhirMedicationPrescription} read FPrescription write SetPrescription;
    property prescriptionObject : TFhirResourceReference{TFhirMedicationPrescription} read FPrescription write SetPrescription;

    {@member wasNotGiven
      Set this to true if the record is saying that the medication was NOT administered.
    }
    {@member wasNotGiven
      Typed access to Set this to true if the record is saying that the medication was NOT administered.
    }
    property wasNotGiven : Boolean read GetWasNotGivenST write SetWasNotGivenST;
    property wasNotGivenObject : TFhirBoolean read FWasNotGiven write SetWasNotGiven;

    {@member reasonNotGivenList
      A code indicating why the administration was not performed.
    }
    property reasonNotGivenList : TFhirCodeableConceptList read FReasonNotGivenList;

    {@member whenGiven
      An interval of time during which the administration took place.  For many administrations, such as swallowing a tablet the lower and upper values of the interval will be the same.
    }
    property whenGiven : TFhirPeriod read FWhenGiven write SetWhenGiven;
    property whenGivenObject : TFhirPeriod read FWhenGiven write SetWhenGiven;

    {@member medication
      Identifies the medication that was administered. This is either a link to a resource representing the details of the medication or a simple attribute carrying a code that identifies the medication from a known list of medications.
    }
    property medication : TFhirResourceReference{TFhirMedication} read FMedication write SetMedication;
    property medicationObject : TFhirResourceReference{TFhirMedication} read FMedication write SetMedication;

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
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member identifier
      Identifier assigned by the dispensing facility - this is an identifier assigned outside FHIR.
    }
    property identifier : TFhirIdentifier read FIdentifier write SetIdentifier;
    property identifierObject : TFhirIdentifier read FIdentifier write SetIdentifier;

    {@member status
      A code specifying the state of the set of dispense events.
    }
    property status : TFhirMedicationDispenseStatus read GetStatusST write SetStatusST;
    property statusObject : TFhirEnum read FStatus write SetStatus;

    {@member patient
      A link to a resource representing the person to whom the medication will be given.
    }
    property patient : TFhirResourceReference{TFhirPatient} read FPatient write SetPatient;
    property patientObject : TFhirResourceReference{TFhirPatient} read FPatient write SetPatient;

    {@member dispenser
      The individual responsible for dispensing the medication.
    }
    property dispenser : TFhirResourceReference{TFhirPractitioner} read FDispenser write SetDispenser;
    property dispenserObject : TFhirResourceReference{TFhirPractitioner} read FDispenser write SetDispenser;

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
    property substitutionObject : TFhirMedicationDispenseSubstitution read FSubstitution write SetSubstitution;

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
    Function GetDateWrittenST : TDateTimeEx;
    Procedure SetDateWrittenST(value : TDateTimeEx);
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
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member identifierList
      External identifier - one that would be used by another non-FHIR system - for example a re-imbursement system might issue its own id for each prescription that is created.  This is particularly important where FHIR only provides part of an erntire workflow process where records have to be tracked through an entire system.
    }
    property identifierList : TFhirIdentifierList read FIdentifierList;

    {@member dateWritten
      The date (and perhaps time) when the prescription was written.
    }
    {@member dateWritten
      Typed access to The date (and perhaps time) when the prescription was written.
    }
    property dateWritten : TDateTimeEx read GetDateWrittenST write SetDateWrittenST;
    property dateWrittenObject : TFhirDateTime read FDateWritten write SetDateWritten;

    {@member status
      A code specifying the state of the order.  Generally this will be active or completed state.
    }
    property status : TFhirMedicationPrescriptionStatus read GetStatusST write SetStatusST;
    property statusObject : TFhirEnum read FStatus write SetStatus;

    {@member patient
      A link to a resource representing the person to whom the medication will be given.
    }
    property patient : TFhirResourceReference{TFhirPatient} read FPatient write SetPatient;
    property patientObject : TFhirResourceReference{TFhirPatient} read FPatient write SetPatient;

    {@member prescriber
      The healthcare professional responsible for authorizing the prescription.
    }
    property prescriber : TFhirResourceReference{TFhirPractitioner} read FPrescriber write SetPrescriber;
    property prescriberObject : TFhirResourceReference{TFhirPractitioner} read FPrescriber write SetPrescriber;

    {@member encounter
      A link to a resource that identifies the particular occurrence of contact between patient and health care provider.
    }
    property encounter : TFhirResourceReference{TFhirEncounter} read FEncounter write SetEncounter;
    property encounterObject : TFhirResourceReference{TFhirEncounter} read FEncounter write SetEncounter;

    {@member reason
      Can be the reason or the indication for writing the prescription.
    }
    property reason : TFhirType read FReason write SetReason;
    property reasonObject : TFhirType read FReason write SetReason;

    {@member medication
      Identifies the medication being administered. This is either a link to a resource representing the details of the medication or a simple attribute carrying a code that identifies the medication from a known list of medications.
    }
    property medication : TFhirResourceReference{TFhirMedication} read FMedication write SetMedication;
    property medicationObject : TFhirResourceReference{TFhirMedication} read FMedication write SetMedication;

    {@member dosageInstructionList
      Indicates how the medication is to be used by the patient.
    }
    property dosageInstructionList : TFhirMedicationPrescriptionDosageInstructionList read FDosageInstructionList;

    {@member dispense
      Deals with details of the dispense part of the order.
    }
    property dispense : TFhirMedicationPrescriptionDispense read FDispense write SetDispense;
    property dispenseObject : TFhirMedicationPrescriptionDispense read FDispense write SetDispense;

    {@member substitution
      Indicates whether or not substitution can or should be part of the dispense. In some cases substitution must happen, in other cases substitution must not happen, and in others it does not matter. This block explains the prescriber's intent. If nothing is specified substitution may be done.
    }
    property substitution : TFhirMedicationPrescriptionSubstitution read FSubstitution write SetSubstitution;
    property substitutionObject : TFhirMedicationPrescriptionSubstitution read FSubstitution write SetSubstitution;

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
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
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
    property patientObject : TFhirResourceReference{TFhirPatient} read FPatient write SetPatient;

    {@member wasNotGiven
      Set this to true if the record is saying that the medication was NOT taken.
    }
    {@member wasNotGiven
      Typed access to Set this to true if the record is saying that the medication was NOT taken.
    }
    property wasNotGiven : Boolean read GetWasNotGivenST write SetWasNotGivenST;
    property wasNotGivenObject : TFhirBoolean read FWasNotGiven write SetWasNotGiven;

    {@member reasonNotGivenList
      A code indicating why the medication was not taken.
    }
    property reasonNotGivenList : TFhirCodeableConceptList read FReasonNotGivenList;

    {@member whenGiven
      The interval of time during which it is being asserted that the patient was taking the medication.
    }
    property whenGiven : TFhirPeriod read FWhenGiven write SetWhenGiven;
    property whenGivenObject : TFhirPeriod read FWhenGiven write SetWhenGiven;

    {@member medication
      Identifies the medication being administered. This is either a link to a resource representing the details of the medication or a simple attribute carrying a code that identifies the medication from a known list of medications.
    }
    property medication : TFhirResourceReference{TFhirMedication} read FMedication write SetMedication;
    property medicationObject : TFhirResourceReference{TFhirMedication} read FMedication write SetMedication;

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
    Function GetTimestampST : TDateTimeEx;
    Procedure SetTimestampST(value : TDateTimeEx);
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
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member identifier
      The identifier of this message.
    }
    {@member identifier
      Typed access to The identifier of this message.
    }
    property identifier : String read GetIdentifierST write SetIdentifierST;
    property identifierObject : TFhirId read FIdentifier write SetIdentifier;

    {@member timestamp
      The time that the message was sent.
    }
    {@member timestamp
      Typed access to The time that the message was sent.
    }
    property timestamp : TDateTimeEx read GetTimestampST write SetTimestampST;
    property timestampObject : TFhirInstant read FTimestamp write SetTimestamp;

    {@member event
      Code that identifies the event this message represents and connects it with it's definition. Events defined as part of the FHIR specification have the system value "http://hl7.org/fhir/message-type".
    }
    property event : TFhirCoding read FEvent write SetEvent;
    property eventObject : TFhirCoding read FEvent write SetEvent;

    {@member response
      Information about the message that this message is a response to.  Only present if this message is a response.
    }
    property response : TFhirMessageHeaderResponse read FResponse write SetResponse;
    property responseObject : TFhirMessageHeaderResponse read FResponse write SetResponse;

    {@member source
      The source application from which this message originated.
    }
    property source : TFhirMessageHeaderSource read FSource write SetSource;
    property sourceObject : TFhirMessageHeaderSource read FSource write SetSource;

    {@member destinationList
      The destination application which the message is intended for.
    }
    property destinationList : TFhirMessageHeaderDestinationList read FDestinationList;

    {@member enterer
      The person or device that performed the data entry leading to this message. Where there is more than one candidate, pick the most proximal to the message. Can provide other enterers in extensions.
    }
    property enterer : TFhirResourceReference{TFhirPractitioner} read FEnterer write SetEnterer;
    property entererObject : TFhirResourceReference{TFhirPractitioner} read FEnterer write SetEnterer;

    {@member author
      The logical author of the message - the person or device that decided the described event should happen. Where there is more than one candidate, pick the most proximal to the MessageHeader. Can provide other authors in extensions.
    }
    property author : TFhirResourceReference{TFhirPractitioner} read FAuthor write SetAuthor;
    property authorObject : TFhirResourceReference{TFhirPractitioner} read FAuthor write SetAuthor;

    {@member receiver
      Allows data conveyed by a message to be addressed to a particular person or department when routing to a specific application isn't sufficient.
    }
    property receiver : TFhirResourceReference{Resource} read FReceiver write SetReceiver;
    property receiverObject : TFhirResourceReference{Resource} read FReceiver write SetReceiver;

    {@member responsible
      The person or organization that accepts overall responsibility for the contents of the message. The implication is that the message event happened under the policies of the responsible party.
    }
    property responsible : TFhirResourceReference{Resource} read FResponsible write SetResponsible;
    property responsibleObject : TFhirResourceReference{Resource} read FResponsible write SetResponsible;

    {@member reason
      Coded indication of the cause for the event - indicates  a reason for the occurance of the event that is a focus of this message.
    }
    property reason : TFhirCodeableConcept read FReason write SetReason;
    property reasonObject : TFhirCodeableConcept read FReason write SetReason;

    {@member dataList
      The actual data of the message - a reference to the root/focus class of the event.
    }
    property dataList : TFhirResourceReferenceList{Resource} read FDataList;

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
    Function GetIssuedST : TDateTimeEx;
    Procedure SetIssuedST(value : TDateTimeEx);
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
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member name
      Describes what was observed. Sometimes this is called the observation "code".
    }
    property name : TFhirCodeableConcept read FName write SetName;
    property nameObject : TFhirCodeableConcept read FName write SetName;

    {@member value
      The information determined as a result of making the observation, if the information has a simple value.
    }
    property value : TFhirType read FValue write SetValue;
    property valueObject : TFhirType read FValue write SetValue;

    {@member interpretation
      The assessment made based on the result of the observation.
    }
    property interpretation : TFhirCodeableConcept read FInterpretation write SetInterpretation;
    property interpretationObject : TFhirCodeableConcept read FInterpretation write SetInterpretation;

    {@member comments
      May include statements about significant, unexpected or unreliable values, or information about the source of the value where this may be relevant to the interpretation of the result.
    }
    {@member comments
      Typed access to May include statements about significant, unexpected or unreliable values, or information about the source of the value where this may be relevant to the interpretation of the result.
    }
    property comments : String read GetCommentsST write SetCommentsST;
    property commentsObject : TFhirString read FComments write SetComments;

    {@member applies
      The time or time-period the observed value is asserted as being true. For biological subjects - e.g. human patients - this is usually called the "physiologically relevant time". This is usually either the time of the procedure or of specimen collection, but very often the source of the date/time is not known, only the date/time itself.
    }
    property applies : TFhirType read FApplies write SetApplies;
    property appliesObject : TFhirType read FApplies write SetApplies;

    {@member issued
      Date/Time this was made available.
    }
    {@member issued
      Typed access to Date/Time this was made available.
    }
    property issued : TDateTimeEx read GetIssuedST write SetIssuedST;
    property issuedObject : TFhirInstant read FIssued write SetIssued;

    {@member status
      The status of the result value.
    }
    property status : TFhirObservationStatus read GetStatusST write SetStatusST;
    property statusObject : TFhirEnum read FStatus write SetStatus;

    {@member reliability
      An estimate of the degree to which quality issues have impacted on the value reported.
    }
    property reliability : TFhirObservationReliability read GetReliabilityST write SetReliabilityST;
    property reliabilityObject : TFhirEnum read FReliability write SetReliability;

    {@member bodySite
      Indicates where on the subject's body the observation was made.
    }
    property bodySite : TFhirCodeableConcept read FBodySite write SetBodySite;
    property bodySiteObject : TFhirCodeableConcept read FBodySite write SetBodySite;

    {@member method
      Indicates the mechanism used to perform the observation.
    }
    property method : TFhirCodeableConcept read FMethod write SetMethod;
    property methodObject : TFhirCodeableConcept read FMethod write SetMethod;

    {@member identifier
      A unique identifier for the simple observation.
    }
    property identifier : TFhirIdentifier read FIdentifier write SetIdentifier;
    property identifierObject : TFhirIdentifier read FIdentifier write SetIdentifier;

    {@member subject
      The thing the observation is being made about.
    }
    property subject : TFhirResourceReference{Resource} read FSubject write SetSubject;
    property subjectObject : TFhirResourceReference{Resource} read FSubject write SetSubject;

    {@member specimen
      The specimen that was used when this observation was made.
    }
    property specimen : TFhirResourceReference{TFhirSpecimen} read FSpecimen write SetSpecimen;
    property specimenObject : TFhirResourceReference{TFhirSpecimen} read FSpecimen write SetSpecimen;

    {@member performerList
      Who was responsible for asserting the observed value as "true".
    }
    property performerList : TFhirResourceReferenceList{Resource} read FPerformerList;

    {@member referenceRangeList
      Guidance on how to interpret the value by comparison to a normal or recommended range.
    }
    property referenceRangeList : TFhirObservationReferenceRangeList read FReferenceRangeList;

    {@member relatedList
      Related observations - either components, or previous observations, or statements of derivation.
    }
    property relatedList : TFhirObservationRelatedList read FRelatedList;

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
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
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
    Function GetDateST : TDateTimeEx;
    Procedure SetDateST(value : TDateTimeEx);
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
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member identifierList
      Identifiers assigned to this order by the orderer or by the receiver.
    }
    property identifierList : TFhirIdentifierList read FIdentifierList;

    {@member date
      When the order was made.
    }
    {@member date
      Typed access to When the order was made.
    }
    property date : TDateTimeEx read GetDateST write SetDateST;
    property dateObject : TFhirDateTime read FDate write SetDate;

    {@member subject
      Patient this order is about.
    }
    property subject : TFhirResourceReference{TFhirPatient} read FSubject write SetSubject;
    property subjectObject : TFhirResourceReference{TFhirPatient} read FSubject write SetSubject;

    {@member source
      Who initiated the order.
    }
    property source : TFhirResourceReference{TFhirPractitioner} read FSource write SetSource;
    property sourceObject : TFhirResourceReference{TFhirPractitioner} read FSource write SetSource;

    {@member target
      Who is intended to fulfill the order.
    }
    property target : TFhirResourceReference{Resource} read FTarget write SetTarget;
    property targetObject : TFhirResourceReference{Resource} read FTarget write SetTarget;

    {@member reason
      Text - why the order was made.
    }
    property reason : TFhirType read FReason write SetReason;
    property reasonObject : TFhirType read FReason write SetReason;

    {@member authority
      If required by policy.
    }
    property authority : TFhirResourceReference{Resource} read FAuthority write SetAuthority;
    property authorityObject : TFhirResourceReference{Resource} read FAuthority write SetAuthority;

    {@member when
      When order should be fulfilled.
    }
    property when : TFhirOrderWhen read FWhen write SetWhen;
    property whenObject : TFhirOrderWhen read FWhen write SetWhen;

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
    Function GetDateST : TDateTimeEx;
    Procedure SetDateST(value : TDateTimeEx);
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
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
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
    property requestObject : TFhirResourceReference{TFhirOrder} read FRequest write SetRequest;

    {@member date
      The date and time at which this order response was made (created/posted).
    }
    {@member date
      Typed access to The date and time at which this order response was made (created/posted).
    }
    property date : TDateTimeEx read GetDateST write SetDateST;
    property dateObject : TFhirDateTime read FDate write SetDate;

    {@member who
      The person, organization, or device credited with making the response.
    }
    property who : TFhirResourceReference{Resource} read FWho write SetWho;
    property whoObject : TFhirResourceReference{Resource} read FWho write SetWho;

    {@member authority
      A reference to an authority policy that is the reason for the response. Usually this is used when the order is rejected, to provide a reason for rejection.
    }
    property authority : TFhirType read FAuthority write SetAuthority;
    property authorityObject : TFhirType read FAuthority write SetAuthority;

    {@member code
      What this response says about the status of the original order.
    }
    property code : TFhirOrderOutcomeCode read GetCodeST write SetCodeST;
    property codeObject : TFhirEnum read FCode write SetCode;

    {@member description
      Additional description about the response - e.g. a text description provided by a human user when making decisions about the order.
    }
    {@member description
      Typed access to Additional description about the response - e.g. a text description provided by a human user when making decisions about the order.
    }
    property description : String read GetDescriptionST write SetDescriptionST;
    property descriptionObject : TFhirString read FDescription write SetDescription;

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
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member identifierList
      Identifier for the organization that is used to identify the organization across multiple disparate systems.
    }
    property identifierList : TFhirIdentifierList read FIdentifierList;

    {@member name
      A name associated with the organization.
    }
    {@member name
      Typed access to A name associated with the organization.
    }
    property name : String read GetNameST write SetNameST;
    property nameObject : TFhirString read FName write SetName;

    {@member type_
      The kind of organization that this is.
    }
    property type_ : TFhirCodeableConcept read FType_ write SetType_;
    property type_Object : TFhirCodeableConcept read FType_ write SetType_;

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
    property partOfObject : TFhirResourceReference{TFhirOrganization} read FPartOf write SetPartOf;

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
    {@member active
      Typed access to Whether the organization's record is still in active use.
    }
    property active : Boolean read GetActiveST write SetActiveST;
    property activeObject : TFhirBoolean read FActive write SetActive;

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
    Function GetCreatedST : TDateTimeEx;
    Procedure SetCreatedST(value : TDateTimeEx);
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
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
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
    property codeObject : TFhirCodeableConcept read FCode write SetCode;

    {@member subject
      Identifies the patient, practitioner, device or any other resource that is the "focus" of this resoruce.
    }
    property subject : TFhirResourceReference{Resource} read FSubject write SetSubject;
    property subjectObject : TFhirResourceReference{Resource} read FSubject write SetSubject;

    {@member author
      Indicates who was responsible for creating the resource instance.
    }
    property author : TFhirResourceReference{Resource} read FAuthor write SetAuthor;
    property authorObject : TFhirResourceReference{Resource} read FAuthor write SetAuthor;

    {@member created
      Identifies when the resource was first created.
    }
    {@member created
      Typed access to Identifies when the resource was first created.
    }
    property created : TDateTimeEx read GetCreatedST write SetCreatedST;
    property createdObject : TFhirDate read FCreated write SetCreated;

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
    FGender : TFhirCodeableConcept;
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
    Procedure SetGender(value : TFhirCodeableConcept);
    Procedure SetBirthDate(value : TFhirDateTime);
    Function GetBirthDateST : TDateTimeEx;
    Procedure SetBirthDateST(value : TDateTimeEx);
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
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
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
    property gender : TFhirCodeableConcept read FGender write SetGender;
    property genderObject : TFhirCodeableConcept read FGender write SetGender;

    {@member birthDate
      The date and time of birth for the individual.
    }
    {@member birthDate
      Typed access to The date and time of birth for the individual.
    }
    property birthDate : TDateTimeEx read GetBirthDateST write SetBirthDateST;
    property birthDateObject : TFhirDateTime read FBirthDate write SetBirthDate;

    {@member deceased
      Indicates if the individual is deceased or not.
    }
    property deceased : TFhirType read FDeceased write SetDeceased;
    property deceasedObject : TFhirType read FDeceased write SetDeceased;

    {@member addressList
      Addresses for the individual.
    }
    property addressList : TFhirAddressList read FAddressList;

    {@member maritalStatus
      This field contains a patient's most recent marital (civil) status.
    }
    property maritalStatus : TFhirCodeableConcept read FMaritalStatus write SetMaritalStatus;
    property maritalStatusObject : TFhirCodeableConcept read FMaritalStatus write SetMaritalStatus;

    {@member multipleBirth
      Indicates whether the patient is part of a multiple or indicates the actual birth order.
    }
    property multipleBirth : TFhirType read FMultipleBirth write SetMultipleBirth;
    property multipleBirthObject : TFhirType read FMultipleBirth write SetMultipleBirth;

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
    property animalObject : TFhirPatientAnimal read FAnimal write SetAnimal;

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
    property managingOrganizationObject : TFhirResourceReference{TFhirOrganization} read FManagingOrganization write SetManagingOrganization;

    {@member link_List
      Link to another patient resource that concerns the same actual person.
    }
    property link_List : TFhirPatientLinkList read FLink_List;

    {@member active
      Whether this patient record is in active use.
    }
    {@member active
      Typed access to Whether this patient record is in active use.
    }
    property active : Boolean read GetActiveST write SetActiveST;
    property activeObject : TFhirBoolean read FActive write SetActive;

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
    FAddress : TFhirAddress;
    FGender : TFhirCodeableConcept;
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
    Procedure SetAddress(value : TFhirAddress);
    Procedure SetGender(value : TFhirCodeableConcept);
    Procedure SetBirthDate(value : TFhirDateTime);
    Function GetBirthDateST : TDateTimeEx;
    Procedure SetBirthDateST(value : TDateTimeEx);
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
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
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
    property nameObject : TFhirHumanName read FName write SetName;

    {@member telecomList
      A contact detail for the practitioner, e.g. a telephone number or an email address.
    }
    property telecomList : TFhirContactList read FTelecomList;

    {@member address
      The postal address where the practitioner can be found or visited or to which mail can be delivered.
    }
    property address : TFhirAddress read FAddress write SetAddress;
    property addressObject : TFhirAddress read FAddress write SetAddress;

    {@member gender
      Administrative Gender - the gender that the person is considered to have for administration and record keeping purposes.
    }
    property gender : TFhirCodeableConcept read FGender write SetGender;
    property genderObject : TFhirCodeableConcept read FGender write SetGender;

    {@member birthDate
      The date and time of birth for the practitioner.
    }
    {@member birthDate
      Typed access to The date and time of birth for the practitioner.
    }
    property birthDate : TDateTimeEx read GetBirthDateST write SetBirthDateST;
    property birthDateObject : TFhirDateTime read FBirthDate write SetBirthDate;

    {@member photoList
      Image of the person.
    }
    property photoList : TFhirAttachmentList read FPhotoList;

    {@member organization
      The organization that the practitioner represents.
    }
    property organization : TFhirResourceReference{TFhirOrganization} read FOrganization write SetOrganization;
    property organizationObject : TFhirResourceReference{TFhirOrganization} read FOrganization write SetOrganization;

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
    property periodObject : TFhirPeriod read FPeriod write SetPeriod;

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
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
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
    property subjectObject : TFhirResourceReference{TFhirPatient} read FSubject write SetSubject;

    {@member type_
      The specific procedure that is performed. Use text if the exact nature of the procedure can't be coded.
    }
    property type_ : TFhirCodeableConcept read FType_ write SetType_;
    property type_Object : TFhirCodeableConcept read FType_ write SetType_;

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
      The dates over which the procedure was performed. Allows a period to support complex procedures that span more that one date, and also allows for the length of the procedure to be captured.
    }
    property date : TFhirPeriod read FDate write SetDate;
    property dateObject : TFhirPeriod read FDate write SetDate;

    {@member encounter
      The encounter during which the procedure was performed.
    }
    property encounter : TFhirResourceReference{TFhirEncounter} read FEncounter write SetEncounter;
    property encounterObject : TFhirResourceReference{TFhirEncounter} read FEncounter write SetEncounter;

    {@member outcome
      What was the outcome of the procedure - did it resolve reasons why the procedure was performed?.
    }
    {@member outcome
      Typed access to What was the outcome of the procedure - did it resolve reasons why the procedure was performed?.
    }
    property outcome : String read GetOutcomeST write SetOutcomeST;
    property outcomeObject : TFhirString read FOutcome write SetOutcome;

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
    {@member followUp
      Typed access to If the procedure required specific follow up - e.g. removal of sutures. The followup may be represented as a simple note, or potentially could be more complex in which case the CarePlan resource can be used.
    }
    property followUp : String read GetFollowUpST write SetFollowUpST;
    property followUpObject : TFhirString read FFollowUp write SetFollowUp;

    {@member relatedItemList
      Procedures may be related to other items such as procedures or medications. For example treating wound dehiscence following a previous procedure.
    }
    property relatedItemList : TFhirProcedureRelatedItemList read FRelatedItemList;

    {@member notes
      Any other notes about the procedure - e.g. the operative notes.
    }
    {@member notes
      Typed access to Any other notes about the procedure - e.g. the operative notes.
    }
    property notes : String read GetNotesST write SetNotesST;
    property notesObject : TFhirString read FNotes write SetNotes;

  end;


  {@Class TFhirProfile : TFhirResource
    A Resource Profile - a statement of use of one or more FHIR Resources.  It may include constraints on Resources and Data Types, Terminology Binding Statements and Extension Definitions.
  }
  {!.Net HL7Connect.Fhir.Profile}
  TFhirProfile = class (TFhirResource)
  private
    FIdentifier : TFhirString;
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
    FqueryList : TFhirProfileQueryList;
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
    Function GetStatusST : TFhirResourceProfileStatus;
    Procedure SetStatusST(value : TFhirResourceProfileStatus);
    Procedure SetExperimental(value : TFhirBoolean);
    Function GetExperimentalST : Boolean;
    Procedure SetExperimentalST(value : Boolean);
    Procedure SetDate(value : TFhirDateTime);
    Function GetDateST : TDateTimeEx;
    Procedure SetDateST(value : TDateTimeEx);
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
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member identifier
      The identifier that is used to identify this profile when it is referenced in a specification, model, design or an instance  (should be globally unique OID, UUID, or URI).
    }
    {@member identifier
      Typed access to The identifier that is used to identify this profile when it is referenced in a specification, model, design or an instance  (should be globally unique OID, UUID, or URI).
    }
    property identifier : String read GetIdentifierST write SetIdentifierST;
    property identifierObject : TFhirString read FIdentifier write SetIdentifier;

    {@member version
      The identifier that is used to identify this version of the profile when it is referenced in a specification, model, design or instance. This is an arbitrary value managed by the profile author manually and the value should be a timestamp.
    }
    {@member version
      Typed access to The identifier that is used to identify this version of the profile when it is referenced in a specification, model, design or instance. This is an arbitrary value managed by the profile author manually and the value should be a timestamp.
    }
    property version : String read GetVersionST write SetVersionST;
    property versionObject : TFhirString read FVersion write SetVersion;

    {@member name
      A free text natural language name identifying the Profile.
    }
    {@member name
      Typed access to A free text natural language name identifying the Profile.
    }
    property name : String read GetNameST write SetNameST;
    property nameObject : TFhirString read FName write SetName;

    {@member publisher
      Details of the individual or organization who accepts responsibility for publishing the profile.
    }
    {@member publisher
      Typed access to Details of the individual or organization who accepts responsibility for publishing the profile.
    }
    property publisher : String read GetPublisherST write SetPublisherST;
    property publisherObject : TFhirString read FPublisher write SetPublisher;

    {@member telecomList
      Contact details to assist a user in finding and communicating with the publisher.
    }
    property telecomList : TFhirContactList read FTelecomList;

    {@member description
      A free text natural language description of the profile and its use.
    }
    {@member description
      Typed access to A free text natural language description of the profile and its use.
    }
    property description : String read GetDescriptionST write SetDescriptionST;
    property descriptionObject : TFhirString read FDescription write SetDescription;

    {@member codeList
      A set of terms from external terminologies that may be used to assist with indexing and searching of templates.
    }
    property codeList : TFhirCodingList read FCodeList;

    {@member status
      The status of the profile.
    }
    property status : TFhirResourceProfileStatus read GetStatusST write SetStatusST;
    property statusObject : TFhirEnum read FStatus write SetStatus;

    {@member experimental
      This profile was authored for testing purposes (or education/evaluation/marketing), and is not intended to be used for genuine usage.
    }
    {@member experimental
      Typed access to This profile was authored for testing purposes (or education/evaluation/marketing), and is not intended to be used for genuine usage.
    }
    property experimental : Boolean read GetExperimentalST write SetExperimentalST;
    property experimentalObject : TFhirBoolean read FExperimental write SetExperimental;

    {@member date
      The date that this version of the profile was published.
    }
    {@member date
      Typed access to The date that this version of the profile was published.
    }
    property date : TDateTimeEx read GetDateST write SetDateST;
    property dateObject : TFhirDateTime read FDate write SetDate;

    {@member requirements
      The Scope and Usage that this profile was created to meet.
    }
    {@member requirements
      Typed access to The Scope and Usage that this profile was created to meet.
    }
    property requirements : String read GetRequirementsST write SetRequirementsST;
    property requirementsObject : TFhirString read FRequirements write SetRequirements;

    {@member fhirVersion
      The version of the FHIR specification on which this profile is based.
    }
    {@member fhirVersion
      Typed access to The version of the FHIR specification on which this profile is based.
    }
    property fhirVersion : String read GetFhirVersionST write SetFhirVersionST;
    property fhirVersionObject : TFhirId read FFhirVersion write SetFhirVersion;

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

    {@member queryList
      Definition of a named query and its parameters and their meaning.
    }
    property queryList : TFhirProfileQueryList read FQueryList;

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
    Function GetRecordedST : TDateTimeEx;
    Procedure SetRecordedST(value : TDateTimeEx);
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
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
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
    property periodObject : TFhirPeriod read FPeriod write SetPeriod;

    {@member recorded
      The instant of time at which the activity was recorded.
    }
    {@member recorded
      Typed access to The instant of time at which the activity was recorded.
    }
    property recorded : TDateTimeEx read GetRecordedST write SetRecordedST;
    property recordedObject : TFhirInstant read FRecorded write SetRecorded;

    {@member reason
      The reason that the activity was taking place.
    }
    property reason : TFhirCodeableConcept read FReason write SetReason;
    property reasonObject : TFhirCodeableConcept read FReason write SetReason;

    {@member location
      Where the activity occurred, if relevant.
    }
    property location : TFhirResourceReference{TFhirLocation} read FLocation write SetLocation;
    property locationObject : TFhirResourceReference{TFhirLocation} read FLocation write SetLocation;

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
    {@member integritySignature
      Typed access to A digital signature on the target resource(s). The signature should match a Provenance.agent.reference in the provenance resource. The signature is only added to support checking cryptographic integrity of the resource, and not to represent workflow and clinical aspects of the signing process, or to support non-repudiation.
    }
    property integritySignature : String read GetIntegritySignatureST write SetIntegritySignatureST;
    property integritySignatureObject : TFhirString read FIntegritySignature write SetIntegritySignature;

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
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member identifier
      Links query and its response(s).
    }
    {@member identifier
      Typed access to Links query and its response(s).
    }
    property identifier : String read GetIdentifierST write SetIdentifierST;
    property identifierObject : TFhirUri read FIdentifier write SetIdentifier;

    {@member parameterList
      Set of query parameters with values.
    }
    property parameterList : TFhirExtensionList read FParameterList;

    {@member response
      If this is a response to a query.
    }
    property response : TFhirQueryResponse read FResponse write SetResponse;
    property responseObject : TFhirQueryResponse read FResponse write SetResponse;

  end;


  {@Class TFhirQuestionnaire : TFhirResource
    A structured set of questions and their answers. The Questionnaire may contain questions, answers or both. The questions are ordered and grouped into coherent subsets, corresponding to the structure of the grouping of the underlying questions.
  }
  {!.Net HL7Connect.Fhir.Questionnaire}
  TFhirQuestionnaire = class (TFhirResource)
  private
    FStatus : TFhirEnum;
    FAuthored : TFhirDateTime;
    FSubject : TFhirResourceReference{Resource};
    FAuthor : TFhirResourceReference{Resource};
    FSource : TFhirResourceReference{Resource};
    FName : TFhirCodeableConcept;
    FidentifierList : TFhirIdentifierList;
    FEncounter : TFhirResourceReference{TFhirEncounter};
    FGroup : TFhirQuestionnaireGroup;
    Procedure SetStatus(value : TFhirEnum);
    Function GetStatusST : TFhirQuestionnaireStatus;
    Procedure SetStatusST(value : TFhirQuestionnaireStatus);
    Procedure SetAuthored(value : TFhirDateTime);
    Function GetAuthoredST : TDateTimeEx;
    Procedure SetAuthoredST(value : TDateTimeEx);
    Procedure SetSubject(value : TFhirResourceReference{Resource});
    Procedure SetAuthor(value : TFhirResourceReference{Resource});
    Procedure SetSource(value : TFhirResourceReference{Resource});
    Procedure SetName(value : TFhirCodeableConcept);
    Procedure SetEncounter(value : TFhirResourceReference{TFhirEncounter});
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
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member status
      The lifecycle status of the questionnaire as a whole.
    }
    property status : TFhirQuestionnaireStatus read GetStatusST write SetStatusST;
    property statusObject : TFhirEnum read FStatus write SetStatus;

    {@member authored
      The date and/or time that this version of the questionnaire was authored.
    }
    {@member authored
      Typed access to The date and/or time that this version of the questionnaire was authored.
    }
    property authored : TDateTimeEx read GetAuthoredST write SetAuthoredST;
    property authoredObject : TFhirDateTime read FAuthored write SetAuthored;

    {@member subject
      The subject of the questionnaires: this is the patient that the answers apply to, but this person is not necessarily the source of information.
    }
    property subject : TFhirResourceReference{Resource} read FSubject write SetSubject;
    property subjectObject : TFhirResourceReference{Resource} read FSubject write SetSubject;

    {@member author
      Person who received the answers to the questions in the Questionnaire and recorded them in the system.
    }
    property author : TFhirResourceReference{Resource} read FAuthor write SetAuthor;
    property authorObject : TFhirResourceReference{Resource} read FAuthor write SetAuthor;

    {@member source
      The person who answered the questions about the subject. Only used when this is not the subject him/herself.
    }
    property source : TFhirResourceReference{Resource} read FSource write SetSource;
    property sourceObject : TFhirResourceReference{Resource} read FSource write SetSource;

    {@member name
      Structured name for a predefined list of questions this questionnaire is responding to.
    }
    property name : TFhirCodeableConcept read FName write SetName;
    property nameObject : TFhirCodeableConcept read FName write SetName;

    {@member identifierList
      This records identifiers associated with this question/answer set that are defined by business processed and/ or used to refer to it when a direct URL reference to the resource itself is not appropriate (e.g. in CDA documents, or in written / printed documentation).
    }
    property identifierList : TFhirIdentifierList read FIdentifierList;

    {@member encounter
      Encounter during which this questionnaire answers were collected. When there were multiple encounters, this is the one considered most relevant to the context of the answers.
    }
    property encounter : TFhirResourceReference{TFhirEncounter} read FEncounter write SetEncounter;
    property encounterObject : TFhirResourceReference{TFhirEncounter} read FEncounter write SetEncounter;

    {@member group
      A group of questions to a possibly similarly grouped set of questions in the questionnaire.
    }
    property group : TFhirQuestionnaireGroup read FGroup write SetGroup;
    property groupObject : TFhirQuestionnaireGroup read FGroup write SetGroup;

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
    FGender : TFhirCodeableConcept;
    FAddress : TFhirAddress;
    FphotoList : TFhirAttachmentList;
    Procedure SetPatient(value : TFhirResourceReference{TFhirPatient});
    Procedure SetRelationship(value : TFhirCodeableConcept);
    Procedure SetName(value : TFhirHumanName);
    Procedure SetGender(value : TFhirCodeableConcept);
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
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
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
    property patientObject : TFhirResourceReference{TFhirPatient} read FPatient write SetPatient;

    {@member relationship
      The nature of the relationship between a patient and the related person.
    }
    property relationship : TFhirCodeableConcept read FRelationship write SetRelationship;
    property relationshipObject : TFhirCodeableConcept read FRelationship write SetRelationship;

    {@member name
      A name associated with the person.
    }
    property name : TFhirHumanName read FName write SetName;
    property nameObject : TFhirHumanName read FName write SetName;

    {@member telecomList
      A contact detail for the person, e.g. a telephone number or an email address.
    }
    property telecomList : TFhirContactList read FTelecomList;

    {@member gender
      Administrative Gender - the gender that the person is considered to have for administration and record keeping purposes.
    }
    property gender : TFhirCodeableConcept read FGender write SetGender;
    property genderObject : TFhirCodeableConcept read FGender write SetGender;

    {@member address
      Address where the related person can be contacted or visited.
    }
    property address : TFhirAddress read FAddress write SetAddress;
    property addressObject : TFhirAddress read FAddress write SetAddress;

    {@member photoList
      Image of the person.
    }
    property photoList : TFhirAttachmentList read FPhotoList;

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
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member event
      Identifies the name, action type, time, and disposition of the audited event.
    }
    property event : TFhirSecurityEventEvent read FEvent write SetEvent;
    property eventObject : TFhirSecurityEventEvent read FEvent write SetEvent;

    {@member participantList
      A person, a hardware device or software process.
    }
    property participantList : TFhirSecurityEventParticipantList read FParticipantList;

    {@member source
      Application systems and processes.
    }
    property source : TFhirSecurityEventSource read FSource write SetSource;
    property sourceObject : TFhirSecurityEventSource read FSource write SetSource;

    {@member object_List
      Specific instances of data or objects that have been accessed.
    }
    property object_List : TFhirSecurityEventObjectList read FObject_List;

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
    Function GetReceivedTimeST : TDateTimeEx;
    Procedure SetReceivedTimeST(value : TDateTimeEx);
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
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
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
    property type_Object : TFhirCodeableConcept read FType_ write SetType_;

    {@member sourceList
      Parent specimen from which the focal specimen was a component.
    }
    property sourceList : TFhirSpecimenSourceList read FSourceList;

    {@member subject
      Where the specimen came from. This may be the patient(s) or from the environment or  a device.
    }
    property subject : TFhirResourceReference{Resource} read FSubject write SetSubject;
    property subjectObject : TFhirResourceReference{Resource} read FSubject write SetSubject;

    {@member accessionIdentifier
      The identifier assigned by the lab when accessioning specimen(s). This is not necessarily the same as the specimen identifier, depending on local lab procedures.
    }
    property accessionIdentifier : TFhirIdentifier read FAccessionIdentifier write SetAccessionIdentifier;
    property accessionIdentifierObject : TFhirIdentifier read FAccessionIdentifier write SetAccessionIdentifier;

    {@member receivedTime
      Time when specimen was received for processing or testing.
    }
    {@member receivedTime
      Typed access to Time when specimen was received for processing or testing.
    }
    property receivedTime : TDateTimeEx read GetReceivedTimeST write SetReceivedTimeST;
    property receivedTimeObject : TFhirDateTime read FReceivedTime write SetReceivedTime;

    {@member collection
      Details concerning the specimen collection.
    }
    property collection : TFhirSpecimenCollection read FCollection write SetCollection;
    property collectionObject : TFhirSpecimenCollection read FCollection write SetCollection;

    {@member treatmentList
      Details concerning treatment and processing steps for the specimen.
    }
    property treatmentList : TFhirSpecimenTreatmentList read FTreatmentList;

    {@member containerList
      The container holding the specimen.  The recursive nature of containers; i.e. blood in tube in tray in rack is not addressed here.
    }
    property containerList : TFhirSpecimenContainerList read FContainerList;

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
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member type_
      A code (or set of codes) that identify this substance.
    }
    property type_ : TFhirCodeableConcept read FType_ write SetType_;
    property type_Object : TFhirCodeableConcept read FType_ write SetType_;

    {@member description
      A description of the substance - its appearance, handling requirements, and other usage notes.
    }
    {@member description
      Typed access to A description of the substance - its appearance, handling requirements, and other usage notes.
    }
    property description : String read GetDescriptionST write SetDescriptionST;
    property descriptionObject : TFhirString read FDescription write SetDescription;

    {@member instance
      Substance may be used to describe a kind of substance, or a specific package/container of the substance: an instance.
    }
    property instance : TFhirSubstanceInstance read FInstance write SetInstance;
    property instanceObject : TFhirSubstanceInstance read FInstance write SetInstance;

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
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member kind
      Category of supply, e.g.  central, non-stock, etc. This is used to support work flows associated with the supply process.
    }
    property kind : TFhirCodeableConcept read FKind write SetKind;
    property kindObject : TFhirCodeableConcept read FKind write SetKind;

    {@member identifier
      Unique identifier for this supply request.
    }
    property identifier : TFhirIdentifier read FIdentifier write SetIdentifier;
    property identifierObject : TFhirIdentifier read FIdentifier write SetIdentifier;

    {@member status
      Status of the supply request.
    }
    property status : TFhirValuesetSupplyStatus read GetStatusST write SetStatusST;
    property statusObject : TFhirEnum read FStatus write SetStatus;

    {@member orderedItem
      The item that is requested to be supplied.
    }
    property orderedItem : TFhirResourceReference{Resource} read FOrderedItem write SetOrderedItem;
    property orderedItemObject : TFhirResourceReference{Resource} read FOrderedItem write SetOrderedItem;

    {@member patient
      A link to a resource representing the person whom the ordered item is for.
    }
    property patient : TFhirResourceReference{TFhirPatient} read FPatient write SetPatient;
    property patientObject : TFhirResourceReference{TFhirPatient} read FPatient write SetPatient;

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
    FPublisher : TFhirString;
    FtelecomList : TFhirContactList;
    FDescription : TFhirString;
    FCopyright : TFhirString;
    FStatus : TFhirEnum;
    FExperimental : TFhirBoolean;
    FExtensible : TFhirBoolean;
    FDate : TFhirDateTime;
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
    Function GetDateST : TDateTimeEx;
    Procedure SetDateST(value : TDateTimeEx);
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
    procedure setProperty(propName : string; propValue : TFHIRObject); override;
    function FhirType : string; override;
    {!script show}
  published
    {@member identifier
      The identifier that is used to identify this value set when it is referenced in a specification, model, design or an instance (should be globally unique OID, UUID, or URI).
    }
    {@member identifier
      Typed access to The identifier that is used to identify this value set when it is referenced in a specification, model, design or an instance (should be globally unique OID, UUID, or URI).
    }
    property identifier : String read GetIdentifierST write SetIdentifierST;
    property identifierObject : TFhirString read FIdentifier write SetIdentifier;

    {@member version
      The identifier that is used to identify this version of the value set when it is referenced in a specification, model, design or instance. This is an arbitrary value managed by the profile author manually and the value should be a timestamp.
    }
    {@member version
      Typed access to The identifier that is used to identify this version of the value set when it is referenced in a specification, model, design or instance. This is an arbitrary value managed by the profile author manually and the value should be a timestamp.
    }
    property version : String read GetVersionST write SetVersionST;
    property versionObject : TFhirString read FVersion write SetVersion;

    {@member name
      A free text natural language name describing the value set.
    }
    {@member name
      Typed access to A free text natural language name describing the value set.
    }
    property name : String read GetNameST write SetNameST;
    property nameObject : TFhirString read FName write SetName;

    {@member publisher
      The name of the individual or organization that published the value set.
    }
    {@member publisher
      Typed access to The name of the individual or organization that published the value set.
    }
    property publisher : String read GetPublisherST write SetPublisherST;
    property publisherObject : TFhirString read FPublisher write SetPublisher;

    {@member telecomList
      Contacts of the publisher to assist a user in finding and communicating with the publisher.
    }
    property telecomList : TFhirContactList read FTelecomList;

    {@member description
      A free text natural language description of the use of the value set - reason for definition, conditions of use, etc.
    }
    {@member description
      Typed access to A free text natural language description of the use of the value set - reason for definition, conditions of use, etc.
    }
    property description : String read GetDescriptionST write SetDescriptionST;
    property descriptionObject : TFhirString read FDescription write SetDescription;

    {@member copyright
      A copyright statement relating to the value set and/or its contents.
    }
    {@member copyright
      Typed access to A copyright statement relating to the value set and/or its contents.
    }
    property copyright : String read GetCopyrightST write SetCopyrightST;
    property copyrightObject : TFhirString read FCopyright write SetCopyright;

    {@member status
      The status of the value set.
    }
    property status : TFhirValuesetStatus read GetStatusST write SetStatusST;
    property statusObject : TFhirEnum read FStatus write SetStatus;

    {@member experimental
      This valueset was authored for testing purposes (or education/evaluation/marketing), and is not intended to be used for genuine usage.
    }
    {@member experimental
      Typed access to This valueset was authored for testing purposes (or education/evaluation/marketing), and is not intended to be used for genuine usage.
    }
    property experimental : Boolean read GetExperimentalST write SetExperimentalST;
    property experimentalObject : TFhirBoolean read FExperimental write SetExperimental;

    {@member extensible
      Whether this is intended to be used with an extensible binding or not.
    }
    {@member extensible
      Typed access to Whether this is intended to be used with an extensible binding or not.
    }
    property extensible : Boolean read GetExtensibleST write SetExtensibleST;
    property extensibleObject : TFhirBoolean read FExtensible write SetExtensible;

    {@member date
      The date that the value set status was last changed.
    }
    {@member date
      Typed access to The date that the value set status was last changed.
    }
    property date : TDateTimeEx read GetDateST write SetDateST;
    property dateObject : TFhirDateTime read FDate write SetDate;

    {@member define
      When value set defines its own codes.
    }
    property define : TFhirValueSetDefine read FDefine write SetDefine;
    property defineObject : TFhirValueSetDefine read FDefine write SetDefine;

    {@member compose
      When value set includes codes from elsewhere.
    }
    property compose : TFhirValueSetCompose read FCompose write SetCompose;
    property composeObject : TFhirValueSetCompose read FCompose write SetCompose;

    {@member expansion
      When value set is an expansion.
    }
    property expansion : TFhirValueSetExpansion read FExpansion write SetExpansion;
    property expansionObject : TFhirValueSetExpansion read FExpansion write SetExpansion;

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
    function makeDateTime(value : TDateTimeEx) : TFhirDateTime;
    {@member newDate
      create a new date
    }
    {!script nolink}
    function newDate : TFhirDate;
    {@member makeDate
      create a new date with the given value
    }
    {!script nolink}
    function makeDate(value : TDateTimeEx) : TFhirDate;
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
    function makeInstant(value : TDateTimeEx) : TFhirInstant;
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
    {@member newConceptMapConcept
      create a new concept
    }
    {!script nolink}
    function newConceptMapConcept : TFhirConceptMapConcept;
    {@member newConceptMapConceptDependsOn
      create a new dependsOn
    }
    {!script nolink}
    function newConceptMapConceptDependsOn : TFhirConceptMapConceptDependsOn;
    {@member newConceptMapConceptMap
      create a new map
    }
    {!script nolink}
    function newConceptMapConceptMap : TFhirConceptMapConceptMap;
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
    {@member newConformanceRestResourceOperation
      create a new operation
    }
    {!script nolink}
    function newConformanceRestResourceOperation : TFhirConformanceRestResourceOperation;
    {@member newConformanceRestResourceSearchParam
      create a new searchParam
    }
    {!script nolink}
    function newConformanceRestResourceSearchParam : TFhirConformanceRestResourceSearchParam;
    {@member newConformanceRestOperation
      create a new operation
    }
    {!script nolink}
    function newConformanceRestOperation : TFhirConformanceRestOperation;
    {@member newConformanceRestQuery
      create a new query
    }
    {!script nolink}
    function newConformanceRestQuery : TFhirConformanceRestQuery;
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
    {@member newProfileStructureElement
      create a new element
    }
    {!script nolink}
    function newProfileStructureElement : TFhirProfileStructureElement;
    {@member newProfileStructureElementSlicing
      create a new slicing
    }
    {!script nolink}
    function newProfileStructureElementSlicing : TFhirProfileStructureElementSlicing;
    {@member newProfileStructureElementDefinition
      create a new definition
    }
    {!script nolink}
    function newProfileStructureElementDefinition : TFhirProfileStructureElementDefinition;
    {@member newProfileStructureElementDefinitionType
      create a new type
    }
    {!script nolink}
    function newProfileStructureElementDefinitionType : TFhirProfileStructureElementDefinitionType;
    {@member newProfileStructureElementDefinitionConstraint
      create a new constraint
    }
    {!script nolink}
    function newProfileStructureElementDefinitionConstraint : TFhirProfileStructureElementDefinitionConstraint;
    {@member newProfileStructureElementDefinitionBinding
      create a new binding
    }
    {!script nolink}
    function newProfileStructureElementDefinitionBinding : TFhirProfileStructureElementDefinitionBinding;
    {@member newProfileStructureElementDefinitionMapping
      create a new mapping
    }
    {!script nolink}
    function newProfileStructureElementDefinitionMapping : TFhirProfileStructureElementDefinitionMapping;
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
    {@member newProfileQuery
      create a new query
    }
    {!script nolink}
    function newProfileQuery : TFhirProfileQuery;
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
    {@member newRelatedPerson
      create a new RelatedPerson
    }
    {!script nolink}
    function newRelatedPerson : TFhirRelatedPerson;
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
    function makeByName(const name : String) : TFHIRElement;
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


{ TFhirResourceListEnumerator }

Constructor TFhirResourceListEnumerator.Create(list : TFhirResourceList);
begin
  inherited Create;
  FIndex := -1;
  FList := list;
end;

Destructor TFhirResourceListEnumerator.Destroy;
begin
  FList.Free;
  inherited;
end;

function TFhirResourceListEnumerator.MoveNext : boolean;
begin
  Result := FIndex < FList.count;
  if Result then
    Inc(FIndex);
end;

function TFhirResourceListEnumerator.GetCurrent : TFhirResource;
begin
  Result := FList[FIndex];
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

function TFhirResourceList.GetEnumerator : TFhirResourceListEnumerator;
begin
  result := TFhirResourceListEnumerator.Create(self.link);
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
  dateObject := TFhirAdverseReaction(oSource).dateObject.Clone;
  subject := TFhirAdverseReaction(oSource).subject.Clone;
  didNotOccurFlagObject := TFhirAdverseReaction(oSource).didNotOccurFlagObject.Clone;
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
     list.add(FDate.Link);
  if (child_name = 'subject') Then
     list.add(FSubject.Link);
  if (child_name = 'didNotOccurFlag') Then
     list.add(FDidNotOccurFlag.Link);
  if (child_name = 'recorder') Then
     list.add(FRecorder.Link);
  if (child_name = 'symptom') Then
     list.addAll(FSymptomList);
  if (child_name = 'exposure') Then
     list.addAll(FExposureList);
end;

procedure TFhirAdverseReaction.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'date', 'dateTime', FDate.Link));{2}
  oList.add(TFHIRProperty.create(self, 'subject', 'Resource(Patient)', FSubject.Link));{2}
  oList.add(TFHIRProperty.create(self, 'didNotOccurFlag', 'boolean', FDidNotOccurFlag.Link));{2}
  oList.add(TFHIRProperty.create(self, 'recorder', 'Resource(Practitioner|Patient)', FRecorder.Link));{2}
  oList.add(TFHIRProperty.create(self, 'symptom', '', FSymptomList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'exposure', '', FExposureList.Link)){3};
end;

procedure TFhirAdverseReaction.setProperty(propName : string; propValue: TFHIRObject);
begin
  if (propName = 'identifier') then IdentifierList.add(propValue as TFhirIdentifier){2}
  else if (propName = 'date') then DateObject := propValue as TFhirDateTime{5a}
  else if (propName = 'subject') then Subject := propValue as TFhirResourceReference{TFhirPatient}{4b}
  else if (propName = 'didNotOccurFlag') then DidNotOccurFlagObject := propValue as TFhirBoolean{5a}
  else if (propName = 'recorder') then Recorder := propValue as TFhirResourceReference{Resource}{4b}
  else if (propName = 'symptom') then SymptomList.add(propValue as TFhirAdverseReactionSymptom){2}
  else if (propName = 'exposure') then ExposureList.add(propValue as TFhirAdverseReactionExposure){2}
  else inherited;
end;

function TFhirAdverseReaction.FhirType : string;
begin
  result := 'AdverseReaction';
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

Function TFhirAdverseReaction.GetDateST : TDateTimeEx;
begin
  if FDate = nil then
    result := nil
  else
    result := FDate.value;
end;

Procedure TFhirAdverseReaction.SetDateST(value : TDateTimeEx);
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
    result := FDidNotOccurFlag.value;
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
  noteObject := TFhirAlert(oSource).noteObject.Clone;
end;

procedure TFhirAlert.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'identifier') Then
     list.addAll(FIdentifierList);
  if (child_name = 'category') Then
     list.add(FCategory.Link);
  if (child_name = 'status') Then
     list.add(FStatus.Link);
  if (child_name = 'subject') Then
     list.add(FSubject.Link);
  if (child_name = 'author') Then
     list.add(FAuthor.Link);
  if (child_name = 'note') Then
     list.add(FNote.Link);
end;

procedure TFhirAlert.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'category', 'CodeableConcept', FCategory.Link));{2}
  oList.add(TFHIRProperty.create(self, 'status', 'code', FStatus.Link));{1}
  oList.add(TFHIRProperty.create(self, 'subject', 'Resource(Patient)', FSubject.Link));{2}
  oList.add(TFHIRProperty.create(self, 'author', 'Resource(Practitioner|Patient|Device)', FAuthor.Link));{2}
  oList.add(TFHIRProperty.create(self, 'note', 'string', FNote.Link));{2}
end;

procedure TFhirAlert.setProperty(propName : string; propValue: TFHIRObject);
begin
  if (propName = 'identifier') then IdentifierList.add(propValue as TFhirIdentifier){2}
  else if (propName = 'category') then Category := propValue as TFhirCodeableConcept{4b}
  else if (propName = 'status') then StatusObject := propValue as TFHIREnum
  else if (propName = 'subject') then Subject := propValue as TFhirResourceReference{TFhirPatient}{4b}
  else if (propName = 'author') then Author := propValue as TFhirResourceReference{Resource}{4b}
  else if (propName = 'note') then NoteObject := propValue as TFhirString{5a}
  else inherited;
end;

function TFhirAlert.FhirType : string;
begin
  result := 'Alert';
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
    result := TFhirAlertStatus(StringArrayIndexOfSensitive(CODES_TFhirAlertStatus, FStatus.value));
end;

Procedure TFhirAlert.SetStatusST(value : TFhirAlertStatus);
begin
  if ord(value) = 0 then
    StatusObject := nil
  else
    StatusObject := TFhirEnum.create(CODES_TFhirAlertStatus[value]);
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
    result := FNote.value;
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
  recordedDateObject := TFhirAllergyIntolerance(oSource).recordedDateObject.Clone;
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
     list.add(FRecordedDate.Link);
  if (child_name = 'status') Then
     list.add(FStatus.Link);
  if (child_name = 'subject') Then
     list.add(FSubject.Link);
  if (child_name = 'recorder') Then
     list.add(FRecorder.Link);
  if (child_name = 'substance') Then
     list.add(FSubstance.Link);
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
  oList.add(TFHIRProperty.create(self, 'recordedDate', 'dateTime', FRecordedDate.Link));{2}
  oList.add(TFHIRProperty.create(self, 'status', 'code', FStatus.Link));{1}
  oList.add(TFHIRProperty.create(self, 'subject', 'Resource(Patient)', FSubject.Link));{2}
  oList.add(TFHIRProperty.create(self, 'recorder', 'Resource(Practitioner|Patient)', FRecorder.Link));{2}
  oList.add(TFHIRProperty.create(self, 'substance', 'Resource(Substance)', FSubstance.Link));{2}
  oList.add(TFHIRProperty.create(self, 'reaction', 'Resource(AdverseReaction)', FReactionList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'sensitivityTest', 'Resource(Observation)', FSensitivityTestList.Link)){3};
end;

procedure TFhirAllergyIntolerance.setProperty(propName : string; propValue: TFHIRObject);
begin
  if (propName = 'identifier') then IdentifierList.add(propValue as TFhirIdentifier){2}
  else if (propName = 'criticality') then CriticalityObject := propValue as TFHIREnum
  else if (propName = 'sensitivityType') then SensitivityTypeObject := propValue as TFHIREnum
  else if (propName = 'recordedDate') then RecordedDateObject := propValue as TFhirDateTime{5a}
  else if (propName = 'status') then StatusObject := propValue as TFHIREnum
  else if (propName = 'subject') then Subject := propValue as TFhirResourceReference{TFhirPatient}{4b}
  else if (propName = 'recorder') then Recorder := propValue as TFhirResourceReference{Resource}{4b}
  else if (propName = 'substance') then Substance := propValue as TFhirResourceReference{TFhirSubstance}{4b}
  else if (propName = 'reaction') then ReactionList.add(propValue as TFhirResourceReference{TFhirAdverseReaction}){2}
  else if (propName = 'sensitivityTest') then SensitivityTestList.add(propValue as TFhirResourceReference{TFhirObservation}){2}
  else inherited;
end;

function TFhirAllergyIntolerance.FhirType : string;
begin
  result := 'AllergyIntolerance';
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
    result := TFhirCriticality(StringArrayIndexOfSensitive(CODES_TFhirCriticality, FCriticality.value));
end;

Procedure TFhirAllergyIntolerance.SetCriticalityST(value : TFhirCriticality);
begin
  if ord(value) = 0 then
    CriticalityObject := nil
  else
    CriticalityObject := TFhirEnum.create(CODES_TFhirCriticality[value]);
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
    result := TFhirSensitivitytype(StringArrayIndexOfSensitive(CODES_TFhirSensitivitytype, FSensitivityType.value));
end;

Procedure TFhirAllergyIntolerance.SetSensitivityTypeST(value : TFhirSensitivitytype);
begin
  if ord(value) = 0 then
    SensitivityTypeObject := nil
  else
    SensitivityTypeObject := TFhirEnum.create(CODES_TFhirSensitivitytype[value]);
end;

Procedure TFhirAllergyIntolerance.SetRecordedDate(value : TFhirDateTime);
begin
  FRecordedDate.free;
  FRecordedDate := value;
end;

Function TFhirAllergyIntolerance.GetRecordedDateST : TDateTimeEx;
begin
  if FRecordedDate = nil then
    result := nil
  else
    result := FRecordedDate.value;
end;

Procedure TFhirAllergyIntolerance.SetRecordedDateST(value : TDateTimeEx);
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
    result := TFhirSensitivitystatus(StringArrayIndexOfSensitive(CODES_TFhirSensitivitystatus, FStatus.value));
end;

Procedure TFhirAllergyIntolerance.SetStatusST(value : TFhirSensitivitystatus);
begin
  if ord(value) = 0 then
    StatusObject := nil
  else
    StatusObject := TFhirEnum.create(CODES_TFhirSensitivitystatus[value]);
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
  modifiedObject := TFhirCarePlan(oSource).modifiedObject.Clone;
  FConcernList.Assign(TFhirCarePlan(oSource).FConcernList);
  FParticipantList.Assign(TFhirCarePlan(oSource).FParticipantList);
  FGoalList.Assign(TFhirCarePlan(oSource).FGoalList);
  FActivityList.Assign(TFhirCarePlan(oSource).FActivityList);
  notesObject := TFhirCarePlan(oSource).notesObject.Clone;
end;

procedure TFhirCarePlan.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'identifier') Then
     list.addAll(FIdentifierList);
  if (child_name = 'patient') Then
     list.add(FPatient.Link);
  if (child_name = 'status') Then
     list.add(FStatus.Link);
  if (child_name = 'period') Then
     list.add(FPeriod.Link);
  if (child_name = 'modified') Then
     list.add(FModified.Link);
  if (child_name = 'concern') Then
     list.addAll(FConcernList);
  if (child_name = 'participant') Then
     list.addAll(FParticipantList);
  if (child_name = 'goal') Then
     list.addAll(FGoalList);
  if (child_name = 'activity') Then
     list.addAll(FActivityList);
  if (child_name = 'notes') Then
     list.add(FNotes.Link);
end;

procedure TFhirCarePlan.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'patient', 'Resource(Patient)', FPatient.Link));{2}
  oList.add(TFHIRProperty.create(self, 'status', 'code', FStatus.Link));{1}
  oList.add(TFHIRProperty.create(self, 'period', 'Period', FPeriod.Link));{2}
  oList.add(TFHIRProperty.create(self, 'modified', 'dateTime', FModified.Link));{2}
  oList.add(TFHIRProperty.create(self, 'concern', 'Resource(Condition)', FConcernList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'participant', '', FParticipantList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'goal', '', FGoalList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'activity', '', FActivityList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'notes', 'string', FNotes.Link));{2}
end;

procedure TFhirCarePlan.setProperty(propName : string; propValue: TFHIRObject);
begin
  if (propName = 'identifier') then IdentifierList.add(propValue as TFhirIdentifier){2}
  else if (propName = 'patient') then Patient := propValue as TFhirResourceReference{TFhirPatient}{4b}
  else if (propName = 'status') then StatusObject := propValue as TFHIREnum
  else if (propName = 'period') then Period := propValue as TFhirPeriod{4b}
  else if (propName = 'modified') then ModifiedObject := propValue as TFhirDateTime{5a}
  else if (propName = 'concern') then ConcernList.add(propValue as TFhirResourceReference{TFhirCondition}){2}
  else if (propName = 'participant') then ParticipantList.add(propValue as TFhirCarePlanParticipant){2}
  else if (propName = 'goal') then GoalList.add(propValue as TFhirCarePlanGoal){2}
  else if (propName = 'activity') then ActivityList.add(propValue as TFhirCarePlanActivity){2}
  else if (propName = 'notes') then NotesObject := propValue as TFhirString{5a}
  else inherited;
end;

function TFhirCarePlan.FhirType : string;
begin
  result := 'CarePlan';
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
    result := TFhirCarePlanStatus(StringArrayIndexOfSensitive(CODES_TFhirCarePlanStatus, FStatus.value));
end;

Procedure TFhirCarePlan.SetStatusST(value : TFhirCarePlanStatus);
begin
  if ord(value) = 0 then
    StatusObject := nil
  else
    StatusObject := TFhirEnum.create(CODES_TFhirCarePlanStatus[value]);
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

Function TFhirCarePlan.GetModifiedST : TDateTimeEx;
begin
  if FModified = nil then
    result := nil
  else
    result := FModified.value;
end;

Procedure TFhirCarePlan.SetModifiedST(value : TDateTimeEx);
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
    result := FNotes.value;
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
  dateObject := TFhirComposition(oSource).dateObject.Clone;
  type_ := TFhirComposition(oSource).type_.Clone;
  class_ := TFhirComposition(oSource).class_.Clone;
  titleObject := TFhirComposition(oSource).titleObject.Clone;
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
     list.add(FIdentifier.Link);
  if (child_name = 'date') Then
     list.add(FDate.Link);
  if (child_name = 'type') Then
     list.add(FType_.Link);
  if (child_name = 'class') Then
     list.add(FClass_.Link);
  if (child_name = 'title') Then
     list.add(FTitle.Link);
  if (child_name = 'status') Then
     list.add(FStatus.Link);
  if (child_name = 'confidentiality') Then
     list.add(FConfidentiality.Link);
  if (child_name = 'subject') Then
     list.add(FSubject.Link);
  if (child_name = 'author') Then
     list.addAll(FAuthorList);
  if (child_name = 'attester') Then
     list.addAll(FAttesterList);
  if (child_name = 'custodian') Then
     list.add(FCustodian.Link);
  if (child_name = 'event') Then
     list.add(FEvent.Link);
  if (child_name = 'encounter') Then
     list.add(FEncounter.Link);
  if (child_name = 'section') Then
     list.addAll(FSectionList);
end;

procedure TFhirComposition.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifier.Link));{2}
  oList.add(TFHIRProperty.create(self, 'date', 'dateTime', FDate.Link));{2}
  oList.add(TFHIRProperty.create(self, 'type', 'CodeableConcept', FType_.Link));{2}
  oList.add(TFHIRProperty.create(self, 'class', 'CodeableConcept', FClass_.Link));{2}
  oList.add(TFHIRProperty.create(self, 'title', 'string', FTitle.Link));{2}
  oList.add(TFHIRProperty.create(self, 'status', 'code', FStatus.Link));{1}
  oList.add(TFHIRProperty.create(self, 'confidentiality', 'Coding', FConfidentiality.Link));{2}
  oList.add(TFHIRProperty.create(self, 'subject', 'Resource(Patient|Practitioner|Group|Device|Location)', FSubject.Link));{2}
  oList.add(TFHIRProperty.create(self, 'author', 'Resource(Practitioner|Device|Patient|RelatedPerson)', FAuthorList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'attester', '', FAttesterList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'custodian', 'Resource(Organization)', FCustodian.Link));{2}
  oList.add(TFHIRProperty.create(self, 'event', '', FEvent.Link));{2}
  oList.add(TFHIRProperty.create(self, 'encounter', 'Resource(Encounter)', FEncounter.Link));{2}
  oList.add(TFHIRProperty.create(self, 'section', '', FSectionList.Link)){3};
end;

procedure TFhirComposition.setProperty(propName : string; propValue: TFHIRObject);
begin
  if (propName = 'identifier') then Identifier := propValue as TFhirIdentifier{4b}
  else if (propName = 'date') then DateObject := propValue as TFhirDateTime{5a}
  else if (propName = 'type') then Type_ := propValue as TFhirCodeableConcept{4b}
  else if (propName = 'class') then Class_ := propValue as TFhirCodeableConcept{4b}
  else if (propName = 'title') then TitleObject := propValue as TFhirString{5a}
  else if (propName = 'status') then StatusObject := propValue as TFHIREnum
  else if (propName = 'confidentiality') then Confidentiality := propValue as TFhirCoding{4b}
  else if (propName = 'subject') then Subject := propValue as TFhirResourceReference{Resource}{4b}
  else if (propName = 'author') then AuthorList.add(propValue as TFhirResourceReference{Resource}){2}
  else if (propName = 'attester') then AttesterList.add(propValue as TFhirCompositionAttester){2}
  else if (propName = 'custodian') then Custodian := propValue as TFhirResourceReference{TFhirOrganization}{4b}
  else if (propName = 'event') then Event := propValue as TFhirCompositionEvent{4b}
  else if (propName = 'encounter') then Encounter := propValue as TFhirResourceReference{TFhirEncounter}{4b}
  else if (propName = 'section') then SectionList.add(propValue as TFhirCompositionSection){2}
  else inherited;
end;

function TFhirComposition.FhirType : string;
begin
  result := 'Composition';
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

Function TFhirComposition.GetDateST : TDateTimeEx;
begin
  if FDate = nil then
    result := nil
  else
    result := FDate.value;
end;

Procedure TFhirComposition.SetDateST(value : TDateTimeEx);
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
    result := FTitle.value;
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
    result := TFhirCompositionStatus(StringArrayIndexOfSensitive(CODES_TFhirCompositionStatus, FStatus.value));
end;

Procedure TFhirComposition.SetStatusST(value : TFhirCompositionStatus);
begin
  if ord(value) = 0 then
    StatusObject := nil
  else
    StatusObject := TFhirEnum.create(CODES_TFhirCompositionStatus[value]);
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
  FConceptList := TFhirConceptMapConceptList.Create;
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
  FConceptList.Free;
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
  identifierObject := TFhirConceptMap(oSource).identifierObject.Clone;
  versionObject := TFhirConceptMap(oSource).versionObject.Clone;
  nameObject := TFhirConceptMap(oSource).nameObject.Clone;
  publisherObject := TFhirConceptMap(oSource).publisherObject.Clone;
  FTelecomList.Assign(TFhirConceptMap(oSource).FTelecomList);
  descriptionObject := TFhirConceptMap(oSource).descriptionObject.Clone;
  copyrightObject := TFhirConceptMap(oSource).copyrightObject.Clone;
  FStatus := TFhirConceptMap(oSource).FStatus.Link;
  experimentalObject := TFhirConceptMap(oSource).experimentalObject.Clone;
  dateObject := TFhirConceptMap(oSource).dateObject.Clone;
  source := TFhirConceptMap(oSource).source.Clone;
  target := TFhirConceptMap(oSource).target.Clone;
  FConceptList.Assign(TFhirConceptMap(oSource).FConceptList);
end;

procedure TFhirConceptMap.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'identifier') Then
     list.add(FIdentifier.Link);
  if (child_name = 'version') Then
     list.add(FVersion.Link);
  if (child_name = 'name') Then
     list.add(FName.Link);
  if (child_name = 'publisher') Then
     list.add(FPublisher.Link);
  if (child_name = 'telecom') Then
     list.addAll(FTelecomList);
  if (child_name = 'description') Then
     list.add(FDescription.Link);
  if (child_name = 'copyright') Then
     list.add(FCopyright.Link);
  if (child_name = 'status') Then
     list.add(FStatus.Link);
  if (child_name = 'experimental') Then
     list.add(FExperimental.Link);
  if (child_name = 'date') Then
     list.add(FDate.Link);
  if (child_name = 'source') Then
     list.add(FSource.Link);
  if (child_name = 'target') Then
     list.add(FTarget.Link);
  if (child_name = 'concept') Then
     list.addAll(FConceptList);
end;

procedure TFhirConceptMap.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'string', FIdentifier.Link));{2}
  oList.add(TFHIRProperty.create(self, 'version', 'string', FVersion.Link));{2}
  oList.add(TFHIRProperty.create(self, 'name', 'string', FName.Link));{2}
  oList.add(TFHIRProperty.create(self, 'publisher', 'string', FPublisher.Link));{2}
  oList.add(TFHIRProperty.create(self, 'telecom', 'Contact', FTelecomList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'description', 'string', FDescription.Link));{2}
  oList.add(TFHIRProperty.create(self, 'copyright', 'string', FCopyright.Link));{2}
  oList.add(TFHIRProperty.create(self, 'status', 'code', FStatus.Link));{1}
  oList.add(TFHIRProperty.create(self, 'experimental', 'boolean', FExperimental.Link));{2}
  oList.add(TFHIRProperty.create(self, 'date', 'dateTime', FDate.Link));{2}
  oList.add(TFHIRProperty.create(self, 'source', 'Resource(ValueSet)', FSource.Link));{2}
  oList.add(TFHIRProperty.create(self, 'target', 'Resource(ValueSet)', FTarget.Link));{2}
  oList.add(TFHIRProperty.create(self, 'concept', '', FConceptList.Link)){3};
end;

procedure TFhirConceptMap.setProperty(propName : string; propValue: TFHIRObject);
begin
  if (propName = 'identifier') then IdentifierObject := propValue as TFhirString{5a}
  else if (propName = 'version') then VersionObject := propValue as TFhirString{5a}
  else if (propName = 'name') then NameObject := propValue as TFhirString{5a}
  else if (propName = 'publisher') then PublisherObject := propValue as TFhirString{5a}
  else if (propName = 'telecom') then TelecomList.add(propValue as TFhirContact){2}
  else if (propName = 'description') then DescriptionObject := propValue as TFhirString{5a}
  else if (propName = 'copyright') then CopyrightObject := propValue as TFhirString{5a}
  else if (propName = 'status') then StatusObject := propValue as TFHIREnum
  else if (propName = 'experimental') then ExperimentalObject := propValue as TFhirBoolean{5a}
  else if (propName = 'date') then DateObject := propValue as TFhirDateTime{5a}
  else if (propName = 'source') then Source := propValue as TFhirResourceReference{TFhirValueSet}{4b}
  else if (propName = 'target') then Target := propValue as TFhirResourceReference{TFhirValueSet}{4b}
  else if (propName = 'concept') then ConceptList.add(propValue as TFhirConceptMapConcept){2}
  else inherited;
end;

function TFhirConceptMap.FhirType : string;
begin
  result := 'ConceptMap';
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
    result := FIdentifier.value;
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
    result := FVersion.value;
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
    result := FName.value;
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
    result := FPublisher.value;
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
    result := FDescription.value;
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
    result := FCopyright.value;
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
    result := TFhirValuesetStatus(StringArrayIndexOfSensitive(CODES_TFhirValuesetStatus, FStatus.value));
end;

Procedure TFhirConceptMap.SetStatusST(value : TFhirValuesetStatus);
begin
  if ord(value) = 0 then
    StatusObject := nil
  else
    StatusObject := TFhirEnum.create(CODES_TFhirValuesetStatus[value]);
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
    result := FExperimental.value;
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

Function TFhirConceptMap.GetDateST : TDateTimeEx;
begin
  if FDate = nil then
    result := nil
  else
    result := FDate.value;
end;

Procedure TFhirConceptMap.SetDateST(value : TDateTimeEx);
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

Procedure TFhirConceptMap.SetSource(value : TFhirResourceReference{TFhirValueSet});
begin
  FSource.free;
  FSource := value;
end;

Procedure TFhirConceptMap.SetTarget(value : TFhirResourceReference{TFhirValueSet});
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
  dateAssertedObject := TFhirCondition(oSource).dateAssertedObject.Clone;
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
  notesObject := TFhirCondition(oSource).notesObject.Clone;
end;

procedure TFhirCondition.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'identifier') Then
     list.addAll(FIdentifierList);
  if (child_name = 'subject') Then
     list.add(FSubject.Link);
  if (child_name = 'encounter') Then
     list.add(FEncounter.Link);
  if (child_name = 'asserter') Then
     list.add(FAsserter.Link);
  if (child_name = 'dateAsserted') Then
     list.add(FDateAsserted.Link);
  if (child_name = 'code') Then
     list.add(FCode.Link);
  if (child_name = 'category') Then
     list.add(FCategory.Link);
  if (child_name = 'status') Then
     list.add(FStatus.Link);
  if (child_name = 'certainty') Then
     list.add(FCertainty.Link);
  if (child_name = 'severity') Then
     list.add(FSeverity.Link);
  if (child_name = 'onset[x]') Then
     list.add(FOnset.Link);
  if (child_name = 'abatement[x]') Then
     list.add(FAbatement.Link);
  if (child_name = 'stage') Then
     list.add(FStage.Link);
  if (child_name = 'evidence') Then
     list.addAll(FEvidenceList);
  if (child_name = 'location') Then
     list.addAll(FLocationList);
  if (child_name = 'relatedItem') Then
     list.addAll(FRelatedItemList);
  if (child_name = 'notes') Then
     list.add(FNotes.Link);
end;

procedure TFhirCondition.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'subject', 'Resource(Patient)', FSubject.Link));{2}
  oList.add(TFHIRProperty.create(self, 'encounter', 'Resource(Encounter)', FEncounter.Link));{2}
  oList.add(TFHIRProperty.create(self, 'asserter', 'Resource(Practitioner|Patient)', FAsserter.Link));{2}
  oList.add(TFHIRProperty.create(self, 'dateAsserted', 'date', FDateAsserted.Link));{2}
  oList.add(TFHIRProperty.create(self, 'code', 'CodeableConcept', FCode.Link));{2}
  oList.add(TFHIRProperty.create(self, 'category', 'CodeableConcept', FCategory.Link));{2}
  oList.add(TFHIRProperty.create(self, 'status', 'code', FStatus.Link));{1}
  oList.add(TFHIRProperty.create(self, 'certainty', 'CodeableConcept', FCertainty.Link));{2}
  oList.add(TFHIRProperty.create(self, 'severity', 'CodeableConcept', FSeverity.Link));{2}
  oList.add(TFHIRProperty.create(self, 'onset[x]', 'date|Age', FOnset.Link));{2}
  oList.add(TFHIRProperty.create(self, 'abatement[x]', 'date|Age|boolean', FAbatement.Link));{2}
  oList.add(TFHIRProperty.create(self, 'stage', '', FStage.Link));{2}
  oList.add(TFHIRProperty.create(self, 'evidence', '', FEvidenceList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'location', '', FLocationList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'relatedItem', '', FRelatedItemList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'notes', 'string', FNotes.Link));{2}
end;

procedure TFhirCondition.setProperty(propName : string; propValue: TFHIRObject);
begin
  if (propName = 'identifier') then IdentifierList.add(propValue as TFhirIdentifier){2}
  else if (propName = 'subject') then Subject := propValue as TFhirResourceReference{TFhirPatient}{4b}
  else if (propName = 'encounter') then Encounter := propValue as TFhirResourceReference{TFhirEncounter}{4b}
  else if (propName = 'asserter') then Asserter := propValue as TFhirResourceReference{Resource}{4b}
  else if (propName = 'dateAsserted') then DateAssertedObject := propValue as TFhirDate{5a}
  else if (propName = 'code') then Code := propValue as TFhirCodeableConcept{4b}
  else if (propName = 'category') then Category := propValue as TFhirCodeableConcept{4b}
  else if (propName = 'status') then StatusObject := propValue as TFHIREnum
  else if (propName = 'certainty') then Certainty := propValue as TFhirCodeableConcept{4b}
  else if (propName = 'severity') then Severity := propValue as TFhirCodeableConcept{4b}
  else if (propName.startsWith('onset')) then Onset := propValue as TFhirType{4}
  else if (propName.startsWith('abatement')) then Abatement := propValue as TFhirType{4}
  else if (propName = 'stage') then Stage := propValue as TFhirConditionStage{4b}
  else if (propName = 'evidence') then EvidenceList.add(propValue as TFhirConditionEvidence){2}
  else if (propName = 'location') then LocationList.add(propValue as TFhirConditionLocation){2}
  else if (propName = 'relatedItem') then RelatedItemList.add(propValue as TFhirConditionRelatedItem){2}
  else if (propName = 'notes') then NotesObject := propValue as TFhirString{5a}
  else inherited;
end;

function TFhirCondition.FhirType : string;
begin
  result := 'Condition';
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

Function TFhirCondition.GetDateAssertedST : TDateTimeEx;
begin
  if FDateAsserted = nil then
    result := nil
  else
    result := FDateAsserted.value;
end;

Procedure TFhirCondition.SetDateAssertedST(value : TDateTimeEx);
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
    result := TFhirConditionStatus(StringArrayIndexOfSensitive(CODES_TFhirConditionStatus, FStatus.value));
end;

Procedure TFhirCondition.SetStatusST(value : TFhirConditionStatus);
begin
  if ord(value) = 0 then
    StatusObject := nil
  else
    StatusObject := TFhirEnum.create(CODES_TFhirConditionStatus[value]);
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
    result := FNotes.value;
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
  identifierObject := TFhirConformance(oSource).identifierObject.Clone;
  versionObject := TFhirConformance(oSource).versionObject.Clone;
  nameObject := TFhirConformance(oSource).nameObject.Clone;
  publisherObject := TFhirConformance(oSource).publisherObject.Clone;
  FTelecomList.Assign(TFhirConformance(oSource).FTelecomList);
  descriptionObject := TFhirConformance(oSource).descriptionObject.Clone;
  FStatus := TFhirConformance(oSource).FStatus.Link;
  experimentalObject := TFhirConformance(oSource).experimentalObject.Clone;
  dateObject := TFhirConformance(oSource).dateObject.Clone;
  software := TFhirConformance(oSource).software.Clone;
  implementation_ := TFhirConformance(oSource).implementation_.Clone;
  fhirVersionObject := TFhirConformance(oSource).fhirVersionObject.Clone;
  acceptUnknownObject := TFhirConformance(oSource).acceptUnknownObject.Clone;
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
     list.add(FIdentifier.Link);
  if (child_name = 'version') Then
     list.add(FVersion.Link);
  if (child_name = 'name') Then
     list.add(FName.Link);
  if (child_name = 'publisher') Then
     list.add(FPublisher.Link);
  if (child_name = 'telecom') Then
     list.addAll(FTelecomList);
  if (child_name = 'description') Then
     list.add(FDescription.Link);
  if (child_name = 'status') Then
     list.add(FStatus.Link);
  if (child_name = 'experimental') Then
     list.add(FExperimental.Link);
  if (child_name = 'date') Then
     list.add(FDate.Link);
  if (child_name = 'software') Then
     list.add(FSoftware.Link);
  if (child_name = 'implementation') Then
     list.add(FImplementation_.Link);
  if (child_name = 'fhirVersion') Then
     list.add(FFhirVersion.Link);
  if (child_name = 'acceptUnknown') Then
     list.add(FAcceptUnknown.Link);
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
  oList.add(TFHIRProperty.create(self, 'identifier', 'string', FIdentifier.Link));{2}
  oList.add(TFHIRProperty.create(self, 'version', 'string', FVersion.Link));{2}
  oList.add(TFHIRProperty.create(self, 'name', 'string', FName.Link));{2}
  oList.add(TFHIRProperty.create(self, 'publisher', 'string', FPublisher.Link));{2}
  oList.add(TFHIRProperty.create(self, 'telecom', 'Contact', FTelecomList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'description', 'string', FDescription.Link));{2}
  oList.add(TFHIRProperty.create(self, 'status', 'code', FStatus.Link));{1}
  oList.add(TFHIRProperty.create(self, 'experimental', 'boolean', FExperimental.Link));{2}
  oList.add(TFHIRProperty.create(self, 'date', 'dateTime', FDate.Link));{2}
  oList.add(TFHIRProperty.create(self, 'software', '', FSoftware.Link));{2}
  oList.add(TFHIRProperty.create(self, 'implementation', '', FImplementation_.Link));{2}
  oList.add(TFHIRProperty.create(self, 'fhirVersion', 'id', FFhirVersion.Link));{2}
  oList.add(TFHIRProperty.create(self, 'acceptUnknown', 'boolean', FAcceptUnknown.Link));{2}
  oList.add(TFHIRProperty.create(self, 'format', 'code', FFormatList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'profile', 'Resource(Profile)', FProfileList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'rest', '', FRestList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'messaging', '', FMessagingList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'document', '', FDocumentList.Link)){3};
end;

procedure TFhirConformance.setProperty(propName : string; propValue: TFHIRObject);
begin
  if (propName = 'identifier') then IdentifierObject := propValue as TFhirString{5a}
  else if (propName = 'version') then VersionObject := propValue as TFhirString{5a}
  else if (propName = 'name') then NameObject := propValue as TFhirString{5a}
  else if (propName = 'publisher') then PublisherObject := propValue as TFhirString{5a}
  else if (propName = 'telecom') then TelecomList.add(propValue as TFhirContact){2}
  else if (propName = 'description') then DescriptionObject := propValue as TFhirString{5a}
  else if (propName = 'status') then StatusObject := propValue as TFHIREnum
  else if (propName = 'experimental') then ExperimentalObject := propValue as TFhirBoolean{5a}
  else if (propName = 'date') then DateObject := propValue as TFhirDateTime{5a}
  else if (propName = 'software') then Software := propValue as TFhirConformanceSoftware{4b}
  else if (propName = 'implementation') then Implementation_ := propValue as TFhirConformanceImplementation{4b}
  else if (propName = 'fhirVersion') then FhirVersionObject := propValue as TFhirId{5a}
  else if (propName = 'acceptUnknown') then AcceptUnknownObject := propValue as TFhirBoolean{5a}
  else if (propName = 'format') then FormatList.add(propValue as TFhirCode){2}
  else if (propName = 'profile') then ProfileList.add(propValue as TFhirResourceReference{TFhirProfile}){2}
  else if (propName = 'rest') then RestList.add(propValue as TFhirConformanceRest){2}
  else if (propName = 'messaging') then MessagingList.add(propValue as TFhirConformanceMessaging){2}
  else if (propName = 'document') then DocumentList.add(propValue as TFhirConformanceDocument){2}
  else inherited;
end;

function TFhirConformance.FhirType : string;
begin
  result := 'Conformance';
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
    result := FIdentifier.value;
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
    result := FVersion.value;
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
    result := FName.value;
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
    result := FPublisher.value;
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
    result := FDescription.value;
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
    result := TFhirConformanceStatementStatus(StringArrayIndexOfSensitive(CODES_TFhirConformanceStatementStatus, FStatus.value));
end;

Procedure TFhirConformance.SetStatusST(value : TFhirConformanceStatementStatus);
begin
  if ord(value) = 0 then
    StatusObject := nil
  else
    StatusObject := TFhirEnum.create(CODES_TFhirConformanceStatementStatus[value]);
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
    result := FExperimental.value;
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

Function TFhirConformance.GetDateST : TDateTimeEx;
begin
  if FDate = nil then
    result := nil
  else
    result := FDate.value;
end;

Procedure TFhirConformance.SetDateST(value : TDateTimeEx);
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
    result := FFhirVersion.value;
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
    result := FAcceptUnknown.value;
end;

Procedure TFhirConformance.SetAcceptUnknownST(value : Boolean);
begin
  if FAcceptUnknown = nil then
    FAcceptUnknown := TFhirBoolean.create;
  FAcceptUnknown.value := value
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
  manufacturerObject := TFhirDevice(oSource).manufacturerObject.Clone;
  modelObject := TFhirDevice(oSource).modelObject.Clone;
  versionObject := TFhirDevice(oSource).versionObject.Clone;
  expiryObject := TFhirDevice(oSource).expiryObject.Clone;
  udiObject := TFhirDevice(oSource).udiObject.Clone;
  lotNumberObject := TFhirDevice(oSource).lotNumberObject.Clone;
  owner := TFhirDevice(oSource).owner.Clone;
  location := TFhirDevice(oSource).location.Clone;
  patient := TFhirDevice(oSource).patient.Clone;
  FContactList.Assign(TFhirDevice(oSource).FContactList);
  urlObject := TFhirDevice(oSource).urlObject.Clone;
end;

procedure TFhirDevice.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'identifier') Then
     list.addAll(FIdentifierList);
  if (child_name = 'type') Then
     list.add(FType_.Link);
  if (child_name = 'manufacturer') Then
     list.add(FManufacturer.Link);
  if (child_name = 'model') Then
     list.add(FModel.Link);
  if (child_name = 'version') Then
     list.add(FVersion.Link);
  if (child_name = 'expiry') Then
     list.add(FExpiry.Link);
  if (child_name = 'udi') Then
     list.add(FUdi.Link);
  if (child_name = 'lotNumber') Then
     list.add(FLotNumber.Link);
  if (child_name = 'owner') Then
     list.add(FOwner.Link);
  if (child_name = 'location') Then
     list.add(FLocation.Link);
  if (child_name = 'patient') Then
     list.add(FPatient.Link);
  if (child_name = 'contact') Then
     list.addAll(FContactList);
  if (child_name = 'url') Then
     list.add(FUrl.Link);
end;

procedure TFhirDevice.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'type', 'CodeableConcept', FType_.Link));{2}
  oList.add(TFHIRProperty.create(self, 'manufacturer', 'string', FManufacturer.Link));{2}
  oList.add(TFHIRProperty.create(self, 'model', 'string', FModel.Link));{2}
  oList.add(TFHIRProperty.create(self, 'version', 'string', FVersion.Link));{2}
  oList.add(TFHIRProperty.create(self, 'expiry', 'date', FExpiry.Link));{2}
  oList.add(TFHIRProperty.create(self, 'udi', 'string', FUdi.Link));{2}
  oList.add(TFHIRProperty.create(self, 'lotNumber', 'string', FLotNumber.Link));{2}
  oList.add(TFHIRProperty.create(self, 'owner', 'Resource(Organization)', FOwner.Link));{2}
  oList.add(TFHIRProperty.create(self, 'location', 'Resource(Location)', FLocation.Link));{2}
  oList.add(TFHIRProperty.create(self, 'patient', 'Resource(Patient)', FPatient.Link));{2}
  oList.add(TFHIRProperty.create(self, 'contact', 'Contact', FContactList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'url', 'uri', FUrl.Link));{2}
end;

procedure TFhirDevice.setProperty(propName : string; propValue: TFHIRObject);
begin
  if (propName = 'identifier') then IdentifierList.add(propValue as TFhirIdentifier){2}
  else if (propName = 'type') then Type_ := propValue as TFhirCodeableConcept{4b}
  else if (propName = 'manufacturer') then ManufacturerObject := propValue as TFhirString{5a}
  else if (propName = 'model') then ModelObject := propValue as TFhirString{5a}
  else if (propName = 'version') then VersionObject := propValue as TFhirString{5a}
  else if (propName = 'expiry') then ExpiryObject := propValue as TFhirDate{5a}
  else if (propName = 'udi') then UdiObject := propValue as TFhirString{5a}
  else if (propName = 'lotNumber') then LotNumberObject := propValue as TFhirString{5a}
  else if (propName = 'owner') then Owner := propValue as TFhirResourceReference{TFhirOrganization}{4b}
  else if (propName = 'location') then Location := propValue as TFhirResourceReference{TFhirLocation}{4b}
  else if (propName = 'patient') then Patient := propValue as TFhirResourceReference{TFhirPatient}{4b}
  else if (propName = 'contact') then ContactList.add(propValue as TFhirContact){2}
  else if (propName = 'url') then UrlObject := propValue as TFhirUri{5a}
  else inherited;
end;

function TFhirDevice.FhirType : string;
begin
  result := 'Device';
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
    result := FManufacturer.value;
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
    result := FModel.value;
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
    result := FVersion.value;
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

Function TFhirDevice.GetExpiryST : TDateTimeEx;
begin
  if FExpiry = nil then
    result := nil
  else
    result := FExpiry.value;
end;

Procedure TFhirDevice.SetExpiryST(value : TDateTimeEx);
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
    result := FUdi.value;
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
    result := FLotNumber.value;
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
    result := FUrl.value;
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
  instantObject := TFhirDeviceObservationReport(oSource).instantObject.Clone;
  identifier := TFhirDeviceObservationReport(oSource).identifier.Clone;
  source := TFhirDeviceObservationReport(oSource).source.Clone;
  subject := TFhirDeviceObservationReport(oSource).subject.Clone;
  FVirtualDeviceList.Assign(TFhirDeviceObservationReport(oSource).FVirtualDeviceList);
end;

procedure TFhirDeviceObservationReport.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'instant') Then
     list.add(FInstant.Link);
  if (child_name = 'identifier') Then
     list.add(FIdentifier.Link);
  if (child_name = 'source') Then
     list.add(FSource.Link);
  if (child_name = 'subject') Then
     list.add(FSubject.Link);
  if (child_name = 'virtualDevice') Then
     list.addAll(FVirtualDeviceList);
end;

procedure TFhirDeviceObservationReport.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'instant', 'instant', FInstant.Link));{2}
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifier.Link));{2}
  oList.add(TFHIRProperty.create(self, 'source', 'Resource(Device)', FSource.Link));{2}
  oList.add(TFHIRProperty.create(self, 'subject', 'Resource(Patient|Device|Location)', FSubject.Link));{2}
  oList.add(TFHIRProperty.create(self, 'virtualDevice', '', FVirtualDeviceList.Link)){3};
end;

procedure TFhirDeviceObservationReport.setProperty(propName : string; propValue: TFHIRObject);
begin
  if (propName = 'instant') then InstantObject := propValue as TFhirInstant{5a}
  else if (propName = 'identifier') then Identifier := propValue as TFhirIdentifier{4b}
  else if (propName = 'source') then Source := propValue as TFhirResourceReference{TFhirDevice}{4b}
  else if (propName = 'subject') then Subject := propValue as TFhirResourceReference{Resource}{4b}
  else if (propName = 'virtualDevice') then VirtualDeviceList.add(propValue as TFhirDeviceObservationReportVirtualDevice){2}
  else inherited;
end;

function TFhirDeviceObservationReport.FhirType : string;
begin
  result := 'DeviceObservationReport';
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

Function TFhirDeviceObservationReport.GetInstantST : TDateTimeEx;
begin
  if FInstant = nil then
    result := nil
  else
    result := FInstant.value;
end;

Procedure TFhirDeviceObservationReport.SetInstantST(value : TDateTimeEx);
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
  clinicalNotesObject := TFhirDiagnosticOrder(oSource).clinicalNotesObject.Clone;
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
     list.add(FSubject.Link);
  if (child_name = 'orderer') Then
     list.add(FOrderer.Link);
  if (child_name = 'identifier') Then
     list.addAll(FIdentifierList);
  if (child_name = 'encounter') Then
     list.add(FEncounter.Link);
  if (child_name = 'clinicalNotes') Then
     list.add(FClinicalNotes.Link);
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
  oList.add(TFHIRProperty.create(self, 'subject', 'Resource(Patient|Group|Location|Device)', FSubject.Link));{2}
  oList.add(TFHIRProperty.create(self, 'orderer', 'Resource(Practitioner)', FOrderer.Link));{2}
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'encounter', 'Resource(Encounter)', FEncounter.Link));{2}
  oList.add(TFHIRProperty.create(self, 'clinicalNotes', 'string', FClinicalNotes.Link));{2}
  oList.add(TFHIRProperty.create(self, 'specimen', 'Resource(Specimen)', FSpecimenList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'status', 'code', FStatus.Link));{1}
  oList.add(TFHIRProperty.create(self, 'priority', 'code', FPriority.Link));{1}
  oList.add(TFHIRProperty.create(self, 'event', '', FEventList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'item', '', FItemList.Link)){3};
end;

procedure TFhirDiagnosticOrder.setProperty(propName : string; propValue: TFHIRObject);
begin
  if (propName = 'subject') then Subject := propValue as TFhirResourceReference{Resource}{4b}
  else if (propName = 'orderer') then Orderer := propValue as TFhirResourceReference{TFhirPractitioner}{4b}
  else if (propName = 'identifier') then IdentifierList.add(propValue as TFhirIdentifier){2}
  else if (propName = 'encounter') then Encounter := propValue as TFhirResourceReference{TFhirEncounter}{4b}
  else if (propName = 'clinicalNotes') then ClinicalNotesObject := propValue as TFhirString{5a}
  else if (propName = 'specimen') then SpecimenList.add(propValue as TFhirResourceReference{TFhirSpecimen}){2}
  else if (propName = 'status') then StatusObject := propValue as TFHIREnum
  else if (propName = 'priority') then PriorityObject := propValue as TFHIREnum
  else if (propName = 'event') then EventList.add(propValue as TFhirDiagnosticOrderEvent){2}
  else if (propName = 'item') then ItemList.add(propValue as TFhirDiagnosticOrderItem){2}
  else inherited;
end;

function TFhirDiagnosticOrder.FhirType : string;
begin
  result := 'DiagnosticOrder';
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
    result := FClinicalNotes.value;
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
    result := TFhirDiagnosticOrderStatus(StringArrayIndexOfSensitive(CODES_TFhirDiagnosticOrderStatus, FStatus.value));
end;

Procedure TFhirDiagnosticOrder.SetStatusST(value : TFhirDiagnosticOrderStatus);
begin
  if ord(value) = 0 then
    StatusObject := nil
  else
    StatusObject := TFhirEnum.create(CODES_TFhirDiagnosticOrderStatus[value]);
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
    result := TFhirDiagnosticOrderPriority(StringArrayIndexOfSensitive(CODES_TFhirDiagnosticOrderPriority, FPriority.value));
end;

Procedure TFhirDiagnosticOrder.SetPriorityST(value : TFhirDiagnosticOrderPriority);
begin
  if ord(value) = 0 then
    PriorityObject := nil
  else
    PriorityObject := TFhirEnum.create(CODES_TFhirDiagnosticOrderPriority[value]);
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
  issuedObject := TFhirDiagnosticReport(oSource).issuedObject.Clone;
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
  conclusionObject := TFhirDiagnosticReport(oSource).conclusionObject.Clone;
  FCodedDiagnosisList.Assign(TFhirDiagnosticReport(oSource).FCodedDiagnosisList);
  FPresentedFormList.Assign(TFhirDiagnosticReport(oSource).FPresentedFormList);
end;

procedure TFhirDiagnosticReport.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'name') Then
     list.add(FName.Link);
  if (child_name = 'status') Then
     list.add(FStatus.Link);
  if (child_name = 'issued') Then
     list.add(FIssued.Link);
  if (child_name = 'subject') Then
     list.add(FSubject.Link);
  if (child_name = 'performer') Then
     list.add(FPerformer.Link);
  if (child_name = 'identifier') Then
     list.add(FIdentifier.Link);
  if (child_name = 'requestDetail') Then
     list.addAll(FRequestDetailList);
  if (child_name = 'serviceCategory') Then
     list.add(FServiceCategory.Link);
  if (child_name = 'diagnostic[x]') Then
     list.add(FDiagnostic.Link);
  if (child_name = 'specimen') Then
     list.addAll(FSpecimenList);
  if (child_name = 'result') Then
     list.addAll(FResultList);
  if (child_name = 'imagingStudy') Then
     list.addAll(FImagingStudyList);
  if (child_name = 'image') Then
     list.addAll(FImageList);
  if (child_name = 'conclusion') Then
     list.add(FConclusion.Link);
  if (child_name = 'codedDiagnosis') Then
     list.addAll(FCodedDiagnosisList);
  if (child_name = 'presentedForm') Then
     list.addAll(FPresentedFormList);
end;

procedure TFhirDiagnosticReport.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'name', 'CodeableConcept', FName.Link));{2}
  oList.add(TFHIRProperty.create(self, 'status', 'code', FStatus.Link));{1}
  oList.add(TFHIRProperty.create(self, 'issued', 'dateTime', FIssued.Link));{2}
  oList.add(TFHIRProperty.create(self, 'subject', 'Resource(Patient|Group|Device|Location)', FSubject.Link));{2}
  oList.add(TFHIRProperty.create(self, 'performer', 'Resource(Practitioner|Organization)', FPerformer.Link));{2}
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifier.Link));{2}
  oList.add(TFHIRProperty.create(self, 'requestDetail', 'Resource(DiagnosticOrder)', FRequestDetailList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'serviceCategory', 'CodeableConcept', FServiceCategory.Link));{2}
  oList.add(TFHIRProperty.create(self, 'diagnostic[x]', 'dateTime|Period', FDiagnostic.Link));{2}
  oList.add(TFHIRProperty.create(self, 'specimen', 'Resource(Specimen)', FSpecimenList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'result', 'Resource(Observation)', FResultList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'imagingStudy', 'Resource(ImagingStudy)', FImagingStudyList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'image', '', FImageList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'conclusion', 'string', FConclusion.Link));{2}
  oList.add(TFHIRProperty.create(self, 'codedDiagnosis', 'CodeableConcept', FCodedDiagnosisList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'presentedForm', 'Attachment', FPresentedFormList.Link)){3};
end;

procedure TFhirDiagnosticReport.setProperty(propName : string; propValue: TFHIRObject);
begin
  if (propName = 'name') then Name := propValue as TFhirCodeableConcept{4b}
  else if (propName = 'status') then StatusObject := propValue as TFHIREnum
  else if (propName = 'issued') then IssuedObject := propValue as TFhirDateTime{5a}
  else if (propName = 'subject') then Subject := propValue as TFhirResourceReference{Resource}{4b}
  else if (propName = 'performer') then Performer := propValue as TFhirResourceReference{Resource}{4b}
  else if (propName = 'identifier') then Identifier := propValue as TFhirIdentifier{4b}
  else if (propName = 'requestDetail') then RequestDetailList.add(propValue as TFhirResourceReference{TFhirDiagnosticOrder}){2}
  else if (propName = 'serviceCategory') then ServiceCategory := propValue as TFhirCodeableConcept{4b}
  else if (propName.startsWith('diagnostic')) then Diagnostic := propValue as TFhirType{4}
  else if (propName = 'specimen') then SpecimenList.add(propValue as TFhirResourceReference{TFhirSpecimen}){2}
  else if (propName = 'result') then ResultList.add(propValue as TFhirResourceReference{TFhirObservation}){2}
  else if (propName = 'imagingStudy') then ImagingStudyList.add(propValue as TFhirResourceReference{TFhirImagingStudy}){2}
  else if (propName = 'image') then ImageList.add(propValue as TFhirDiagnosticReportImage){2}
  else if (propName = 'conclusion') then ConclusionObject := propValue as TFhirString{5a}
  else if (propName = 'codedDiagnosis') then CodedDiagnosisList.add(propValue as TFhirCodeableConcept){2}
  else if (propName = 'presentedForm') then PresentedFormList.add(propValue as TFhirAttachment){2}
  else inherited;
end;

function TFhirDiagnosticReport.FhirType : string;
begin
  result := 'DiagnosticReport';
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
    result := TFhirDiagnosticReportStatus(StringArrayIndexOfSensitive(CODES_TFhirDiagnosticReportStatus, FStatus.value));
end;

Procedure TFhirDiagnosticReport.SetStatusST(value : TFhirDiagnosticReportStatus);
begin
  if ord(value) = 0 then
    StatusObject := nil
  else
    StatusObject := TFhirEnum.create(CODES_TFhirDiagnosticReportStatus[value]);
end;

Procedure TFhirDiagnosticReport.SetIssued(value : TFhirDateTime);
begin
  FIssued.free;
  FIssued := value;
end;

Function TFhirDiagnosticReport.GetIssuedST : TDateTimeEx;
begin
  if FIssued = nil then
    result := nil
  else
    result := FIssued.value;
end;

Procedure TFhirDiagnosticReport.SetIssuedST(value : TDateTimeEx);
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
    result := FConclusion.value;
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
  createdObject := TFhirDocumentManifest(oSource).createdObject.Clone;
  sourceObject := TFhirDocumentManifest(oSource).sourceObject.Clone;
  FStatus := TFhirDocumentManifest(oSource).FStatus.Link;
  supercedes := TFhirDocumentManifest(oSource).supercedes.Clone;
  descriptionObject := TFhirDocumentManifest(oSource).descriptionObject.Clone;
  confidentiality := TFhirDocumentManifest(oSource).confidentiality.Clone;
  FContentList.Assign(TFhirDocumentManifest(oSource).FContentList);
end;

procedure TFhirDocumentManifest.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'masterIdentifier') Then
     list.add(FMasterIdentifier.Link);
  if (child_name = 'identifier') Then
     list.addAll(FIdentifierList);
  if (child_name = 'subject') Then
     list.addAll(FSubjectList);
  if (child_name = 'recipient') Then
     list.addAll(FRecipientList);
  if (child_name = 'type') Then
     list.add(FType_.Link);
  if (child_name = 'author') Then
     list.addAll(FAuthorList);
  if (child_name = 'created') Then
     list.add(FCreated.Link);
  if (child_name = 'source') Then
     list.add(FSource.Link);
  if (child_name = 'status') Then
     list.add(FStatus.Link);
  if (child_name = 'supercedes') Then
     list.add(FSupercedes.Link);
  if (child_name = 'description') Then
     list.add(FDescription.Link);
  if (child_name = 'confidentiality') Then
     list.add(FConfidentiality.Link);
  if (child_name = 'content') Then
     list.addAll(FContentList);
end;

procedure TFhirDocumentManifest.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'masterIdentifier', 'Identifier', FMasterIdentifier.Link));{2}
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'subject', 'Resource(Patient|Practitioner|Group|Device)', FSubjectList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'recipient', 'Resource(Patient|Practitioner|Organization)', FRecipientList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'type', 'CodeableConcept', FType_.Link));{2}
  oList.add(TFHIRProperty.create(self, 'author', 'Resource(Practitioner|Device|Patient|RelatedPerson)', FAuthorList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'created', 'dateTime', FCreated.Link));{2}
  oList.add(TFHIRProperty.create(self, 'source', 'uri', FSource.Link));{2}
  oList.add(TFHIRProperty.create(self, 'status', 'code', FStatus.Link));{1}
  oList.add(TFHIRProperty.create(self, 'supercedes', 'Resource(DocumentManifest)', FSupercedes.Link));{2}
  oList.add(TFHIRProperty.create(self, 'description', 'string', FDescription.Link));{2}
  oList.add(TFHIRProperty.create(self, 'confidentiality', 'CodeableConcept', FConfidentiality.Link));{2}
  oList.add(TFHIRProperty.create(self, 'content', 'Resource(DocumentReference|Binary|Media)', FContentList.Link)){3};
end;

procedure TFhirDocumentManifest.setProperty(propName : string; propValue: TFHIRObject);
begin
  if (propName = 'masterIdentifier') then MasterIdentifier := propValue as TFhirIdentifier{4b}
  else if (propName = 'identifier') then IdentifierList.add(propValue as TFhirIdentifier){2}
  else if (propName = 'subject') then SubjectList.add(propValue as TFhirResourceReference{Resource}){2}
  else if (propName = 'recipient') then RecipientList.add(propValue as TFhirResourceReference{Resource}){2}
  else if (propName = 'type') then Type_ := propValue as TFhirCodeableConcept{4b}
  else if (propName = 'author') then AuthorList.add(propValue as TFhirResourceReference{Resource}){2}
  else if (propName = 'created') then CreatedObject := propValue as TFhirDateTime{5a}
  else if (propName = 'source') then SourceObject := propValue as TFhirUri{5a}
  else if (propName = 'status') then StatusObject := propValue as TFHIREnum
  else if (propName = 'supercedes') then Supercedes := propValue as TFhirResourceReference{TFhirDocumentManifest}{4b}
  else if (propName = 'description') then DescriptionObject := propValue as TFhirString{5a}
  else if (propName = 'confidentiality') then Confidentiality := propValue as TFhirCodeableConcept{4b}
  else if (propName = 'content') then ContentList.add(propValue as TFhirResourceReference{Resource}){2}
  else inherited;
end;

function TFhirDocumentManifest.FhirType : string;
begin
  result := 'DocumentManifest';
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

Function TFhirDocumentManifest.GetCreatedST : TDateTimeEx;
begin
  if FCreated = nil then
    result := nil
  else
    result := FCreated.value;
end;

Procedure TFhirDocumentManifest.SetCreatedST(value : TDateTimeEx);
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
    result := FSource.value;
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
    result := TFhirDocumentReferenceStatus(StringArrayIndexOfSensitive(CODES_TFhirDocumentReferenceStatus, FStatus.value));
end;

Procedure TFhirDocumentManifest.SetStatusST(value : TFhirDocumentReferenceStatus);
begin
  if ord(value) = 0 then
    StatusObject := nil
  else
    StatusObject := TFhirEnum.create(CODES_TFhirDocumentReferenceStatus[value]);
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
    result := FDescription.value;
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
  policyManagerObject := TFhirDocumentReference(oSource).policyManagerObject.Clone;
  authenticator := TFhirDocumentReference(oSource).authenticator.Clone;
  createdObject := TFhirDocumentReference(oSource).createdObject.Clone;
  indexedObject := TFhirDocumentReference(oSource).indexedObject.Clone;
  FStatus := TFhirDocumentReference(oSource).FStatus.Link;
  docStatus := TFhirDocumentReference(oSource).docStatus.Clone;
  FRelatesToList.Assign(TFhirDocumentReference(oSource).FRelatesToList);
  descriptionObject := TFhirDocumentReference(oSource).descriptionObject.Clone;
  FConfidentialityList.Assign(TFhirDocumentReference(oSource).FConfidentialityList);
  primaryLanguageObject := TFhirDocumentReference(oSource).primaryLanguageObject.Clone;
  mimeTypeObject := TFhirDocumentReference(oSource).mimeTypeObject.Clone;
  FFormatList.Assign(TFhirDocumentReference(oSource).FFormatList);
  sizeObject := TFhirDocumentReference(oSource).sizeObject.Clone;
  hashObject := TFhirDocumentReference(oSource).hashObject.Clone;
  locationObject := TFhirDocumentReference(oSource).locationObject.Clone;
  service := TFhirDocumentReference(oSource).service.Clone;
  context := TFhirDocumentReference(oSource).context.Clone;
end;

procedure TFhirDocumentReference.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'masterIdentifier') Then
     list.add(FMasterIdentifier.Link);
  if (child_name = 'identifier') Then
     list.addAll(FIdentifierList);
  if (child_name = 'subject') Then
     list.add(FSubject.Link);
  if (child_name = 'type') Then
     list.add(FType_.Link);
  if (child_name = 'class') Then
     list.add(FClass_.Link);
  if (child_name = 'author') Then
     list.addAll(FAuthorList);
  if (child_name = 'custodian') Then
     list.add(FCustodian.Link);
  if (child_name = 'policyManager') Then
     list.add(FPolicyManager.Link);
  if (child_name = 'authenticator') Then
     list.add(FAuthenticator.Link);
  if (child_name = 'created') Then
     list.add(FCreated.Link);
  if (child_name = 'indexed') Then
     list.add(FIndexed.Link);
  if (child_name = 'status') Then
     list.add(FStatus.Link);
  if (child_name = 'docStatus') Then
     list.add(FDocStatus.Link);
  if (child_name = 'relatesTo') Then
     list.addAll(FRelatesToList);
  if (child_name = 'description') Then
     list.add(FDescription.Link);
  if (child_name = 'confidentiality') Then
     list.addAll(FConfidentialityList);
  if (child_name = 'primaryLanguage') Then
     list.add(FPrimaryLanguage.Link);
  if (child_name = 'mimeType') Then
     list.add(FMimeType.Link);
  if (child_name = 'format') Then
     list.addAll(FFormatList);
  if (child_name = 'size') Then
     list.add(FSize.Link);
  if (child_name = 'hash') Then
     list.add(FHash.Link);
  if (child_name = 'location') Then
     list.add(FLocation.Link);
  if (child_name = 'service') Then
     list.add(FService.Link);
  if (child_name = 'context') Then
     list.add(FContext.Link);
end;

procedure TFhirDocumentReference.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'masterIdentifier', 'Identifier', FMasterIdentifier.Link));{2}
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'subject', 'Resource(Patient|Practitioner|Group|Device)', FSubject.Link));{2}
  oList.add(TFHIRProperty.create(self, 'type', 'CodeableConcept', FType_.Link));{2}
  oList.add(TFHIRProperty.create(self, 'class', 'CodeableConcept', FClass_.Link));{2}
  oList.add(TFHIRProperty.create(self, 'author', 'Resource(Practitioner|Device|Patient|RelatedPerson)', FAuthorList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'custodian', 'Resource(Organization)', FCustodian.Link));{2}
  oList.add(TFHIRProperty.create(self, 'policyManager', 'uri', FPolicyManager.Link));{2}
  oList.add(TFHIRProperty.create(self, 'authenticator', 'Resource(Practitioner|Organization)', FAuthenticator.Link));{2}
  oList.add(TFHIRProperty.create(self, 'created', 'dateTime', FCreated.Link));{2}
  oList.add(TFHIRProperty.create(self, 'indexed', 'instant', FIndexed.Link));{2}
  oList.add(TFHIRProperty.create(self, 'status', 'code', FStatus.Link));{1}
  oList.add(TFHIRProperty.create(self, 'docStatus', 'CodeableConcept', FDocStatus.Link));{2}
  oList.add(TFHIRProperty.create(self, 'relatesTo', '', FRelatesToList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'description', 'string', FDescription.Link));{2}
  oList.add(TFHIRProperty.create(self, 'confidentiality', 'CodeableConcept', FConfidentialityList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'primaryLanguage', 'code', FPrimaryLanguage.Link));{2}
  oList.add(TFHIRProperty.create(self, 'mimeType', 'code', FMimeType.Link));{2}
  oList.add(TFHIRProperty.create(self, 'format', 'uri', FFormatList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'size', 'integer', FSize.Link));{2}
  oList.add(TFHIRProperty.create(self, 'hash', 'string', FHash.Link));{2}
  oList.add(TFHIRProperty.create(self, 'location', 'uri', FLocation.Link));{2}
  oList.add(TFHIRProperty.create(self, 'service', '', FService.Link));{2}
  oList.add(TFHIRProperty.create(self, 'context', '', FContext.Link));{2}
end;

procedure TFhirDocumentReference.setProperty(propName : string; propValue: TFHIRObject);
begin
  if (propName = 'masterIdentifier') then MasterIdentifier := propValue as TFhirIdentifier{4b}
  else if (propName = 'identifier') then IdentifierList.add(propValue as TFhirIdentifier){2}
  else if (propName = 'subject') then Subject := propValue as TFhirResourceReference{Resource}{4b}
  else if (propName = 'type') then Type_ := propValue as TFhirCodeableConcept{4b}
  else if (propName = 'class') then Class_ := propValue as TFhirCodeableConcept{4b}
  else if (propName = 'author') then AuthorList.add(propValue as TFhirResourceReference{Resource}){2}
  else if (propName = 'custodian') then Custodian := propValue as TFhirResourceReference{TFhirOrganization}{4b}
  else if (propName = 'policyManager') then PolicyManagerObject := propValue as TFhirUri{5a}
  else if (propName = 'authenticator') then Authenticator := propValue as TFhirResourceReference{Resource}{4b}
  else if (propName = 'created') then CreatedObject := propValue as TFhirDateTime{5a}
  else if (propName = 'indexed') then IndexedObject := propValue as TFhirInstant{5a}
  else if (propName = 'status') then StatusObject := propValue as TFHIREnum
  else if (propName = 'docStatus') then DocStatus := propValue as TFhirCodeableConcept{4b}
  else if (propName = 'relatesTo') then RelatesToList.add(propValue as TFhirDocumentReferenceRelatesTo){2}
  else if (propName = 'description') then DescriptionObject := propValue as TFhirString{5a}
  else if (propName = 'confidentiality') then ConfidentialityList.add(propValue as TFhirCodeableConcept){2}
  else if (propName = 'primaryLanguage') then
    if propValue is TFHIRCode then
      PrimaryLanguageObject := propValue as TFhirCode{5}
    else if propValue is TFHIREnum then
      PrimaryLanguageObject := TFHIRCode.create(TFHIREnum(propValue).value)
    else
      raise Exception.Create('Type mismatch: cannot convert from "'+propValue.className+'" to "TFHIRCode"'){5a}
  else if (propName = 'mimeType') then
    if propValue is TFHIRCode then
      MimeTypeObject := propValue as TFhirCode{5}
    else if propValue is TFHIREnum then
      MimeTypeObject := TFHIRCode.create(TFHIREnum(propValue).value)
    else
      raise Exception.Create('Type mismatch: cannot convert from "'+propValue.className+'" to "TFHIRCode"'){5a}
  else if (propName = 'format') then FormatList.add(propValue as TFhirUri){2}
  else if (propName = 'size') then SizeObject := propValue as TFhirInteger{5a}
  else if (propName = 'hash') then HashObject := propValue as TFhirString{5a}
  else if (propName = 'location') then LocationObject := propValue as TFhirUri{5a}
  else if (propName = 'service') then Service := propValue as TFhirDocumentReferenceService{4b}
  else if (propName = 'context') then Context := propValue as TFhirDocumentReferenceContext{4b}
  else inherited;
end;

function TFhirDocumentReference.FhirType : string;
begin
  result := 'DocumentReference';
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
    result := FPolicyManager.value;
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

Function TFhirDocumentReference.GetCreatedST : TDateTimeEx;
begin
  if FCreated = nil then
    result := nil
  else
    result := FCreated.value;
end;

Procedure TFhirDocumentReference.SetCreatedST(value : TDateTimeEx);
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

Function TFhirDocumentReference.GetIndexedST : TDateTimeEx;
begin
  if FIndexed = nil then
    result := nil
  else
    result := FIndexed.value;
end;

Procedure TFhirDocumentReference.SetIndexedST(value : TDateTimeEx);
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
    result := TFhirDocumentReferenceStatus(StringArrayIndexOfSensitive(CODES_TFhirDocumentReferenceStatus, FStatus.value));
end;

Procedure TFhirDocumentReference.SetStatusST(value : TFhirDocumentReferenceStatus);
begin
  if ord(value) = 0 then
    StatusObject := nil
  else
    StatusObject := TFhirEnum.create(CODES_TFhirDocumentReferenceStatus[value]);
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
    result := FDescription.value;
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
    result := FPrimaryLanguage.value;
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
    result := FMimeType.value;
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
    result := FSize.value;
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
    result := FHash.value;
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
    result := FLocation.value;
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
  if (child_name = 'class') Then
     list.add(FClass_.Link);
  if (child_name = 'type') Then
     list.addAll(FType_List);
  if (child_name = 'subject') Then
     list.add(FSubject.Link);
  if (child_name = 'participant') Then
     list.addAll(FParticipantList);
  if (child_name = 'period') Then
     list.add(FPeriod.Link);
  if (child_name = 'length') Then
     list.add(FLength.Link);
  if (child_name = 'reason') Then
     list.add(FReason.Link);
  if (child_name = 'indication') Then
     list.add(FIndication.Link);
  if (child_name = 'priority') Then
     list.add(FPriority.Link);
  if (child_name = 'hospitalization') Then
     list.add(FHospitalization.Link);
  if (child_name = 'location') Then
     list.addAll(FLocationList);
  if (child_name = 'serviceProvider') Then
     list.add(FServiceProvider.Link);
  if (child_name = 'partOf') Then
     list.add(FPartOf.Link);
end;

procedure TFhirEncounter.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'status', 'code', FStatus.Link));{1}
  oList.add(TFHIRProperty.create(self, 'class', 'code', FClass_.Link));{1}
  oList.add(TFHIRProperty.create(self, 'type', 'CodeableConcept', FType_List.Link)){3};
  oList.add(TFHIRProperty.create(self, 'subject', 'Resource(Patient)', FSubject.Link));{2}
  oList.add(TFHIRProperty.create(self, 'participant', '', FParticipantList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'period', 'Period', FPeriod.Link));{2}
  oList.add(TFHIRProperty.create(self, 'length', 'Duration', FLength.Link));{2}
  oList.add(TFHIRProperty.create(self, 'reason', 'CodeableConcept', FReason.Link));{2}
  oList.add(TFHIRProperty.create(self, 'indication', 'Resource(Any)', FIndication.Link));{2}
  oList.add(TFHIRProperty.create(self, 'priority', 'CodeableConcept', FPriority.Link));{2}
  oList.add(TFHIRProperty.create(self, 'hospitalization', '', FHospitalization.Link));{2}
  oList.add(TFHIRProperty.create(self, 'location', '', FLocationList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'serviceProvider', 'Resource(Organization)', FServiceProvider.Link));{2}
  oList.add(TFHIRProperty.create(self, 'partOf', 'Resource(Encounter)', FPartOf.Link));{2}
end;

procedure TFhirEncounter.setProperty(propName : string; propValue: TFHIRObject);
begin
  if (propName = 'identifier') then IdentifierList.add(propValue as TFhirIdentifier){2}
  else if (propName = 'status') then StatusObject := propValue as TFHIREnum
  else if (propName = 'class') then Class_Object := propValue as TFHIREnum
  else if (propName = 'type') then Type_List.add(propValue as TFhirCodeableConcept){2}
  else if (propName = 'subject') then Subject := propValue as TFhirResourceReference{TFhirPatient}{4b}
  else if (propName = 'participant') then ParticipantList.add(propValue as TFhirEncounterParticipant){2}
  else if (propName = 'period') then Period := propValue as TFhirPeriod{4b}
  else if (propName = 'length') then Length := propValue as TFhirQuantity{4b}
  else if (propName = 'reason') then Reason := propValue as TFhirCodeableConcept{4b}
  else if (propName = 'indication') then Indication := propValue as TFhirResourceReference{Resource}{4b}
  else if (propName = 'priority') then Priority := propValue as TFhirCodeableConcept{4b}
  else if (propName = 'hospitalization') then Hospitalization := propValue as TFhirEncounterHospitalization{4b}
  else if (propName = 'location') then LocationList.add(propValue as TFhirEncounterLocation){2}
  else if (propName = 'serviceProvider') then ServiceProvider := propValue as TFhirResourceReference{TFhirOrganization}{4b}
  else if (propName = 'partOf') then PartOf := propValue as TFhirResourceReference{TFhirEncounter}{4b}
  else inherited;
end;

function TFhirEncounter.FhirType : string;
begin
  result := 'Encounter';
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
    result := TFhirEncounterState(StringArrayIndexOfSensitive(CODES_TFhirEncounterState, FStatus.value));
end;

Procedure TFhirEncounter.SetStatusST(value : TFhirEncounterState);
begin
  if ord(value) = 0 then
    StatusObject := nil
  else
    StatusObject := TFhirEnum.create(CODES_TFhirEncounterState[value]);
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
    result := TFhirEncounterClass(StringArrayIndexOfSensitive(CODES_TFhirEncounterClass, FClass_.value));
end;

Procedure TFhirEncounter.SetClass_ST(value : TFhirEncounterClass);
begin
  if ord(value) = 0 then
    Class_Object := nil
  else
    Class_Object := TFhirEnum.create(CODES_TFhirEncounterClass[value]);
end;

Procedure TFhirEncounter.SetSubject(value : TFhirResourceReference{TFhirPatient});
begin
  FSubject.free;
  FSubject := value;
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
  noteObject := TFhirFamilyHistory(oSource).noteObject.Clone;
  FRelationList.Assign(TFhirFamilyHistory(oSource).FRelationList);
end;

procedure TFhirFamilyHistory.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'identifier') Then
     list.addAll(FIdentifierList);
  if (child_name = 'subject') Then
     list.add(FSubject.Link);
  if (child_name = 'note') Then
     list.add(FNote.Link);
  if (child_name = 'relation') Then
     list.addAll(FRelationList);
end;

procedure TFhirFamilyHistory.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'subject', 'Resource(Patient)', FSubject.Link));{2}
  oList.add(TFHIRProperty.create(self, 'note', 'string', FNote.Link));{2}
  oList.add(TFHIRProperty.create(self, 'relation', '', FRelationList.Link)){3};
end;

procedure TFhirFamilyHistory.setProperty(propName : string; propValue: TFHIRObject);
begin
  if (propName = 'identifier') then IdentifierList.add(propValue as TFhirIdentifier){2}
  else if (propName = 'subject') then Subject := propValue as TFhirResourceReference{TFhirPatient}{4b}
  else if (propName = 'note') then NoteObject := propValue as TFhirString{5a}
  else if (propName = 'relation') then RelationList.add(propValue as TFhirFamilyHistoryRelation){2}
  else inherited;
end;

function TFhirFamilyHistory.FhirType : string;
begin
  result := 'FamilyHistory';
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
    result := FNote.value;
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
  actualObject := TFhirGroup(oSource).actualObject.Clone;
  code := TFhirGroup(oSource).code.Clone;
  nameObject := TFhirGroup(oSource).nameObject.Clone;
  quantityObject := TFhirGroup(oSource).quantityObject.Clone;
  FCharacteristicList.Assign(TFhirGroup(oSource).FCharacteristicList);
  FMemberList.Assign(TFhirGroup(oSource).FMemberList);
end;

procedure TFhirGroup.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'identifier') Then
     list.add(FIdentifier.Link);
  if (child_name = 'type') Then
     list.add(FType_.Link);
  if (child_name = 'actual') Then
     list.add(FActual.Link);
  if (child_name = 'code') Then
     list.add(FCode.Link);
  if (child_name = 'name') Then
     list.add(FName.Link);
  if (child_name = 'quantity') Then
     list.add(FQuantity.Link);
  if (child_name = 'characteristic') Then
     list.addAll(FCharacteristicList);
  if (child_name = 'member') Then
     list.addAll(FMemberList);
end;

procedure TFhirGroup.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifier.Link));{2}
  oList.add(TFHIRProperty.create(self, 'type', 'code', FType_.Link));{1}
  oList.add(TFHIRProperty.create(self, 'actual', 'boolean', FActual.Link));{2}
  oList.add(TFHIRProperty.create(self, 'code', 'CodeableConcept', FCode.Link));{2}
  oList.add(TFHIRProperty.create(self, 'name', 'string', FName.Link));{2}
  oList.add(TFHIRProperty.create(self, 'quantity', 'integer', FQuantity.Link));{2}
  oList.add(TFHIRProperty.create(self, 'characteristic', '', FCharacteristicList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'member', 'Resource(Patient|Practitioner|Device|Medication|Substance)', FMemberList.Link)){3};
end;

procedure TFhirGroup.setProperty(propName : string; propValue: TFHIRObject);
begin
  if (propName = 'identifier') then Identifier := propValue as TFhirIdentifier{4b}
  else if (propName = 'type') then Type_Object := propValue as TFHIREnum
  else if (propName = 'actual') then ActualObject := propValue as TFhirBoolean{5a}
  else if (propName = 'code') then Code := propValue as TFhirCodeableConcept{4b}
  else if (propName = 'name') then NameObject := propValue as TFhirString{5a}
  else if (propName = 'quantity') then QuantityObject := propValue as TFhirInteger{5a}
  else if (propName = 'characteristic') then CharacteristicList.add(propValue as TFhirGroupCharacteristic){2}
  else if (propName = 'member') then MemberList.add(propValue as TFhirResourceReference{Resource}){2}
  else inherited;
end;

function TFhirGroup.FhirType : string;
begin
  result := 'Group';
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
    result := TFhirGroupType(StringArrayIndexOfSensitive(CODES_TFhirGroupType, FType_.value));
end;

Procedure TFhirGroup.SetType_ST(value : TFhirGroupType);
begin
  if ord(value) = 0 then
    Type_Object := nil
  else
    Type_Object := TFhirEnum.create(CODES_TFhirGroupType[value]);
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
    result := FActual.value;
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
    result := FName.value;
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
    result := FQuantity.value;
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
  dateTimeObject := TFhirImagingStudy(oSource).dateTimeObject.Clone;
  subject := TFhirImagingStudy(oSource).subject.Clone;
  uidObject := TFhirImagingStudy(oSource).uidObject.Clone;
  accessionNo := TFhirImagingStudy(oSource).accessionNo.Clone;
  FIdentifierList.Assign(TFhirImagingStudy(oSource).FIdentifierList);
  FOrderList.Assign(TFhirImagingStudy(oSource).FOrderList);
  FModality.Assign(TFhirImagingStudy(oSource).FModality);
  referrer := TFhirImagingStudy(oSource).referrer.Clone;
  FAvailability := TFhirImagingStudy(oSource).FAvailability.Link;
  urlObject := TFhirImagingStudy(oSource).urlObject.Clone;
  numberOfSeriesObject := TFhirImagingStudy(oSource).numberOfSeriesObject.Clone;
  numberOfInstancesObject := TFhirImagingStudy(oSource).numberOfInstancesObject.Clone;
  clinicalInformationObject := TFhirImagingStudy(oSource).clinicalInformationObject.Clone;
  FProcedure_List.Assign(TFhirImagingStudy(oSource).FProcedure_List);
  interpreter := TFhirImagingStudy(oSource).interpreter.Clone;
  descriptionObject := TFhirImagingStudy(oSource).descriptionObject.Clone;
  FSeriesList.Assign(TFhirImagingStudy(oSource).FSeriesList);
end;

procedure TFhirImagingStudy.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'dateTime') Then
     list.add(FDateTime.Link);
  if (child_name = 'subject') Then
     list.add(FSubject.Link);
  if (child_name = 'uid') Then
     list.add(FUid.Link);
  if (child_name = 'accessionNo') Then
     list.add(FAccessionNo.Link);
  if (child_name = 'identifier') Then
     list.addAll(FIdentifierList);
  if (child_name = 'order') Then
     list.addAll(FOrderList);
  if (child_name = 'modality') Then
     list.addAll(FModality);
  if (child_name = 'referrer') Then
     list.add(FReferrer.Link);
  if (child_name = 'availability') Then
     list.add(FAvailability.Link);
  if (child_name = 'url') Then
     list.add(FUrl.Link);
  if (child_name = 'numberOfSeries') Then
     list.add(FNumberOfSeries.Link);
  if (child_name = 'numberOfInstances') Then
     list.add(FNumberOfInstances.Link);
  if (child_name = 'clinicalInformation') Then
     list.add(FClinicalInformation.Link);
  if (child_name = 'procedure') Then
     list.addAll(FProcedure_List);
  if (child_name = 'interpreter') Then
     list.add(FInterpreter.Link);
  if (child_name = 'description') Then
     list.add(FDescription.Link);
  if (child_name = 'series') Then
     list.addAll(FSeriesList);
end;

procedure TFhirImagingStudy.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'dateTime', 'dateTime', FDateTime.Link));{2}
  oList.add(TFHIRProperty.create(self, 'subject', 'Resource(Patient)', FSubject.Link));{2}
  oList.add(TFHIRProperty.create(self, 'uid', 'oid', FUid.Link));{2}
  oList.add(TFHIRProperty.create(self, 'accessionNo', 'Identifier', FAccessionNo.Link));{2}
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'order', 'Resource(DiagnosticOrder)', FOrderList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'modality', 'code', FModality.Link)){3};
  oList.add(TFHIRProperty.create(self, 'referrer', 'Resource(Practitioner)', FReferrer.Link));{2}
  oList.add(TFHIRProperty.create(self, 'availability', 'code', FAvailability.Link));{1}
  oList.add(TFHIRProperty.create(self, 'url', 'uri', FUrl.Link));{2}
  oList.add(TFHIRProperty.create(self, 'numberOfSeries', 'integer', FNumberOfSeries.Link));{2}
  oList.add(TFHIRProperty.create(self, 'numberOfInstances', 'integer', FNumberOfInstances.Link));{2}
  oList.add(TFHIRProperty.create(self, 'clinicalInformation', 'string', FClinicalInformation.Link));{2}
  oList.add(TFHIRProperty.create(self, 'procedure', 'Coding', FProcedure_List.Link)){3};
  oList.add(TFHIRProperty.create(self, 'interpreter', 'Resource(Practitioner)', FInterpreter.Link));{2}
  oList.add(TFHIRProperty.create(self, 'description', 'string', FDescription.Link));{2}
  oList.add(TFHIRProperty.create(self, 'series', '', FSeriesList.Link)){3};
end;

procedure TFhirImagingStudy.setProperty(propName : string; propValue: TFHIRObject);
begin
  if (propName = 'dateTime') then DateTimeObject := propValue as TFhirDateTime{5a}
  else if (propName = 'subject') then Subject := propValue as TFhirResourceReference{TFhirPatient}{4b}
  else if (propName = 'uid') then UidObject := propValue as TFhirOid{5a}
  else if (propName = 'accessionNo') then AccessionNo := propValue as TFhirIdentifier{4b}
  else if (propName = 'identifier') then IdentifierList.add(propValue as TFhirIdentifier){2}
  else if (propName = 'order') then OrderList.add(propValue as TFhirResourceReference{TFhirDiagnosticOrder}){2}
  else if (propName = 'modality') then FModality.add(propValue as TFHIREnum) {1}
  else if (propName = 'referrer') then Referrer := propValue as TFhirResourceReference{TFhirPractitioner}{4b}
  else if (propName = 'availability') then AvailabilityObject := propValue as TFHIREnum
  else if (propName = 'url') then UrlObject := propValue as TFhirUri{5a}
  else if (propName = 'numberOfSeries') then NumberOfSeriesObject := propValue as TFhirInteger{5a}
  else if (propName = 'numberOfInstances') then NumberOfInstancesObject := propValue as TFhirInteger{5a}
  else if (propName = 'clinicalInformation') then ClinicalInformationObject := propValue as TFhirString{5a}
  else if (propName = 'procedure') then Procedure_List.add(propValue as TFhirCoding){2}
  else if (propName = 'interpreter') then Interpreter := propValue as TFhirResourceReference{TFhirPractitioner}{4b}
  else if (propName = 'description') then DescriptionObject := propValue as TFhirString{5a}
  else if (propName = 'series') then SeriesList.add(propValue as TFhirImagingStudySeries){2}
  else inherited;
end;

function TFhirImagingStudy.FhirType : string;
begin
  result := 'ImagingStudy';
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

Function TFhirImagingStudy.GetDateTimeST : TDateTimeEx;
begin
  if FDateTime = nil then
    result := nil
  else
    result := FDateTime.value;
end;

Procedure TFhirImagingStudy.SetDateTimeST(value : TDateTimeEx);
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
    result := FUid.value;
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
    result := TFhirInstanceAvailability(StringArrayIndexOfSensitive(CODES_TFhirInstanceAvailability, FAvailability.value));
end;

Procedure TFhirImagingStudy.SetAvailabilityST(value : TFhirInstanceAvailability);
begin
  if ord(value) = 0 then
    AvailabilityObject := nil
  else
    AvailabilityObject := TFhirEnum.create(CODES_TFhirInstanceAvailability[value]);
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
    result := FUrl.value;
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
    result := FNumberOfSeries.value;
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
    result := FNumberOfInstances.value;
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
    result := FClinicalInformation.value;
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
    result := FDescription.value;
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
  dateObject := TFhirImmunization(oSource).dateObject.Clone;
  vaccineType := TFhirImmunization(oSource).vaccineType.Clone;
  subject := TFhirImmunization(oSource).subject.Clone;
  refusedIndicatorObject := TFhirImmunization(oSource).refusedIndicatorObject.Clone;
  reportedObject := TFhirImmunization(oSource).reportedObject.Clone;
  performer := TFhirImmunization(oSource).performer.Clone;
  requester := TFhirImmunization(oSource).requester.Clone;
  manufacturer := TFhirImmunization(oSource).manufacturer.Clone;
  location := TFhirImmunization(oSource).location.Clone;
  lotNumberObject := TFhirImmunization(oSource).lotNumberObject.Clone;
  expirationDateObject := TFhirImmunization(oSource).expirationDateObject.Clone;
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
     list.add(FDate.Link);
  if (child_name = 'vaccineType') Then
     list.add(FVaccineType.Link);
  if (child_name = 'subject') Then
     list.add(FSubject.Link);
  if (child_name = 'refusedIndicator') Then
     list.add(FRefusedIndicator.Link);
  if (child_name = 'reported') Then
     list.add(FReported.Link);
  if (child_name = 'performer') Then
     list.add(FPerformer.Link);
  if (child_name = 'requester') Then
     list.add(FRequester.Link);
  if (child_name = 'manufacturer') Then
     list.add(FManufacturer.Link);
  if (child_name = 'location') Then
     list.add(FLocation.Link);
  if (child_name = 'lotNumber') Then
     list.add(FLotNumber.Link);
  if (child_name = 'expirationDate') Then
     list.add(FExpirationDate.Link);
  if (child_name = 'site') Then
     list.add(FSite.Link);
  if (child_name = 'route') Then
     list.add(FRoute.Link);
  if (child_name = 'doseQuantity') Then
     list.add(FDoseQuantity.Link);
  if (child_name = 'explanation') Then
     list.add(FExplanation.Link);
  if (child_name = 'reaction') Then
     list.addAll(FReactionList);
  if (child_name = 'vaccinationProtocol') Then
     list.addAll(FVaccinationProtocolList);
end;

procedure TFhirImmunization.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'date', 'dateTime', FDate.Link));{2}
  oList.add(TFHIRProperty.create(self, 'vaccineType', 'CodeableConcept', FVaccineType.Link));{2}
  oList.add(TFHIRProperty.create(self, 'subject', 'Resource(Patient)', FSubject.Link));{2}
  oList.add(TFHIRProperty.create(self, 'refusedIndicator', 'boolean', FRefusedIndicator.Link));{2}
  oList.add(TFHIRProperty.create(self, 'reported', 'boolean', FReported.Link));{2}
  oList.add(TFHIRProperty.create(self, 'performer', 'Resource(Practitioner)', FPerformer.Link));{2}
  oList.add(TFHIRProperty.create(self, 'requester', 'Resource(Practitioner)', FRequester.Link));{2}
  oList.add(TFHIRProperty.create(self, 'manufacturer', 'Resource(Organization)', FManufacturer.Link));{2}
  oList.add(TFHIRProperty.create(self, 'location', 'Resource(Location)', FLocation.Link));{2}
  oList.add(TFHIRProperty.create(self, 'lotNumber', 'string', FLotNumber.Link));{2}
  oList.add(TFHIRProperty.create(self, 'expirationDate', 'date', FExpirationDate.Link));{2}
  oList.add(TFHIRProperty.create(self, 'site', 'CodeableConcept', FSite.Link));{2}
  oList.add(TFHIRProperty.create(self, 'route', 'CodeableConcept', FRoute.Link));{2}
  oList.add(TFHIRProperty.create(self, 'doseQuantity', 'Quantity', FDoseQuantity.Link));{2}
  oList.add(TFHIRProperty.create(self, 'explanation', '', FExplanation.Link));{2}
  oList.add(TFHIRProperty.create(self, 'reaction', '', FReactionList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'vaccinationProtocol', '', FVaccinationProtocolList.Link)){3};
end;

procedure TFhirImmunization.setProperty(propName : string; propValue: TFHIRObject);
begin
  if (propName = 'identifier') then IdentifierList.add(propValue as TFhirIdentifier){2}
  else if (propName = 'date') then DateObject := propValue as TFhirDateTime{5a}
  else if (propName = 'vaccineType') then VaccineType := propValue as TFhirCodeableConcept{4b}
  else if (propName = 'subject') then Subject := propValue as TFhirResourceReference{TFhirPatient}{4b}
  else if (propName = 'refusedIndicator') then RefusedIndicatorObject := propValue as TFhirBoolean{5a}
  else if (propName = 'reported') then ReportedObject := propValue as TFhirBoolean{5a}
  else if (propName = 'performer') then Performer := propValue as TFhirResourceReference{TFhirPractitioner}{4b}
  else if (propName = 'requester') then Requester := propValue as TFhirResourceReference{TFhirPractitioner}{4b}
  else if (propName = 'manufacturer') then Manufacturer := propValue as TFhirResourceReference{TFhirOrganization}{4b}
  else if (propName = 'location') then Location := propValue as TFhirResourceReference{TFhirLocation}{4b}
  else if (propName = 'lotNumber') then LotNumberObject := propValue as TFhirString{5a}
  else if (propName = 'expirationDate') then ExpirationDateObject := propValue as TFhirDate{5a}
  else if (propName = 'site') then Site := propValue as TFhirCodeableConcept{4b}
  else if (propName = 'route') then Route := propValue as TFhirCodeableConcept{4b}
  else if (propName = 'doseQuantity') then DoseQuantity := propValue as TFhirQuantity{4b}
  else if (propName = 'explanation') then Explanation := propValue as TFhirImmunizationExplanation{4b}
  else if (propName = 'reaction') then ReactionList.add(propValue as TFhirImmunizationReaction){2}
  else if (propName = 'vaccinationProtocol') then VaccinationProtocolList.add(propValue as TFhirImmunizationVaccinationProtocol){2}
  else inherited;
end;

function TFhirImmunization.FhirType : string;
begin
  result := 'Immunization';
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

Function TFhirImmunization.GetDateST : TDateTimeEx;
begin
  if FDate = nil then
    result := nil
  else
    result := FDate.value;
end;

Procedure TFhirImmunization.SetDateST(value : TDateTimeEx);
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
    result := FRefusedIndicator.value;
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
    result := FReported.value;
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
    result := FLotNumber.value;
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

Function TFhirImmunization.GetExpirationDateST : TDateTimeEx;
begin
  if FExpirationDate = nil then
    result := nil
  else
    result := FExpirationDate.value;
end;

Procedure TFhirImmunization.SetExpirationDateST(value : TDateTimeEx);
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
     list.add(FSubject.Link);
  if (child_name = 'recommendation') Then
     list.addAll(FRecommendationList);
end;

procedure TFhirImmunizationRecommendation.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'subject', 'Resource(Patient)', FSubject.Link));{2}
  oList.add(TFHIRProperty.create(self, 'recommendation', '', FRecommendationList.Link)){3};
end;

procedure TFhirImmunizationRecommendation.setProperty(propName : string; propValue: TFHIRObject);
begin
  if (propName = 'identifier') then IdentifierList.add(propValue as TFhirIdentifier){2}
  else if (propName = 'subject') then Subject := propValue as TFhirResourceReference{TFhirPatient}{4b}
  else if (propName = 'recommendation') then RecommendationList.add(propValue as TFhirImmunizationRecommendationRecommendation){2}
  else inherited;
end;

function TFhirImmunizationRecommendation.FhirType : string;
begin
  result := 'ImmunizationRecommendation';
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
  dateObject := TFhirList(oSource).dateObject.Clone;
  orderedObject := TFhirList(oSource).orderedObject.Clone;
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
     list.add(FCode.Link);
  if (child_name = 'subject') Then
     list.add(FSubject.Link);
  if (child_name = 'source') Then
     list.add(FSource.Link);
  if (child_name = 'date') Then
     list.add(FDate.Link);
  if (child_name = 'ordered') Then
     list.add(FOrdered.Link);
  if (child_name = 'mode') Then
     list.add(FMode.Link);
  if (child_name = 'entry') Then
     list.addAll(FEntryList);
  if (child_name = 'emptyReason') Then
     list.add(FEmptyReason.Link);
end;

procedure TFhirList.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'code', 'CodeableConcept', FCode.Link));{2}
  oList.add(TFHIRProperty.create(self, 'subject', 'Resource(Patient|Group|Device|Location)', FSubject.Link));{2}
  oList.add(TFHIRProperty.create(self, 'source', 'Resource(Practitioner|Patient|Device)', FSource.Link));{2}
  oList.add(TFHIRProperty.create(self, 'date', 'dateTime', FDate.Link));{2}
  oList.add(TFHIRProperty.create(self, 'ordered', 'boolean', FOrdered.Link));{2}
  oList.add(TFHIRProperty.create(self, 'mode', 'code', FMode.Link));{1}
  oList.add(TFHIRProperty.create(self, 'entry', '', FEntryList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'emptyReason', 'CodeableConcept', FEmptyReason.Link));{2}
end;

procedure TFhirList.setProperty(propName : string; propValue: TFHIRObject);
begin
  if (propName = 'identifier') then IdentifierList.add(propValue as TFhirIdentifier){2}
  else if (propName = 'code') then Code := propValue as TFhirCodeableConcept{4b}
  else if (propName = 'subject') then Subject := propValue as TFhirResourceReference{Resource}{4b}
  else if (propName = 'source') then Source := propValue as TFhirResourceReference{Resource}{4b}
  else if (propName = 'date') then DateObject := propValue as TFhirDateTime{5a}
  else if (propName = 'ordered') then OrderedObject := propValue as TFhirBoolean{5a}
  else if (propName = 'mode') then ModeObject := propValue as TFHIREnum
  else if (propName = 'entry') then EntryList.add(propValue as TFhirListEntry){2}
  else if (propName = 'emptyReason') then EmptyReason := propValue as TFhirCodeableConcept{4b}
  else inherited;
end;

function TFhirList.FhirType : string;
begin
  result := 'List';
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

Function TFhirList.GetDateST : TDateTimeEx;
begin
  if FDate = nil then
    result := nil
  else
    result := FDate.value;
end;

Procedure TFhirList.SetDateST(value : TDateTimeEx);
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
    result := FOrdered.value;
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
    result := TFhirListMode(StringArrayIndexOfSensitive(CODES_TFhirListMode, FMode.value));
end;

Procedure TFhirList.SetModeST(value : TFhirListMode);
begin
  if ord(value) = 0 then
    ModeObject := nil
  else
    ModeObject := TFhirEnum.create(CODES_TFhirListMode[value]);
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
  FTelecomList := TFhirContactList.Create;
end;

destructor TFhirLocation.Destroy;
begin
  FIdentifier.free;
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
  identifier := TFhirLocation(oSource).identifier.Clone;
  nameObject := TFhirLocation(oSource).nameObject.Clone;
  descriptionObject := TFhirLocation(oSource).descriptionObject.Clone;
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
     list.add(FIdentifier.Link);
  if (child_name = 'name') Then
     list.add(FName.Link);
  if (child_name = 'description') Then
     list.add(FDescription.Link);
  if (child_name = 'type') Then
     list.add(FType_.Link);
  if (child_name = 'telecom') Then
     list.addAll(FTelecomList);
  if (child_name = 'address') Then
     list.add(FAddress.Link);
  if (child_name = 'physicalType') Then
     list.add(FPhysicalType.Link);
  if (child_name = 'position') Then
     list.add(FPosition.Link);
  if (child_name = 'managingOrganization') Then
     list.add(FManagingOrganization.Link);
  if (child_name = 'status') Then
     list.add(FStatus.Link);
  if (child_name = 'partOf') Then
     list.add(FPartOf.Link);
  if (child_name = 'mode') Then
     list.add(FMode.Link);
end;

procedure TFhirLocation.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifier.Link));{2}
  oList.add(TFHIRProperty.create(self, 'name', 'string', FName.Link));{2}
  oList.add(TFHIRProperty.create(self, 'description', 'string', FDescription.Link));{2}
  oList.add(TFHIRProperty.create(self, 'type', 'CodeableConcept', FType_.Link));{2}
  oList.add(TFHIRProperty.create(self, 'telecom', 'Contact', FTelecomList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'address', 'Address', FAddress.Link));{2}
  oList.add(TFHIRProperty.create(self, 'physicalType', 'CodeableConcept', FPhysicalType.Link));{2}
  oList.add(TFHIRProperty.create(self, 'position', '', FPosition.Link));{2}
  oList.add(TFHIRProperty.create(self, 'managingOrganization', 'Resource(Organization)', FManagingOrganization.Link));{2}
  oList.add(TFHIRProperty.create(self, 'status', 'code', FStatus.Link));{1}
  oList.add(TFHIRProperty.create(self, 'partOf', 'Resource(Location)', FPartOf.Link));{2}
  oList.add(TFHIRProperty.create(self, 'mode', 'code', FMode.Link));{1}
end;

procedure TFhirLocation.setProperty(propName : string; propValue: TFHIRObject);
begin
  if (propName = 'identifier') then Identifier := propValue as TFhirIdentifier{4b}
  else if (propName = 'name') then NameObject := propValue as TFhirString{5a}
  else if (propName = 'description') then DescriptionObject := propValue as TFhirString{5a}
  else if (propName = 'type') then Type_ := propValue as TFhirCodeableConcept{4b}
  else if (propName = 'telecom') then TelecomList.add(propValue as TFhirContact){2}
  else if (propName = 'address') then Address := propValue as TFhirAddress{4b}
  else if (propName = 'physicalType') then PhysicalType := propValue as TFhirCodeableConcept{4b}
  else if (propName = 'position') then Position := propValue as TFhirLocationPosition{4b}
  else if (propName = 'managingOrganization') then ManagingOrganization := propValue as TFhirResourceReference{TFhirOrganization}{4b}
  else if (propName = 'status') then StatusObject := propValue as TFHIREnum
  else if (propName = 'partOf') then PartOf := propValue as TFhirResourceReference{TFhirLocation}{4b}
  else if (propName = 'mode') then ModeObject := propValue as TFHIREnum
  else inherited;
end;

function TFhirLocation.FhirType : string;
begin
  result := 'Location';
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

Procedure TFhirLocation.SetIdentifier(value : TFhirIdentifier);
begin
  FIdentifier.free;
  FIdentifier := value;
end;

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
    result := FName.value;
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
    result := FDescription.value;
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
    result := TFhirLocationStatus(StringArrayIndexOfSensitive(CODES_TFhirLocationStatus, FStatus.value));
end;

Procedure TFhirLocation.SetStatusST(value : TFhirLocationStatus);
begin
  if ord(value) = 0 then
    StatusObject := nil
  else
    StatusObject := TFhirEnum.create(CODES_TFhirLocationStatus[value]);
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
    result := TFhirLocationMode(StringArrayIndexOfSensitive(CODES_TFhirLocationMode, FMode.value));
end;

Procedure TFhirLocation.SetModeST(value : TFhirLocationMode);
begin
  if ord(value) = 0 then
    ModeObject := nil
  else
    ModeObject := TFhirEnum.create(CODES_TFhirLocationMode[value]);
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
  dateTimeObject := TFhirMedia(oSource).dateTimeObject.Clone;
  subject := TFhirMedia(oSource).subject.Clone;
  operator := TFhirMedia(oSource).operator.Clone;
  view := TFhirMedia(oSource).view.Clone;
  deviceNameObject := TFhirMedia(oSource).deviceNameObject.Clone;
  heightObject := TFhirMedia(oSource).heightObject.Clone;
  widthObject := TFhirMedia(oSource).widthObject.Clone;
  framesObject := TFhirMedia(oSource).framesObject.Clone;
  lengthObject := TFhirMedia(oSource).lengthObject.Clone;
  content := TFhirMedia(oSource).content.Clone;
end;

procedure TFhirMedia.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'type') Then
     list.add(FType_.Link);
  if (child_name = 'subtype') Then
     list.add(FSubtype.Link);
  if (child_name = 'identifier') Then
     list.addAll(FIdentifierList);
  if (child_name = 'dateTime') Then
     list.add(FDateTime.Link);
  if (child_name = 'subject') Then
     list.add(FSubject.Link);
  if (child_name = 'operator') Then
     list.add(FOperator.Link);
  if (child_name = 'view') Then
     list.add(FView.Link);
  if (child_name = 'deviceName') Then
     list.add(FDeviceName.Link);
  if (child_name = 'height') Then
     list.add(FHeight.Link);
  if (child_name = 'width') Then
     list.add(FWidth.Link);
  if (child_name = 'frames') Then
     list.add(FFrames.Link);
  if (child_name = 'length') Then
     list.add(FLength.Link);
  if (child_name = 'content') Then
     list.add(FContent.Link);
end;

procedure TFhirMedia.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'type', 'code', FType_.Link));{1}
  oList.add(TFHIRProperty.create(self, 'subtype', 'CodeableConcept', FSubtype.Link));{2}
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'dateTime', 'dateTime', FDateTime.Link));{2}
  oList.add(TFHIRProperty.create(self, 'subject', 'Resource(Patient|Practitioner|Group|Device|Specimen)', FSubject.Link));{2}
  oList.add(TFHIRProperty.create(self, 'operator', 'Resource(Practitioner)', FOperator.Link));{2}
  oList.add(TFHIRProperty.create(self, 'view', 'CodeableConcept', FView.Link));{2}
  oList.add(TFHIRProperty.create(self, 'deviceName', 'string', FDeviceName.Link));{2}
  oList.add(TFHIRProperty.create(self, 'height', 'integer', FHeight.Link));{2}
  oList.add(TFHIRProperty.create(self, 'width', 'integer', FWidth.Link));{2}
  oList.add(TFHIRProperty.create(self, 'frames', 'integer', FFrames.Link));{2}
  oList.add(TFHIRProperty.create(self, 'length', 'integer', FLength.Link));{2}
  oList.add(TFHIRProperty.create(self, 'content', 'Attachment', FContent.Link));{2}
end;

procedure TFhirMedia.setProperty(propName : string; propValue: TFHIRObject);
begin
  if (propName = 'type') then Type_Object := propValue as TFHIREnum
  else if (propName = 'subtype') then Subtype := propValue as TFhirCodeableConcept{4b}
  else if (propName = 'identifier') then IdentifierList.add(propValue as TFhirIdentifier){2}
  else if (propName = 'dateTime') then DateTimeObject := propValue as TFhirDateTime{5a}
  else if (propName = 'subject') then Subject := propValue as TFhirResourceReference{Resource}{4b}
  else if (propName = 'operator') then Operator := propValue as TFhirResourceReference{TFhirPractitioner}{4b}
  else if (propName = 'view') then View := propValue as TFhirCodeableConcept{4b}
  else if (propName = 'deviceName') then DeviceNameObject := propValue as TFhirString{5a}
  else if (propName = 'height') then HeightObject := propValue as TFhirInteger{5a}
  else if (propName = 'width') then WidthObject := propValue as TFhirInteger{5a}
  else if (propName = 'frames') then FramesObject := propValue as TFhirInteger{5a}
  else if (propName = 'length') then LengthObject := propValue as TFhirInteger{5a}
  else if (propName = 'content') then Content := propValue as TFhirAttachment{4b}
  else inherited;
end;

function TFhirMedia.FhirType : string;
begin
  result := 'Media';
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
    result := TFhirMediaType(StringArrayIndexOfSensitive(CODES_TFhirMediaType, FType_.value));
end;

Procedure TFhirMedia.SetType_ST(value : TFhirMediaType);
begin
  if ord(value) = 0 then
    Type_Object := nil
  else
    Type_Object := TFhirEnum.create(CODES_TFhirMediaType[value]);
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

Function TFhirMedia.GetDateTimeST : TDateTimeEx;
begin
  if FDateTime = nil then
    result := nil
  else
    result := FDateTime.value;
end;

Procedure TFhirMedia.SetDateTimeST(value : TDateTimeEx);
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
    result := FDeviceName.value;
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
    result := FHeight.value;
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
    result := FWidth.value;
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
    result := FFrames.value;
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
    result := FLength.value;
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
  nameObject := TFhirMedication(oSource).nameObject.Clone;
  code := TFhirMedication(oSource).code.Clone;
  isBrandObject := TFhirMedication(oSource).isBrandObject.Clone;
  manufacturer := TFhirMedication(oSource).manufacturer.Clone;
  FKind := TFhirMedication(oSource).FKind.Link;
  product := TFhirMedication(oSource).product.Clone;
  package := TFhirMedication(oSource).package.Clone;
end;

procedure TFhirMedication.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'name') Then
     list.add(FName.Link);
  if (child_name = 'code') Then
     list.add(FCode.Link);
  if (child_name = 'isBrand') Then
     list.add(FIsBrand.Link);
  if (child_name = 'manufacturer') Then
     list.add(FManufacturer.Link);
  if (child_name = 'kind') Then
     list.add(FKind.Link);
  if (child_name = 'product') Then
     list.add(FProduct.Link);
  if (child_name = 'package') Then
     list.add(FPackage.Link);
end;

procedure TFhirMedication.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'name', 'string', FName.Link));{2}
  oList.add(TFHIRProperty.create(self, 'code', 'CodeableConcept', FCode.Link));{2}
  oList.add(TFHIRProperty.create(self, 'isBrand', 'boolean', FIsBrand.Link));{2}
  oList.add(TFHIRProperty.create(self, 'manufacturer', 'Resource(Organization)', FManufacturer.Link));{2}
  oList.add(TFHIRProperty.create(self, 'kind', 'code', FKind.Link));{1}
  oList.add(TFHIRProperty.create(self, 'product', '', FProduct.Link));{2}
  oList.add(TFHIRProperty.create(self, 'package', '', FPackage.Link));{2}
end;

procedure TFhirMedication.setProperty(propName : string; propValue: TFHIRObject);
begin
  if (propName = 'name') then NameObject := propValue as TFhirString{5a}
  else if (propName = 'code') then Code := propValue as TFhirCodeableConcept{4b}
  else if (propName = 'isBrand') then IsBrandObject := propValue as TFhirBoolean{5a}
  else if (propName = 'manufacturer') then Manufacturer := propValue as TFhirResourceReference{TFhirOrganization}{4b}
  else if (propName = 'kind') then KindObject := propValue as TFHIREnum
  else if (propName = 'product') then Product := propValue as TFhirMedicationProduct{4b}
  else if (propName = 'package') then Package := propValue as TFhirMedicationPackage{4b}
  else inherited;
end;

function TFhirMedication.FhirType : string;
begin
  result := 'Medication';
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
    result := FName.value;
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
    result := FIsBrand.value;
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
    result := TFhirMedicationKind(StringArrayIndexOfSensitive(CODES_TFhirMedicationKind, FKind.value));
end;

Procedure TFhirMedication.SetKindST(value : TFhirMedicationKind);
begin
  if ord(value) = 0 then
    KindObject := nil
  else
    KindObject := TFhirEnum.create(CODES_TFhirMedicationKind[value]);
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
  wasNotGivenObject := TFhirMedicationAdministration(oSource).wasNotGivenObject.Clone;
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
     list.add(FPatient.Link);
  if (child_name = 'practitioner') Then
     list.add(FPractitioner.Link);
  if (child_name = 'encounter') Then
     list.add(FEncounter.Link);
  if (child_name = 'prescription') Then
     list.add(FPrescription.Link);
  if (child_name = 'wasNotGiven') Then
     list.add(FWasNotGiven.Link);
  if (child_name = 'reasonNotGiven') Then
     list.addAll(FReasonNotGivenList);
  if (child_name = 'whenGiven') Then
     list.add(FWhenGiven.Link);
  if (child_name = 'medication') Then
     list.add(FMedication.Link);
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
  oList.add(TFHIRProperty.create(self, 'patient', 'Resource(Patient)', FPatient.Link));{2}
  oList.add(TFHIRProperty.create(self, 'practitioner', 'Resource(Practitioner)', FPractitioner.Link));{2}
  oList.add(TFHIRProperty.create(self, 'encounter', 'Resource(Encounter)', FEncounter.Link));{2}
  oList.add(TFHIRProperty.create(self, 'prescription', 'Resource(MedicationPrescription)', FPrescription.Link));{2}
  oList.add(TFHIRProperty.create(self, 'wasNotGiven', 'boolean', FWasNotGiven.Link));{2}
  oList.add(TFHIRProperty.create(self, 'reasonNotGiven', 'CodeableConcept', FReasonNotGivenList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'whenGiven', 'Period', FWhenGiven.Link));{2}
  oList.add(TFHIRProperty.create(self, 'medication', 'Resource(Medication)', FMedication.Link));{2}
  oList.add(TFHIRProperty.create(self, 'device', 'Resource(Device)', FDeviceList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'dosage', '', FDosageList.Link)){3};
end;

procedure TFhirMedicationAdministration.setProperty(propName : string; propValue: TFHIRObject);
begin
  if (propName = 'identifier') then IdentifierList.add(propValue as TFhirIdentifier){2}
  else if (propName = 'status') then StatusObject := propValue as TFHIREnum
  else if (propName = 'patient') then Patient := propValue as TFhirResourceReference{TFhirPatient}{4b}
  else if (propName = 'practitioner') then Practitioner := propValue as TFhirResourceReference{TFhirPractitioner}{4b}
  else if (propName = 'encounter') then Encounter := propValue as TFhirResourceReference{TFhirEncounter}{4b}
  else if (propName = 'prescription') then Prescription := propValue as TFhirResourceReference{TFhirMedicationPrescription}{4b}
  else if (propName = 'wasNotGiven') then WasNotGivenObject := propValue as TFhirBoolean{5a}
  else if (propName = 'reasonNotGiven') then ReasonNotGivenList.add(propValue as TFhirCodeableConcept){2}
  else if (propName = 'whenGiven') then WhenGiven := propValue as TFhirPeriod{4b}
  else if (propName = 'medication') then Medication := propValue as TFhirResourceReference{TFhirMedication}{4b}
  else if (propName = 'device') then DeviceList.add(propValue as TFhirResourceReference{TFhirDevice}){2}
  else if (propName = 'dosage') then DosageList.add(propValue as TFhirMedicationAdministrationDosage){2}
  else inherited;
end;

function TFhirMedicationAdministration.FhirType : string;
begin
  result := 'MedicationAdministration';
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
    result := TFhirMedicationAdminStatus(StringArrayIndexOfSensitive(CODES_TFhirMedicationAdminStatus, FStatus.value));
end;

Procedure TFhirMedicationAdministration.SetStatusST(value : TFhirMedicationAdminStatus);
begin
  if ord(value) = 0 then
    StatusObject := nil
  else
    StatusObject := TFhirEnum.create(CODES_TFhirMedicationAdminStatus[value]);
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
    result := FWasNotGiven.value;
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
     list.add(FIdentifier.Link);
  if (child_name = 'status') Then
     list.add(FStatus.Link);
  if (child_name = 'patient') Then
     list.add(FPatient.Link);
  if (child_name = 'dispenser') Then
     list.add(FDispenser.Link);
  if (child_name = 'authorizingPrescription') Then
     list.addAll(FAuthorizingPrescriptionList);
  if (child_name = 'dispense') Then
     list.addAll(FDispenseList);
  if (child_name = 'substitution') Then
     list.add(FSubstitution.Link);
end;

procedure TFhirMedicationDispense.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifier.Link));{2}
  oList.add(TFHIRProperty.create(self, 'status', 'code', FStatus.Link));{1}
  oList.add(TFHIRProperty.create(self, 'patient', 'Resource(Patient)', FPatient.Link));{2}
  oList.add(TFHIRProperty.create(self, 'dispenser', 'Resource(Practitioner)', FDispenser.Link));{2}
  oList.add(TFHIRProperty.create(self, 'authorizingPrescription', 'Resource(MedicationPrescription)', FAuthorizingPrescriptionList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'dispense', '', FDispenseList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'substitution', '', FSubstitution.Link));{2}
end;

procedure TFhirMedicationDispense.setProperty(propName : string; propValue: TFHIRObject);
begin
  if (propName = 'identifier') then Identifier := propValue as TFhirIdentifier{4b}
  else if (propName = 'status') then StatusObject := propValue as TFHIREnum
  else if (propName = 'patient') then Patient := propValue as TFhirResourceReference{TFhirPatient}{4b}
  else if (propName = 'dispenser') then Dispenser := propValue as TFhirResourceReference{TFhirPractitioner}{4b}
  else if (propName = 'authorizingPrescription') then AuthorizingPrescriptionList.add(propValue as TFhirResourceReference{TFhirMedicationPrescription}){2}
  else if (propName = 'dispense') then DispenseList.add(propValue as TFhirMedicationDispenseDispense){2}
  else if (propName = 'substitution') then Substitution := propValue as TFhirMedicationDispenseSubstitution{4b}
  else inherited;
end;

function TFhirMedicationDispense.FhirType : string;
begin
  result := 'MedicationDispense';
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
    result := TFhirMedicationDispenseStatus(StringArrayIndexOfSensitive(CODES_TFhirMedicationDispenseStatus, FStatus.value));
end;

Procedure TFhirMedicationDispense.SetStatusST(value : TFhirMedicationDispenseStatus);
begin
  if ord(value) = 0 then
    StatusObject := nil
  else
    StatusObject := TFhirEnum.create(CODES_TFhirMedicationDispenseStatus[value]);
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
  dateWrittenObject := TFhirMedicationPrescription(oSource).dateWrittenObject.Clone;
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
     list.add(FDateWritten.Link);
  if (child_name = 'status') Then
     list.add(FStatus.Link);
  if (child_name = 'patient') Then
     list.add(FPatient.Link);
  if (child_name = 'prescriber') Then
     list.add(FPrescriber.Link);
  if (child_name = 'encounter') Then
     list.add(FEncounter.Link);
  if (child_name = 'reason[x]') Then
     list.add(FReason.Link);
  if (child_name = 'medication') Then
     list.add(FMedication.Link);
  if (child_name = 'dosageInstruction') Then
     list.addAll(FDosageInstructionList);
  if (child_name = 'dispense') Then
     list.add(FDispense.Link);
  if (child_name = 'substitution') Then
     list.add(FSubstitution.Link);
end;

procedure TFhirMedicationPrescription.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'dateWritten', 'dateTime', FDateWritten.Link));{2}
  oList.add(TFHIRProperty.create(self, 'status', 'code', FStatus.Link));{1}
  oList.add(TFHIRProperty.create(self, 'patient', 'Resource(Patient)', FPatient.Link));{2}
  oList.add(TFHIRProperty.create(self, 'prescriber', 'Resource(Practitioner)', FPrescriber.Link));{2}
  oList.add(TFHIRProperty.create(self, 'encounter', 'Resource(Encounter)', FEncounter.Link));{2}
  oList.add(TFHIRProperty.create(self, 'reason[x]', 'CodeableConcept|Resource(Condition)', FReason.Link));{2}
  oList.add(TFHIRProperty.create(self, 'medication', 'Resource(Medication)', FMedication.Link));{2}
  oList.add(TFHIRProperty.create(self, 'dosageInstruction', '', FDosageInstructionList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'dispense', '', FDispense.Link));{2}
  oList.add(TFHIRProperty.create(self, 'substitution', '', FSubstitution.Link));{2}
end;

procedure TFhirMedicationPrescription.setProperty(propName : string; propValue: TFHIRObject);
begin
  if (propName = 'identifier') then IdentifierList.add(propValue as TFhirIdentifier){2}
  else if (propName = 'dateWritten') then DateWrittenObject := propValue as TFhirDateTime{5a}
  else if (propName = 'status') then StatusObject := propValue as TFHIREnum
  else if (propName = 'patient') then Patient := propValue as TFhirResourceReference{TFhirPatient}{4b}
  else if (propName = 'prescriber') then Prescriber := propValue as TFhirResourceReference{TFhirPractitioner}{4b}
  else if (propName = 'encounter') then Encounter := propValue as TFhirResourceReference{TFhirEncounter}{4b}
  else if (propName.startsWith('reason')) then Reason := propValue as TFhirType{4}
  else if (propName = 'medication') then Medication := propValue as TFhirResourceReference{TFhirMedication}{4b}
  else if (propName = 'dosageInstruction') then DosageInstructionList.add(propValue as TFhirMedicationPrescriptionDosageInstruction){2}
  else if (propName = 'dispense') then Dispense := propValue as TFhirMedicationPrescriptionDispense{4b}
  else if (propName = 'substitution') then Substitution := propValue as TFhirMedicationPrescriptionSubstitution{4b}
  else inherited;
end;

function TFhirMedicationPrescription.FhirType : string;
begin
  result := 'MedicationPrescription';
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

Function TFhirMedicationPrescription.GetDateWrittenST : TDateTimeEx;
begin
  if FDateWritten = nil then
    result := nil
  else
    result := FDateWritten.value;
end;

Procedure TFhirMedicationPrescription.SetDateWrittenST(value : TDateTimeEx);
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
    result := TFhirMedicationPrescriptionStatus(StringArrayIndexOfSensitive(CODES_TFhirMedicationPrescriptionStatus, FStatus.value));
end;

Procedure TFhirMedicationPrescription.SetStatusST(value : TFhirMedicationPrescriptionStatus);
begin
  if ord(value) = 0 then
    StatusObject := nil
  else
    StatusObject := TFhirEnum.create(CODES_TFhirMedicationPrescriptionStatus[value]);
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
  wasNotGivenObject := TFhirMedicationStatement(oSource).wasNotGivenObject.Clone;
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
     list.add(FPatient.Link);
  if (child_name = 'wasNotGiven') Then
     list.add(FWasNotGiven.Link);
  if (child_name = 'reasonNotGiven') Then
     list.addAll(FReasonNotGivenList);
  if (child_name = 'whenGiven') Then
     list.add(FWhenGiven.Link);
  if (child_name = 'medication') Then
     list.add(FMedication.Link);
  if (child_name = 'device') Then
     list.addAll(FDeviceList);
  if (child_name = 'dosage') Then
     list.addAll(FDosageList);
end;

procedure TFhirMedicationStatement.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'patient', 'Resource(Patient)', FPatient.Link));{2}
  oList.add(TFHIRProperty.create(self, 'wasNotGiven', 'boolean', FWasNotGiven.Link));{2}
  oList.add(TFHIRProperty.create(self, 'reasonNotGiven', 'CodeableConcept', FReasonNotGivenList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'whenGiven', 'Period', FWhenGiven.Link));{2}
  oList.add(TFHIRProperty.create(self, 'medication', 'Resource(Medication)', FMedication.Link));{2}
  oList.add(TFHIRProperty.create(self, 'device', 'Resource(Device)', FDeviceList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'dosage', '', FDosageList.Link)){3};
end;

procedure TFhirMedicationStatement.setProperty(propName : string; propValue: TFHIRObject);
begin
  if (propName = 'identifier') then IdentifierList.add(propValue as TFhirIdentifier){2}
  else if (propName = 'patient') then Patient := propValue as TFhirResourceReference{TFhirPatient}{4b}
  else if (propName = 'wasNotGiven') then WasNotGivenObject := propValue as TFhirBoolean{5a}
  else if (propName = 'reasonNotGiven') then ReasonNotGivenList.add(propValue as TFhirCodeableConcept){2}
  else if (propName = 'whenGiven') then WhenGiven := propValue as TFhirPeriod{4b}
  else if (propName = 'medication') then Medication := propValue as TFhirResourceReference{TFhirMedication}{4b}
  else if (propName = 'device') then DeviceList.add(propValue as TFhirResourceReference{TFhirDevice}){2}
  else if (propName = 'dosage') then DosageList.add(propValue as TFhirMedicationStatementDosage){2}
  else inherited;
end;

function TFhirMedicationStatement.FhirType : string;
begin
  result := 'MedicationStatement';
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
    result := FWasNotGiven.value;
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
  identifierObject := TFhirMessageHeader(oSource).identifierObject.Clone;
  timestampObject := TFhirMessageHeader(oSource).timestampObject.Clone;
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
     list.add(FIdentifier.Link);
  if (child_name = 'timestamp') Then
     list.add(FTimestamp.Link);
  if (child_name = 'event') Then
     list.add(FEvent.Link);
  if (child_name = 'response') Then
     list.add(FResponse.Link);
  if (child_name = 'source') Then
     list.add(FSource.Link);
  if (child_name = 'destination') Then
     list.addAll(FDestinationList);
  if (child_name = 'enterer') Then
     list.add(FEnterer.Link);
  if (child_name = 'author') Then
     list.add(FAuthor.Link);
  if (child_name = 'receiver') Then
     list.add(FReceiver.Link);
  if (child_name = 'responsible') Then
     list.add(FResponsible.Link);
  if (child_name = 'reason') Then
     list.add(FReason.Link);
  if (child_name = 'data') Then
     list.addAll(FDataList);
end;

procedure TFhirMessageHeader.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'id', FIdentifier.Link));{2}
  oList.add(TFHIRProperty.create(self, 'timestamp', 'instant', FTimestamp.Link));{2}
  oList.add(TFHIRProperty.create(self, 'event', 'Coding', FEvent.Link));{2}
  oList.add(TFHIRProperty.create(self, 'response', '', FResponse.Link));{2}
  oList.add(TFHIRProperty.create(self, 'source', '', FSource.Link));{2}
  oList.add(TFHIRProperty.create(self, 'destination', '', FDestinationList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'enterer', 'Resource(Practitioner)', FEnterer.Link));{2}
  oList.add(TFHIRProperty.create(self, 'author', 'Resource(Practitioner)', FAuthor.Link));{2}
  oList.add(TFHIRProperty.create(self, 'receiver', 'Resource(Practitioner|Organization)', FReceiver.Link));{2}
  oList.add(TFHIRProperty.create(self, 'responsible', 'Resource(Practitioner|Organization)', FResponsible.Link));{2}
  oList.add(TFHIRProperty.create(self, 'reason', 'CodeableConcept', FReason.Link));{2}
  oList.add(TFHIRProperty.create(self, 'data', 'Resource(Any)', FDataList.Link)){3};
end;

procedure TFhirMessageHeader.setProperty(propName : string; propValue: TFHIRObject);
begin
  if (propName = 'identifier') then IdentifierObject := propValue as TFhirId{5a}
  else if (propName = 'timestamp') then TimestampObject := propValue as TFhirInstant{5a}
  else if (propName = 'event') then Event := propValue as TFhirCoding{4b}
  else if (propName = 'response') then Response := propValue as TFhirMessageHeaderResponse{4b}
  else if (propName = 'source') then Source := propValue as TFhirMessageHeaderSource{4b}
  else if (propName = 'destination') then DestinationList.add(propValue as TFhirMessageHeaderDestination){2}
  else if (propName = 'enterer') then Enterer := propValue as TFhirResourceReference{TFhirPractitioner}{4b}
  else if (propName = 'author') then Author := propValue as TFhirResourceReference{TFhirPractitioner}{4b}
  else if (propName = 'receiver') then Receiver := propValue as TFhirResourceReference{Resource}{4b}
  else if (propName = 'responsible') then Responsible := propValue as TFhirResourceReference{Resource}{4b}
  else if (propName = 'reason') then Reason := propValue as TFhirCodeableConcept{4b}
  else if (propName = 'data') then DataList.add(propValue as TFhirResourceReference{Resource}){2}
  else inherited;
end;

function TFhirMessageHeader.FhirType : string;
begin
  result := 'MessageHeader';
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
    result := FIdentifier.value;
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

Function TFhirMessageHeader.GetTimestampST : TDateTimeEx;
begin
  if FTimestamp = nil then
    result := nil
  else
    result := FTimestamp.value;
end;

Procedure TFhirMessageHeader.SetTimestampST(value : TDateTimeEx);
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
  commentsObject := TFhirObservation(oSource).commentsObject.Clone;
  applies := TFhirObservation(oSource).applies.Clone;
  issuedObject := TFhirObservation(oSource).issuedObject.Clone;
  FStatus := TFhirObservation(oSource).FStatus.Link;
  FReliability := TFhirObservation(oSource).FReliability.Link;
  bodySite := TFhirObservation(oSource).bodySite.Clone;
  method := TFhirObservation(oSource).method.Clone;
  identifier := TFhirObservation(oSource).identifier.Clone;
  subject := TFhirObservation(oSource).subject.Clone;
  specimen := TFhirObservation(oSource).specimen.Clone;
  FPerformerList.Assign(TFhirObservation(oSource).FPerformerList);
  FReferenceRangeList.Assign(TFhirObservation(oSource).FReferenceRangeList);
  FRelatedList.Assign(TFhirObservation(oSource).FRelatedList);
end;

procedure TFhirObservation.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'name') Then
     list.add(FName.Link);
  if (child_name = 'value[x]') Then
     list.add(FValue.Link);
  if (child_name = 'interpretation') Then
     list.add(FInterpretation.Link);
  if (child_name = 'comments') Then
     list.add(FComments.Link);
  if (child_name = 'applies[x]') Then
     list.add(FApplies.Link);
  if (child_name = 'issued') Then
     list.add(FIssued.Link);
  if (child_name = 'status') Then
     list.add(FStatus.Link);
  if (child_name = 'reliability') Then
     list.add(FReliability.Link);
  if (child_name = 'bodySite') Then
     list.add(FBodySite.Link);
  if (child_name = 'method') Then
     list.add(FMethod.Link);
  if (child_name = 'identifier') Then
     list.add(FIdentifier.Link);
  if (child_name = 'subject') Then
     list.add(FSubject.Link);
  if (child_name = 'specimen') Then
     list.add(FSpecimen.Link);
  if (child_name = 'performer') Then
     list.addAll(FPerformerList);
  if (child_name = 'referenceRange') Then
     list.addAll(FReferenceRangeList);
  if (child_name = 'related') Then
     list.addAll(FRelatedList);
end;

procedure TFhirObservation.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'name', 'CodeableConcept', FName.Link));{2}
  oList.add(TFHIRProperty.create(self, 'value[x]', 'Quantity|CodeableConcept|Attachment|Ratio|Period|SampledData|string', FValue.Link));{2}
  oList.add(TFHIRProperty.create(self, 'interpretation', 'CodeableConcept', FInterpretation.Link));{2}
  oList.add(TFHIRProperty.create(self, 'comments', 'string', FComments.Link));{2}
  oList.add(TFHIRProperty.create(self, 'applies[x]', 'dateTime|Period', FApplies.Link));{2}
  oList.add(TFHIRProperty.create(self, 'issued', 'instant', FIssued.Link));{2}
  oList.add(TFHIRProperty.create(self, 'status', 'code', FStatus.Link));{1}
  oList.add(TFHIRProperty.create(self, 'reliability', 'code', FReliability.Link));{1}
  oList.add(TFHIRProperty.create(self, 'bodySite', 'CodeableConcept', FBodySite.Link));{2}
  oList.add(TFHIRProperty.create(self, 'method', 'CodeableConcept', FMethod.Link));{2}
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifier.Link));{2}
  oList.add(TFHIRProperty.create(self, 'subject', 'Resource(Patient|Group|Device|Location)', FSubject.Link));{2}
  oList.add(TFHIRProperty.create(self, 'specimen', 'Resource(Specimen)', FSpecimen.Link));{2}
  oList.add(TFHIRProperty.create(self, 'performer', 'Resource(Practitioner|Device|Organization)', FPerformerList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'referenceRange', '', FReferenceRangeList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'related', '', FRelatedList.Link)){3};
end;

procedure TFhirObservation.setProperty(propName : string; propValue: TFHIRObject);
begin
  if (propName = 'name') then Name := propValue as TFhirCodeableConcept{4b}
  else if (propName.startsWith('value')) then Value := propValue as TFhirType{4}
  else if (propName = 'interpretation') then Interpretation := propValue as TFhirCodeableConcept{4b}
  else if (propName = 'comments') then CommentsObject := propValue as TFhirString{5a}
  else if (propName.startsWith('applies')) then Applies := propValue as TFhirType{4}
  else if (propName = 'issued') then IssuedObject := propValue as TFhirInstant{5a}
  else if (propName = 'status') then StatusObject := propValue as TFHIREnum
  else if (propName = 'reliability') then ReliabilityObject := propValue as TFHIREnum
  else if (propName = 'bodySite') then BodySite := propValue as TFhirCodeableConcept{4b}
  else if (propName = 'method') then Method := propValue as TFhirCodeableConcept{4b}
  else if (propName = 'identifier') then Identifier := propValue as TFhirIdentifier{4b}
  else if (propName = 'subject') then Subject := propValue as TFhirResourceReference{Resource}{4b}
  else if (propName = 'specimen') then Specimen := propValue as TFhirResourceReference{TFhirSpecimen}{4b}
  else if (propName = 'performer') then PerformerList.add(propValue as TFhirResourceReference{Resource}){2}
  else if (propName = 'referenceRange') then ReferenceRangeList.add(propValue as TFhirObservationReferenceRange){2}
  else if (propName = 'related') then RelatedList.add(propValue as TFhirObservationRelated){2}
  else inherited;
end;

function TFhirObservation.FhirType : string;
begin
  result := 'Observation';
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
    result := FComments.value;
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

Function TFhirObservation.GetIssuedST : TDateTimeEx;
begin
  if FIssued = nil then
    result := nil
  else
    result := FIssued.value;
end;

Procedure TFhirObservation.SetIssuedST(value : TDateTimeEx);
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
    result := TFhirObservationStatus(StringArrayIndexOfSensitive(CODES_TFhirObservationStatus, FStatus.value));
end;

Procedure TFhirObservation.SetStatusST(value : TFhirObservationStatus);
begin
  if ord(value) = 0 then
    StatusObject := nil
  else
    StatusObject := TFhirEnum.create(CODES_TFhirObservationStatus[value]);
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
    result := TFhirObservationReliability(StringArrayIndexOfSensitive(CODES_TFhirObservationReliability, FReliability.value));
end;

Procedure TFhirObservation.SetReliabilityST(value : TFhirObservationReliability);
begin
  if ord(value) = 0 then
    ReliabilityObject := nil
  else
    ReliabilityObject := TFhirEnum.create(CODES_TFhirObservationReliability[value]);
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

procedure TFhirOperationOutcome.setProperty(propName : string; propValue: TFHIRObject);
begin
  if (propName = 'issue') then IssueList.add(propValue as TFhirOperationOutcomeIssue){2}
  else inherited;
end;

function TFhirOperationOutcome.FhirType : string;
begin
  result := 'OperationOutcome';
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
  dateObject := TFhirOrder(oSource).dateObject.Clone;
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
     list.add(FDate.Link);
  if (child_name = 'subject') Then
     list.add(FSubject.Link);
  if (child_name = 'source') Then
     list.add(FSource.Link);
  if (child_name = 'target') Then
     list.add(FTarget.Link);
  if (child_name = 'reason[x]') Then
     list.add(FReason.Link);
  if (child_name = 'authority') Then
     list.add(FAuthority.Link);
  if (child_name = 'when') Then
     list.add(FWhen.Link);
  if (child_name = 'detail') Then
     list.addAll(FDetailList);
end;

procedure TFhirOrder.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'date', 'dateTime', FDate.Link));{2}
  oList.add(TFHIRProperty.create(self, 'subject', 'Resource(Patient)', FSubject.Link));{2}
  oList.add(TFHIRProperty.create(self, 'source', 'Resource(Practitioner)', FSource.Link));{2}
  oList.add(TFHIRProperty.create(self, 'target', 'Resource(Organization|Device|Practitioner)', FTarget.Link));{2}
  oList.add(TFHIRProperty.create(self, 'reason[x]', 'CodeableConcept|Resource(Any)', FReason.Link));{2}
  oList.add(TFHIRProperty.create(self, 'authority', 'Resource(Any)', FAuthority.Link));{2}
  oList.add(TFHIRProperty.create(self, 'when', '', FWhen.Link));{2}
  oList.add(TFHIRProperty.create(self, 'detail', 'Resource(Any)', FDetailList.Link)){3};
end;

procedure TFhirOrder.setProperty(propName : string; propValue: TFHIRObject);
begin
  if (propName = 'identifier') then IdentifierList.add(propValue as TFhirIdentifier){2}
  else if (propName = 'date') then DateObject := propValue as TFhirDateTime{5a}
  else if (propName = 'subject') then Subject := propValue as TFhirResourceReference{TFhirPatient}{4b}
  else if (propName = 'source') then Source := propValue as TFhirResourceReference{TFhirPractitioner}{4b}
  else if (propName = 'target') then Target := propValue as TFhirResourceReference{Resource}{4b}
  else if (propName.startsWith('reason')) then Reason := propValue as TFhirType{4}
  else if (propName = 'authority') then Authority := propValue as TFhirResourceReference{Resource}{4b}
  else if (propName = 'when') then When := propValue as TFhirOrderWhen{4b}
  else if (propName = 'detail') then DetailList.add(propValue as TFhirResourceReference{Resource}){2}
  else inherited;
end;

function TFhirOrder.FhirType : string;
begin
  result := 'Order';
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

Function TFhirOrder.GetDateST : TDateTimeEx;
begin
  if FDate = nil then
    result := nil
  else
    result := FDate.value;
end;

Procedure TFhirOrder.SetDateST(value : TDateTimeEx);
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
  dateObject := TFhirOrderResponse(oSource).dateObject.Clone;
  who := TFhirOrderResponse(oSource).who.Clone;
  authority := TFhirOrderResponse(oSource).authority.Clone;
  FCode := TFhirOrderResponse(oSource).FCode.Link;
  descriptionObject := TFhirOrderResponse(oSource).descriptionObject.Clone;
  FFulfillmentList.Assign(TFhirOrderResponse(oSource).FFulfillmentList);
end;

procedure TFhirOrderResponse.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'identifier') Then
     list.addAll(FIdentifierList);
  if (child_name = 'request') Then
     list.add(FRequest.Link);
  if (child_name = 'date') Then
     list.add(FDate.Link);
  if (child_name = 'who') Then
     list.add(FWho.Link);
  if (child_name = 'authority[x]') Then
     list.add(FAuthority.Link);
  if (child_name = 'code') Then
     list.add(FCode.Link);
  if (child_name = 'description') Then
     list.add(FDescription.Link);
  if (child_name = 'fulfillment') Then
     list.addAll(FFulfillmentList);
end;

procedure TFhirOrderResponse.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'request', 'Resource(Order)', FRequest.Link));{2}
  oList.add(TFHIRProperty.create(self, 'date', 'dateTime', FDate.Link));{2}
  oList.add(TFHIRProperty.create(self, 'who', 'Resource(Practitioner|Organization|Device)', FWho.Link));{2}
  oList.add(TFHIRProperty.create(self, 'authority[x]', 'CodeableConcept|Resource(Any)', FAuthority.Link));{2}
  oList.add(TFHIRProperty.create(self, 'code', 'code', FCode.Link));{1}
  oList.add(TFHIRProperty.create(self, 'description', 'string', FDescription.Link));{2}
  oList.add(TFHIRProperty.create(self, 'fulfillment', 'Resource(Any)', FFulfillmentList.Link)){3};
end;

procedure TFhirOrderResponse.setProperty(propName : string; propValue: TFHIRObject);
begin
  if (propName = 'identifier') then IdentifierList.add(propValue as TFhirIdentifier){2}
  else if (propName = 'request') then Request := propValue as TFhirResourceReference{TFhirOrder}{4b}
  else if (propName = 'date') then DateObject := propValue as TFhirDateTime{5a}
  else if (propName = 'who') then Who := propValue as TFhirResourceReference{Resource}{4b}
  else if (propName.startsWith('authority')) then Authority := propValue as TFhirType{4}
  else if (propName = 'code') then CodeObject := propValue as TFHIREnum
  else if (propName = 'description') then DescriptionObject := propValue as TFhirString{5a}
  else if (propName = 'fulfillment') then FulfillmentList.add(propValue as TFhirResourceReference{Resource}){2}
  else inherited;
end;

function TFhirOrderResponse.FhirType : string;
begin
  result := 'OrderResponse';
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

Function TFhirOrderResponse.GetDateST : TDateTimeEx;
begin
  if FDate = nil then
    result := nil
  else
    result := FDate.value;
end;

Procedure TFhirOrderResponse.SetDateST(value : TDateTimeEx);
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
    result := TFhirOrderOutcomeCode(StringArrayIndexOfSensitive(CODES_TFhirOrderOutcomeCode, FCode.value));
end;

Procedure TFhirOrderResponse.SetCodeST(value : TFhirOrderOutcomeCode);
begin
  if ord(value) = 0 then
    CodeObject := nil
  else
    CodeObject := TFhirEnum.create(CODES_TFhirOrderOutcomeCode[value]);
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
    result := FDescription.value;
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
  nameObject := TFhirOrganization(oSource).nameObject.Clone;
  type_ := TFhirOrganization(oSource).type_.Clone;
  FTelecomList.Assign(TFhirOrganization(oSource).FTelecomList);
  FAddressList.Assign(TFhirOrganization(oSource).FAddressList);
  partOf := TFhirOrganization(oSource).partOf.Clone;
  FContactList.Assign(TFhirOrganization(oSource).FContactList);
  FLocationList.Assign(TFhirOrganization(oSource).FLocationList);
  activeObject := TFhirOrganization(oSource).activeObject.Clone;
end;

procedure TFhirOrganization.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'identifier') Then
     list.addAll(FIdentifierList);
  if (child_name = 'name') Then
     list.add(FName.Link);
  if (child_name = 'type') Then
     list.add(FType_.Link);
  if (child_name = 'telecom') Then
     list.addAll(FTelecomList);
  if (child_name = 'address') Then
     list.addAll(FAddressList);
  if (child_name = 'partOf') Then
     list.add(FPartOf.Link);
  if (child_name = 'contact') Then
     list.addAll(FContactList);
  if (child_name = 'location') Then
     list.addAll(FLocationList);
  if (child_name = 'active') Then
     list.add(FActive.Link);
end;

procedure TFhirOrganization.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'name', 'string', FName.Link));{2}
  oList.add(TFHIRProperty.create(self, 'type', 'CodeableConcept', FType_.Link));{2}
  oList.add(TFHIRProperty.create(self, 'telecom', 'Contact', FTelecomList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'address', 'Address', FAddressList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'partOf', 'Resource(Organization)', FPartOf.Link));{2}
  oList.add(TFHIRProperty.create(self, 'contact', '', FContactList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'location', 'Resource(Location)', FLocationList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'active', 'boolean', FActive.Link));{2}
end;

procedure TFhirOrganization.setProperty(propName : string; propValue: TFHIRObject);
begin
  if (propName = 'identifier') then IdentifierList.add(propValue as TFhirIdentifier){2}
  else if (propName = 'name') then NameObject := propValue as TFhirString{5a}
  else if (propName = 'type') then Type_ := propValue as TFhirCodeableConcept{4b}
  else if (propName = 'telecom') then TelecomList.add(propValue as TFhirContact){2}
  else if (propName = 'address') then AddressList.add(propValue as TFhirAddress){2}
  else if (propName = 'partOf') then PartOf := propValue as TFhirResourceReference{TFhirOrganization}{4b}
  else if (propName = 'contact') then ContactList.add(propValue as TFhirOrganizationContact){2}
  else if (propName = 'location') then LocationList.add(propValue as TFhirResourceReference{TFhirLocation}){2}
  else if (propName = 'active') then ActiveObject := propValue as TFhirBoolean{5a}
  else inherited;
end;

function TFhirOrganization.FhirType : string;
begin
  result := 'Organization';
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
    result := FName.value;
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
    result := FActive.value;
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
  createdObject := TFhirOther(oSource).createdObject.Clone;
end;

procedure TFhirOther.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'identifier') Then
     list.addAll(FIdentifierList);
  if (child_name = 'code') Then
     list.add(FCode.Link);
  if (child_name = 'subject') Then
     list.add(FSubject.Link);
  if (child_name = 'author') Then
     list.add(FAuthor.Link);
  if (child_name = 'created') Then
     list.add(FCreated.Link);
end;

procedure TFhirOther.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'code', 'CodeableConcept', FCode.Link));{2}
  oList.add(TFHIRProperty.create(self, 'subject', 'Resource(Any)', FSubject.Link));{2}
  oList.add(TFHIRProperty.create(self, 'author', 'Resource(Practitioner|Patient|RelatedPerson)', FAuthor.Link));{2}
  oList.add(TFHIRProperty.create(self, 'created', 'date', FCreated.Link));{2}
end;

procedure TFhirOther.setProperty(propName : string; propValue: TFHIRObject);
begin
  if (propName = 'identifier') then IdentifierList.add(propValue as TFhirIdentifier){2}
  else if (propName = 'code') then Code := propValue as TFhirCodeableConcept{4b}
  else if (propName = 'subject') then Subject := propValue as TFhirResourceReference{Resource}{4b}
  else if (propName = 'author') then Author := propValue as TFhirResourceReference{Resource}{4b}
  else if (propName = 'created') then CreatedObject := propValue as TFhirDate{5a}
  else inherited;
end;

function TFhirOther.FhirType : string;
begin
  result := 'Other';
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

Function TFhirOther.GetCreatedST : TDateTimeEx;
begin
  if FCreated = nil then
    result := nil
  else
    result := FCreated.value;
end;

Procedure TFhirOther.SetCreatedST(value : TDateTimeEx);
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
  gender := TFhirPatient(oSource).gender.Clone;
  birthDateObject := TFhirPatient(oSource).birthDateObject.Clone;
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
  activeObject := TFhirPatient(oSource).activeObject.Clone;
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
     list.add(FBirthDate.Link);
  if (child_name = 'deceased[x]') Then
     list.add(FDeceased.Link);
  if (child_name = 'address') Then
     list.addAll(FAddressList);
  if (child_name = 'maritalStatus') Then
     list.add(FMaritalStatus.Link);
  if (child_name = 'multipleBirth[x]') Then
     list.add(FMultipleBirth.Link);
  if (child_name = 'photo') Then
     list.addAll(FPhotoList);
  if (child_name = 'contact') Then
     list.addAll(FContactList);
  if (child_name = 'animal') Then
     list.add(FAnimal.Link);
  if (child_name = 'communication') Then
     list.addAll(FCommunicationList);
  if (child_name = 'careProvider') Then
     list.addAll(FCareProviderList);
  if (child_name = 'managingOrganization') Then
     list.add(FManagingOrganization.Link);
  if (child_name = 'link') Then
     list.addAll(FLink_List);
  if (child_name = 'active') Then
     list.add(FActive.Link);
end;

procedure TFhirPatient.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'name', 'HumanName', FNameList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'telecom', 'Contact', FTelecomList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'gender', 'CodeableConcept', FGender.Link));{2}
  oList.add(TFHIRProperty.create(self, 'birthDate', 'dateTime', FBirthDate.Link));{2}
  oList.add(TFHIRProperty.create(self, 'deceased[x]', 'boolean|dateTime', FDeceased.Link));{2}
  oList.add(TFHIRProperty.create(self, 'address', 'Address', FAddressList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'maritalStatus', 'CodeableConcept', FMaritalStatus.Link));{2}
  oList.add(TFHIRProperty.create(self, 'multipleBirth[x]', 'boolean|integer', FMultipleBirth.Link));{2}
  oList.add(TFHIRProperty.create(self, 'photo', 'Attachment', FPhotoList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'contact', '', FContactList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'animal', '', FAnimal.Link));{2}
  oList.add(TFHIRProperty.create(self, 'communication', 'CodeableConcept', FCommunicationList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'careProvider', 'Resource(Organization|Practitioner)', FCareProviderList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'managingOrganization', 'Resource(Organization)', FManagingOrganization.Link));{2}
  oList.add(TFHIRProperty.create(self, 'link', '', FLink_List.Link)){3};
  oList.add(TFHIRProperty.create(self, 'active', 'boolean', FActive.Link));{2}
end;

procedure TFhirPatient.setProperty(propName : string; propValue: TFHIRObject);
begin
  if (propName = 'identifier') then IdentifierList.add(propValue as TFhirIdentifier){2}
  else if (propName = 'name') then NameList.add(propValue as TFhirHumanName){2}
  else if (propName = 'telecom') then TelecomList.add(propValue as TFhirContact){2}
  else if (propName = 'gender') then Gender := propValue as TFhirCodeableConcept{4b}
  else if (propName = 'birthDate') then BirthDateObject := propValue as TFhirDateTime{5a}
  else if (propName.startsWith('deceased')) then Deceased := propValue as TFhirType{4}
  else if (propName = 'address') then AddressList.add(propValue as TFhirAddress){2}
  else if (propName = 'maritalStatus') then MaritalStatus := propValue as TFhirCodeableConcept{4b}
  else if (propName.startsWith('multipleBirth')) then MultipleBirth := propValue as TFhirType{4}
  else if (propName = 'photo') then PhotoList.add(propValue as TFhirAttachment){2}
  else if (propName = 'contact') then ContactList.add(propValue as TFhirPatientContact){2}
  else if (propName = 'animal') then Animal := propValue as TFhirPatientAnimal{4b}
  else if (propName = 'communication') then CommunicationList.add(propValue as TFhirCodeableConcept){2}
  else if (propName = 'careProvider') then CareProviderList.add(propValue as TFhirResourceReference{Resource}){2}
  else if (propName = 'managingOrganization') then ManagingOrganization := propValue as TFhirResourceReference{TFhirOrganization}{4b}
  else if (propName = 'link') then Link_List.add(propValue as TFhirPatientLink){2}
  else if (propName = 'active') then ActiveObject := propValue as TFhirBoolean{5a}
  else inherited;
end;

function TFhirPatient.FhirType : string;
begin
  result := 'Patient';
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

Procedure TFhirPatient.SetGender(value : TFhirCodeableConcept);
begin
  FGender.free;
  FGender := value;
end;

Procedure TFhirPatient.SetBirthDate(value : TFhirDateTime);
begin
  FBirthDate.free;
  FBirthDate := value;
end;

Function TFhirPatient.GetBirthDateST : TDateTimeEx;
begin
  if FBirthDate = nil then
    result := nil
  else
    result := FBirthDate.value;
end;

Procedure TFhirPatient.SetBirthDateST(value : TDateTimeEx);
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
    result := FActive.value;
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
  FAddress.free;
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
  address := TFhirPractitioner(oSource).address.Clone;
  gender := TFhirPractitioner(oSource).gender.Clone;
  birthDateObject := TFhirPractitioner(oSource).birthDateObject.Clone;
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
     list.add(FName.Link);
  if (child_name = 'telecom') Then
     list.addAll(FTelecomList);
  if (child_name = 'address') Then
     list.add(FAddress.Link);
  if (child_name = 'gender') Then
     list.add(FGender.Link);
  if (child_name = 'birthDate') Then
     list.add(FBirthDate.Link);
  if (child_name = 'photo') Then
     list.addAll(FPhotoList);
  if (child_name = 'organization') Then
     list.add(FOrganization.Link);
  if (child_name = 'role') Then
     list.addAll(FRoleList);
  if (child_name = 'specialty') Then
     list.addAll(FSpecialtyList);
  if (child_name = 'period') Then
     list.add(FPeriod.Link);
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
  oList.add(TFHIRProperty.create(self, 'name', 'HumanName', FName.Link));{2}
  oList.add(TFHIRProperty.create(self, 'telecom', 'Contact', FTelecomList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'address', 'Address', FAddress.Link));{2}
  oList.add(TFHIRProperty.create(self, 'gender', 'CodeableConcept', FGender.Link));{2}
  oList.add(TFHIRProperty.create(self, 'birthDate', 'dateTime', FBirthDate.Link));{2}
  oList.add(TFHIRProperty.create(self, 'photo', 'Attachment', FPhotoList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'organization', 'Resource(Organization)', FOrganization.Link));{2}
  oList.add(TFHIRProperty.create(self, 'role', 'CodeableConcept', FRoleList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'specialty', 'CodeableConcept', FSpecialtyList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'period', 'Period', FPeriod.Link));{2}
  oList.add(TFHIRProperty.create(self, 'location', 'Resource(Location)', FLocationList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'qualification', '', FQualificationList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'communication', 'CodeableConcept', FCommunicationList.Link)){3};
end;

procedure TFhirPractitioner.setProperty(propName : string; propValue: TFHIRObject);
begin
  if (propName = 'identifier') then IdentifierList.add(propValue as TFhirIdentifier){2}
  else if (propName = 'name') then Name := propValue as TFhirHumanName{4b}
  else if (propName = 'telecom') then TelecomList.add(propValue as TFhirContact){2}
  else if (propName = 'address') then Address := propValue as TFhirAddress{4b}
  else if (propName = 'gender') then Gender := propValue as TFhirCodeableConcept{4b}
  else if (propName = 'birthDate') then BirthDateObject := propValue as TFhirDateTime{5a}
  else if (propName = 'photo') then PhotoList.add(propValue as TFhirAttachment){2}
  else if (propName = 'organization') then Organization := propValue as TFhirResourceReference{TFhirOrganization}{4b}
  else if (propName = 'role') then RoleList.add(propValue as TFhirCodeableConcept){2}
  else if (propName = 'specialty') then SpecialtyList.add(propValue as TFhirCodeableConcept){2}
  else if (propName = 'period') then Period := propValue as TFhirPeriod{4b}
  else if (propName = 'location') then LocationList.add(propValue as TFhirResourceReference{TFhirLocation}){2}
  else if (propName = 'qualification') then QualificationList.add(propValue as TFhirPractitionerQualification){2}
  else if (propName = 'communication') then CommunicationList.add(propValue as TFhirCodeableConcept){2}
  else inherited;
end;

function TFhirPractitioner.FhirType : string;
begin
  result := 'Practitioner';
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

Procedure TFhirPractitioner.SetAddress(value : TFhirAddress);
begin
  FAddress.free;
  FAddress := value;
end;

Procedure TFhirPractitioner.SetGender(value : TFhirCodeableConcept);
begin
  FGender.free;
  FGender := value;
end;

Procedure TFhirPractitioner.SetBirthDate(value : TFhirDateTime);
begin
  FBirthDate.free;
  FBirthDate := value;
end;

Function TFhirPractitioner.GetBirthDateST : TDateTimeEx;
begin
  if FBirthDate = nil then
    result := nil
  else
    result := FBirthDate.value;
end;

Procedure TFhirPractitioner.SetBirthDateST(value : TDateTimeEx);
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
  outcomeObject := TFhirProcedure(oSource).outcomeObject.Clone;
  FReportList.Assign(TFhirProcedure(oSource).FReportList);
  FComplicationList.Assign(TFhirProcedure(oSource).FComplicationList);
  followUpObject := TFhirProcedure(oSource).followUpObject.Clone;
  FRelatedItemList.Assign(TFhirProcedure(oSource).FRelatedItemList);
  notesObject := TFhirProcedure(oSource).notesObject.Clone;
end;

procedure TFhirProcedure.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'identifier') Then
     list.addAll(FIdentifierList);
  if (child_name = 'subject') Then
     list.add(FSubject.Link);
  if (child_name = 'type') Then
     list.add(FType_.Link);
  if (child_name = 'bodySite') Then
     list.addAll(FBodySiteList);
  if (child_name = 'indication') Then
     list.addAll(FIndicationList);
  if (child_name = 'performer') Then
     list.addAll(FPerformerList);
  if (child_name = 'date') Then
     list.add(FDate.Link);
  if (child_name = 'encounter') Then
     list.add(FEncounter.Link);
  if (child_name = 'outcome') Then
     list.add(FOutcome.Link);
  if (child_name = 'report') Then
     list.addAll(FReportList);
  if (child_name = 'complication') Then
     list.addAll(FComplicationList);
  if (child_name = 'followUp') Then
     list.add(FFollowUp.Link);
  if (child_name = 'relatedItem') Then
     list.addAll(FRelatedItemList);
  if (child_name = 'notes') Then
     list.add(FNotes.Link);
end;

procedure TFhirProcedure.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'subject', 'Resource(Patient)', FSubject.Link));{2}
  oList.add(TFHIRProperty.create(self, 'type', 'CodeableConcept', FType_.Link));{2}
  oList.add(TFHIRProperty.create(self, 'bodySite', 'CodeableConcept', FBodySiteList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'indication', 'CodeableConcept', FIndicationList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'performer', '', FPerformerList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'date', 'Period', FDate.Link));{2}
  oList.add(TFHIRProperty.create(self, 'encounter', 'Resource(Encounter)', FEncounter.Link));{2}
  oList.add(TFHIRProperty.create(self, 'outcome', 'string', FOutcome.Link));{2}
  oList.add(TFHIRProperty.create(self, 'report', 'Resource(DiagnosticReport)', FReportList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'complication', 'CodeableConcept', FComplicationList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'followUp', 'string', FFollowUp.Link));{2}
  oList.add(TFHIRProperty.create(self, 'relatedItem', '', FRelatedItemList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'notes', 'string', FNotes.Link));{2}
end;

procedure TFhirProcedure.setProperty(propName : string; propValue: TFHIRObject);
begin
  if (propName = 'identifier') then IdentifierList.add(propValue as TFhirIdentifier){2}
  else if (propName = 'subject') then Subject := propValue as TFhirResourceReference{TFhirPatient}{4b}
  else if (propName = 'type') then Type_ := propValue as TFhirCodeableConcept{4b}
  else if (propName = 'bodySite') then BodySiteList.add(propValue as TFhirCodeableConcept){2}
  else if (propName = 'indication') then IndicationList.add(propValue as TFhirCodeableConcept){2}
  else if (propName = 'performer') then PerformerList.add(propValue as TFhirProcedurePerformer){2}
  else if (propName = 'date') then Date := propValue as TFhirPeriod{4b}
  else if (propName = 'encounter') then Encounter := propValue as TFhirResourceReference{TFhirEncounter}{4b}
  else if (propName = 'outcome') then OutcomeObject := propValue as TFhirString{5a}
  else if (propName = 'report') then ReportList.add(propValue as TFhirResourceReference{TFhirDiagnosticReport}){2}
  else if (propName = 'complication') then ComplicationList.add(propValue as TFhirCodeableConcept){2}
  else if (propName = 'followUp') then FollowUpObject := propValue as TFhirString{5a}
  else if (propName = 'relatedItem') then RelatedItemList.add(propValue as TFhirProcedureRelatedItem){2}
  else if (propName = 'notes') then NotesObject := propValue as TFhirString{5a}
  else inherited;
end;

function TFhirProcedure.FhirType : string;
begin
  result := 'Procedure';
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
    result := FOutcome.value;
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
    result := FFollowUp.value;
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
    result := FNotes.value;
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
  FTelecomList := TFhirContactList.Create;
  FCodeList := TFhirCodingList.Create;
  FMappingList := TFhirProfileMappingList.Create;
  FStructureList := TFhirProfileStructureList.Create;
  FExtensionDefnList := TFhirProfileExtensionDefnList.Create;
  FQueryList := TFhirProfileQueryList.Create;
end;

destructor TFhirProfile.Destroy;
begin
  FIdentifier.free;
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
  FQueryList.Free;
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
  identifierObject := TFhirProfile(oSource).identifierObject.Clone;
  versionObject := TFhirProfile(oSource).versionObject.Clone;
  nameObject := TFhirProfile(oSource).nameObject.Clone;
  publisherObject := TFhirProfile(oSource).publisherObject.Clone;
  FTelecomList.Assign(TFhirProfile(oSource).FTelecomList);
  descriptionObject := TFhirProfile(oSource).descriptionObject.Clone;
  FCodeList.Assign(TFhirProfile(oSource).FCodeList);
  FStatus := TFhirProfile(oSource).FStatus.Link;
  experimentalObject := TFhirProfile(oSource).experimentalObject.Clone;
  dateObject := TFhirProfile(oSource).dateObject.Clone;
  requirementsObject := TFhirProfile(oSource).requirementsObject.Clone;
  fhirVersionObject := TFhirProfile(oSource).fhirVersionObject.Clone;
  FMappingList.Assign(TFhirProfile(oSource).FMappingList);
  FStructureList.Assign(TFhirProfile(oSource).FStructureList);
  FExtensionDefnList.Assign(TFhirProfile(oSource).FExtensionDefnList);
  FQueryList.Assign(TFhirProfile(oSource).FQueryList);
end;

procedure TFhirProfile.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'identifier') Then
     list.add(FIdentifier.Link);
  if (child_name = 'version') Then
     list.add(FVersion.Link);
  if (child_name = 'name') Then
     list.add(FName.Link);
  if (child_name = 'publisher') Then
     list.add(FPublisher.Link);
  if (child_name = 'telecom') Then
     list.addAll(FTelecomList);
  if (child_name = 'description') Then
     list.add(FDescription.Link);
  if (child_name = 'code') Then
     list.addAll(FCodeList);
  if (child_name = 'status') Then
     list.add(FStatus.Link);
  if (child_name = 'experimental') Then
     list.add(FExperimental.Link);
  if (child_name = 'date') Then
     list.add(FDate.Link);
  if (child_name = 'requirements') Then
     list.add(FRequirements.Link);
  if (child_name = 'fhirVersion') Then
     list.add(FFhirVersion.Link);
  if (child_name = 'mapping') Then
     list.addAll(FMappingList);
  if (child_name = 'structure') Then
     list.addAll(FStructureList);
  if (child_name = 'extensionDefn') Then
     list.addAll(FExtensionDefnList);
  if (child_name = 'query') Then
     list.addAll(FQueryList);
end;

procedure TFhirProfile.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'string', FIdentifier.Link));{2}
  oList.add(TFHIRProperty.create(self, 'version', 'string', FVersion.Link));{2}
  oList.add(TFHIRProperty.create(self, 'name', 'string', FName.Link));{2}
  oList.add(TFHIRProperty.create(self, 'publisher', 'string', FPublisher.Link));{2}
  oList.add(TFHIRProperty.create(self, 'telecom', 'Contact', FTelecomList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'description', 'string', FDescription.Link));{2}
  oList.add(TFHIRProperty.create(self, 'code', 'Coding', FCodeList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'status', 'code', FStatus.Link));{1}
  oList.add(TFHIRProperty.create(self, 'experimental', 'boolean', FExperimental.Link));{2}
  oList.add(TFHIRProperty.create(self, 'date', 'dateTime', FDate.Link));{2}
  oList.add(TFHIRProperty.create(self, 'requirements', 'string', FRequirements.Link));{2}
  oList.add(TFHIRProperty.create(self, 'fhirVersion', 'id', FFhirVersion.Link));{2}
  oList.add(TFHIRProperty.create(self, 'mapping', '', FMappingList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'structure', '', FStructureList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'extensionDefn', '', FExtensionDefnList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'query', '', FQueryList.Link)){3};
end;

procedure TFhirProfile.setProperty(propName : string; propValue: TFHIRObject);
begin
  if (propName = 'identifier') then IdentifierObject := propValue as TFhirString{5a}
  else if (propName = 'version') then VersionObject := propValue as TFhirString{5a}
  else if (propName = 'name') then NameObject := propValue as TFhirString{5a}
  else if (propName = 'publisher') then PublisherObject := propValue as TFhirString{5a}
  else if (propName = 'telecom') then TelecomList.add(propValue as TFhirContact){2}
  else if (propName = 'description') then DescriptionObject := propValue as TFhirString{5a}
  else if (propName = 'code') then CodeList.add(propValue as TFhirCoding){2}
  else if (propName = 'status') then StatusObject := propValue as TFHIREnum
  else if (propName = 'experimental') then ExperimentalObject := propValue as TFhirBoolean{5a}
  else if (propName = 'date') then DateObject := propValue as TFhirDateTime{5a}
  else if (propName = 'requirements') then RequirementsObject := propValue as TFhirString{5a}
  else if (propName = 'fhirVersion') then FhirVersionObject := propValue as TFhirId{5a}
  else if (propName = 'mapping') then MappingList.add(propValue as TFhirProfileMapping){2}
  else if (propName = 'structure') then StructureList.add(propValue as TFhirProfileStructure){2}
  else if (propName = 'extensionDefn') then ExtensionDefnList.add(propValue as TFhirProfileExtensionDefn){2}
  else if (propName = 'query') then QueryList.add(propValue as TFhirProfileQuery){2}
  else inherited;
end;

function TFhirProfile.FhirType : string;
begin
  result := 'Profile';
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

Procedure TFhirProfile.SetIdentifier(value : TFhirString);
begin
  FIdentifier.free;
  FIdentifier := value;
end;

Function TFhirProfile.GetIdentifierST : String;
begin
  if FIdentifier = nil then
    result := ''
  else
    result := FIdentifier.value;
end;

Procedure TFhirProfile.SetIdentifierST(value : String);
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
    result := FVersion.value;
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
    result := FName.value;
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
    result := FPublisher.value;
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
    result := FDescription.value;
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
    result := TFhirResourceProfileStatus(StringArrayIndexOfSensitive(CODES_TFhirResourceProfileStatus, FStatus.value));
end;

Procedure TFhirProfile.SetStatusST(value : TFhirResourceProfileStatus);
begin
  if ord(value) = 0 then
    StatusObject := nil
  else
    StatusObject := TFhirEnum.create(CODES_TFhirResourceProfileStatus[value]);
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
    result := FExperimental.value;
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

Function TFhirProfile.GetDateST : TDateTimeEx;
begin
  if FDate = nil then
    result := nil
  else
    result := FDate.value;
end;

Procedure TFhirProfile.SetDateST(value : TDateTimeEx);
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
    result := FRequirements.value;
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
    result := FFhirVersion.value;
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
  recordedObject := TFhirProvenance(oSource).recordedObject.Clone;
  reason := TFhirProvenance(oSource).reason.Clone;
  location := TFhirProvenance(oSource).location.Clone;
  FPolicyList.Assign(TFhirProvenance(oSource).FPolicyList);
  FAgentList.Assign(TFhirProvenance(oSource).FAgentList);
  FEntityList.Assign(TFhirProvenance(oSource).FEntityList);
  integritySignatureObject := TFhirProvenance(oSource).integritySignatureObject.Clone;
end;

procedure TFhirProvenance.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'target') Then
     list.addAll(FTargetList);
  if (child_name = 'period') Then
     list.add(FPeriod.Link);
  if (child_name = 'recorded') Then
     list.add(FRecorded.Link);
  if (child_name = 'reason') Then
     list.add(FReason.Link);
  if (child_name = 'location') Then
     list.add(FLocation.Link);
  if (child_name = 'policy') Then
     list.addAll(FPolicyList);
  if (child_name = 'agent') Then
     list.addAll(FAgentList);
  if (child_name = 'entity') Then
     list.addAll(FEntityList);
  if (child_name = 'integritySignature') Then
     list.add(FIntegritySignature.Link);
end;

procedure TFhirProvenance.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'target', 'Resource(Any)', FTargetList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'period', 'Period', FPeriod.Link));{2}
  oList.add(TFHIRProperty.create(self, 'recorded', 'instant', FRecorded.Link));{2}
  oList.add(TFHIRProperty.create(self, 'reason', 'CodeableConcept', FReason.Link));{2}
  oList.add(TFHIRProperty.create(self, 'location', 'Resource(Location)', FLocation.Link));{2}
  oList.add(TFHIRProperty.create(self, 'policy', 'uri', FPolicyList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'agent', '', FAgentList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'entity', '', FEntityList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'integritySignature', 'string', FIntegritySignature.Link));{2}
end;

procedure TFhirProvenance.setProperty(propName : string; propValue: TFHIRObject);
begin
  if (propName = 'target') then TargetList.add(propValue as TFhirResourceReference{Resource}){2}
  else if (propName = 'period') then Period := propValue as TFhirPeriod{4b}
  else if (propName = 'recorded') then RecordedObject := propValue as TFhirInstant{5a}
  else if (propName = 'reason') then Reason := propValue as TFhirCodeableConcept{4b}
  else if (propName = 'location') then Location := propValue as TFhirResourceReference{TFhirLocation}{4b}
  else if (propName = 'policy') then PolicyList.add(propValue as TFhirUri){2}
  else if (propName = 'agent') then AgentList.add(propValue as TFhirProvenanceAgent){2}
  else if (propName = 'entity') then EntityList.add(propValue as TFhirProvenanceEntity){2}
  else if (propName = 'integritySignature') then IntegritySignatureObject := propValue as TFhirString{5a}
  else inherited;
end;

function TFhirProvenance.FhirType : string;
begin
  result := 'Provenance';
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

Function TFhirProvenance.GetRecordedST : TDateTimeEx;
begin
  if FRecorded = nil then
    result := nil
  else
    result := FRecorded.value;
end;

Procedure TFhirProvenance.SetRecordedST(value : TDateTimeEx);
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
    result := FIntegritySignature.value;
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
  identifierObject := TFhirQuery(oSource).identifierObject.Clone;
  FParameterList.Assign(TFhirQuery(oSource).FParameterList);
  response := TFhirQuery(oSource).response.Clone;
end;

procedure TFhirQuery.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'identifier') Then
     list.add(FIdentifier.Link);
  if (child_name = 'parameter') Then
     list.addAll(FParameterList);
  if (child_name = 'response') Then
     list.add(FResponse.Link);
end;

procedure TFhirQuery.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'uri', FIdentifier.Link));{2}
  oList.add(TFHIRProperty.create(self, 'parameter', 'Extension', FParameterList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'response', '', FResponse.Link));{2}
end;

procedure TFhirQuery.setProperty(propName : string; propValue: TFHIRObject);
begin
  if (propName = 'identifier') then IdentifierObject := propValue as TFhirUri{5a}
  else if (propName = 'parameter') then ParameterList.add(propValue as TFhirExtension){2}
  else if (propName = 'response') then Response := propValue as TFhirQueryResponse{4b}
  else inherited;
end;

function TFhirQuery.FhirType : string;
begin
  result := 'Query';
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
    result := FIdentifier.value;
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
  FStatus.free;
  FAuthored.free;
  FSubject.free;
  FAuthor.free;
  FSource.free;
  FName.free;
  FIdentifierList.Free;
  FEncounter.free;
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
  FStatus := TFhirQuestionnaire(oSource).FStatus.Link;
  authoredObject := TFhirQuestionnaire(oSource).authoredObject.Clone;
  subject := TFhirQuestionnaire(oSource).subject.Clone;
  author := TFhirQuestionnaire(oSource).author.Clone;
  source := TFhirQuestionnaire(oSource).source.Clone;
  name := TFhirQuestionnaire(oSource).name.Clone;
  FIdentifierList.Assign(TFhirQuestionnaire(oSource).FIdentifierList);
  encounter := TFhirQuestionnaire(oSource).encounter.Clone;
  group := TFhirQuestionnaire(oSource).group.Clone;
end;

procedure TFhirQuestionnaire.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'status') Then
     list.add(FStatus.Link);
  if (child_name = 'authored') Then
     list.add(FAuthored.Link);
  if (child_name = 'subject') Then
     list.add(FSubject.Link);
  if (child_name = 'author') Then
     list.add(FAuthor.Link);
  if (child_name = 'source') Then
     list.add(FSource.Link);
  if (child_name = 'name') Then
     list.add(FName.Link);
  if (child_name = 'identifier') Then
     list.addAll(FIdentifierList);
  if (child_name = 'encounter') Then
     list.add(FEncounter.Link);
  if (child_name = 'group') Then
     list.add(FGroup.Link);
end;

procedure TFhirQuestionnaire.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'status', 'code', FStatus.Link));{1}
  oList.add(TFHIRProperty.create(self, 'authored', 'dateTime', FAuthored.Link));{2}
  oList.add(TFHIRProperty.create(self, 'subject', 'Resource(Patient|RelatedPerson)', FSubject.Link));{2}
  oList.add(TFHIRProperty.create(self, 'author', 'Resource(Practitioner|Patient|RelatedPerson)', FAuthor.Link));{2}
  oList.add(TFHIRProperty.create(self, 'source', 'Resource(Patient|Practitioner|RelatedPerson)', FSource.Link));{2}
  oList.add(TFHIRProperty.create(self, 'name', 'CodeableConcept', FName.Link));{2}
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'encounter', 'Resource(Encounter)', FEncounter.Link));{2}
  oList.add(TFHIRProperty.create(self, 'group', '', FGroup.Link));{2}
end;

procedure TFhirQuestionnaire.setProperty(propName : string; propValue: TFHIRObject);
begin
  if (propName = 'status') then StatusObject := propValue as TFHIREnum
  else if (propName = 'authored') then AuthoredObject := propValue as TFhirDateTime{5a}
  else if (propName = 'subject') then Subject := propValue as TFhirResourceReference{Resource}{4b}
  else if (propName = 'author') then Author := propValue as TFhirResourceReference{Resource}{4b}
  else if (propName = 'source') then Source := propValue as TFhirResourceReference{Resource}{4b}
  else if (propName = 'name') then Name := propValue as TFhirCodeableConcept{4b}
  else if (propName = 'identifier') then IdentifierList.add(propValue as TFhirIdentifier){2}
  else if (propName = 'encounter') then Encounter := propValue as TFhirResourceReference{TFhirEncounter}{4b}
  else if (propName = 'group') then Group := propValue as TFhirQuestionnaireGroup{4b}
  else inherited;
end;

function TFhirQuestionnaire.FhirType : string;
begin
  result := 'Questionnaire';
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
    result := TFhirQuestionnaireStatus(StringArrayIndexOfSensitive(CODES_TFhirQuestionnaireStatus, FStatus.value));
end;

Procedure TFhirQuestionnaire.SetStatusST(value : TFhirQuestionnaireStatus);
begin
  if ord(value) = 0 then
    StatusObject := nil
  else
    StatusObject := TFhirEnum.create(CODES_TFhirQuestionnaireStatus[value]);
end;

Procedure TFhirQuestionnaire.SetAuthored(value : TFhirDateTime);
begin
  FAuthored.free;
  FAuthored := value;
end;

Function TFhirQuestionnaire.GetAuthoredST : TDateTimeEx;
begin
  if FAuthored = nil then
    result := nil
  else
    result := FAuthored.value;
end;

Procedure TFhirQuestionnaire.SetAuthoredST(value : TDateTimeEx);
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

Procedure TFhirQuestionnaire.SetSubject(value : TFhirResourceReference{Resource});
begin
  FSubject.free;
  FSubject := value;
end;

Procedure TFhirQuestionnaire.SetAuthor(value : TFhirResourceReference{Resource});
begin
  FAuthor.free;
  FAuthor := value;
end;

Procedure TFhirQuestionnaire.SetSource(value : TFhirResourceReference{Resource});
begin
  FSource.free;
  FSource := value;
end;

Procedure TFhirQuestionnaire.SetName(value : TFhirCodeableConcept);
begin
  FName.free;
  FName := value;
end;

Procedure TFhirQuestionnaire.SetEncounter(value : TFhirResourceReference{TFhirEncounter});
begin
  FEncounter.free;
  FEncounter := value;
end;

Procedure TFhirQuestionnaire.SetGroup(value : TFhirQuestionnaireGroup);
begin
  FGroup.free;
  FGroup := value;
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
  gender := TFhirRelatedPerson(oSource).gender.Clone;
  address := TFhirRelatedPerson(oSource).address.Clone;
  FPhotoList.Assign(TFhirRelatedPerson(oSource).FPhotoList);
end;

procedure TFhirRelatedPerson.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'identifier') Then
     list.addAll(FIdentifierList);
  if (child_name = 'patient') Then
     list.add(FPatient.Link);
  if (child_name = 'relationship') Then
     list.add(FRelationship.Link);
  if (child_name = 'name') Then
     list.add(FName.Link);
  if (child_name = 'telecom') Then
     list.addAll(FTelecomList);
  if (child_name = 'gender') Then
     list.add(FGender.Link);
  if (child_name = 'address') Then
     list.add(FAddress.Link);
  if (child_name = 'photo') Then
     list.addAll(FPhotoList);
end;

procedure TFhirRelatedPerson.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'patient', 'Resource(Patient)', FPatient.Link));{2}
  oList.add(TFHIRProperty.create(self, 'relationship', 'CodeableConcept', FRelationship.Link));{2}
  oList.add(TFHIRProperty.create(self, 'name', 'HumanName', FName.Link));{2}
  oList.add(TFHIRProperty.create(self, 'telecom', 'Contact', FTelecomList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'gender', 'CodeableConcept', FGender.Link));{2}
  oList.add(TFHIRProperty.create(self, 'address', 'Address', FAddress.Link));{2}
  oList.add(TFHIRProperty.create(self, 'photo', 'Attachment', FPhotoList.Link)){3};
end;

procedure TFhirRelatedPerson.setProperty(propName : string; propValue: TFHIRObject);
begin
  if (propName = 'identifier') then IdentifierList.add(propValue as TFhirIdentifier){2}
  else if (propName = 'patient') then Patient := propValue as TFhirResourceReference{TFhirPatient}{4b}
  else if (propName = 'relationship') then Relationship := propValue as TFhirCodeableConcept{4b}
  else if (propName = 'name') then Name := propValue as TFhirHumanName{4b}
  else if (propName = 'telecom') then TelecomList.add(propValue as TFhirContact){2}
  else if (propName = 'gender') then Gender := propValue as TFhirCodeableConcept{4b}
  else if (propName = 'address') then Address := propValue as TFhirAddress{4b}
  else if (propName = 'photo') then PhotoList.add(propValue as TFhirAttachment){2}
  else inherited;
end;

function TFhirRelatedPerson.FhirType : string;
begin
  result := 'RelatedPerson';
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

Procedure TFhirRelatedPerson.SetGender(value : TFhirCodeableConcept);
begin
  FGender.free;
  FGender := value;
end;

Procedure TFhirRelatedPerson.SetAddress(value : TFhirAddress);
begin
  FAddress.free;
  FAddress := value;
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
     list.add(FEvent.Link);
  if (child_name = 'participant') Then
     list.addAll(FParticipantList);
  if (child_name = 'source') Then
     list.add(FSource.Link);
  if (child_name = 'object') Then
     list.addAll(FObject_List);
end;

procedure TFhirSecurityEvent.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'event', '', FEvent.Link));{2}
  oList.add(TFHIRProperty.create(self, 'participant', '', FParticipantList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'source', '', FSource.Link));{2}
  oList.add(TFHIRProperty.create(self, 'object', '', FObject_List.Link)){3};
end;

procedure TFhirSecurityEvent.setProperty(propName : string; propValue: TFHIRObject);
begin
  if (propName = 'event') then Event := propValue as TFhirSecurityEventEvent{4b}
  else if (propName = 'participant') then ParticipantList.add(propValue as TFhirSecurityEventParticipant){2}
  else if (propName = 'source') then Source := propValue as TFhirSecurityEventSource{4b}
  else if (propName = 'object') then Object_List.add(propValue as TFhirSecurityEventObject){2}
  else inherited;
end;

function TFhirSecurityEvent.FhirType : string;
begin
  result := 'SecurityEvent';
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
  receivedTimeObject := TFhirSpecimen(oSource).receivedTimeObject.Clone;
  collection := TFhirSpecimen(oSource).collection.Clone;
  FTreatmentList.Assign(TFhirSpecimen(oSource).FTreatmentList);
  FContainerList.Assign(TFhirSpecimen(oSource).FContainerList);
end;

procedure TFhirSpecimen.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'identifier') Then
     list.addAll(FIdentifierList);
  if (child_name = 'type') Then
     list.add(FType_.Link);
  if (child_name = 'source') Then
     list.addAll(FSourceList);
  if (child_name = 'subject') Then
     list.add(FSubject.Link);
  if (child_name = 'accessionIdentifier') Then
     list.add(FAccessionIdentifier.Link);
  if (child_name = 'receivedTime') Then
     list.add(FReceivedTime.Link);
  if (child_name = 'collection') Then
     list.add(FCollection.Link);
  if (child_name = 'treatment') Then
     list.addAll(FTreatmentList);
  if (child_name = 'container') Then
     list.addAll(FContainerList);
end;

procedure TFhirSpecimen.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifierList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'type', 'CodeableConcept', FType_.Link));{2}
  oList.add(TFHIRProperty.create(self, 'source', '', FSourceList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'subject', 'Resource(Patient|Group|Device|Substance)', FSubject.Link));{2}
  oList.add(TFHIRProperty.create(self, 'accessionIdentifier', 'Identifier', FAccessionIdentifier.Link));{2}
  oList.add(TFHIRProperty.create(self, 'receivedTime', 'dateTime', FReceivedTime.Link));{2}
  oList.add(TFHIRProperty.create(self, 'collection', '', FCollection.Link));{2}
  oList.add(TFHIRProperty.create(self, 'treatment', '', FTreatmentList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'container', '', FContainerList.Link)){3};
end;

procedure TFhirSpecimen.setProperty(propName : string; propValue: TFHIRObject);
begin
  if (propName = 'identifier') then IdentifierList.add(propValue as TFhirIdentifier){2}
  else if (propName = 'type') then Type_ := propValue as TFhirCodeableConcept{4b}
  else if (propName = 'source') then SourceList.add(propValue as TFhirSpecimenSource){2}
  else if (propName = 'subject') then Subject := propValue as TFhirResourceReference{Resource}{4b}
  else if (propName = 'accessionIdentifier') then AccessionIdentifier := propValue as TFhirIdentifier{4b}
  else if (propName = 'receivedTime') then ReceivedTimeObject := propValue as TFhirDateTime{5a}
  else if (propName = 'collection') then Collection := propValue as TFhirSpecimenCollection{4b}
  else if (propName = 'treatment') then TreatmentList.add(propValue as TFhirSpecimenTreatment){2}
  else if (propName = 'container') then ContainerList.add(propValue as TFhirSpecimenContainer){2}
  else inherited;
end;

function TFhirSpecimen.FhirType : string;
begin
  result := 'Specimen';
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

Function TFhirSpecimen.GetReceivedTimeST : TDateTimeEx;
begin
  if FReceivedTime = nil then
    result := nil
  else
    result := FReceivedTime.value;
end;

Procedure TFhirSpecimen.SetReceivedTimeST(value : TDateTimeEx);
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
  descriptionObject := TFhirSubstance(oSource).descriptionObject.Clone;
  instance := TFhirSubstance(oSource).instance.Clone;
  FIngredientList.Assign(TFhirSubstance(oSource).FIngredientList);
end;

procedure TFhirSubstance.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'type') Then
     list.add(FType_.Link);
  if (child_name = 'description') Then
     list.add(FDescription.Link);
  if (child_name = 'instance') Then
     list.add(FInstance.Link);
  if (child_name = 'ingredient') Then
     list.addAll(FIngredientList);
end;

procedure TFhirSubstance.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'type', 'CodeableConcept', FType_.Link));{2}
  oList.add(TFHIRProperty.create(self, 'description', 'string', FDescription.Link));{2}
  oList.add(TFHIRProperty.create(self, 'instance', '', FInstance.Link));{2}
  oList.add(TFHIRProperty.create(self, 'ingredient', '', FIngredientList.Link)){3};
end;

procedure TFhirSubstance.setProperty(propName : string; propValue: TFHIRObject);
begin
  if (propName = 'type') then Type_ := propValue as TFhirCodeableConcept{4b}
  else if (propName = 'description') then DescriptionObject := propValue as TFhirString{5a}
  else if (propName = 'instance') then Instance := propValue as TFhirSubstanceInstance{4b}
  else if (propName = 'ingredient') then IngredientList.add(propValue as TFhirSubstanceIngredient){2}
  else inherited;
end;

function TFhirSubstance.FhirType : string;
begin
  result := 'Substance';
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
    result := FDescription.value;
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
     list.add(FKind.Link);
  if (child_name = 'identifier') Then
     list.add(FIdentifier.Link);
  if (child_name = 'status') Then
     list.add(FStatus.Link);
  if (child_name = 'orderedItem') Then
     list.add(FOrderedItem.Link);
  if (child_name = 'patient') Then
     list.add(FPatient.Link);
  if (child_name = 'dispense') Then
     list.addAll(FDispenseList);
end;

procedure TFhirSupply.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'kind', 'CodeableConcept', FKind.Link));{2}
  oList.add(TFHIRProperty.create(self, 'identifier', 'Identifier', FIdentifier.Link));{2}
  oList.add(TFHIRProperty.create(self, 'status', 'code', FStatus.Link));{1}
  oList.add(TFHIRProperty.create(self, 'orderedItem', 'Resource(Medication|Substance|Device)', FOrderedItem.Link));{2}
  oList.add(TFHIRProperty.create(self, 'patient', 'Resource(Patient)', FPatient.Link));{2}
  oList.add(TFHIRProperty.create(self, 'dispense', '', FDispenseList.Link)){3};
end;

procedure TFhirSupply.setProperty(propName : string; propValue: TFHIRObject);
begin
  if (propName = 'kind') then Kind := propValue as TFhirCodeableConcept{4b}
  else if (propName = 'identifier') then Identifier := propValue as TFhirIdentifier{4b}
  else if (propName = 'status') then StatusObject := propValue as TFHIREnum
  else if (propName = 'orderedItem') then OrderedItem := propValue as TFhirResourceReference{Resource}{4b}
  else if (propName = 'patient') then Patient := propValue as TFhirResourceReference{TFhirPatient}{4b}
  else if (propName = 'dispense') then DispenseList.add(propValue as TFhirSupplyDispense){2}
  else inherited;
end;

function TFhirSupply.FhirType : string;
begin
  result := 'Supply';
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
    result := TFhirValuesetSupplyStatus(StringArrayIndexOfSensitive(CODES_TFhirValuesetSupplyStatus, FStatus.value));
end;

Procedure TFhirSupply.SetStatusST(value : TFhirValuesetSupplyStatus);
begin
  if ord(value) = 0 then
    StatusObject := nil
  else
    StatusObject := TFhirEnum.create(CODES_TFhirValuesetSupplyStatus[value]);
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
  FPublisher.free;
  FTelecomList.Free;
  FDescription.free;
  FCopyright.free;
  FStatus.free;
  FExperimental.free;
  FExtensible.free;
  FDate.free;
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
  identifierObject := TFhirValueSet(oSource).identifierObject.Clone;
  versionObject := TFhirValueSet(oSource).versionObject.Clone;
  nameObject := TFhirValueSet(oSource).nameObject.Clone;
  publisherObject := TFhirValueSet(oSource).publisherObject.Clone;
  FTelecomList.Assign(TFhirValueSet(oSource).FTelecomList);
  descriptionObject := TFhirValueSet(oSource).descriptionObject.Clone;
  copyrightObject := TFhirValueSet(oSource).copyrightObject.Clone;
  FStatus := TFhirValueSet(oSource).FStatus.Link;
  experimentalObject := TFhirValueSet(oSource).experimentalObject.Clone;
  extensibleObject := TFhirValueSet(oSource).extensibleObject.Clone;
  dateObject := TFhirValueSet(oSource).dateObject.Clone;
  define := TFhirValueSet(oSource).define.Clone;
  compose := TFhirValueSet(oSource).compose.Clone;
  expansion := TFhirValueSet(oSource).expansion.Clone;
end;

procedure TFhirValueSet.GetChildrenByName(child_name : string; list : TFHIRObjectList);
begin
  inherited;
  if (child_name = 'identifier') Then
     list.add(FIdentifier.Link);
  if (child_name = 'version') Then
     list.add(FVersion.Link);
  if (child_name = 'name') Then
     list.add(FName.Link);
  if (child_name = 'publisher') Then
     list.add(FPublisher.Link);
  if (child_name = 'telecom') Then
     list.addAll(FTelecomList);
  if (child_name = 'description') Then
     list.add(FDescription.Link);
  if (child_name = 'copyright') Then
     list.add(FCopyright.Link);
  if (child_name = 'status') Then
     list.add(FStatus.Link);
  if (child_name = 'experimental') Then
     list.add(FExperimental.Link);
  if (child_name = 'extensible') Then
     list.add(FExtensible.Link);
  if (child_name = 'date') Then
     list.add(FDate.Link);
  if (child_name = 'define') Then
     list.add(FDefine.Link);
  if (child_name = 'compose') Then
     list.add(FCompose.Link);
  if (child_name = 'expansion') Then
     list.add(FExpansion.Link);
end;

procedure TFhirValueSet.ListProperties(oList: TFHIRPropertyList; bInheritedProperties: Boolean);
begin
  inherited;
  oList.add(TFHIRProperty.create(self, 'identifier', 'string', FIdentifier.Link));{2}
  oList.add(TFHIRProperty.create(self, 'version', 'string', FVersion.Link));{2}
  oList.add(TFHIRProperty.create(self, 'name', 'string', FName.Link));{2}
  oList.add(TFHIRProperty.create(self, 'publisher', 'string', FPublisher.Link));{2}
  oList.add(TFHIRProperty.create(self, 'telecom', 'Contact', FTelecomList.Link)){3};
  oList.add(TFHIRProperty.create(self, 'description', 'string', FDescription.Link));{2}
  oList.add(TFHIRProperty.create(self, 'copyright', 'string', FCopyright.Link));{2}
  oList.add(TFHIRProperty.create(self, 'status', 'code', FStatus.Link));{1}
  oList.add(TFHIRProperty.create(self, 'experimental', 'boolean', FExperimental.Link));{2}
  oList.add(TFHIRProperty.create(self, 'extensible', 'boolean', FExtensible.Link));{2}
  oList.add(TFHIRProperty.create(self, 'date', 'dateTime', FDate.Link));{2}
  oList.add(TFHIRProperty.create(self, 'define', '', FDefine.Link));{2}
  oList.add(TFHIRProperty.create(self, 'compose', '', FCompose.Link));{2}
  oList.add(TFHIRProperty.create(self, 'expansion', '', FExpansion.Link));{2}
end;

procedure TFhirValueSet.setProperty(propName : string; propValue: TFHIRObject);
begin
  if (propName = 'identifier') then IdentifierObject := propValue as TFhirString{5a}
  else if (propName = 'version') then VersionObject := propValue as TFhirString{5a}
  else if (propName = 'name') then NameObject := propValue as TFhirString{5a}
  else if (propName = 'publisher') then PublisherObject := propValue as TFhirString{5a}
  else if (propName = 'telecom') then TelecomList.add(propValue as TFhirContact){2}
  else if (propName = 'description') then DescriptionObject := propValue as TFhirString{5a}
  else if (propName = 'copyright') then CopyrightObject := propValue as TFhirString{5a}
  else if (propName = 'status') then StatusObject := propValue as TFHIREnum
  else if (propName = 'experimental') then ExperimentalObject := propValue as TFhirBoolean{5a}
  else if (propName = 'extensible') then ExtensibleObject := propValue as TFhirBoolean{5a}
  else if (propName = 'date') then DateObject := propValue as TFhirDateTime{5a}
  else if (propName = 'define') then Define := propValue as TFhirValueSetDefine{4b}
  else if (propName = 'compose') then Compose := propValue as TFhirValueSetCompose{4b}
  else if (propName = 'expansion') then Expansion := propValue as TFhirValueSetExpansion{4b}
  else inherited;
end;

function TFhirValueSet.FhirType : string;
begin
  result := 'ValueSet';
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
    result := FIdentifier.value;
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
    result := FVersion.value;
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
    result := FName.value;
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
    result := FPublisher.value;
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
    result := FDescription.value;
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
    result := FCopyright.value;
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
    result := TFhirValuesetStatus(StringArrayIndexOfSensitive(CODES_TFhirValuesetStatus, FStatus.value));
end;

Procedure TFhirValueSet.SetStatusST(value : TFhirValuesetStatus);
begin
  if ord(value) = 0 then
    StatusObject := nil
  else
    StatusObject := TFhirEnum.create(CODES_TFhirValuesetStatus[value]);
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
    result := FExperimental.value;
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
    result := FExtensible.value;
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

Function TFhirValueSet.GetDateST : TDateTimeEx;
begin
  if FDate = nil then
    result := nil
  else
    result := FDate.value;
end;

Procedure TFhirValueSet.SetDateST(value : TDateTimeEx);
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

function TFhirResourceFactory.makeDateTime(value : TDateTimeEx) : TFhirDateTime;
begin
  result := TFhirDateTime.create;
  result.value := value;
end;

function TFhirResourceFactory.newDate : TFhirDate;
begin
  result := TFhirDate.create;
end;

function TFhirResourceFactory.makeDate(value : TDateTimeEx) : TFhirDate;
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

function TFhirResourceFactory.makeInstant(value : TDateTimeEx) : TFhirInstant;
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

function TFhirResourceFactory.newConceptMapConcept : TFhirConceptMapConcept;
begin
  result := TFhirConceptMapConcept.create;
end;

function TFhirResourceFactory.newConceptMapConceptDependsOn : TFhirConceptMapConceptDependsOn;
begin
  result := TFhirConceptMapConceptDependsOn.create;
end;

function TFhirResourceFactory.newConceptMapConceptMap : TFhirConceptMapConceptMap;
begin
  result := TFhirConceptMapConceptMap.create;
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

function TFhirResourceFactory.newConformanceRestResourceOperation : TFhirConformanceRestResourceOperation;
begin
  result := TFhirConformanceRestResourceOperation.create;
end;

function TFhirResourceFactory.newConformanceRestResourceSearchParam : TFhirConformanceRestResourceSearchParam;
begin
  result := TFhirConformanceRestResourceSearchParam.create;
end;

function TFhirResourceFactory.newConformanceRestOperation : TFhirConformanceRestOperation;
begin
  result := TFhirConformanceRestOperation.create;
end;

function TFhirResourceFactory.newConformanceRestQuery : TFhirConformanceRestQuery;
begin
  result := TFhirConformanceRestQuery.create;
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

function TFhirResourceFactory.newProfileStructureElement : TFhirProfileStructureElement;
begin
  result := TFhirProfileStructureElement.create;
end;

function TFhirResourceFactory.newProfileStructureElementSlicing : TFhirProfileStructureElementSlicing;
begin
  result := TFhirProfileStructureElementSlicing.create;
end;

function TFhirResourceFactory.newProfileStructureElementDefinition : TFhirProfileStructureElementDefinition;
begin
  result := TFhirProfileStructureElementDefinition.create;
end;

function TFhirResourceFactory.newProfileStructureElementDefinitionType : TFhirProfileStructureElementDefinitionType;
begin
  result := TFhirProfileStructureElementDefinitionType.create;
end;

function TFhirResourceFactory.newProfileStructureElementDefinitionConstraint : TFhirProfileStructureElementDefinitionConstraint;
begin
  result := TFhirProfileStructureElementDefinitionConstraint.create;
end;

function TFhirResourceFactory.newProfileStructureElementDefinitionBinding : TFhirProfileStructureElementDefinitionBinding;
begin
  result := TFhirProfileStructureElementDefinitionBinding.create;
end;

function TFhirResourceFactory.newProfileStructureElementDefinitionMapping : TFhirProfileStructureElementDefinitionMapping;
begin
  result := TFhirProfileStructureElementDefinitionMapping.create;
end;

function TFhirResourceFactory.newProfileStructureSearchParam : TFhirProfileStructureSearchParam;
begin
  result := TFhirProfileStructureSearchParam.create;
end;

function TFhirResourceFactory.newProfileExtensionDefn : TFhirProfileExtensionDefn;
begin
  result := TFhirProfileExtensionDefn.create;
end;

function TFhirResourceFactory.newProfileQuery : TFhirProfileQuery;
begin
  result := TFhirProfileQuery.create;
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

function TFhirResourceFactory.newRelatedPerson : TFhirRelatedPerson;
begin
  result := TFhirRelatedPerson.create;
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


function TFHIRResourceFactory.makeByName(const name : String) : TFHIRElement;
begin
  if name = 'Enum' then
    result := newEnum()
  else if name = 'Integer' then
    result := newInteger()
  else if name = 'DateTime' then
    result := newDateTime()
  else if name = 'Date' then
    result := newDate()
  else if name = 'Decimal' then
    result := newDecimal()
  else if name = 'Uri' then
    result := newUri()
  else if name = 'Base64Binary' then
    result := newBase64Binary()
  else if name = 'String' then
    result := newString()
  else if name = 'Boolean' then
    result := newBoolean()
  else if name = 'Instant' then
    result := newInstant()
  else if name = 'Code' then
    result := newCode()
  else if name = 'Id' then
    result := newId()
  else if name = 'Oid' then
    result := newOid()
  else if name = 'Uuid' then
    result := newUuid()
  else if name = 'Extension' then
    result := newExtension()
  else if name = 'Narrative' then
    result := newNarrative()
  else if name = 'Period' then
    result := newPeriod()
  else if name = 'Coding' then
    result := newCoding()
  else if name = 'Range' then
    result := newRange()
  else if name = 'Quantity' then
    result := newQuantity()
  else if name = 'Attachment' then
    result := newAttachment()
  else if name = 'Ratio' then
    result := newRatio()
  else if name = 'SampledData' then
    result := newSampledData()
  else if name = 'ResourceReference' then
    result := newResourceReference()
  else if name = 'CodeableConcept' then
    result := newCodeableConcept()
  else if name = 'Identifier' then
    result := newIdentifier()
  else if name = 'Schedule.repeat' then
    result := newScheduleRepeat()
  else if name = 'Schedule' then
    result := newSchedule()
  else if name = 'Contact' then
    result := newContact()
  else if name = 'Address' then
    result := newAddress()
  else if name = 'HumanName' then
    result := newHumanName()
  else if name = 'AdverseReaction.symptom' then
    result := newAdverseReactionSymptom()
  else if name = 'AdverseReaction.exposure' then
    result := newAdverseReactionExposure()
  else if name = 'AdverseReaction' then
    result := newAdverseReaction()
  else if name = 'Alert' then
    result := newAlert()
  else if name = 'AllergyIntolerance' then
    result := newAllergyIntolerance()
  else if name = 'CarePlan.participant' then
    result := newCarePlanParticipant()
  else if name = 'CarePlan.goal' then
    result := newCarePlanGoal()
  else if name = 'CarePlan.activity' then
    result := newCarePlanActivity()
  else if name = 'CarePlan.activity.simple' then
    result := newCarePlanActivitySimple()
  else if name = 'CarePlan' then
    result := newCarePlan()
  else if name = 'Composition.attester' then
    result := newCompositionAttester()
  else if name = 'Composition.event' then
    result := newCompositionEvent()
  else if name = 'Composition.section' then
    result := newCompositionSection()
  else if name = 'Composition' then
    result := newComposition()
  else if name = 'ConceptMap.concept' then
    result := newConceptMapConcept()
  else if name = 'ConceptMap.concept.dependsOn' then
    result := newConceptMapConceptDependsOn()
  else if name = 'ConceptMap.concept.map' then
    result := newConceptMapConceptMap()
  else if name = 'ConceptMap' then
    result := newConceptMap()
  else if name = 'Condition.stage' then
    result := newConditionStage()
  else if name = 'Condition.evidence' then
    result := newConditionEvidence()
  else if name = 'Condition.location' then
    result := newConditionLocation()
  else if name = 'Condition.relatedItem' then
    result := newConditionRelatedItem()
  else if name = 'Condition' then
    result := newCondition()
  else if name = 'Conformance.software' then
    result := newConformanceSoftware()
  else if name = 'Conformance.implementation' then
    result := newConformanceImplementation()
  else if name = 'Conformance.rest' then
    result := newConformanceRest()
  else if name = 'Conformance.rest.security' then
    result := newConformanceRestSecurity()
  else if name = 'Conformance.rest.security.certificate' then
    result := newConformanceRestSecurityCertificate()
  else if name = 'Conformance.rest.resource' then
    result := newConformanceRestResource()
  else if name = 'Conformance.rest.resource.operation' then
    result := newConformanceRestResourceOperation()
  else if name = 'Conformance.rest.resource.searchParam' then
    result := newConformanceRestResourceSearchParam()
  else if name = 'Conformance.rest.operation' then
    result := newConformanceRestOperation()
  else if name = 'Conformance.rest.query' then
    result := newConformanceRestQuery()
  else if name = 'Conformance.messaging' then
    result := newConformanceMessaging()
  else if name = 'Conformance.messaging.event' then
    result := newConformanceMessagingEvent()
  else if name = 'Conformance.document' then
    result := newConformanceDocument()
  else if name = 'Conformance' then
    result := newConformance()
  else if name = 'Device' then
    result := newDevice()
  else if name = 'DeviceObservationReport.virtualDevice' then
    result := newDeviceObservationReportVirtualDevice()
  else if name = 'DeviceObservationReport.virtualDevice.channel' then
    result := newDeviceObservationReportVirtualDeviceChannel()
  else if name = 'DeviceObservationReport.virtualDevice.channel.metric' then
    result := newDeviceObservationReportVirtualDeviceChannelMetric()
  else if name = 'DeviceObservationReport' then
    result := newDeviceObservationReport()
  else if name = 'DiagnosticOrder.event' then
    result := newDiagnosticOrderEvent()
  else if name = 'DiagnosticOrder.item' then
    result := newDiagnosticOrderItem()
  else if name = 'DiagnosticOrder' then
    result := newDiagnosticOrder()
  else if name = 'DiagnosticReport.image' then
    result := newDiagnosticReportImage()
  else if name = 'DiagnosticReport' then
    result := newDiagnosticReport()
  else if name = 'DocumentManifest' then
    result := newDocumentManifest()
  else if name = 'DocumentReference.relatesTo' then
    result := newDocumentReferenceRelatesTo()
  else if name = 'DocumentReference.service' then
    result := newDocumentReferenceService()
  else if name = 'DocumentReference.service.parameter' then
    result := newDocumentReferenceServiceParameter()
  else if name = 'DocumentReference.context' then
    result := newDocumentReferenceContext()
  else if name = 'DocumentReference' then
    result := newDocumentReference()
  else if name = 'Encounter.participant' then
    result := newEncounterParticipant()
  else if name = 'Encounter.hospitalization' then
    result := newEncounterHospitalization()
  else if name = 'Encounter.hospitalization.accomodation' then
    result := newEncounterHospitalizationAccomodation()
  else if name = 'Encounter.location' then
    result := newEncounterLocation()
  else if name = 'Encounter' then
    result := newEncounter()
  else if name = 'FamilyHistory.relation' then
    result := newFamilyHistoryRelation()
  else if name = 'FamilyHistory.relation.condition' then
    result := newFamilyHistoryRelationCondition()
  else if name = 'FamilyHistory' then
    result := newFamilyHistory()
  else if name = 'Group.characteristic' then
    result := newGroupCharacteristic()
  else if name = 'Group' then
    result := newGroup()
  else if name = 'ImagingStudy.series' then
    result := newImagingStudySeries()
  else if name = 'ImagingStudy.series.instance' then
    result := newImagingStudySeriesInstance()
  else if name = 'ImagingStudy' then
    result := newImagingStudy()
  else if name = 'Immunization.explanation' then
    result := newImmunizationExplanation()
  else if name = 'Immunization.reaction' then
    result := newImmunizationReaction()
  else if name = 'Immunization.vaccinationProtocol' then
    result := newImmunizationVaccinationProtocol()
  else if name = 'Immunization' then
    result := newImmunization()
  else if name = 'ImmunizationRecommendation.recommendation' then
    result := newImmunizationRecommendationRecommendation()
  else if name = 'ImmunizationRecommendation.recommendation.dateCriterion' then
    result := newImmunizationRecommendationRecommendationDateCriterion()
  else if name = 'ImmunizationRecommendation.recommendation.protocol' then
    result := newImmunizationRecommendationRecommendationProtocol()
  else if name = 'ImmunizationRecommendation' then
    result := newImmunizationRecommendation()
  else if name = 'List.entry' then
    result := newListEntry()
  else if name = 'List' then
    result := newList()
  else if name = 'Location.position' then
    result := newLocationPosition()
  else if name = 'Location' then
    result := newLocation()
  else if name = 'Media' then
    result := newMedia()
  else if name = 'Medication.product' then
    result := newMedicationProduct()
  else if name = 'Medication.product.ingredient' then
    result := newMedicationProductIngredient()
  else if name = 'Medication.package' then
    result := newMedicationPackage()
  else if name = 'Medication.package.content' then
    result := newMedicationPackageContent()
  else if name = 'Medication' then
    result := newMedication()
  else if name = 'MedicationAdministration.dosage' then
    result := newMedicationAdministrationDosage()
  else if name = 'MedicationAdministration' then
    result := newMedicationAdministration()
  else if name = 'MedicationDispense.dispense' then
    result := newMedicationDispenseDispense()
  else if name = 'MedicationDispense.dispense.dosage' then
    result := newMedicationDispenseDispenseDosage()
  else if name = 'MedicationDispense.substitution' then
    result := newMedicationDispenseSubstitution()
  else if name = 'MedicationDispense' then
    result := newMedicationDispense()
  else if name = 'MedicationPrescription.dosageInstruction' then
    result := newMedicationPrescriptionDosageInstruction()
  else if name = 'MedicationPrescription.dispense' then
    result := newMedicationPrescriptionDispense()
  else if name = 'MedicationPrescription.substitution' then
    result := newMedicationPrescriptionSubstitution()
  else if name = 'MedicationPrescription' then
    result := newMedicationPrescription()
  else if name = 'MedicationStatement.dosage' then
    result := newMedicationStatementDosage()
  else if name = 'MedicationStatement' then
    result := newMedicationStatement()
  else if name = 'MessageHeader.response' then
    result := newMessageHeaderResponse()
  else if name = 'MessageHeader.source' then
    result := newMessageHeaderSource()
  else if name = 'MessageHeader.destination' then
    result := newMessageHeaderDestination()
  else if name = 'MessageHeader' then
    result := newMessageHeader()
  else if name = 'Observation.referenceRange' then
    result := newObservationReferenceRange()
  else if name = 'Observation.related' then
    result := newObservationRelated()
  else if name = 'Observation' then
    result := newObservation()
  else if name = 'OperationOutcome.issue' then
    result := newOperationOutcomeIssue()
  else if name = 'OperationOutcome' then
    result := newOperationOutcome()
  else if name = 'Order.when' then
    result := newOrderWhen()
  else if name = 'Order' then
    result := newOrder()
  else if name = 'OrderResponse' then
    result := newOrderResponse()
  else if name = 'Organization.contact' then
    result := newOrganizationContact()
  else if name = 'Organization' then
    result := newOrganization()
  else if name = 'Other' then
    result := newOther()
  else if name = 'Patient.contact' then
    result := newPatientContact()
  else if name = 'Patient.animal' then
    result := newPatientAnimal()
  else if name = 'Patient.link' then
    result := newPatientLink()
  else if name = 'Patient' then
    result := newPatient()
  else if name = 'Practitioner.qualification' then
    result := newPractitionerQualification()
  else if name = 'Practitioner' then
    result := newPractitioner()
  else if name = 'Procedure.performer' then
    result := newProcedurePerformer()
  else if name = 'Procedure.relatedItem' then
    result := newProcedureRelatedItem()
  else if name = 'Procedure' then
    result := newProcedure()
  else if name = 'Profile.mapping' then
    result := newProfileMapping()
  else if name = 'Profile.structure' then
    result := newProfileStructure()
  else if name = 'Profile.structure.element' then
    result := newProfileStructureElement()
  else if name = 'Profile.structure.element.slicing' then
    result := newProfileStructureElementSlicing()
  else if name = 'Profile.structure.element.definition' then
    result := newProfileStructureElementDefinition()
  else if name = 'Profile.structure.element.definition.type' then
    result := newProfileStructureElementDefinitionType()
  else if name = 'Profile.structure.element.definition.constraint' then
    result := newProfileStructureElementDefinitionConstraint()
  else if name = 'Profile.structure.element.definition.binding' then
    result := newProfileStructureElementDefinitionBinding()
  else if name = 'Profile.structure.element.definition.mapping' then
    result := newProfileStructureElementDefinitionMapping()
  else if name = 'Profile.structure.searchParam' then
    result := newProfileStructureSearchParam()
  else if name = 'Profile.extensionDefn' then
    result := newProfileExtensionDefn()
  else if name = 'Profile.query' then
    result := newProfileQuery()
  else if name = 'Profile' then
    result := newProfile()
  else if name = 'Provenance.agent' then
    result := newProvenanceAgent()
  else if name = 'Provenance.entity' then
    result := newProvenanceEntity()
  else if name = 'Provenance' then
    result := newProvenance()
  else if name = 'Query.response' then
    result := newQueryResponse()
  else if name = 'Query' then
    result := newQuery()
  else if name = 'Questionnaire.group' then
    result := newQuestionnaireGroup()
  else if name = 'Questionnaire.group.question' then
    result := newQuestionnaireGroupQuestion()
  else if name = 'Questionnaire' then
    result := newQuestionnaire()
  else if name = 'RelatedPerson' then
    result := newRelatedPerson()
  else if name = 'SecurityEvent.event' then
    result := newSecurityEventEvent()
  else if name = 'SecurityEvent.participant' then
    result := newSecurityEventParticipant()
  else if name = 'SecurityEvent.participant.network' then
    result := newSecurityEventParticipantNetwork()
  else if name = 'SecurityEvent.source' then
    result := newSecurityEventSource()
  else if name = 'SecurityEvent.object' then
    result := newSecurityEventObject()
  else if name = 'SecurityEvent.object.detail' then
    result := newSecurityEventObjectDetail()
  else if name = 'SecurityEvent' then
    result := newSecurityEvent()
  else if name = 'Specimen.source' then
    result := newSpecimenSource()
  else if name = 'Specimen.collection' then
    result := newSpecimenCollection()
  else if name = 'Specimen.treatment' then
    result := newSpecimenTreatment()
  else if name = 'Specimen.container' then
    result := newSpecimenContainer()
  else if name = 'Specimen' then
    result := newSpecimen()
  else if name = 'Substance.instance' then
    result := newSubstanceInstance()
  else if name = 'Substance.ingredient' then
    result := newSubstanceIngredient()
  else if name = 'Substance' then
    result := newSubstance()
  else if name = 'Supply.dispense' then
    result := newSupplyDispense()
  else if name = 'Supply' then
    result := newSupply()
  else if name = 'ValueSet.define' then
    result := newValueSetDefine()
  else if name = 'ValueSet.define.concept' then
    result := newValueSetDefineConcept()
  else if name = 'ValueSet.compose' then
    result := newValueSetCompose()
  else if name = 'ValueSet.compose.include' then
    result := newValueSetComposeInclude()
  else if name = 'ValueSet.compose.include.filter' then
    result := newValueSetComposeIncludeFilter()
  else if name = 'ValueSet.expansion' then
    result := newValueSetExpansion()
  else if name = 'ValueSet.expansion.contains' then
    result := newValueSetExpansionContains()
  else if name = 'ValueSet' then
    result := newValueSet()
  else
    result := nil;
end;


end.

