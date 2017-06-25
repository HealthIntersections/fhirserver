{!Wrapper uses FHIRBase, FHIRBase_Wrapper, FHIRTypes, FHIRTypes_Wrapper, FHIRComponents, FHIRComponents_Wrapper, FHIRResources, FHIRResources_Wrapper}
{!ignore ALL_RESOURCE_TYPES}

unit FHIRConstants;

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
  SysUtils, Classes, StringSupport, DecimalSupport, AdvBuffers,  FHIRBase, FHIRTypes, FHIRComponents, FHIRResources;

Const
  CODES_TFhirResourceType : Array[TFhirResourceType] of String = ('', 'AdverseReaction', 'Alert', 'AllergyIntolerance', 'CarePlan', 'Composition', 'ConceptMap', 'Condition', 'Conformance', 'Device', 'DeviceObservationReport', 'DiagnosticOrder', 'DiagnosticReport', 'DocumentManifest', 'DocumentReference', 'Encounter', 'FamilyHistory', 'Group', 'ImagingStudy', 'Immunization', 'ImmunizationRecommendation', 'List', 'Location', 'Media', 'Medication', 'MedicationAdministration', 'MedicationDispense', 'MedicationPrescription', 'MedicationStatement', 'MessageHeader', 'Observation', 'OperationOutcome', 'Order', 'OrderResponse', 'Organization', 'Other', 'Patient', 'Practitioner', 'Procedure', 'Profile', 'Provenance', 'Query', 'Questionnaire', 'RelatedPerson', 'SecurityEvent', 'Specimen', 'Substance', 'Supply', 'ValueSet', 'Binary');
  PLURAL_CODES_TFhirResourceType : Array[TFhirResourceType] of String = ('', 'adversereactions',
     'alerts',
     'allergyintolerances',
     'careplans',
     'compositions',
     'conceptmaps',
     'conditions',
     'conformances',
     'devices',
     'deviceobservationreports',
     'diagnosticorders',
     'diagnosticreports',
     'documentmanifests',
     'documentreferences',
     'encounters',
     'familyhistories',
     'groups',
     'imagingstudies',
     'immunizations',
     'immunizationrecommendations',
     'lists',
     'locations',
     'media',
     'medications',
     'medicationadministrations',
     'medicationdispenses',
     'medicationprescriptions',
     'medicationstatements',
     'messageheaders',
     'observations',
     'operationoutcomes',
     'orders',
     'orderresponses',
     'organizations',
     'others',
     'patients',
     'practitioners',
     'procedures',
     'profiles',
     'provenances',
     'queries',
     'questionnaires',
     'relatedpeople',
     'securityevents',
     'specimen',
     'substances',
     'supplies',
     'valuesets',
     'binaries');
  LOWERCASE_CODES_TFhirResourceType : Array[TFhirResourceType] of String = ('', 'adversereaction',
     'alert',
     'allergyintolerance',
     'careplan',
     'composition',
     'conceptmap',
     'condition',
     'conformance',
     'device',
     'deviceobservationreport',
     'diagnosticorder',
     'diagnosticreport',
     'documentmanifest',
     'documentreference',
     'encounter',
     'familyhistory',
     'group',
     'imagingstudy',
     'immunization',
     'immunizationrecommendation',
     'list',
     'location',
     'media',
     'medication',
     'medicationadministration',
     'medicationdispense',
     'medicationprescription',
     'medicationstatement',
     'messageheader',
     'observation',
     'operationoutcome',
     'order',
     'orderresponse',
     'organization',
     'other',
     'patient',
     'practitioner',
     'procedure',
     'profile',
     'provenance',
     'query',
     'questionnaire',
     'relatedperson',
     'securityevent',
     'specimen',
     'substance',
     'supply',
     'valueset',
     'binary');
  CLASSES_TFhirResourceType : Array[TFhirResourceType] of TFhirResourceClass = (nil, TFhirAdverseReaction,
     TFhirAlert,
     TFhirAllergyIntolerance,
     TFhirCarePlan,
     TFhirComposition,
     TFhirConceptMap,
     TFhirCondition,
     TFhirConformance,
     TFhirDevice,
     TFhirDeviceObservationReport,
     TFhirDiagnosticOrder,
     TFhirDiagnosticReport,
     TFhirDocumentManifest,
     TFhirDocumentReference,
     TFhirEncounter,
     TFhirFamilyHistory,
     TFhirGroup,
     TFhirImagingStudy,
     TFhirImmunization,
     TFhirImmunizationRecommendation,
     TFhirList,
     TFhirLocation,
     TFhirMedia,
     TFhirMedication,
     TFhirMedicationAdministration,
     TFhirMedicationDispense,
     TFhirMedicationPrescription,
     TFhirMedicationStatement,
     TFhirMessageHeader,
     TFhirObservation,
     TFhirOperationOutcome,
     TFhirOrder,
     TFhirOrderResponse,
     TFhirOrganization,
     TFhirOther,
     TFhirPatient,
     TFhirPractitioner,
     TFhirProcedure,
     TFhirProfile,
     TFhirProvenance,
     TFhirQuery,
     TFhirQuestionnaire,
     TFhirRelatedPerson,
     TFhirSecurityEvent,
     TFhirSpecimen,
     TFhirSubstance,
     TFhirSupply,
     TFhirValueSet,
     TFhirBinary);
  ALL_RESOURCE_TYPES = [frtAdverseReaction,
     frtAlert,
     frtAllergyIntolerance,
     frtCarePlan,
     frtComposition,
     frtConceptMap,
     frtCondition,
     frtConformance,
     frtDevice,
     frtDeviceObservationReport,
     frtDiagnosticOrder,
     frtDiagnosticReport,
     frtDocumentManifest,
     frtDocumentReference,
     frtEncounter,
     frtFamilyHistory,
     frtGroup,
     frtImagingStudy,
     frtImmunization,
     frtImmunizationRecommendation,
     frtList,
     frtLocation,
     frtMedia,
     frtMedication,
     frtMedicationAdministration,
     frtMedicationDispense,
     frtMedicationPrescription,
     frtMedicationStatement,
     frtMessageHeader,
     frtObservation,
     frtOperationOutcome,
     frtOrder,
     frtOrderResponse,
     frtOrganization,
     frtOther,
     frtPatient,
     frtPractitioner,
     frtProcedure,
     frtProfile,
     frtProvenance,
     frtQuery,
     frtQuestionnaire,
     frtRelatedPerson,
     frtSecurityEvent,
     frtSpecimen,
     frtSubstance,
     frtSupply,
     frtValueSet,
     frtBinary];


  COMPARTMENT_PARAM_NAMES : Array[TFhirResourceType, TFhirResourceType] of String = (('', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''),
     ('', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''),
     ('', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''),
     ('', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''),
     ('', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''),
     ('', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''),
     ('', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''),
     ('', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''),
     ('', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''),
     ('', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''),
     ('', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''),
     ('', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''),
     ('', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''),
     ('', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''),
     ('', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''),
     ('', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''),
     ('', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''),
     ('', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''),
     ('', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''),
     ('', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''),
     ('', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''),
     ('', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''),
     ('', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''),
     ('', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''),
     ('', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''),
     ('', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''),
     ('', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''),
     ('', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''),
     ('', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''),
     ('', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''),
     ('', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''),
     ('', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''),
     ('', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''),
     ('', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''),
     ('', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''),
     ('', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''),
     ('', 'subject', 'subject', 'subject', 'subject', 'subject', '', 'subject', '', '', 'subject', 'subject', 'subject', 'subject', 'subject', 'subject', 'subject', '', 'subject', 'subject', 'subject', 'subject', '', 'subject', '', 'patient', 'patient', 'patient', 'patient', '', 'subject', '', 'subject', 'request.subject', '', 'patient', '{def}', '', 'subject', '', 'target.subject | target.patient', '', 'subject', 'patient', 'reference.subject | reference.patient', 'subject', '', 'patient', '', ''),
     ('', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''),
     ('', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''),
     ('', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''),
     ('', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''),
     ('', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''),
     ('', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''),
     ('', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''),
     ('', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''),
     ('', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''),
     ('', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''),
     ('', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''),
     ('', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''),
     ('', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', '', ''));

  CODES_TSearchParamsAdverseReaction : Array[TSearchParamsAdverseReaction] of String = ('_id', '_language', 'date', 'subject', 'substance', 'symptom');
  DESC_TSearchParamsAdverseReaction : Array[TSearchParamsAdverseReaction] of String = ('The logical resource id associated with the resource (must be supported by all servers)',
     'The language of the resource',
     'The date of the reaction',
     'The subject that the sensitivity is about',
     'The name or code of the substance that produces the sensitivity',
     'One of the symptoms of the reaction');
  TYPES_TSearchParamsAdverseReaction : Array[TSearchParamsAdverseReaction] of TFhirSearchParamType = ( SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeDate,  SearchParamTypeReference,  SearchParamTypeReference,  SearchParamTypeToken);
//  CHECK_TSearchParamsAdverseReaction : Array[TSearchParamsAdverseReaction] of TSearchParamsAdverseReaction = ( spAdverseReaction__id,  spAdverseReaction__language,  spAdverseReaction_Date,  spAdverseReaction_Subject,  spAdverseReaction_Substance,  spAdverseReaction_Symptom);
  PATHS_TSearchParamsAdverseReaction : Array[TSearchParamsAdverseReaction] of String = ('_id: []',
     '_language: []',
     'date: []',
     'subject: [frtPatient]',
     'substance: [frtSubstance]',
     'symptom: []');
  TARGETS_TSearchParamsAdverseReaction : Array[TSearchParamsAdverseReaction] of TFhirResourceTypeSet = ([], [], [], [frtPatient], [frtSubstance], []);
  CODES_TSearchParamsAlert : Array[TSearchParamsAlert] of String = ('_id', '_language', 'subject');
  DESC_TSearchParamsAlert : Array[TSearchParamsAlert] of String = ('The logical resource id associated with the resource (must be supported by all servers)',
     'The language of the resource',
     'The identity of a subject to list alerts for');
  TYPES_TSearchParamsAlert : Array[TSearchParamsAlert] of TFhirSearchParamType = ( SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeReference);
//  CHECK_TSearchParamsAlert : Array[TSearchParamsAlert] of TSearchParamsAlert = ( spAlert__id,  spAlert__language,  spAlert_Subject);
  PATHS_TSearchParamsAlert : Array[TSearchParamsAlert] of String = ('_id: []',
     '_language: []',
     'subject: [frtPatient]');
  TARGETS_TSearchParamsAlert : Array[TSearchParamsAlert] of TFhirResourceTypeSet = ([], [], [frtPatient]);
  CODES_TSearchParamsAllergyIntolerance : Array[TSearchParamsAllergyIntolerance] of String = ('_id', '_language', 'date', 'recorder', 'status', 'subject', 'substance', 'type');
  DESC_TSearchParamsAllergyIntolerance : Array[TSearchParamsAllergyIntolerance] of String = ('The logical resource id associated with the resource (must be supported by all servers)',
     'The language of the resource',
     'Recorded date/time.',
     'Who recorded the sensitivity',
     'The status of the sensitivity',
     'The subject that the sensitivity is about',
     'The name or code of the substance that produces the sensitivity',
     'The type of sensitivity');
  TYPES_TSearchParamsAllergyIntolerance : Array[TSearchParamsAllergyIntolerance] of TFhirSearchParamType = ( SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeDate,  SearchParamTypeReference,  SearchParamTypeToken,  SearchParamTypeReference,  SearchParamTypeReference,  SearchParamTypeToken);
//  CHECK_TSearchParamsAllergyIntolerance : Array[TSearchParamsAllergyIntolerance] of TSearchParamsAllergyIntolerance = ( spAllergyIntolerance__id,  spAllergyIntolerance__language,  spAllergyIntolerance_Date,  spAllergyIntolerance_Recorder,  spAllergyIntolerance_Status,  spAllergyIntolerance_Subject,  spAllergyIntolerance_Substance,  spAllergyIntolerance_Type);
  PATHS_TSearchParamsAllergyIntolerance : Array[TSearchParamsAllergyIntolerance] of String = ('_id: []',
     '_language: []',
     'date: []',
     'recorder: [frtPatient, frtPractitioner]',
     'status: []',
     'subject: [frtPatient]',
     'substance: [frtSubstance]',
     'type: []');
  TARGETS_TSearchParamsAllergyIntolerance : Array[TSearchParamsAllergyIntolerance] of TFhirResourceTypeSet = ([], [], [], [frtPatient, frtPractitioner], [], [frtPatient], [frtSubstance], []);
  CODES_TSearchParamsCarePlan : Array[TSearchParamsCarePlan] of String = ('_id', '_language', 'activitycode', 'activitydate', 'activitydetail', 'condition', 'date', 'participant', 'patient');
  DESC_TSearchParamsCarePlan : Array[TSearchParamsCarePlan] of String = ('The logical resource id associated with the resource (must be supported by all servers)',
     'The language of the resource',
     'Detail type of activity',
     'Specified date occurs within period specified by CarePlan.activity.timingSchedule',
     'Activity details defined in specific resource',
     'Health issues this plan addresses',
     'Time period plan covers',
     'Who is involved',
     'Who care plan is for');
  TYPES_TSearchParamsCarePlan : Array[TSearchParamsCarePlan] of TFhirSearchParamType = ( SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeDate,  SearchParamTypeReference,  SearchParamTypeReference,  SearchParamTypeDate,  SearchParamTypeReference,  SearchParamTypeReference);
//  CHECK_TSearchParamsCarePlan : Array[TSearchParamsCarePlan] of TSearchParamsCarePlan = ( spCarePlan__id,  spCarePlan__language,  spCarePlan_Activitycode,  spCarePlan_Activitydate,  spCarePlan_Activitydetail,  spCarePlan_Condition,  spCarePlan_Date,  spCarePlan_Participant,  spCarePlan_Patient);
  PATHS_TSearchParamsCarePlan : Array[TSearchParamsCarePlan] of String = ('_id: []',
     '_language: []',
     'activitycode: []',
     'activitydate: []',
     'activitydetail: [frtMedicationPrescription, frtEncounter, frtProcedure, frtDiagnosticOrder]',
     'condition: [frtCondition]',
     'date: []',
     'participant: [frtOrganization, frtPatient, frtPractitioner, frtRelatedPerson]',
     'patient: [frtPatient]');
  TARGETS_TSearchParamsCarePlan : Array[TSearchParamsCarePlan] of TFhirResourceTypeSet = ([], [], [], [], [frtMedicationPrescription, frtEncounter, frtProcedure, frtDiagnosticOrder], [frtCondition], [], [frtOrganization, frtPatient, frtPractitioner, frtRelatedPerson], [frtPatient]);
  CODES_TSearchParamsComposition : Array[TSearchParamsComposition] of String = ('_id', '_language', 'attester', 'author', 'class', 'context', 'date', 'identifier', 'section-content', 'section-type', 'subject', 'type');
  DESC_TSearchParamsComposition : Array[TSearchParamsComposition] of String = ('The logical resource id associated with the resource (must be supported by all servers)',
     'The language of the resource',
     'Who attested the composition',
     'Who and/or what authored the composition',
     'Categorization of Composition',
     'Code(s) that apply to the event being documented',
     'Composition editing time',
     'Logical identifier of composition (version-independent)',
     'The actual data for the section',
     'Classification of section (recommended)',
     'Who and/or what the composition is about',
     'Kind of composition (LOINC if possible)');
  TYPES_TSearchParamsComposition : Array[TSearchParamsComposition] of TFhirSearchParamType = ( SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeReference,  SearchParamTypeReference,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeDate,  SearchParamTypeToken,  SearchParamTypeReference,  SearchParamTypeToken,  SearchParamTypeReference,  SearchParamTypeToken);
//  CHECK_TSearchParamsComposition : Array[TSearchParamsComposition] of TSearchParamsComposition = ( spComposition__id,  spComposition__language,  spComposition_Attester,  spComposition_Author,  spComposition_Class,  spComposition_Context,  spComposition_Date,  spComposition_Identifier,  spComposition_Section_content,  spComposition_Section_type,  spComposition_Subject,  spComposition_Type);
  PATHS_TSearchParamsComposition : Array[TSearchParamsComposition] of String = ('_id: []',
     '_language: []',
     'attester: [frtOrganization, frtPatient, frtPractitioner]',
     'author: [frtDevice, frtPatient, frtPractitioner, frtRelatedPerson]',
     'class: []',
     'context: []',
     'date: []',
     'identifier: []',
     'section-content: ALL_RESOURCE_TYPES',
     'section-type: []',
     'subject: [frtDevice, frtLocation, frtPatient, frtPractitioner, frtGroup]',
     'type: []');
  TARGETS_TSearchParamsComposition : Array[TSearchParamsComposition] of TFhirResourceTypeSet = ([], [], [frtOrganization, frtPatient, frtPractitioner], [frtDevice, frtPatient, frtPractitioner, frtRelatedPerson], [], [], [], [], ALL_RESOURCE_TYPES, [], [frtDevice, frtLocation, frtPatient, frtPractitioner, frtGroup], []);
  CODES_TSearchParamsConceptMap : Array[TSearchParamsConceptMap] of String = ('_id', '_language', 'date', 'dependson', 'description', 'identifier', 'name', 'product', 'publisher', 'source', 'status', 'system', 'target', 'version');
  DESC_TSearchParamsConceptMap : Array[TSearchParamsConceptMap] of String = ('The logical resource id associated with the resource (must be supported by all servers)',
     'The language of the resource',
     'The concept map publication date',
     'Reference to element/field/valueset provides the context',
     'Text search in the description of the concept map',
     'The identifier of the concept map',
     'Name of the concept map',
     'Reference to element/field/valueset provides the context',
     'Name of the publisher of the concept map',
     'The system for any concepts mapped by this concept map',
     'Status of the concept map',
     'The system for any destination concepts mapped by this map',
     'Provides context to the mappings',
     'The version identifier of the concept map');
  TYPES_TSearchParamsConceptMap : Array[TSearchParamsConceptMap] of TFhirSearchParamType = ( SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeDate,  SearchParamTypeToken,  SearchParamTypeString,  SearchParamTypeToken,  SearchParamTypeString,  SearchParamTypeToken,  SearchParamTypeString,  SearchParamTypeReference,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeReference,  SearchParamTypeToken);
//  CHECK_TSearchParamsConceptMap : Array[TSearchParamsConceptMap] of TSearchParamsConceptMap = ( spConceptMap__id,  spConceptMap__language,  spConceptMap_Date,  spConceptMap_Dependson,  spConceptMap_Description,  spConceptMap_Identifier,  spConceptMap_Name,  spConceptMap_Product,  spConceptMap_Publisher,  spConceptMap_Source,  spConceptMap_Status,  spConceptMap_System,  spConceptMap_Target,  spConceptMap_Version);
  PATHS_TSearchParamsConceptMap : Array[TSearchParamsConceptMap] of String = ('_id: []',
     '_language: []',
     'date: []',
     'dependson: []',
     'description: []',
     'identifier: []',
     'name: []',
     'product: []',
     'publisher: []',
     'source: [frtValueSet]',
     'status: []',
     'system: []',
     'target: [frtValueSet]',
     'version: []');
  TARGETS_TSearchParamsConceptMap : Array[TSearchParamsConceptMap] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [frtValueSet], [], [], [frtValueSet], []);
  CODES_TSearchParamsCondition : Array[TSearchParamsCondition] of String = ('_id', '_language', 'asserter', 'category', 'code', 'date-asserted', 'encounter', 'evidence', 'location', 'onset', 'related-code', 'related-item', 'severity', 'stage', 'status', 'subject');
  DESC_TSearchParamsCondition : Array[TSearchParamsCondition] of String = ('The logical resource id associated with the resource (must be supported by all servers)',
     'The language of the resource',
     'Person who asserts this condition',
     'The category of the condition',
     'Code for the condition',
     'When first detected/suspected/entered',
     'Encounter when condition first asserted',
     'Manifestation/symptom',
     'Location - may include laterality',
     'When the Condition started (if started on a date)',
     'Relationship target by means of a predefined code',
     'Relationship target resource',
     'The severity of the condition',
     'Simple summary (disease specific)',
     'The status of the condition',
     'Who has the condition?');
  TYPES_TSearchParamsCondition : Array[TSearchParamsCondition] of TFhirSearchParamType = ( SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeReference,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeDate,  SearchParamTypeReference,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeDate,  SearchParamTypeToken,  SearchParamTypeReference,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeReference);
//  CHECK_TSearchParamsCondition : Array[TSearchParamsCondition] of TSearchParamsCondition = ( spCondition__id,  spCondition__language,  spCondition_Asserter,  spCondition_Category,  spCondition_Code,  spCondition_Date_asserted,  spCondition_Encounter,  spCondition_Evidence,  spCondition_Location,  spCondition_Onset,  spCondition_Related_code,  spCondition_Related_item,  spCondition_Severity,  spCondition_Stage,  spCondition_Status,  spCondition_Subject);
  PATHS_TSearchParamsCondition : Array[TSearchParamsCondition] of String = ('_id: []',
     '_language: []',
     'asserter: [frtPatient, frtPractitioner]',
     'category: []',
     'code: []',
     'date-asserted: []',
     'encounter: [frtEncounter]',
     'evidence: []',
     'location: []',
     'onset: []',
     'related-code: []',
     'related-item: [frtCondition, frtMedicationAdministration, frtProcedure, frtMedicationStatement, frtImmunization]',
     'severity: []',
     'stage: []',
     'status: []',
     'subject: [frtPatient]');
  TARGETS_TSearchParamsCondition : Array[TSearchParamsCondition] of TFhirResourceTypeSet = ([], [], [frtPatient, frtPractitioner], [], [], [], [frtEncounter], [], [], [], [], [frtCondition, frtMedicationAdministration, frtProcedure, frtMedicationStatement, frtImmunization], [], [], [], [frtPatient]);
  CODES_TSearchParamsConformance : Array[TSearchParamsConformance] of String = ('_id', '_language', 'date', 'description', 'event', 'fhirversion', 'format', 'identifier', 'mode', 'name', 'profile', 'publisher', 'resource', 'security', 'software', 'status', 'supported-profile', 'version');
  DESC_TSearchParamsConformance : Array[TSearchParamsConformance] of String = ('The logical resource id associated with the resource (must be supported by all servers)',
     'The language of the resource',
     'The conformance statement publication date',
     'Text search in the description of the conformance statement',
     'Event code in a conformance statement',
     'The version of FHIR',
     'formats supported (xml | json | mime type)',
     'The identifier of the conformance statement',
     'Mode - restful (server/client) or messaging (sender/receiver)',
     'Name of the conformance statement',
     'A profile id invoked in a conformance statement',
     'Name of the publisher of the conformance statement',
     'Name of a resource mentioned in a conformance statement',
     'Information about security of implementation',
     'Part of a the name of a software application',
     'The current status of the conformance statement',
     'Profiles supported by the system',
     'The version identifier of the conformance statement');
  TYPES_TSearchParamsConformance : Array[TSearchParamsConformance] of TFhirSearchParamType = ( SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeDate,  SearchParamTypeString,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeString,  SearchParamTypeReference,  SearchParamTypeString,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeString,  SearchParamTypeToken,  SearchParamTypeReference,  SearchParamTypeToken);
//  CHECK_TSearchParamsConformance : Array[TSearchParamsConformance] of TSearchParamsConformance = ( spConformance__id,  spConformance__language,  spConformance_Date,  spConformance_Description,  spConformance_Event,  spConformance_Fhirversion,  spConformance_Format,  spConformance_Identifier,  spConformance_Mode,  spConformance_Name,  spConformance_Profile,  spConformance_Publisher,  spConformance_Resource,  spConformance_Security,  spConformance_Software,  spConformance_Status,  spConformance_Supported_profile,  spConformance_Version);
  PATHS_TSearchParamsConformance : Array[TSearchParamsConformance] of String = ('_id: []',
     '_language: []',
     'date: []',
     'description: []',
     'event: []',
     'fhirversion: []',
     'format: []',
     'identifier: []',
     'mode: []',
     'name: []',
     'profile: [frtProfile]',
     'publisher: []',
     'resource: []',
     'security: []',
     'software: []',
     'status: []',
     'supported-profile: [frtProfile]',
     'version: []');
  TARGETS_TSearchParamsConformance : Array[TSearchParamsConformance] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [frtProfile], [], [], [], [], [], [frtProfile], []);
  CODES_TSearchParamsDevice : Array[TSearchParamsDevice] of String = ('_id', '_language', 'identifier', 'location', 'manufacturer', 'model', 'organization', 'patient', 'type', 'udi');
  DESC_TSearchParamsDevice : Array[TSearchParamsDevice] of String = ('The logical resource id associated with the resource (must be supported by all servers)',
     'The language of the resource',
     'Instance id from manufacturer, owner and others',
     'A location, where the resource is found',
     'The manufacturer of the device',
     'The model of the device',
     'The organization responsible for the device',
     'Patient information, if the resource is affixed to a person',
     'The type of the device',
     'FDA Mandated Unique Device Identifier');
  TYPES_TSearchParamsDevice : Array[TSearchParamsDevice] of TFhirSearchParamType = ( SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeReference,  SearchParamTypeString,  SearchParamTypeString,  SearchParamTypeReference,  SearchParamTypeReference,  SearchParamTypeToken,  SearchParamTypeString);
//  CHECK_TSearchParamsDevice : Array[TSearchParamsDevice] of TSearchParamsDevice = ( spDevice__id,  spDevice__language,  spDevice_Identifier,  spDevice_Location,  spDevice_Manufacturer,  spDevice_Model,  spDevice_Organization,  spDevice_Patient,  spDevice_Type,  spDevice_Udi);
  PATHS_TSearchParamsDevice : Array[TSearchParamsDevice] of String = ('_id: []',
     '_language: []',
     'identifier: []',
     'location: [frtLocation]',
     'manufacturer: []',
     'model: []',
     'organization: [frtOrganization]',
     'patient: [frtPatient]',
     'type: []',
     'udi: []');
  TARGETS_TSearchParamsDevice : Array[TSearchParamsDevice] of TFhirResourceTypeSet = ([], [], [], [frtLocation], [], [], [frtOrganization], [frtPatient], [], []);
  CODES_TSearchParamsDeviceObservationReport : Array[TSearchParamsDeviceObservationReport] of String = ('_id', '_language', 'channel', 'code', 'observation', 'source', 'subject');
  DESC_TSearchParamsDeviceObservationReport : Array[TSearchParamsDeviceObservationReport] of String = ('The logical resource id associated with the resource (must be supported by all servers)',
     'The language of the resource',
     'The channel code',
     'The compatment code',
     'The data for the metric',
     'Identifies/describes where the data came from',
     'Subject of the measurement');
  TYPES_TSearchParamsDeviceObservationReport : Array[TSearchParamsDeviceObservationReport] of TFhirSearchParamType = ( SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeReference,  SearchParamTypeReference,  SearchParamTypeReference);
//  CHECK_TSearchParamsDeviceObservationReport : Array[TSearchParamsDeviceObservationReport] of TSearchParamsDeviceObservationReport = ( spDeviceObservationReport__id,  spDeviceObservationReport__language,  spDeviceObservationReport_Channel,  spDeviceObservationReport_Code,  spDeviceObservationReport_Observation,  spDeviceObservationReport_Source,  spDeviceObservationReport_Subject);
  PATHS_TSearchParamsDeviceObservationReport : Array[TSearchParamsDeviceObservationReport] of String = ('_id: []',
     '_language: []',
     'channel: []',
     'code: []',
     'observation: [frtObservation]',
     'source: [frtDevice]',
     'subject: [frtDevice, frtLocation, frtPatient]');
  TARGETS_TSearchParamsDeviceObservationReport : Array[TSearchParamsDeviceObservationReport] of TFhirResourceTypeSet = ([], [], [], [], [frtObservation], [frtDevice], [frtDevice, frtLocation, frtPatient]);
  CODES_TSearchParamsDiagnosticOrder : Array[TSearchParamsDiagnosticOrder] of String = ('_id', '_language', 'actor', 'bodysite', 'code', 'encounter', 'event-date', 'event-status', 'event-status-date', 'identifier', 'item-date', 'item-past-status', 'item-status', 'item-status-date', 'orderer', 'specimen', 'status', 'subject');
  DESC_TSearchParamsDiagnosticOrder : Array[TSearchParamsDiagnosticOrder] of String = ('The logical resource id associated with the resource (must be supported by all servers)',
     'The language of the resource',
     'Who recorded or did this',
     'Location of requested test (if applicable)',
     'Code to indicate the item (test or panel) being ordered',
     'The encounter that this diagnostic order is associated with',
     'The date at which the event happened',
     'requested | received | accepted | in progress | review | completed | suspended | rejected | failed',
     'A combination of past-status and date',
     'Identifiers assigned to this order',
     'The date at which the event happened',
     'requested | received | accepted | in progress | review | completed | suspended | rejected | failed',
     'requested | received | accepted | in progress | review | completed | suspended | rejected | failed',
     'A combination of item-past-status and item-date',
     'Who ordered the test',
     'If the whole order relates to specific specimens',
     'requested | received | accepted | in progress | review | completed | suspended | rejected | failed',
     'Who and/or what test is about');
  TYPES_TSearchParamsDiagnosticOrder : Array[TSearchParamsDiagnosticOrder] of TFhirSearchParamType = ( SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeReference,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeReference,  SearchParamTypeDate,  SearchParamTypeToken,  SearchParamTypeComposite,  SearchParamTypeToken,  SearchParamTypeDate,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeComposite,  SearchParamTypeReference,  SearchParamTypeReference,  SearchParamTypeToken,  SearchParamTypeReference);
//  CHECK_TSearchParamsDiagnosticOrder : Array[TSearchParamsDiagnosticOrder] of TSearchParamsDiagnosticOrder = ( spDiagnosticOrder__id,  spDiagnosticOrder__language,  spDiagnosticOrder_Actor,  spDiagnosticOrder_Bodysite,  spDiagnosticOrder_Code,  spDiagnosticOrder_Encounter,  spDiagnosticOrder_Event_date,  spDiagnosticOrder_Event_status,  spDiagnosticOrder_Event_status_date,  spDiagnosticOrder_Identifier,  spDiagnosticOrder_Item_date,  spDiagnosticOrder_Item_past_status,  spDiagnosticOrder_Item_status,  spDiagnosticOrder_Item_status_date,  spDiagnosticOrder_Orderer,  spDiagnosticOrder_Specimen,  spDiagnosticOrder_Status,  spDiagnosticOrder_Subject);
  PATHS_TSearchParamsDiagnosticOrder : Array[TSearchParamsDiagnosticOrder] of String = ('_id: []',
     '_language: []',
     'actor: [frtDevice, frtPractitioner]',
     'bodysite: []',
     'code: []',
     'encounter: [frtEncounter]',
     'event-date: []',
     'event-status: []',
     'event-status-date: []',
     'identifier: []',
     'item-date: []',
     'item-past-status: []',
     'item-status: []',
     'item-status-date: []',
     'orderer: [frtPractitioner]',
     'specimen: [frtSpecimen]',
     'status: []',
     'subject: [frtDevice, frtLocation, frtPatient, frtGroup]');
  TARGETS_TSearchParamsDiagnosticOrder : Array[TSearchParamsDiagnosticOrder] of TFhirResourceTypeSet = ([], [], [frtDevice, frtPractitioner], [], [], [frtEncounter], [], [], [], [], [], [], [], [], [frtPractitioner], [frtSpecimen], [], [frtDevice, frtLocation, frtPatient, frtGroup]);
  CODES_TSearchParamsDiagnosticReport : Array[TSearchParamsDiagnosticReport] of String = ('_id', '_language', 'date', 'diagnosis', 'identifier', 'image', 'issued', 'name', 'performer', 'request', 'result', 'service', 'specimen', 'status', 'subject');
  DESC_TSearchParamsDiagnosticReport : Array[TSearchParamsDiagnosticReport] of String = ('The logical resource id associated with the resource (must be supported by all servers)',
     'The language of the resource',
     'The clinically relevant time of the report',
     'A coded diagnosis on the report',
     'An identifier for the report',
     'Reference to the image source',
     'When the report was issued',
     'The name of the report (e.g. the code for the report as a whole, as opposed to codes for the atomic results, which are the names on the observation resource referred to from the result)',
     'Who was the source of the report (organization)',
     'What was requested',
     'Link to an atomic result (observation resource)',
     'Which diagnostic discipline/department created the report',
     'The specimen details',
     'The status of the report',
     'The subject of the report');
  TYPES_TSearchParamsDiagnosticReport : Array[TSearchParamsDiagnosticReport] of TFhirSearchParamType = ( SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeDate,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeReference,  SearchParamTypeDate,  SearchParamTypeToken,  SearchParamTypeReference,  SearchParamTypeReference,  SearchParamTypeReference,  SearchParamTypeToken,  SearchParamTypeReference,  SearchParamTypeToken,  SearchParamTypeReference);
//  CHECK_TSearchParamsDiagnosticReport : Array[TSearchParamsDiagnosticReport] of TSearchParamsDiagnosticReport = ( spDiagnosticReport__id,  spDiagnosticReport__language,  spDiagnosticReport_Date,  spDiagnosticReport_Diagnosis,  spDiagnosticReport_Identifier,  spDiagnosticReport_Image,  spDiagnosticReport_Issued,  spDiagnosticReport_Name,  spDiagnosticReport_Performer,  spDiagnosticReport_Request,  spDiagnosticReport_Result,  spDiagnosticReport_Service,  spDiagnosticReport_Specimen,  spDiagnosticReport_Status,  spDiagnosticReport_Subject);
  PATHS_TSearchParamsDiagnosticReport : Array[TSearchParamsDiagnosticReport] of String = ('_id: []',
     '_language: []',
     'date: []',
     'diagnosis: []',
     'identifier: []',
     'image: [frtMedia]',
     'issued: []',
     'name: []',
     'performer: [frtOrganization, frtPractitioner]',
     'request: [frtDiagnosticOrder]',
     'result: [frtObservation]',
     'service: []',
     'specimen: [frtSpecimen]',
     'status: []',
     'subject: [frtDevice, frtLocation, frtPatient, frtGroup]');
  TARGETS_TSearchParamsDiagnosticReport : Array[TSearchParamsDiagnosticReport] of TFhirResourceTypeSet = ([], [], [], [], [], [frtMedia], [], [], [frtOrganization, frtPractitioner], [frtDiagnosticOrder], [frtObservation], [], [frtSpecimen], [], [frtDevice, frtLocation, frtPatient, frtGroup]);
  CODES_TSearchParamsDocumentManifest : Array[TSearchParamsDocumentManifest] of String = ('_id', '_language', 'author', 'confidentiality', 'content', 'created', 'description', 'identifier', 'recipient', 'status', 'subject', 'supersedes', 'type');
  DESC_TSearchParamsDocumentManifest : Array[TSearchParamsDocumentManifest] of String = ('The logical resource id associated with the resource (must be supported by all servers)',
     'The language of the resource',
     'Who and/or what authored the document',
     'Sensitivity of set of documents',
     'Contents of this set of documents',
     'When this document manifest created',
     'Human-readable description (title)',
     'Unique Identifier for the set of documents',
     'Intended to get notified about this set of documents',
     'current | superceded | entered in error',
     'The subject of the set of documents',
     'If this document manifest replaces another',
     'What kind of document set this is');
  TYPES_TSearchParamsDocumentManifest : Array[TSearchParamsDocumentManifest] of TFhirSearchParamType = ( SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeReference,  SearchParamTypeToken,  SearchParamTypeReference,  SearchParamTypeDate,  SearchParamTypeString,  SearchParamTypeToken,  SearchParamTypeReference,  SearchParamTypeToken,  SearchParamTypeReference,  SearchParamTypeReference,  SearchParamTypeToken);
//  CHECK_TSearchParamsDocumentManifest : Array[TSearchParamsDocumentManifest] of TSearchParamsDocumentManifest = ( spDocumentManifest__id,  spDocumentManifest__language,  spDocumentManifest_Author,  spDocumentManifest_Confidentiality,  spDocumentManifest_Content,  spDocumentManifest_Created,  spDocumentManifest_Description,  spDocumentManifest_Identifier,  spDocumentManifest_Recipient,  spDocumentManifest_Status,  spDocumentManifest_Subject,  spDocumentManifest_Supersedes,  spDocumentManifest_Type);
  PATHS_TSearchParamsDocumentManifest : Array[TSearchParamsDocumentManifest] of String = ('_id: []',
     '_language: []',
     'author: [frtDevice, frtPatient, frtPractitioner, frtRelatedPerson]',
     'confidentiality: []',
     'content: [frtMedia, frtDocumentReference]',
     'created: []',
     'description: []',
     'identifier: []',
     'recipient: [frtOrganization, frtPatient, frtPractitioner]',
     'status: []',
     'subject: [frtDevice, frtPatient, frtPractitioner, frtGroup]',
     'supersedes: [frtDocumentManifest]',
     'type: []');
  TARGETS_TSearchParamsDocumentManifest : Array[TSearchParamsDocumentManifest] of TFhirResourceTypeSet = ([], [], [frtDevice, frtPatient, frtPractitioner, frtRelatedPerson], [], [frtMedia, frtDocumentReference], [], [], [], [frtOrganization, frtPatient, frtPractitioner], [], [frtDevice, frtPatient, frtPractitioner, frtGroup], [frtDocumentManifest], []);
  CODES_TSearchParamsDocumentReference : Array[TSearchParamsDocumentReference] of String = ('_id', '_language', 'authenticator', 'author', 'class', 'confidentiality', 'created', 'custodian', 'description', 'event', 'facility', 'format', 'identifier', 'indexed', 'language', 'location', 'period', 'relatesto', 'relation', 'relationship', 'size', 'status', 'subject', 'type');
  DESC_TSearchParamsDocumentReference : Array[TSearchParamsDocumentReference] of String = ('The logical resource id associated with the resource (must be supported by all servers)',
     'The language of the resource',
     'Who/What authenticated the document',
     'Who and/or what authored the document',
     'Categorization of Document',
     'Sensitivity of source document',
     'Document creation time',
     'Org which maintains the document',
     'Human-readable description (title)',
     'Main Clinical Acts Documented',
     'Kind of facility where patient was seen',
     'Format/content rules for the document',
     'Master Version Specific Identifier',
     'When this document reference created',
     'The marked primary language for the document',
     'Where to access the document',
     'Time of service that is being documented',
     'Target of the relationship',
     'replaces | transforms | signs | appends',
     'Combination of relation and relatesTo',
     'Size of the document in bytes',
     'current | superceded | entered in error',
     'Who|what is the subject of the document',
     'What kind of document this is (LOINC if possible)');
  TYPES_TSearchParamsDocumentReference : Array[TSearchParamsDocumentReference] of TFhirSearchParamType = ( SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeReference,  SearchParamTypeReference,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeDate,  SearchParamTypeReference,  SearchParamTypeString,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeDate,  SearchParamTypeToken,  SearchParamTypeString,  SearchParamTypeDate,  SearchParamTypeReference,  SearchParamTypeToken,  SearchParamTypeComposite,  SearchParamTypeNumber,  SearchParamTypeToken,  SearchParamTypeReference,  SearchParamTypeToken);
//  CHECK_TSearchParamsDocumentReference : Array[TSearchParamsDocumentReference] of TSearchParamsDocumentReference = ( spDocumentReference__id,  spDocumentReference__language,  spDocumentReference_Authenticator,  spDocumentReference_Author,  spDocumentReference_Class,  spDocumentReference_Confidentiality,  spDocumentReference_Created,  spDocumentReference_Custodian,  spDocumentReference_Description,  spDocumentReference_Event,  spDocumentReference_Facility,  spDocumentReference_Format,  spDocumentReference_Identifier,  spDocumentReference_Indexed,  spDocumentReference_Language,  spDocumentReference_Location,  spDocumentReference_Period,  spDocumentReference_Relatesto,  spDocumentReference_Relation,  spDocumentReference_Relationship,  spDocumentReference_Size,  spDocumentReference_Status,  spDocumentReference_Subject,  spDocumentReference_Type);
  PATHS_TSearchParamsDocumentReference : Array[TSearchParamsDocumentReference] of String = ('_id: []',
     '_language: []',
     'authenticator: [frtOrganization, frtPractitioner]',
     'author: [frtDevice, frtPatient, frtPractitioner, frtRelatedPerson]',
     'class: []',
     'confidentiality: []',
     'created: []',
     'custodian: [frtOrganization]',
     'description: []',
     'event: []',
     'facility: []',
     'format: []',
     'identifier: []',
     'indexed: []',
     'language: []',
     'location: []',
     'period: []',
     'relatesto: [frtDocumentReference]',
     'relation: []',
     'relationship: []',
     'size: []',
     'status: []',
     'subject: [frtDevice, frtPatient, frtPractitioner, frtGroup]',
     'type: []');
  TARGETS_TSearchParamsDocumentReference : Array[TSearchParamsDocumentReference] of TFhirResourceTypeSet = ([], [], [frtOrganization, frtPractitioner], [frtDevice, frtPatient, frtPractitioner, frtRelatedPerson], [], [], [], [frtOrganization], [], [], [], [], [], [], [], [], [], [frtDocumentReference], [], [], [], [], [frtDevice, frtPatient, frtPractitioner, frtGroup], []);
  CODES_TSearchParamsEncounter : Array[TSearchParamsEncounter] of String = ('_id', '_language', 'date', 'identifier', 'indication', 'length', 'location', 'location-period', 'status', 'subject');
  DESC_TSearchParamsEncounter : Array[TSearchParamsEncounter] of String = ('The logical resource id associated with the resource (must be supported by all servers)',
     'The language of the resource',
     'A date within the period the Encounter lasted',
     'Identifier(s) by which this encounter is known',
     'Reason the encounter takes place (resource)',
     'Length of encounter in days',
     'Location the encounter takes place',
     'Time period during which the patient was present at the location',
     'planned | in progress | onleave | finished | cancelled',
     'The patient present at the encounter');
  TYPES_TSearchParamsEncounter : Array[TSearchParamsEncounter] of TFhirSearchParamType = ( SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeDate,  SearchParamTypeToken,  SearchParamTypeReference,  SearchParamTypeNumber,  SearchParamTypeReference,  SearchParamTypeDate,  SearchParamTypeToken,  SearchParamTypeReference);
//  CHECK_TSearchParamsEncounter : Array[TSearchParamsEncounter] of TSearchParamsEncounter = ( spEncounter__id,  spEncounter__language,  spEncounter_Date,  spEncounter_Identifier,  spEncounter_Indication,  spEncounter_Length,  spEncounter_Location,  spEncounter_Location_period,  spEncounter_Status,  spEncounter_Subject);
  PATHS_TSearchParamsEncounter : Array[TSearchParamsEncounter] of String = ('_id: []',
     '_language: []',
     'date: []',
     'identifier: []',
     'indication: ALL_RESOURCE_TYPES',
     'length: []',
     'location: [frtLocation]',
     'location-period: []',
     'status: []',
     'subject: [frtPatient]');
  TARGETS_TSearchParamsEncounter : Array[TSearchParamsEncounter] of TFhirResourceTypeSet = ([], [], [], [], ALL_RESOURCE_TYPES, [], [frtLocation], [], [], [frtPatient]);
  CODES_TSearchParamsFamilyHistory : Array[TSearchParamsFamilyHistory] of String = ('_id', '_language', 'subject');
  DESC_TSearchParamsFamilyHistory : Array[TSearchParamsFamilyHistory] of String = ('The logical resource id associated with the resource (must be supported by all servers)',
     'The language of the resource',
     'The identity of a subject to list family history items for');
  TYPES_TSearchParamsFamilyHistory : Array[TSearchParamsFamilyHistory] of TFhirSearchParamType = ( SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeReference);
//  CHECK_TSearchParamsFamilyHistory : Array[TSearchParamsFamilyHistory] of TSearchParamsFamilyHistory = ( spFamilyHistory__id,  spFamilyHistory__language,  spFamilyHistory_Subject);
  PATHS_TSearchParamsFamilyHistory : Array[TSearchParamsFamilyHistory] of String = ('_id: []',
     '_language: []',
     'subject: [frtPatient]');
  TARGETS_TSearchParamsFamilyHistory : Array[TSearchParamsFamilyHistory] of TFhirResourceTypeSet = ([], [], [frtPatient]);
  CODES_TSearchParamsGroup : Array[TSearchParamsGroup] of String = ('_id', '_language', 'actual', 'characteristic', 'characteristic-value', 'code', 'exclude', 'identifier', 'member', 'type', 'value');
  DESC_TSearchParamsGroup : Array[TSearchParamsGroup] of String = ('The logical resource id associated with the resource (must be supported by all servers)',
     'The language of the resource',
     'Descriptive or actual',
     'Kind of characteristic',
     'A composite of both characteristic and value',
     'The kind of resources contained',
     'Group includes or excludes',
     'Unique id',
     'Who is in group',
     'The type of resources the group contains',
     'Value held by characteristic');
  TYPES_TSearchParamsGroup : Array[TSearchParamsGroup] of TFhirSearchParamType = ( SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeComposite,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeReference,  SearchParamTypeToken,  SearchParamTypeToken);
//  CHECK_TSearchParamsGroup : Array[TSearchParamsGroup] of TSearchParamsGroup = ( spGroup__id,  spGroup__language,  spGroup_Actual,  spGroup_Characteristic,  spGroup_Characteristic_value,  spGroup_Code,  spGroup_Exclude,  spGroup_Identifier,  spGroup_Member,  spGroup_Type,  spGroup_Value);
  PATHS_TSearchParamsGroup : Array[TSearchParamsGroup] of String = ('_id: []',
     '_language: []',
     'actual: []',
     'characteristic: []',
     'characteristic-value: []',
     'code: []',
     'exclude: []',
     'identifier: []',
     'member: [frtMedication, frtDevice, frtPatient, frtSubstance, frtPractitioner]',
     'type: []',
     'value: []');
  TARGETS_TSearchParamsGroup : Array[TSearchParamsGroup] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [frtMedication, frtDevice, frtPatient, frtSubstance, frtPractitioner], [], []);
  CODES_TSearchParamsImagingStudy : Array[TSearchParamsImagingStudy] of String = ('_id', '_language', 'accession', 'bodysite', 'date', 'dicom-class', 'modality', 'series', 'size', 'study', 'subject', 'uid');
  DESC_TSearchParamsImagingStudy : Array[TSearchParamsImagingStudy] of String = ('The logical resource id associated with the resource (must be supported by all servers)',
     'The language of the resource',
     'The accession id for the image',
     'Body part examined (Map from 0018,0015)',
     'The date the study was done was taken',
     'DICOM class type (0008,0016)',
     'The modality of the image',
     'The series id for the image',
     'The size of the image in MB - may include > or < in the value',
     'The study id for the image',
     'Who the study is about',
     'Formal identifier for this instance (0008,0018)');
  TYPES_TSearchParamsImagingStudy : Array[TSearchParamsImagingStudy] of TFhirSearchParamType = ( SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeDate,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeNumber,  SearchParamTypeToken,  SearchParamTypeReference,  SearchParamTypeToken);
//  CHECK_TSearchParamsImagingStudy : Array[TSearchParamsImagingStudy] of TSearchParamsImagingStudy = ( spImagingStudy__id,  spImagingStudy__language,  spImagingStudy_Accession,  spImagingStudy_Bodysite,  spImagingStudy_Date,  spImagingStudy_Dicom_class,  spImagingStudy_Modality,  spImagingStudy_Series,  spImagingStudy_Size,  spImagingStudy_Study,  spImagingStudy_Subject,  spImagingStudy_Uid);
  PATHS_TSearchParamsImagingStudy : Array[TSearchParamsImagingStudy] of String = ('_id: []',
     '_language: []',
     'accession: []',
     'bodysite: []',
     'date: []',
     'dicom-class: []',
     'modality: []',
     'series: []',
     'size: []',
     'study: []',
     'subject: [frtPatient]',
     'uid: []');
  TARGETS_TSearchParamsImagingStudy : Array[TSearchParamsImagingStudy] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [frtPatient], []);
  CODES_TSearchParamsImmunization : Array[TSearchParamsImmunization] of String = ('_id', '_language', 'date', 'dose-sequence', 'identifier', 'location', 'lot-number', 'manufacturer', 'performer', 'reaction', 'reaction-date', 'reason', 'refusal-reason', 'refused', 'requester', 'subject', 'vaccine-type');
  DESC_TSearchParamsImmunization : Array[TSearchParamsImmunization] of String = ('The logical resource id associated with the resource (must be supported by all servers)',
     'The language of the resource',
     'Vaccination  Administration / Refusal Date',
     'What dose number within series?',
     'Business identifier',
     'The service delivery location or facility in which the vaccine was / was to be administered',
     'Vaccine Lot Number',
     'Vaccine Manufacturer',
     'The practitioner who administered the vaccination',
     'Additional information on reaction',
     'When did reaction start?',
     'Why immunization occurred',
     'Explanation of refusal / exemption',
     'Was immunization refused?',
     'The practitioner who ordered the vaccination',
     'The subject of the vaccination event / refusal',
     'Vaccine Product Type Administered');
  TYPES_TSearchParamsImmunization : Array[TSearchParamsImmunization] of TFhirSearchParamType = ( SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeDate,  SearchParamTypeNumber,  SearchParamTypeToken,  SearchParamTypeReference,  SearchParamTypeString,  SearchParamTypeReference,  SearchParamTypeReference,  SearchParamTypeReference,  SearchParamTypeDate,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeReference,  SearchParamTypeReference,  SearchParamTypeToken);
//  CHECK_TSearchParamsImmunization : Array[TSearchParamsImmunization] of TSearchParamsImmunization = ( spImmunization__id,  spImmunization__language,  spImmunization_Date,  spImmunization_Dose_sequence,  spImmunization_Identifier,  spImmunization_Location,  spImmunization_Lot_number,  spImmunization_Manufacturer,  spImmunization_Performer,  spImmunization_Reaction,  spImmunization_Reaction_date,  spImmunization_Reason,  spImmunization_Refusal_reason,  spImmunization_Refused,  spImmunization_Requester,  spImmunization_Subject,  spImmunization_Vaccine_type);
  PATHS_TSearchParamsImmunization : Array[TSearchParamsImmunization] of String = ('_id: []',
     '_language: []',
     'date: []',
     'dose-sequence: []',
     'identifier: []',
     'location: [frtLocation]',
     'lot-number: []',
     'manufacturer: [frtOrganization]',
     'performer: [frtPractitioner]',
     'reaction: [frtObservation, frtAdverseReaction]',
     'reaction-date: []',
     'reason: []',
     'refusal-reason: []',
     'refused: []',
     'requester: [frtPractitioner]',
     'subject: [frtPatient]',
     'vaccine-type: []');
  TARGETS_TSearchParamsImmunization : Array[TSearchParamsImmunization] of TFhirResourceTypeSet = ([], [], [], [], [], [frtLocation], [], [frtOrganization], [frtPractitioner], [frtObservation, frtAdverseReaction], [], [], [], [], [frtPractitioner], [frtPatient], []);
  CODES_TSearchParamsImmunizationRecommendation : Array[TSearchParamsImmunizationRecommendation] of String = ('_id', '_language', 'date', 'dose-number', 'dose-sequence', 'identifier', 'information', 'status', 'subject', 'support', 'vaccine-type');
  DESC_TSearchParamsImmunizationRecommendation : Array[TSearchParamsImmunizationRecommendation] of String = ('The logical resource id associated with the resource (must be supported by all servers)',
     'The language of the resource',
     'Date recommendation created',
     'Recommended dose number',
     'Number of dose within sequence',
     'Business identifier',
     'Patient observations supporting recommendation',
     'Vaccine administration status',
     'Who this profile is for',
     'Past immunizations supporting recommendation',
     'Vaccine recommendation applies to');
  TYPES_TSearchParamsImmunizationRecommendation : Array[TSearchParamsImmunizationRecommendation] of TFhirSearchParamType = ( SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeDate,  SearchParamTypeNumber,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeReference,  SearchParamTypeToken,  SearchParamTypeReference,  SearchParamTypeReference,  SearchParamTypeToken);
//  CHECK_TSearchParamsImmunizationRecommendation : Array[TSearchParamsImmunizationRecommendation] of TSearchParamsImmunizationRecommendation = ( spImmunizationRecommendation__id,  spImmunizationRecommendation__language,  spImmunizationRecommendation_Date,  spImmunizationRecommendation_Dose_number,  spImmunizationRecommendation_Dose_sequence,  spImmunizationRecommendation_Identifier,  spImmunizationRecommendation_Information,  spImmunizationRecommendation_Status,  spImmunizationRecommendation_Subject,  spImmunizationRecommendation_Support,  spImmunizationRecommendation_Vaccine_type);
  PATHS_TSearchParamsImmunizationRecommendation : Array[TSearchParamsImmunizationRecommendation] of String = ('_id: []',
     '_language: []',
     'date: []',
     'dose-number: []',
     'dose-sequence: []',
     'identifier: []',
     'information: [frtAllergyIntolerance, frtObservation, frtAdverseReaction]',
     'status: []',
     'subject: [frtPatient]',
     'support: [frtImmunization]',
     'vaccine-type: []');
  TARGETS_TSearchParamsImmunizationRecommendation : Array[TSearchParamsImmunizationRecommendation] of TFhirResourceTypeSet = ([], [], [], [], [], [], [frtAllergyIntolerance, frtObservation, frtAdverseReaction], [], [frtPatient], [frtImmunization], []);
  CODES_TSearchParamsList : Array[TSearchParamsList] of String = ('_id', '_language', 'code', 'date', 'empty-reason', 'item', 'source', 'subject');
  DESC_TSearchParamsList : Array[TSearchParamsList] of String = ('The logical resource id associated with the resource (must be supported by all servers)',
     'The language of the resource',
     'What the purpose of this list is',
     'When the list was prepared',
     'Why list is empty',
     'Actual entry',
     'Who and/or what defined the list contents',
     'If all resources have the same subject');
  TYPES_TSearchParamsList : Array[TSearchParamsList] of TFhirSearchParamType = ( SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeDate,  SearchParamTypeToken,  SearchParamTypeReference,  SearchParamTypeReference,  SearchParamTypeReference);
//  CHECK_TSearchParamsList : Array[TSearchParamsList] of TSearchParamsList = ( spList__id,  spList__language,  spList_Code,  spList_Date,  spList_Empty_reason,  spList_Item,  spList_Source,  spList_Subject);
  PATHS_TSearchParamsList : Array[TSearchParamsList] of String = ('_id: []',
     '_language: []',
     'code: []',
     'date: []',
     'empty-reason: []',
     'item: ALL_RESOURCE_TYPES',
     'source: [frtDevice, frtPatient, frtPractitioner]',
     'subject: [frtDevice, frtLocation, frtPatient, frtGroup]');
  TARGETS_TSearchParamsList : Array[TSearchParamsList] of TFhirResourceTypeSet = ([], [], [], [], [], ALL_RESOURCE_TYPES, [frtDevice, frtPatient, frtPractitioner], [frtDevice, frtLocation, frtPatient, frtGroup]);
  CODES_TSearchParamsLocation : Array[TSearchParamsLocation] of String = ('_id', '_language', 'address', 'identifier', 'name', 'near', 'near-distance', 'partof', 'status', 'type');
  DESC_TSearchParamsLocation : Array[TSearchParamsLocation] of String = ('The logical resource id associated with the resource (must be supported by all servers)',
     'The language of the resource',
     'A (part of the) address of the location',
     'Unique code or number identifying the location to its users',
     'A (portion of the) name of the location',
     'The coordinates expressed as [lat],[long] (using KML, see notes) to find locations near to (servers may search using a square rather than a circle for efficiency)',
     'A distance quantity to limit the near search to locations within a specific distance',
     'The location of which this location is a part',
     'Searches for locations with a specific kind of status',
     'A code for the type of location');
  TYPES_TSearchParamsLocation : Array[TSearchParamsLocation] of TFhirSearchParamType = ( SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeString,  SearchParamTypeToken,  SearchParamTypeString,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeReference,  SearchParamTypeToken,  SearchParamTypeToken);
//  CHECK_TSearchParamsLocation : Array[TSearchParamsLocation] of TSearchParamsLocation = ( spLocation__id,  spLocation__language,  spLocation_Address,  spLocation_Identifier,  spLocation_Name,  spLocation_Near,  spLocation_Near_distance,  spLocation_Partof,  spLocation_Status,  spLocation_Type);
  PATHS_TSearchParamsLocation : Array[TSearchParamsLocation] of String = ('_id: []',
     '_language: []',
     'address: []',
     'identifier: []',
     'name: []',
     'near: []',
     'near-distance: []',
     'partof: [frtLocation]',
     'status: []',
     'type: []');
  TARGETS_TSearchParamsLocation : Array[TSearchParamsLocation] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [frtLocation], [], []);
  CODES_TSearchParamsMedia : Array[TSearchParamsMedia] of String = ('_id', '_language', 'date', 'identifier', 'operator', 'subject', 'subtype', 'type', 'view');
  DESC_TSearchParamsMedia : Array[TSearchParamsMedia] of String = ('The logical resource id associated with the resource (must be supported by all servers)',
     'The language of the resource',
     'When the media was taken/recorded (end)',
     'Identifier(s) for the image',
     'The person who generated the image',
     'Who/What this Media is a record of',
     'The type of acquisition equipment/process',
     'photo | video | audio',
     'Imaging view e.g Lateral or Antero-posterior');
  TYPES_TSearchParamsMedia : Array[TSearchParamsMedia] of TFhirSearchParamType = ( SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeDate,  SearchParamTypeToken,  SearchParamTypeReference,  SearchParamTypeReference,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeToken);
//  CHECK_TSearchParamsMedia : Array[TSearchParamsMedia] of TSearchParamsMedia = ( spMedia__id,  spMedia__language,  spMedia_Date,  spMedia_Identifier,  spMedia_Operator,  spMedia_Subject,  spMedia_Subtype,  spMedia_Type,  spMedia_View);
  PATHS_TSearchParamsMedia : Array[TSearchParamsMedia] of String = ('_id: []',
     '_language: []',
     'date: []',
     'identifier: []',
     'operator: [frtPractitioner]',
     'subject: [frtSpecimen, frtDevice, frtPatient, frtPractitioner, frtGroup]',
     'subtype: []',
     'type: []',
     'view: []');
  TARGETS_TSearchParamsMedia : Array[TSearchParamsMedia] of TFhirResourceTypeSet = ([], [], [], [], [frtPractitioner], [frtSpecimen, frtDevice, frtPatient, frtPractitioner, frtGroup], [], [], []);
  CODES_TSearchParamsMedication : Array[TSearchParamsMedication] of String = ('_id', '_language', 'code', 'container', 'content', 'form', 'ingredient', 'manufacturer', 'name');
  DESC_TSearchParamsMedication : Array[TSearchParamsMedication] of String = ('The logical resource id associated with the resource (must be supported by all servers)',
     'The language of the resource',
     'Codes that identify this medication',
     'E.g. box, vial, blister-pack',
     'A product in the package',
     'powder | tablets | carton +',
     'The product contained',
     'Manufacturer of the item',
     'Common / Commercial name');
  TYPES_TSearchParamsMedication : Array[TSearchParamsMedication] of TFhirSearchParamType = ( SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeReference,  SearchParamTypeToken,  SearchParamTypeReference,  SearchParamTypeReference,  SearchParamTypeString);
//  CHECK_TSearchParamsMedication : Array[TSearchParamsMedication] of TSearchParamsMedication = ( spMedication__id,  spMedication__language,  spMedication_Code,  spMedication_Container,  spMedication_Content,  spMedication_Form,  spMedication_Ingredient,  spMedication_Manufacturer,  spMedication_Name);
  PATHS_TSearchParamsMedication : Array[TSearchParamsMedication] of String = ('_id: []',
     '_language: []',
     'code: []',
     'container: []',
     'content: [frtMedication]',
     'form: []',
     'ingredient: [frtMedication, frtSubstance]',
     'manufacturer: [frtOrganization]',
     'name: []');
  TARGETS_TSearchParamsMedication : Array[TSearchParamsMedication] of TFhirResourceTypeSet = ([], [], [], [], [frtMedication], [], [frtMedication, frtSubstance], [frtOrganization], []);
  CODES_TSearchParamsMedicationAdministration : Array[TSearchParamsMedicationAdministration] of String = ('_id', '_language', 'device', 'encounter', 'identifier', 'medication', 'notgiven', 'patient', 'prescription', 'status', 'whengiven');
  DESC_TSearchParamsMedicationAdministration : Array[TSearchParamsMedicationAdministration] of String = ('The logical resource id associated with the resource (must be supported by all servers)',
     'The language of the resource',
     'Return administrations with this administration device identity',
     'Return administrations that share this encounter',
     'Return administrations with this external identity',
     'Return administrations of this medication',
     'Administrations that were not made',
     'The identity of a patient to list administrations  for',
     'The identity of a prescription to list administrations from',
     'MedicationAdministration event status (for example one of active/paused/completed/nullified)',
     'Date of administration');
  TYPES_TSearchParamsMedicationAdministration : Array[TSearchParamsMedicationAdministration] of TFhirSearchParamType = ( SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeReference,  SearchParamTypeReference,  SearchParamTypeToken,  SearchParamTypeReference,  SearchParamTypeToken,  SearchParamTypeReference,  SearchParamTypeReference,  SearchParamTypeToken,  SearchParamTypeDate);
//  CHECK_TSearchParamsMedicationAdministration : Array[TSearchParamsMedicationAdministration] of TSearchParamsMedicationAdministration = ( spMedicationAdministration__id,  spMedicationAdministration__language,  spMedicationAdministration_Device,  spMedicationAdministration_Encounter,  spMedicationAdministration_Identifier,  spMedicationAdministration_Medication,  spMedicationAdministration_Notgiven,  spMedicationAdministration_Patient,  spMedicationAdministration_Prescription,  spMedicationAdministration_Status,  spMedicationAdministration_Whengiven);
  PATHS_TSearchParamsMedicationAdministration : Array[TSearchParamsMedicationAdministration] of String = ('_id: []',
     '_language: []',
     'device: [frtDevice]',
     'encounter: [frtEncounter]',
     'identifier: []',
     'medication: [frtMedication]',
     'notgiven: []',
     'patient: [frtPatient]',
     'prescription: [frtMedicationPrescription]',
     'status: []',
     'whengiven: []');
  TARGETS_TSearchParamsMedicationAdministration : Array[TSearchParamsMedicationAdministration] of TFhirResourceTypeSet = ([], [], [frtDevice], [frtEncounter], [], [frtMedication], [], [frtPatient], [frtMedicationPrescription], [], []);
  CODES_TSearchParamsMedicationDispense : Array[TSearchParamsMedicationDispense] of String = ('_id', '_language', 'destination', 'dispenser', 'identifier', 'medication', 'patient', 'prescription', 'responsibleparty', 'status', 'type', 'whenhandedover', 'whenprepared');
  DESC_TSearchParamsMedicationDispense : Array[TSearchParamsMedicationDispense] of String = ('The logical resource id associated with the resource (must be supported by all servers)',
     'The language of the resource',
     'Return dispenses that should be sent to a secific destination',
     'Return all dispenses performed by a specific indiividual',
     'Return dispenses with this external identity',
     'Returns dispenses of this medicine',
     'The identity of a patient to list dispenses  for',
     'The identity of a prescription to list dispenses from',
     'Return all dispenses with the specified responsible party',
     'Status of the dispense',
     'Return all dispenses of a specific type',
     'Date when medication handed over to patient (outpatient setting), or supplied to ward or clinic (inpatient setting)',
     'Date when medication prepared');
  TYPES_TSearchParamsMedicationDispense : Array[TSearchParamsMedicationDispense] of TFhirSearchParamType = ( SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeReference,  SearchParamTypeReference,  SearchParamTypeToken,  SearchParamTypeReference,  SearchParamTypeReference,  SearchParamTypeReference,  SearchParamTypeReference,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeDate,  SearchParamTypeDate);
//  CHECK_TSearchParamsMedicationDispense : Array[TSearchParamsMedicationDispense] of TSearchParamsMedicationDispense = ( spMedicationDispense__id,  spMedicationDispense__language,  spMedicationDispense_Destination,  spMedicationDispense_Dispenser,  spMedicationDispense_Identifier,  spMedicationDispense_Medication,  spMedicationDispense_Patient,  spMedicationDispense_Prescription,  spMedicationDispense_Responsibleparty,  spMedicationDispense_Status,  spMedicationDispense_Type,  spMedicationDispense_Whenhandedover,  spMedicationDispense_Whenprepared);
  PATHS_TSearchParamsMedicationDispense : Array[TSearchParamsMedicationDispense] of String = ('_id: []',
     '_language: []',
     'destination: [frtLocation]',
     'dispenser: [frtPractitioner]',
     'identifier: []',
     'medication: [frtMedication]',
     'patient: [frtPatient]',
     'prescription: [frtMedicationPrescription]',
     'responsibleparty: [frtPractitioner]',
     'status: []',
     'type: []',
     'whenhandedover: []',
     'whenprepared: []');
  TARGETS_TSearchParamsMedicationDispense : Array[TSearchParamsMedicationDispense] of TFhirResourceTypeSet = ([], [], [frtLocation], [frtPractitioner], [], [frtMedication], [frtPatient], [frtMedicationPrescription], [frtPractitioner], [], [], [], []);
  CODES_TSearchParamsMedicationPrescription : Array[TSearchParamsMedicationPrescription] of String = ('_id', '_language', 'datewritten', 'encounter', 'identifier', 'medication', 'patient', 'status');
  DESC_TSearchParamsMedicationPrescription : Array[TSearchParamsMedicationPrescription] of String = ('The logical resource id associated with the resource (must be supported by all servers)',
     'The language of the resource',
     'Return prescriptions written on this date',
     'Return prescriptions with this encounter identity',
     'Return prescriptions with this external identity',
     'Code for medicine or text in medicine name',
     'The identity of a patient to list dispenses  for',
     'Status of the prescription');
  TYPES_TSearchParamsMedicationPrescription : Array[TSearchParamsMedicationPrescription] of TFhirSearchParamType = ( SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeDate,  SearchParamTypeReference,  SearchParamTypeToken,  SearchParamTypeReference,  SearchParamTypeReference,  SearchParamTypeToken);
//  CHECK_TSearchParamsMedicationPrescription : Array[TSearchParamsMedicationPrescription] of TSearchParamsMedicationPrescription = ( spMedicationPrescription__id,  spMedicationPrescription__language,  spMedicationPrescription_Datewritten,  spMedicationPrescription_Encounter,  spMedicationPrescription_Identifier,  spMedicationPrescription_Medication,  spMedicationPrescription_Patient,  spMedicationPrescription_Status);
  PATHS_TSearchParamsMedicationPrescription : Array[TSearchParamsMedicationPrescription] of String = ('_id: []',
     '_language: []',
     'datewritten: []',
     'encounter: [frtEncounter]',
     'identifier: []',
     'medication: [frtMedication]',
     'patient: [frtPatient]',
     'status: []');
  TARGETS_TSearchParamsMedicationPrescription : Array[TSearchParamsMedicationPrescription] of TFhirResourceTypeSet = ([], [], [], [frtEncounter], [], [frtMedication], [frtPatient], []);
  CODES_TSearchParamsMedicationStatement : Array[TSearchParamsMedicationStatement] of String = ('_id', '_language', 'device', 'identifier', 'medication', 'patient', 'when-given');
  DESC_TSearchParamsMedicationStatement : Array[TSearchParamsMedicationStatement] of String = ('The logical resource id associated with the resource (must be supported by all servers)',
     'The language of the resource',
     'Return administrations with this administration device identity',
     'Return administrations with this external identity',
     'Code for medicine or text in medicine name',
     'The identity of a patient to list administrations  for',
     'Date of administration');
  TYPES_TSearchParamsMedicationStatement : Array[TSearchParamsMedicationStatement] of TFhirSearchParamType = ( SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeReference,  SearchParamTypeToken,  SearchParamTypeReference,  SearchParamTypeReference,  SearchParamTypeDate);
//  CHECK_TSearchParamsMedicationStatement : Array[TSearchParamsMedicationStatement] of TSearchParamsMedicationStatement = ( spMedicationStatement__id,  spMedicationStatement__language,  spMedicationStatement_Device,  spMedicationStatement_Identifier,  spMedicationStatement_Medication,  spMedicationStatement_Patient,  spMedicationStatement_When_given);
  PATHS_TSearchParamsMedicationStatement : Array[TSearchParamsMedicationStatement] of String = ('_id: []',
     '_language: []',
     'device: [frtDevice]',
     'identifier: []',
     'medication: [frtMedication]',
     'patient: [frtPatient]',
     'when-given: []');
  TARGETS_TSearchParamsMedicationStatement : Array[TSearchParamsMedicationStatement] of TFhirResourceTypeSet = ([], [], [frtDevice], [], [frtMedication], [frtPatient], []);
  CODES_TSearchParamsMessageHeader : Array[TSearchParamsMessageHeader] of String = ('_id', '_language');
  DESC_TSearchParamsMessageHeader : Array[TSearchParamsMessageHeader] of String = ('The logical resource id associated with the resource (must be supported by all servers)',
     'The language of the resource');
  TYPES_TSearchParamsMessageHeader : Array[TSearchParamsMessageHeader] of TFhirSearchParamType = ( SearchParamTypeToken,  SearchParamTypeToken);
//  CHECK_TSearchParamsMessageHeader : Array[TSearchParamsMessageHeader] of TSearchParamsMessageHeader = ( spMessageHeader__id,  spMessageHeader__language);
  PATHS_TSearchParamsMessageHeader : Array[TSearchParamsMessageHeader] of String = ('_id: []',
     '_language: []');
  TARGETS_TSearchParamsMessageHeader : Array[TSearchParamsMessageHeader] of TFhirResourceTypeSet = ([], []);
  CODES_TSearchParamsObservation : Array[TSearchParamsObservation] of String = ('_id', '_language', 'date', 'name', 'name-value-[x]', 'performer', 'related', 'related-target', 'related-type', 'reliability', 'specimen', 'status', 'subject', 'value-concept', 'value-date', 'value-quantity', 'value-string');
  DESC_TSearchParamsObservation : Array[TSearchParamsObservation] of String = ('The logical resource id associated with the resource (must be supported by all servers)',
     'The language of the resource',
     'Obtained date/time. If the obtained element is a period, a date that falls in the period',
     'The name of the observation type',
     'Both name and one of the value parameters',
     'Who and/or what performed the observation',
     'Related Observations - search on related-type and related-target together',
     'Observation that is related to this one',
     'has-component | has-member | derived-from | sequel-to | replaces | qualified-by | interfered-by',
     'The reliability of the observation',
     'Specimen used for this observation',
     'The status of the observation',
     'The subject that the observation is about',
     'The value of the observation, if the value is a CodeableConcept',
     'The value of the observation, if the value is a Period',
     'The value of the observation, if the value is a Quantity, or a SampledData (just search on the bounds of the values in sampled data)',
     'The value of the observation, if the value is a string, and also searches in CodeableConcept.text');
  TYPES_TSearchParamsObservation : Array[TSearchParamsObservation] of TFhirSearchParamType = ( SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeDate,  SearchParamTypeToken,  SearchParamTypeComposite,  SearchParamTypeReference,  SearchParamTypeComposite,  SearchParamTypeReference,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeReference,  SearchParamTypeToken,  SearchParamTypeReference,  SearchParamTypeToken,  SearchParamTypeDate,  SearchParamTypeQuantity,  SearchParamTypeString);
//  CHECK_TSearchParamsObservation : Array[TSearchParamsObservation] of TSearchParamsObservation = ( spObservation__id,  spObservation__language,  spObservation_Date,  spObservation_Name,  spObservation_Name_value_x,  spObservation_Performer,  spObservation_Related,  spObservation_Related_target,  spObservation_Related_type,  spObservation_Reliability,  spObservation_Specimen,  spObservation_Status,  spObservation_Subject,  spObservation_Value_concept,  spObservation_Value_date,  spObservation_Value_quantity,  spObservation_Value_string);
  PATHS_TSearchParamsObservation : Array[TSearchParamsObservation] of String = ('_id: []',
     '_language: []',
     'date: []',
     'name: []',
     'name-value-[x]: []',
     'performer: [frtDevice, frtOrganization, frtPractitioner]',
     'related: []',
     'related-target: [frtObservation]',
     'related-type: []',
     'reliability: []',
     'specimen: [frtSpecimen]',
     'status: []',
     'subject: [frtDevice, frtLocation, frtPatient, frtGroup]',
     'value-concept: []',
     'value-date: []',
     'value-quantity: []',
     'value-string: []');
  TARGETS_TSearchParamsObservation : Array[TSearchParamsObservation] of TFhirResourceTypeSet = ([], [], [], [], [], [frtDevice, frtOrganization, frtPractitioner], [], [frtObservation], [], [], [frtSpecimen], [], [frtDevice, frtLocation, frtPatient, frtGroup], [], [], [], []);
  CODES_TSearchParamsOperationOutcome : Array[TSearchParamsOperationOutcome] of String = ('_id', '_language');
  DESC_TSearchParamsOperationOutcome : Array[TSearchParamsOperationOutcome] of String = ('The logical resource id associated with the resource (must be supported by all servers)',
     'The language of the resource');
  TYPES_TSearchParamsOperationOutcome : Array[TSearchParamsOperationOutcome] of TFhirSearchParamType = ( SearchParamTypeToken,  SearchParamTypeToken);
//  CHECK_TSearchParamsOperationOutcome : Array[TSearchParamsOperationOutcome] of TSearchParamsOperationOutcome = ( spOperationOutcome__id,  spOperationOutcome__language);
  PATHS_TSearchParamsOperationOutcome : Array[TSearchParamsOperationOutcome] of String = ('_id: []',
     '_language: []');
  TARGETS_TSearchParamsOperationOutcome : Array[TSearchParamsOperationOutcome] of TFhirResourceTypeSet = ([], []);
  CODES_TSearchParamsOrder : Array[TSearchParamsOrder] of String = ('_id', '_language', 'authority', 'date', 'detail', 'source', 'subject', 'target', 'when', 'when_code');
  DESC_TSearchParamsOrder : Array[TSearchParamsOrder] of String = ('The logical resource id associated with the resource (must be supported by all servers)',
     'The language of the resource',
     'If required by policy',
     'When the order was made',
     'What action is being ordered',
     'Who initiated the order',
     'Patient this order is about',
     'Who is intended to fulfill the order',
     'A formal schedule',
     'Code specifies when request should be done. The code may simply be a priority code');
  TYPES_TSearchParamsOrder : Array[TSearchParamsOrder] of TFhirSearchParamType = ( SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeReference,  SearchParamTypeDate,  SearchParamTypeReference,  SearchParamTypeReference,  SearchParamTypeReference,  SearchParamTypeReference,  SearchParamTypeDate,  SearchParamTypeToken);
//  CHECK_TSearchParamsOrder : Array[TSearchParamsOrder] of TSearchParamsOrder = ( spOrder__id,  spOrder__language,  spOrder_Authority,  spOrder_Date,  spOrder_Detail,  spOrder_Source,  spOrder_Subject,  spOrder_Target,  spOrder_When,  spOrder_When_code);
  PATHS_TSearchParamsOrder : Array[TSearchParamsOrder] of String = ('_id: []',
     '_language: []',
     'authority: ALL_RESOURCE_TYPES',
     'date: []',
     'detail: ALL_RESOURCE_TYPES',
     'source: [frtPractitioner]',
     'subject: [frtPatient]',
     'target: [frtDevice, frtOrganization, frtPractitioner]',
     'when: []',
     'when_code: []');
  TARGETS_TSearchParamsOrder : Array[TSearchParamsOrder] of TFhirResourceTypeSet = ([], [], ALL_RESOURCE_TYPES, [], ALL_RESOURCE_TYPES, [frtPractitioner], [frtPatient], [frtDevice, frtOrganization, frtPractitioner], [], []);
  CODES_TSearchParamsOrderResponse : Array[TSearchParamsOrderResponse] of String = ('_id', '_language', 'code', 'date', 'fulfillment', 'request', 'who');
  DESC_TSearchParamsOrderResponse : Array[TSearchParamsOrderResponse] of String = ('The logical resource id associated with the resource (must be supported by all servers)',
     'The language of the resource',
     'pending | review | rejected | error | accepted | cancelled | replaced | aborted | complete',
     'When the response was made',
     'Details of the outcome of performing the order',
     'The order that this is a response to',
     'Who made the response');
  TYPES_TSearchParamsOrderResponse : Array[TSearchParamsOrderResponse] of TFhirSearchParamType = ( SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeDate,  SearchParamTypeReference,  SearchParamTypeReference,  SearchParamTypeReference);
//  CHECK_TSearchParamsOrderResponse : Array[TSearchParamsOrderResponse] of TSearchParamsOrderResponse = ( spOrderResponse__id,  spOrderResponse__language,  spOrderResponse_Code,  spOrderResponse_Date,  spOrderResponse_Fulfillment,  spOrderResponse_Request,  spOrderResponse_Who);
  PATHS_TSearchParamsOrderResponse : Array[TSearchParamsOrderResponse] of String = ('_id: []',
     '_language: []',
     'code: []',
     'date: []',
     'fulfillment: ALL_RESOURCE_TYPES',
     'request: [frtOrder]',
     'who: [frtDevice, frtOrganization, frtPractitioner]');
  TARGETS_TSearchParamsOrderResponse : Array[TSearchParamsOrderResponse] of TFhirResourceTypeSet = ([], [], [], [], ALL_RESOURCE_TYPES, [frtOrder], [frtDevice, frtOrganization, frtPractitioner]);
  CODES_TSearchParamsOrganization : Array[TSearchParamsOrganization] of String = ('_id', '_language', 'active', 'identifier', 'name', 'partof', 'phonetic', 'type');
  DESC_TSearchParamsOrganization : Array[TSearchParamsOrganization] of String = ('The logical resource id associated with the resource (must be supported by all servers)',
     'The language of the resource',
     'Whether the organization''s record is active',
     'Any identifier for the organization (not the accreditation issuer''s identifier)',
     'A portion of the organization''s name',
     'Search all organizations that are part of the given organization',
     'A portion of the organization''s name using some kind of phonetic matching algorithm',
     'A code for the type of organization');
  TYPES_TSearchParamsOrganization : Array[TSearchParamsOrganization] of TFhirSearchParamType = ( SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeString,  SearchParamTypeReference,  SearchParamTypeString,  SearchParamTypeToken);
//  CHECK_TSearchParamsOrganization : Array[TSearchParamsOrganization] of TSearchParamsOrganization = ( spOrganization__id,  spOrganization__language,  spOrganization_Active,  spOrganization_Identifier,  spOrganization_Name,  spOrganization_Partof,  spOrganization_Phonetic,  spOrganization_Type);
  PATHS_TSearchParamsOrganization : Array[TSearchParamsOrganization] of String = ('_id: []',
     '_language: []',
     'active: []',
     'identifier: []',
     'name: []',
     'partof: [frtOrganization]',
     'phonetic: []',
     'type: []');
  TARGETS_TSearchParamsOrganization : Array[TSearchParamsOrganization] of TFhirResourceTypeSet = ([], [], [], [], [], [frtOrganization], [], []);
  CODES_TSearchParamsOther : Array[TSearchParamsOther] of String = ('_id', '_language', 'code', 'created', 'subject');
  DESC_TSearchParamsOther : Array[TSearchParamsOther] of String = ('The logical resource id associated with the resource (must be supported by all servers)',
     'The language of the resource',
     'Kind of Resource',
     'When created',
     'Identifies the');
  TYPES_TSearchParamsOther : Array[TSearchParamsOther] of TFhirSearchParamType = ( SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeDate,  SearchParamTypeReference);
//  CHECK_TSearchParamsOther : Array[TSearchParamsOther] of TSearchParamsOther = ( spOther__id,  spOther__language,  spOther_Code,  spOther_Created,  spOther_Subject);
  PATHS_TSearchParamsOther : Array[TSearchParamsOther] of String = ('_id: []',
     '_language: []',
     'code: []',
     'created: []',
     'subject: ALL_RESOURCE_TYPES');
  TARGETS_TSearchParamsOther : Array[TSearchParamsOther] of TFhirResourceTypeSet = ([], [], [], [], ALL_RESOURCE_TYPES);
  CODES_TSearchParamsPatient : Array[TSearchParamsPatient] of String = ('_id', '_language', 'active', 'address', 'animal-breed', 'animal-species', 'birthdate', 'family', 'gender', 'given', 'identifier', 'language', 'link', 'name', 'phonetic', 'provider', 'telecom');
  DESC_TSearchParamsPatient : Array[TSearchParamsPatient] of String = ('The logical resource id associated with the resource (must be supported by all servers)',
     'The language of the resource',
     'Whether the patient record is active',
     'An address in any kind of address/part of the patient',
     'The breed for animal patients',
     'The species for animal patients',
     'The patient''s date of birth',
     'A portion of the family name of the patient',
     'Gender of the patient',
     'A portion of the given name of the patient',
     'A patient identifier',
     'Language code (irrespective of use value)',
     'All patients linked to the given patient',
     'A portion of either family or given name of the patient',
     'A portion of either family or given name using some kind of phonetic matching algorithm',
     'The organization at which this person is a patient',
     'The value in any kind of telecom details of the patient');
  TYPES_TSearchParamsPatient : Array[TSearchParamsPatient] of TFhirSearchParamType = ( SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeString,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeDate,  SearchParamTypeString,  SearchParamTypeToken,  SearchParamTypeString,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeReference,  SearchParamTypeString,  SearchParamTypeString,  SearchParamTypeReference,  SearchParamTypeString);
//  CHECK_TSearchParamsPatient : Array[TSearchParamsPatient] of TSearchParamsPatient = ( spPatient__id,  spPatient__language,  spPatient_Active,  spPatient_Address,  spPatient_Animal_breed,  spPatient_Animal_species,  spPatient_Birthdate,  spPatient_Family,  spPatient_Gender,  spPatient_Given,  spPatient_Identifier,  spPatient_Language,  spPatient_Link,  spPatient_Name,  spPatient_Phonetic,  spPatient_Provider,  spPatient_Telecom);
  PATHS_TSearchParamsPatient : Array[TSearchParamsPatient] of String = ('_id: []',
     '_language: []',
     'active: []',
     'address: []',
     'animal-breed: []',
     'animal-species: []',
     'birthdate: []',
     'family: []',
     'gender: []',
     'given: []',
     'identifier: []',
     'language: []',
     'link: [frtPatient]',
     'name: []',
     'phonetic: []',
     'provider: [frtOrganization]',
     'telecom: []');
  TARGETS_TSearchParamsPatient : Array[TSearchParamsPatient] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [], [], [frtPatient], [], [], [frtOrganization], []);
  CODES_TSearchParamsPractitioner : Array[TSearchParamsPractitioner] of String = ('_id', '_language', 'address', 'family', 'gender', 'given', 'identifier', 'name', 'organization', 'phonetic', 'telecom');
  DESC_TSearchParamsPractitioner : Array[TSearchParamsPractitioner] of String = ('The logical resource id associated with the resource (must be supported by all servers)',
     'The language of the resource',
     'An address in any kind of address/part',
     'A portion of the family name',
     'Gender of the practitioner',
     'A portion of the given name',
     'A practitioner''s Identifier',
     'A portion of either family or given name',
     'The identity of the organization the practitioner represents / acts on behalf of',
     'A portion of either family or given name using some kind of phonetic matching algorithm',
     'The value in any kind of contact');
  TYPES_TSearchParamsPractitioner : Array[TSearchParamsPractitioner] of TFhirSearchParamType = ( SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeString,  SearchParamTypeString,  SearchParamTypeToken,  SearchParamTypeString,  SearchParamTypeToken,  SearchParamTypeString,  SearchParamTypeReference,  SearchParamTypeString,  SearchParamTypeString);
//  CHECK_TSearchParamsPractitioner : Array[TSearchParamsPractitioner] of TSearchParamsPractitioner = ( spPractitioner__id,  spPractitioner__language,  spPractitioner_Address,  spPractitioner_Family,  spPractitioner_Gender,  spPractitioner_Given,  spPractitioner_Identifier,  spPractitioner_Name,  spPractitioner_Organization,  spPractitioner_Phonetic,  spPractitioner_Telecom);
  PATHS_TSearchParamsPractitioner : Array[TSearchParamsPractitioner] of String = ('_id: []',
     '_language: []',
     'address: []',
     'family: []',
     'gender: []',
     'given: []',
     'identifier: []',
     'name: []',
     'organization: [frtOrganization]',
     'phonetic: []',
     'telecom: []');
  TARGETS_TSearchParamsPractitioner : Array[TSearchParamsPractitioner] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [frtOrganization], [], []);
  CODES_TSearchParamsProcedure : Array[TSearchParamsProcedure] of String = ('_id', '_language', 'date', 'subject', 'type');
  DESC_TSearchParamsProcedure : Array[TSearchParamsProcedure] of String = ('The logical resource id associated with the resource (must be supported by all servers)',
     'The language of the resource',
     'The date the procedure was performed on',
     'The identity of a patient to list procedures  for',
     'Type of procedure');
  TYPES_TSearchParamsProcedure : Array[TSearchParamsProcedure] of TFhirSearchParamType = ( SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeDate,  SearchParamTypeReference,  SearchParamTypeToken);
//  CHECK_TSearchParamsProcedure : Array[TSearchParamsProcedure] of TSearchParamsProcedure = ( spProcedure__id,  spProcedure__language,  spProcedure_Date,  spProcedure_Subject,  spProcedure_Type);
  PATHS_TSearchParamsProcedure : Array[TSearchParamsProcedure] of String = ('_id: []',
     '_language: []',
     'date: []',
     'subject: [frtPatient]',
     'type: []');
  TARGETS_TSearchParamsProcedure : Array[TSearchParamsProcedure] of TFhirResourceTypeSet = ([], [], [], [frtPatient], []);
  CODES_TSearchParamsProfile : Array[TSearchParamsProfile] of String = ('_id', '_language', 'code', 'date', 'description', 'extension', 'identifier', 'name', 'publisher', 'status', 'type', 'valueset', 'version');
  DESC_TSearchParamsProfile : Array[TSearchParamsProfile] of String = ('The logical resource id associated with the resource (must be supported by all servers)',
     'The language of the resource',
     'A code for the profile in the format uri::code (server may choose to do subsumption)',
     'The profile publication date',
     'Text search in the description of the profile',
     'An extension code (use or definition)',
     'The identifier of the profile',
     'Name of the profile',
     'Name of the publisher of the profile',
     'The current status of the profile',
     'Type of resource that is constrained in the profile',
     'A vocabulary binding code',
     'The version identifier of the profile');
  TYPES_TSearchParamsProfile : Array[TSearchParamsProfile] of TFhirSearchParamType = ( SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeDate,  SearchParamTypeString,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeString,  SearchParamTypeString,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeReference,  SearchParamTypeToken);
//  CHECK_TSearchParamsProfile : Array[TSearchParamsProfile] of TSearchParamsProfile = ( spProfile__id,  spProfile__language,  spProfile_Code,  spProfile_Date,  spProfile_Description,  spProfile_Extension,  spProfile_Identifier,  spProfile_Name,  spProfile_Publisher,  spProfile_Status,  spProfile_Type,  spProfile_Valueset,  spProfile_Version);
  PATHS_TSearchParamsProfile : Array[TSearchParamsProfile] of String = ('_id: []',
     '_language: []',
     'code: []',
     'date: []',
     'description: []',
     'extension: []',
     'identifier: []',
     'name: []',
     'publisher: []',
     'status: []',
     'type: []',
     'valueset: [frtValueSet]',
     'version: []');
  TARGETS_TSearchParamsProfile : Array[TSearchParamsProfile] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [], [frtValueSet], []);
  CODES_TSearchParamsProvenance : Array[TSearchParamsProvenance] of String = ('_id', '_language', 'end', 'location', 'party', 'partytype', 'start', 'target');
  DESC_TSearchParamsProvenance : Array[TSearchParamsProvenance] of String = ('The logical resource id associated with the resource (must be supported by all servers)',
     'The language of the resource',
     'End time with inclusive boundary, if not ongoing',
     'Where the activity occurred, if relevant',
     'Identity of agent (urn or url)',
     'e.g. Resource | Person | Application | Record | Document +',
     'Starting time with inclusive boundary',
     'Target resource(s) (usually version specific)');
  TYPES_TSearchParamsProvenance : Array[TSearchParamsProvenance] of TFhirSearchParamType = ( SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeDate,  SearchParamTypeReference,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeDate,  SearchParamTypeReference);
//  CHECK_TSearchParamsProvenance : Array[TSearchParamsProvenance] of TSearchParamsProvenance = ( spProvenance__id,  spProvenance__language,  spProvenance_End,  spProvenance_Location,  spProvenance_Party,  spProvenance_Partytype,  spProvenance_Start,  spProvenance_Target);
  PATHS_TSearchParamsProvenance : Array[TSearchParamsProvenance] of String = ('_id: []',
     '_language: []',
     'end: []',
     'location: [frtLocation]',
     'party: []',
     'partytype: []',
     'start: []',
     'target: ALL_RESOURCE_TYPES');
  TARGETS_TSearchParamsProvenance : Array[TSearchParamsProvenance] of TFhirResourceTypeSet = ([], [], [], [frtLocation], [], [], [], ALL_RESOURCE_TYPES);
  CODES_TSearchParamsQuery : Array[TSearchParamsQuery] of String = ('_id', '_language', 'identifier', 'response');
  DESC_TSearchParamsQuery : Array[TSearchParamsQuery] of String = ('The logical resource id associated with the resource (must be supported by all servers)',
     'The language of the resource',
     'Links query and its response(s)',
     'Links response to source query');
  TYPES_TSearchParamsQuery : Array[TSearchParamsQuery] of TFhirSearchParamType = ( SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeToken);
//  CHECK_TSearchParamsQuery : Array[TSearchParamsQuery] of TSearchParamsQuery = ( spQuery__id,  spQuery__language,  spQuery_Identifier,  spQuery_Response);
  PATHS_TSearchParamsQuery : Array[TSearchParamsQuery] of String = ('_id: []',
     '_language: []',
     'identifier: []',
     'response: []');
  TARGETS_TSearchParamsQuery : Array[TSearchParamsQuery] of TFhirResourceTypeSet = ([], [], [], []);
  CODES_TSearchParamsQuestionnaire : Array[TSearchParamsQuestionnaire] of String = ('_id', '_language', 'author', 'authored', 'encounter', 'identifier', 'name', 'status', 'subject');
  DESC_TSearchParamsQuestionnaire : Array[TSearchParamsQuestionnaire] of String = ('The logical resource id associated with the resource (must be supported by all servers)',
     'The language of the resource',
     'The author of the questionnaire',
     'When the questionnaire was authored',
     'Encounter during which questionnaire was authored',
     'An identifier for the questionnaire',
     'Name of the questionnaire',
     'The status of the questionnaire',
     'The subject of the questionnaire');
  TYPES_TSearchParamsQuestionnaire : Array[TSearchParamsQuestionnaire] of TFhirSearchParamType = ( SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeReference,  SearchParamTypeDate,  SearchParamTypeReference,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeReference);
//  CHECK_TSearchParamsQuestionnaire : Array[TSearchParamsQuestionnaire] of TSearchParamsQuestionnaire = ( spQuestionnaire__id,  spQuestionnaire__language,  spQuestionnaire_Author,  spQuestionnaire_Authored,  spQuestionnaire_Encounter,  spQuestionnaire_Identifier,  spQuestionnaire_Name,  spQuestionnaire_Status,  spQuestionnaire_Subject);
  PATHS_TSearchParamsQuestionnaire : Array[TSearchParamsQuestionnaire] of String = ('_id: []',
     '_language: []',
     'author: [frtPatient, frtPractitioner, frtRelatedPerson]',
     'authored: []',
     'encounter: [frtEncounter]',
     'identifier: []',
     'name: []',
     'status: []',
     'subject: [frtPatient, frtRelatedPerson]');
  TARGETS_TSearchParamsQuestionnaire : Array[TSearchParamsQuestionnaire] of TFhirResourceTypeSet = ([], [], [frtPatient, frtPractitioner, frtRelatedPerson], [], [frtEncounter], [], [], [], [frtPatient, frtRelatedPerson]);
  CODES_TSearchParamsRelatedPerson : Array[TSearchParamsRelatedPerson] of String = ('_id', '_language', 'address', 'gender', 'identifier', 'name', 'patient', 'phonetic', 'telecom');
  DESC_TSearchParamsRelatedPerson : Array[TSearchParamsRelatedPerson] of String = ('The logical resource id associated with the resource (must be supported by all servers)',
     'The language of the resource',
     'An address in any kind of address/part',
     'Gender of the person',
     'A patient Identifier',
     'A portion of name in any name part',
     'The patient this person is related to',
     'A portion of name using some kind of phonetic matching algorithm',
     'The value in any kind of contact');
  TYPES_TSearchParamsRelatedPerson : Array[TSearchParamsRelatedPerson] of TFhirSearchParamType = ( SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeString,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeString,  SearchParamTypeReference,  SearchParamTypeString,  SearchParamTypeString);
//  CHECK_TSearchParamsRelatedPerson : Array[TSearchParamsRelatedPerson] of TSearchParamsRelatedPerson = ( spRelatedPerson__id,  spRelatedPerson__language,  spRelatedPerson_Address,  spRelatedPerson_Gender,  spRelatedPerson_Identifier,  spRelatedPerson_Name,  spRelatedPerson_Patient,  spRelatedPerson_Phonetic,  spRelatedPerson_Telecom);
  PATHS_TSearchParamsRelatedPerson : Array[TSearchParamsRelatedPerson] of String = ('_id: []',
     '_language: []',
     'address: []',
     'gender: []',
     'identifier: []',
     'name: []',
     'patient: [frtPatient]',
     'phonetic: []',
     'telecom: []');
  TARGETS_TSearchParamsRelatedPerson : Array[TSearchParamsRelatedPerson] of TFhirResourceTypeSet = ([], [], [], [], [], [], [frtPatient], [], []);
  CODES_TSearchParamsSecurityEvent : Array[TSearchParamsSecurityEvent] of String = ('_id', '_language', 'action', 'address', 'altid', 'date', 'desc', 'identity', 'name', 'object-type', 'patientid', 'reference', 'site', 'source', 'subtype', 'type', 'user');
  DESC_TSearchParamsSecurityEvent : Array[TSearchParamsSecurityEvent] of String = ('The logical resource id associated with the resource (must be supported by all servers)',
     'The language of the resource',
     'Type of action performed during the event',
     'Identifier for the network access point of the user device',
     'Alternative User id e.g. authentication',
     'Time when the event occurred on source',
     'Instance-specific descriptor for Object',
     'Specific instance of object (e.g. versioned)',
     'Human-meaningful name for the user',
     'Object type being audited',
     'The id of the patient (one of multiple kinds of participations)',
     'Specific instance of resource (e.g. versioned)',
     'Logical source location within the enterprise',
     'The id of source where event originated',
     'More specific type/id for the event',
     'Type/identifier of event',
     'Unique identifier for the user');
  TYPES_TSearchParamsSecurityEvent : Array[TSearchParamsSecurityEvent] of TFhirSearchParamType = ( SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeDate,  SearchParamTypeString,  SearchParamTypeToken,  SearchParamTypeString,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeReference,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeToken);
//  CHECK_TSearchParamsSecurityEvent : Array[TSearchParamsSecurityEvent] of TSearchParamsSecurityEvent = ( spSecurityEvent__id,  spSecurityEvent__language,  spSecurityEvent_Action,  spSecurityEvent_Address,  spSecurityEvent_Altid,  spSecurityEvent_Date,  spSecurityEvent_Desc,  spSecurityEvent_Identity,  spSecurityEvent_Name,  spSecurityEvent_Object_type,  spSecurityEvent_Patientid,  spSecurityEvent_Reference,  spSecurityEvent_Site,  spSecurityEvent_Source,  spSecurityEvent_Subtype,  spSecurityEvent_Type,  spSecurityEvent_User);
  PATHS_TSearchParamsSecurityEvent : Array[TSearchParamsSecurityEvent] of String = ('_id: []',
     '_language: []',
     'action: []',
     'address: []',
     'altid: []',
     'date: []',
     'desc: []',
     'identity: []',
     'name: []',
     'object-type: []',
     'patientid: []',
     'reference: ALL_RESOURCE_TYPES',
     'site: []',
     'source: []',
     'subtype: []',
     'type: []',
     'user: []');
  TARGETS_TSearchParamsSecurityEvent : Array[TSearchParamsSecurityEvent] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [], ALL_RESOURCE_TYPES, [], [], [], [], []);
  CODES_TSearchParamsSpecimen : Array[TSearchParamsSpecimen] of String = ('_id', '_language', 'subject');
  DESC_TSearchParamsSpecimen : Array[TSearchParamsSpecimen] of String = ('The logical resource id associated with the resource (must be supported by all servers)',
     'The language of the resource',
     'The subject of the specimen');
  TYPES_TSearchParamsSpecimen : Array[TSearchParamsSpecimen] of TFhirSearchParamType = ( SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeReference);
//  CHECK_TSearchParamsSpecimen : Array[TSearchParamsSpecimen] of TSearchParamsSpecimen = ( spSpecimen__id,  spSpecimen__language,  spSpecimen_Subject);
  PATHS_TSearchParamsSpecimen : Array[TSearchParamsSpecimen] of String = ('_id: []',
     '_language: []',
     'subject: [frtDevice, frtPatient, frtSubstance, frtGroup]');
  TARGETS_TSearchParamsSpecimen : Array[TSearchParamsSpecimen] of TFhirResourceTypeSet = ([], [], [frtDevice, frtPatient, frtSubstance, frtGroup]);
  CODES_TSearchParamsSubstance : Array[TSearchParamsSubstance] of String = ('_id', '_language', 'expiry', 'identifier', 'quantity', 'substance', 'type');
  DESC_TSearchParamsSubstance : Array[TSearchParamsSubstance] of String = ('The logical resource id associated with the resource (must be supported by all servers)',
     'The language of the resource',
     'When no longer valid to use',
     'Identifier of the package/container',
     'Amount of substance in the package',
     'A component of the substance',
     'The type of the substance');
  TYPES_TSearchParamsSubstance : Array[TSearchParamsSubstance] of TFhirSearchParamType = ( SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeDate,  SearchParamTypeToken,  SearchParamTypeNumber,  SearchParamTypeReference,  SearchParamTypeToken);
//  CHECK_TSearchParamsSubstance : Array[TSearchParamsSubstance] of TSearchParamsSubstance = ( spSubstance__id,  spSubstance__language,  spSubstance_Expiry,  spSubstance_Identifier,  spSubstance_Quantity,  spSubstance_Substance,  spSubstance_Type);
  PATHS_TSearchParamsSubstance : Array[TSearchParamsSubstance] of String = ('_id: []',
     '_language: []',
     'expiry: []',
     'identifier: []',
     'quantity: []',
     'substance: [frtSubstance]',
     'type: []');
  TARGETS_TSearchParamsSubstance : Array[TSearchParamsSubstance] of TFhirResourceTypeSet = ([], [], [], [], [], [frtSubstance], []);
  CODES_TSearchParamsSupply : Array[TSearchParamsSupply] of String = ('_id', '_language', 'dispenseid', 'dispensestatus', 'identifier', 'kind', 'patient', 'status', 'supplier');
  DESC_TSearchParamsSupply : Array[TSearchParamsSupply] of String = ('The logical resource id associated with the resource (must be supported by all servers)',
     'The language of the resource',
     'External identifier',
     'in progress | dispensed | abandoned',
     'Unique identifier',
     'The kind of supply (central, non-stock, etc)',
     'Patient for whom the item is supplied',
     'requested | dispensed | received | failed | cancelled',
     'Dispenser');
  TYPES_TSearchParamsSupply : Array[TSearchParamsSupply] of TFhirSearchParamType = ( SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeReference,  SearchParamTypeToken,  SearchParamTypeReference);
//  CHECK_TSearchParamsSupply : Array[TSearchParamsSupply] of TSearchParamsSupply = ( spSupply__id,  spSupply__language,  spSupply_Dispenseid,  spSupply_Dispensestatus,  spSupply_Identifier,  spSupply_Kind,  spSupply_Patient,  spSupply_Status,  spSupply_Supplier);
  PATHS_TSearchParamsSupply : Array[TSearchParamsSupply] of String = ('_id: []',
     '_language: []',
     'dispenseid: []',
     'dispensestatus: []',
     'identifier: []',
     'kind: []',
     'patient: [frtPatient]',
     'status: []',
     'supplier: [frtPractitioner]');
  TARGETS_TSearchParamsSupply : Array[TSearchParamsSupply] of TFhirResourceTypeSet = ([], [], [], [], [], [], [frtPatient], [], [frtPractitioner]);
  CODES_TSearchParamsValueSet : Array[TSearchParamsValueSet] of String = ('_id', '_language', 'code', 'date', 'description', 'identifier', 'name', 'publisher', 'reference', 'status', 'system', 'version');
  DESC_TSearchParamsValueSet : Array[TSearchParamsValueSet] of String = ('The logical resource id associated with the resource (must be supported by all servers)',
     'The language of the resource',
     'A code defined in the value set',
     'The value set publication date',
     'Text search in the description of the value set',
     'The identifier of the value set',
     'The name of the value set',
     'Name of the publisher of the value set',
     'A code system included or excluded in the value set or an imported value set',
     'The status of the value set',
     'The system for any codes defined by this value set',
     'The version identifier of the value set');
  TYPES_TSearchParamsValueSet : Array[TSearchParamsValueSet] of TFhirSearchParamType = ( SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeDate,  SearchParamTypeString,  SearchParamTypeToken,  SearchParamTypeString,  SearchParamTypeString,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeToken,  SearchParamTypeToken);
//  CHECK_TSearchParamsValueSet : Array[TSearchParamsValueSet] of TSearchParamsValueSet = ( spValueSet__id,  spValueSet__language,  spValueSet_Code,  spValueSet_Date,  spValueSet_Description,  spValueSet_Identifier,  spValueSet_Name,  spValueSet_Publisher,  spValueSet_Reference,  spValueSet_Status,  spValueSet_System,  spValueSet_Version);
  PATHS_TSearchParamsValueSet : Array[TSearchParamsValueSet] of String = ('_id: []',
     '_language: []',
     'code: []',
     'date: []',
     'description: []',
     'identifier: []',
     'name: []',
     'publisher: []',
     'reference: []',
     'status: []',
     'system: []',
     'version: []');
  TARGETS_TSearchParamsValueSet : Array[TSearchParamsValueSet] of TFhirResourceTypeSet = ([], [], [], [], [], [], [], [], [], [], [], []);
  FHIR_GENERATED_VERSION = '0.0.82';

  FHIR_GENERATED_REVISION = '2943';

  FHIR_GENERATED_DATE = '20140930180829';



implementation

end.

