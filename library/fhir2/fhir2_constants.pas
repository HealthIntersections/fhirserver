unit fhir2_constants;

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
  SysUtils, Classes, 
  fsl_utilities, fsl_stream, 
  fhir_objects, fhir2_types,
  fhir2_resources_base, fhir2_resources_canonical, fhir2_resources_admin, fhir2_resources_clinical, fhir2_resources_other;

const
  currentFHIRVersionRelease = fhirVersionRelease2;

Type
{$IFDEF FHIR_ACCOUNT}
  // Search Parameters for Account
  TSearchParamsAccount = (
    spAccount__content, 
    spAccount__id, 
    spAccount__lastUpdated, 
    spAccount__profile, 
    spAccount__query, 
    spAccount__security, 
    spAccount__tag, 
    spAccount__text, 
    spAccount_Balance, 
    spAccount_Identifier, 
    spAccount_Name, 
    spAccount_Owner, 
    spAccount_Patient, 
    spAccount_Period, 
    spAccount_Status, 
    spAccount_Subject, 
    spAccount_Type); 
{$ENDIF}

{$IFDEF FHIR_ALLERGYINTOLERANCE}
  // Search Parameters for AllergyIntolerance
  TSearchParamsAllergyIntolerance = (
    spAllergyIntolerance__content, 
    spAllergyIntolerance__id, 
    spAllergyIntolerance__lastUpdated, 
    spAllergyIntolerance__profile, 
    spAllergyIntolerance__query, 
    spAllergyIntolerance__security, 
    spAllergyIntolerance__tag, 
    spAllergyIntolerance__text, 
    spAllergyIntolerance_Category, 
    spAllergyIntolerance_Criticality, 
    spAllergyIntolerance_Date, 
    spAllergyIntolerance_Identifier, 
    spAllergyIntolerance_Lastdate, 
    spAllergyIntolerance_Manifestation, 
    spAllergyIntolerance_Onset, 
    spAllergyIntolerance_Patient, 
    spAllergyIntolerance_Recorder, 
    spAllergyIntolerance_Reporter, 
    spAllergyIntolerance_Route, 
    spAllergyIntolerance_Severity, 
    spAllergyIntolerance_Status, 
    spAllergyIntolerance_Substance, 
    spAllergyIntolerance_Type); 
{$ENDIF}

{$IFDEF FHIR_APPOINTMENT}
  // Search Parameters for Appointment
  TSearchParamsAppointment = (
    spAppointment__content, 
    spAppointment__id, 
    spAppointment__lastUpdated, 
    spAppointment__profile, 
    spAppointment__query, 
    spAppointment__security, 
    spAppointment__tag, 
    spAppointment__text, 
    spAppointment_Actor, 
    spAppointment_Date, 
    spAppointment_Identifier, 
    spAppointment_Location, 
    spAppointment_Partstatus, 
    spAppointment_Patient, 
    spAppointment_Practitioner, 
    spAppointment_Status); 
{$ENDIF}

{$IFDEF FHIR_APPOINTMENTRESPONSE}
  // Search Parameters for AppointmentResponse
  TSearchParamsAppointmentResponse = (
    spAppointmentResponse__content, 
    spAppointmentResponse__id, 
    spAppointmentResponse__lastUpdated, 
    spAppointmentResponse__profile, 
    spAppointmentResponse__query, 
    spAppointmentResponse__security, 
    spAppointmentResponse__tag, 
    spAppointmentResponse__text, 
    spAppointmentResponse_Actor, 
    spAppointmentResponse_Appointment, 
    spAppointmentResponse_Identifier, 
    spAppointmentResponse_Location, 
    spAppointmentResponse_Partstatus, 
    spAppointmentResponse_Patient, 
    spAppointmentResponse_Practitioner); 
{$ENDIF}

{$IFDEF FHIR_AUDITEVENT}
  // Search Parameters for AuditEvent
  TSearchParamsAuditEvent = (
    spAuditEvent__content, 
    spAuditEvent__id, 
    spAuditEvent__lastUpdated, 
    spAuditEvent__profile, 
    spAuditEvent__query, 
    spAuditEvent__security, 
    spAuditEvent__tag, 
    spAuditEvent__text, 
    spAuditEvent_Action, 
    spAuditEvent_Address, 
    spAuditEvent_Altid, 
    spAuditEvent_Date, 
    spAuditEvent_Desc, 
    spAuditEvent_Identity, 
    spAuditEvent_Name, 
    spAuditEvent_Objecttype, 
    spAuditEvent_Participant, 
    spAuditEvent_Patient, 
    spAuditEvent_Policy, 
    spAuditEvent_Reference, 
    spAuditEvent_Site, 
    spAuditEvent_Source, 
    spAuditEvent_Subtype, 
    spAuditEvent_Type, 
    spAuditEvent_User); 
{$ENDIF}

{$IFDEF FHIR_BASIC}
  // Search Parameters for Basic
  TSearchParamsBasic = (
    spBasic__content, 
    spBasic__id, 
    spBasic__lastUpdated, 
    spBasic__profile, 
    spBasic__query, 
    spBasic__security, 
    spBasic__tag, 
    spBasic__text, 
    spBasic_Author, 
    spBasic_Code, 
    spBasic_Created, 
    spBasic_Identifier, 
    spBasic_Patient, 
    spBasic_Subject); 
{$ENDIF}

{$IFDEF FHIR_BINARY}
  // Search Parameters for Binary
  TSearchParamsBinary = (
    spBinary__content, 
    spBinary__id, 
    spBinary__lastUpdated, 
    spBinary__profile, 
    spBinary__query, 
    spBinary__security, 
    spBinary__tag, 
    spBinary__text, 
    spBinary_Contenttype); 
{$ENDIF}

{$IFDEF FHIR_BODYSITE}
  // Search Parameters for BodySite
  TSearchParamsBodySite = (
    spBodySite__content, 
    spBodySite__id, 
    spBodySite__lastUpdated, 
    spBodySite__profile, 
    spBodySite__query, 
    spBodySite__security, 
    spBodySite__tag, 
    spBodySite__text, 
    spBodySite_Code, 
    spBodySite_Identifier, 
    spBodySite_Patient); 
{$ENDIF}

{$IFDEF FHIR_BUNDLE}
  // Search Parameters for Bundle
  TSearchParamsBundle = (
    spBundle__content, 
    spBundle__id, 
    spBundle__lastUpdated, 
    spBundle__profile, 
    spBundle__query, 
    spBundle__security, 
    spBundle__tag, 
    spBundle__text, 
    spBundle_Composition, 
    spBundle_Message, 
    spBundle_Type); 
{$ENDIF}

{$IFDEF FHIR_CAREPLAN}
  // Search Parameters for CarePlan
  TSearchParamsCarePlan = (
    spCarePlan__content, 
    spCarePlan__id, 
    spCarePlan__lastUpdated, 
    spCarePlan__profile, 
    spCarePlan__query, 
    spCarePlan__security, 
    spCarePlan__tag, 
    spCarePlan__text, 
    spCarePlan_Activitycode, 
    spCarePlan_Activitydate, 
    spCarePlan_Activityreference, 
    spCarePlan_Condition, 
    spCarePlan_Date, 
    spCarePlan_Goal, 
    spCarePlan_Participant, 
    spCarePlan_Patient, 
    spCarePlan_Performer, 
    spCarePlan_Related, 
    spCarePlan_Relatedcode, 
    spCarePlan_Relatedplan, 
    spCarePlan_Subject); 
{$ENDIF}

{$IFDEF FHIR_CLAIM}
  // Search Parameters for Claim
  TSearchParamsClaim = (
    spClaim__content, 
    spClaim__id, 
    spClaim__lastUpdated, 
    spClaim__profile, 
    spClaim__query, 
    spClaim__security, 
    spClaim__tag, 
    spClaim__text, 
    spClaim_Identifier, 
    spClaim_Patient, 
    spClaim_Priority, 
    spClaim_Provider, 
    spClaim_Use); 
{$ENDIF}

{$IFDEF FHIR_CLAIMRESPONSE}
  // Search Parameters for ClaimResponse
  TSearchParamsClaimResponse = (
    spClaimResponse__content, 
    spClaimResponse__id, 
    spClaimResponse__lastUpdated, 
    spClaimResponse__profile, 
    spClaimResponse__query, 
    spClaimResponse__security, 
    spClaimResponse__tag, 
    spClaimResponse__text, 
    spClaimResponse_Identifier); 
{$ENDIF}

{$IFDEF FHIR_CLINICALIMPRESSION}
  // Search Parameters for ClinicalImpression
  TSearchParamsClinicalImpression = (
    spClinicalImpression__content, 
    spClinicalImpression__id, 
    spClinicalImpression__lastUpdated, 
    spClinicalImpression__profile, 
    spClinicalImpression__query, 
    spClinicalImpression__security, 
    spClinicalImpression__tag, 
    spClinicalImpression__text, 
    spClinicalImpression_Action, 
    spClinicalImpression_Assessor, 
    spClinicalImpression_Date, 
    spClinicalImpression_Finding, 
    spClinicalImpression_Investigation, 
    spClinicalImpression_Patient, 
    spClinicalImpression_Plan, 
    spClinicalImpression_Previous, 
    spClinicalImpression_Problem, 
    spClinicalImpression_Resolved, 
    spClinicalImpression_Ruledout, 
    spClinicalImpression_Status, 
    spClinicalImpression_Trigger, 
    spClinicalImpression_Triggercode); 
{$ENDIF}

{$IFDEF FHIR_COMMUNICATION}
  // Search Parameters for Communication
  TSearchParamsCommunication = (
    spCommunication__content, 
    spCommunication__id, 
    spCommunication__lastUpdated, 
    spCommunication__profile, 
    spCommunication__query, 
    spCommunication__security, 
    spCommunication__tag, 
    spCommunication__text, 
    spCommunication_Category, 
    spCommunication_Encounter, 
    spCommunication_Identifier, 
    spCommunication_Medium, 
    spCommunication_Patient, 
    spCommunication_Received, 
    spCommunication_Recipient, 
    spCommunication_Request, 
    spCommunication_Sender, 
    spCommunication_Sent, 
    spCommunication_Status, 
    spCommunication_Subject); 
{$ENDIF}

{$IFDEF FHIR_COMMUNICATIONREQUEST}
  // Search Parameters for CommunicationRequest
  TSearchParamsCommunicationRequest = (
    spCommunicationRequest__content, 
    spCommunicationRequest__id, 
    spCommunicationRequest__lastUpdated, 
    spCommunicationRequest__profile, 
    spCommunicationRequest__query, 
    spCommunicationRequest__security, 
    spCommunicationRequest__tag, 
    spCommunicationRequest__text, 
    spCommunicationRequest_Category, 
    spCommunicationRequest_Encounter, 
    spCommunicationRequest_Identifier, 
    spCommunicationRequest_Medium, 
    spCommunicationRequest_Patient, 
    spCommunicationRequest_Priority, 
    spCommunicationRequest_Recipient, 
    spCommunicationRequest_Requested, 
    spCommunicationRequest_Requester, 
    spCommunicationRequest_Sender, 
    spCommunicationRequest_Status, 
    spCommunicationRequest_Subject, 
    spCommunicationRequest_Time); 
{$ENDIF}

{$IFDEF FHIR_COMPOSITION}
  // Search Parameters for Composition
  TSearchParamsComposition = (
    spComposition__content, 
    spComposition__id, 
    spComposition__lastUpdated, 
    spComposition__profile, 
    spComposition__query, 
    spComposition__security, 
    spComposition__tag, 
    spComposition__text, 
    spComposition_Attester, 
    spComposition_Author, 
    spComposition_Class, 
    spComposition_Confidentiality, 
    spComposition_Context, 
    spComposition_Date, 
    spComposition_Encounter, 
    spComposition_Entry, 
    spComposition_Identifier, 
    spComposition_Patient, 
    spComposition_Period, 
    spComposition_Section, 
    spComposition_Status, 
    spComposition_Subject, 
    spComposition_Title, 
    spComposition_Type); 
{$ENDIF}

{$IFDEF FHIR_CONCEPTMAP}
  // Search Parameters for ConceptMap
  TSearchParamsConceptMap = (
    spConceptMap__content, 
    spConceptMap__id, 
    spConceptMap__lastUpdated, 
    spConceptMap__profile, 
    spConceptMap__query, 
    spConceptMap__security, 
    spConceptMap__tag, 
    spConceptMap__text, 
    spConceptMap_Context, 
    spConceptMap_Date, 
    spConceptMap_Dependson, 
    spConceptMap_Description, 
    spConceptMap_Identifier, 
    spConceptMap_Name, 
    spConceptMap_Product, 
    spConceptMap_Publisher, 
    spConceptMap_Source, 
    spConceptMap_Sourcecode, 
    spConceptMap_Sourcesystem, 
    spConceptMap_Sourceuri, 
    spConceptMap_Status, 
    spConceptMap_Target, 
    spConceptMap_Targetcode, 
    spConceptMap_Targetsystem, 
    spConceptMap_Url, 
    spConceptMap_Version); 
{$ENDIF}

{$IFDEF FHIR_CONDITION}
  // Search Parameters for Condition
  TSearchParamsCondition = (
    spCondition__content, 
    spCondition__id, 
    spCondition__lastUpdated, 
    spCondition__profile, 
    spCondition__query, 
    spCondition__security, 
    spCondition__tag, 
    spCondition__text, 
    spCondition_Age, 
    spCondition_Asserter, 
    spCondition_Bodysite, 
    spCondition_Category, 
    spCondition_Clinicalstatus, 
    spCondition_Code, 
    spCondition_Daterecorded, 
    spCondition_Encounter, 
    spCondition_Evidence, 
    spCondition_Identifier, 
    spCondition_Onset, 
    spCondition_Onsetinfo, 
    spCondition_Patient, 
    spCondition_Severity, 
    spCondition_Stage); 
{$ENDIF}

{$IFDEF FHIR_CONFORMANCE}
  // Search Parameters for Conformance
  TSearchParamsConformance = (
    spConformance__content, 
    spConformance__id, 
    spConformance__lastUpdated, 
    spConformance__profile, 
    spConformance__query, 
    spConformance__security, 
    spConformance__tag, 
    spConformance__text, 
    spConformance_Date, 
    spConformance_Description, 
    spConformance_Event, 
    spConformance_Fhirversion, 
    spConformance_Format, 
    spConformance_Mode, 
    spConformance_Name, 
    spConformance_Profile, 
    spConformance_Publisher, 
    spConformance_Resource, 
    spConformance_Security, 
    spConformance_Software, 
    spConformance_Status, 
    spConformance_Supportedprofile, 
    spConformance_Url, 
    spConformance_Version); 
{$ENDIF}

{$IFDEF FHIR_CONTRACT}
  // Search Parameters for Contract
  TSearchParamsContract = (
    spContract__content, 
    spContract__id, 
    spContract__lastUpdated, 
    spContract__profile, 
    spContract__query, 
    spContract__security, 
    spContract__tag, 
    spContract__text, 
    spContract_Actor, 
    spContract_Identifier, 
    spContract_Patient, 
    spContract_Signer, 
    spContract_Subject); 
{$ENDIF}

{$IFDEF FHIR_COVERAGE}
  // Search Parameters for Coverage
  TSearchParamsCoverage = (
    spCoverage__content, 
    spCoverage__id, 
    spCoverage__lastUpdated, 
    spCoverage__profile, 
    spCoverage__query, 
    spCoverage__security, 
    spCoverage__tag, 
    spCoverage__text, 
    spCoverage_Dependent, 
    spCoverage_Group, 
    spCoverage_Identifier, 
    spCoverage_Issuer, 
    spCoverage_Plan, 
    spCoverage_Sequence, 
    spCoverage_Subplan, 
    spCoverage_Type); 
{$ENDIF}

{$IFDEF FHIR_DATAELEMENT}
  // Search Parameters for DataElement
  TSearchParamsDataElement = (
    spDataElement__content, 
    spDataElement__id, 
    spDataElement__lastUpdated, 
    spDataElement__profile, 
    spDataElement__query, 
    spDataElement__security, 
    spDataElement__tag, 
    spDataElement__text, 
    spDataElement_Code, 
    spDataElement_Context, 
    spDataElement_Date, 
    spDataElement_Description, 
    spDataElement_Identifier, 
    spDataElement_Name, 
    spDataElement_ObjectClass, 
    spDataElement_ObjectClassProperty, 
    spDataElement_Publisher, 
    spDataElement_Status, 
    spDataElement_Stringency, 
    spDataElement_Url, 
    spDataElement_Version); 
{$ENDIF}

{$IFDEF FHIR_DETECTEDISSUE}
  // Search Parameters for DetectedIssue
  TSearchParamsDetectedIssue = (
    spDetectedIssue__content, 
    spDetectedIssue__id, 
    spDetectedIssue__lastUpdated, 
    spDetectedIssue__profile, 
    spDetectedIssue__query, 
    spDetectedIssue__security, 
    spDetectedIssue__tag, 
    spDetectedIssue__text, 
    spDetectedIssue_Author, 
    spDetectedIssue_Category, 
    spDetectedIssue_Date, 
    spDetectedIssue_Identifier, 
    spDetectedIssue_Implicated, 
    spDetectedIssue_Patient); 
{$ENDIF}

{$IFDEF FHIR_DEVICE}
  // Search Parameters for Device
  TSearchParamsDevice = (
    spDevice__content, 
    spDevice__id, 
    spDevice__lastUpdated, 
    spDevice__profile, 
    spDevice__query, 
    spDevice__security, 
    spDevice__tag, 
    spDevice__text, 
    spDevice_Identifier, 
    spDevice_Location, 
    spDevice_Manufacturer, 
    spDevice_Model, 
    spDevice_Organization, 
    spDevice_Patient, 
    spDevice_Type, 
    spDevice_Udi, 
    spDevice_Url); 
{$ENDIF}

{$IFDEF FHIR_DEVICECOMPONENT}
  // Search Parameters for DeviceComponent
  TSearchParamsDeviceComponent = (
    spDeviceComponent__content, 
    spDeviceComponent__id, 
    spDeviceComponent__lastUpdated, 
    spDeviceComponent__profile, 
    spDeviceComponent__query, 
    spDeviceComponent__security, 
    spDeviceComponent__tag, 
    spDeviceComponent__text, 
    spDeviceComponent_Parent, 
    spDeviceComponent_Source, 
    spDeviceComponent_Type); 
{$ENDIF}

{$IFDEF FHIR_DEVICEMETRIC}
  // Search Parameters for DeviceMetric
  TSearchParamsDeviceMetric = (
    spDeviceMetric__content, 
    spDeviceMetric__id, 
    spDeviceMetric__lastUpdated, 
    spDeviceMetric__profile, 
    spDeviceMetric__query, 
    spDeviceMetric__security, 
    spDeviceMetric__tag, 
    spDeviceMetric__text, 
    spDeviceMetric_Category, 
    spDeviceMetric_Identifier, 
    spDeviceMetric_Parent, 
    spDeviceMetric_Source, 
    spDeviceMetric_Type); 
{$ENDIF}

{$IFDEF FHIR_DEVICEUSEREQUEST}
  // Search Parameters for DeviceUseRequest
  TSearchParamsDeviceUseRequest = (
    spDeviceUseRequest__content, 
    spDeviceUseRequest__id, 
    spDeviceUseRequest__lastUpdated, 
    spDeviceUseRequest__profile, 
    spDeviceUseRequest__query, 
    spDeviceUseRequest__security, 
    spDeviceUseRequest__tag, 
    spDeviceUseRequest__text, 
    spDeviceUseRequest_Device, 
    spDeviceUseRequest_Patient, 
    spDeviceUseRequest_Subject); 
{$ENDIF}

{$IFDEF FHIR_DEVICEUSESTATEMENT}
  // Search Parameters for DeviceUseStatement
  TSearchParamsDeviceUseStatement = (
    spDeviceUseStatement__content, 
    spDeviceUseStatement__id, 
    spDeviceUseStatement__lastUpdated, 
    spDeviceUseStatement__profile, 
    spDeviceUseStatement__query, 
    spDeviceUseStatement__security, 
    spDeviceUseStatement__tag, 
    spDeviceUseStatement__text, 
    spDeviceUseStatement_Device, 
    spDeviceUseStatement_Patient, 
    spDeviceUseStatement_Subject); 
{$ENDIF}

{$IFDEF FHIR_DIAGNOSTICORDER}
  // Search Parameters for DiagnosticOrder
  TSearchParamsDiagnosticOrder = (
    spDiagnosticOrder__content, 
    spDiagnosticOrder__id, 
    spDiagnosticOrder__lastUpdated, 
    spDiagnosticOrder__profile, 
    spDiagnosticOrder__query, 
    spDiagnosticOrder__security, 
    spDiagnosticOrder__tag, 
    spDiagnosticOrder__text, 
    spDiagnosticOrder_Actor, 
    spDiagnosticOrder_Bodysite, 
    spDiagnosticOrder_Code, 
    spDiagnosticOrder_Encounter, 
    spDiagnosticOrder_Eventdate, 
    spDiagnosticOrder_Eventstatus, 
    spDiagnosticOrder_Eventstatusdate, 
    spDiagnosticOrder_Identifier, 
    spDiagnosticOrder_Itemdate, 
    spDiagnosticOrder_Itempaststatus, 
    spDiagnosticOrder_Itemstatus, 
    spDiagnosticOrder_Itemstatusdate, 
    spDiagnosticOrder_Orderer, 
    spDiagnosticOrder_Patient, 
    spDiagnosticOrder_Specimen, 
    spDiagnosticOrder_Status, 
    spDiagnosticOrder_Subject); 
{$ENDIF}

{$IFDEF FHIR_DIAGNOSTICREPORT}
  // Search Parameters for DiagnosticReport
  TSearchParamsDiagnosticReport = (
    spDiagnosticReport__content, 
    spDiagnosticReport__id, 
    spDiagnosticReport__lastUpdated, 
    spDiagnosticReport__profile, 
    spDiagnosticReport__query, 
    spDiagnosticReport__security, 
    spDiagnosticReport__tag, 
    spDiagnosticReport__text, 
    spDiagnosticReport_Category, 
    spDiagnosticReport_Code, 
    spDiagnosticReport_Date, 
    spDiagnosticReport_Diagnosis, 
    spDiagnosticReport_Encounter, 
    spDiagnosticReport_Identifier, 
    spDiagnosticReport_Image, 
    spDiagnosticReport_Issued, 
    spDiagnosticReport_Patient, 
    spDiagnosticReport_Performer, 
    spDiagnosticReport_Request, 
    spDiagnosticReport_Result, 
    spDiagnosticReport_Specimen, 
    spDiagnosticReport_Status, 
    spDiagnosticReport_Subject); 
{$ENDIF}

{$IFDEF FHIR_DOCUMENTMANIFEST}
  // Search Parameters for DocumentManifest
  TSearchParamsDocumentManifest = (
    spDocumentManifest__content, 
    spDocumentManifest__id, 
    spDocumentManifest__lastUpdated, 
    spDocumentManifest__profile, 
    spDocumentManifest__query, 
    spDocumentManifest__security, 
    spDocumentManifest__tag, 
    spDocumentManifest__text, 
    spDocumentManifest_Author, 
    spDocumentManifest_Contentref, 
    spDocumentManifest_Created, 
    spDocumentManifest_Description, 
    spDocumentManifest_Identifier, 
    spDocumentManifest_Patient, 
    spDocumentManifest_Recipient, 
    spDocumentManifest_Relatedid, 
    spDocumentManifest_Relatedref, 
    spDocumentManifest_Source, 
    spDocumentManifest_Status, 
    spDocumentManifest_Subject, 
    spDocumentManifest_Type); 
{$ENDIF}

{$IFDEF FHIR_DOCUMENTREFERENCE}
  // Search Parameters for DocumentReference
  TSearchParamsDocumentReference = (
    spDocumentReference__content, 
    spDocumentReference__id, 
    spDocumentReference__lastUpdated, 
    spDocumentReference__profile, 
    spDocumentReference__query, 
    spDocumentReference__security, 
    spDocumentReference__tag, 
    spDocumentReference__text, 
    spDocumentReference_Authenticator, 
    spDocumentReference_Author, 
    spDocumentReference_Class, 
    spDocumentReference_Created, 
    spDocumentReference_Custodian, 
    spDocumentReference_Description, 
    spDocumentReference_Encounter, 
    spDocumentReference_Event, 
    spDocumentReference_Facility, 
    spDocumentReference_Format, 
    spDocumentReference_Identifier, 
    spDocumentReference_Indexed, 
    spDocumentReference_Language, 
    spDocumentReference_Location, 
    spDocumentReference_Patient, 
    spDocumentReference_Period, 
    spDocumentReference_Relatedid, 
    spDocumentReference_Relatedref, 
    spDocumentReference_Relatesto, 
    spDocumentReference_Relation, 
    spDocumentReference_Relationship, 
    spDocumentReference_Securitylabel, 
    spDocumentReference_Setting, 
    spDocumentReference_Status, 
    spDocumentReference_Subject, 
    spDocumentReference_Type); 
{$ENDIF}

{$IFDEF FHIR_ELIGIBILITYREQUEST}
  // Search Parameters for EligibilityRequest
  TSearchParamsEligibilityRequest = (
    spEligibilityRequest__content, 
    spEligibilityRequest__id, 
    spEligibilityRequest__lastUpdated, 
    spEligibilityRequest__profile, 
    spEligibilityRequest__query, 
    spEligibilityRequest__security, 
    spEligibilityRequest__tag, 
    spEligibilityRequest__text, 
    spEligibilityRequest_Identifier); 
{$ENDIF}

{$IFDEF FHIR_ELIGIBILITYRESPONSE}
  // Search Parameters for EligibilityResponse
  TSearchParamsEligibilityResponse = (
    spEligibilityResponse__content, 
    spEligibilityResponse__id, 
    spEligibilityResponse__lastUpdated, 
    spEligibilityResponse__profile, 
    spEligibilityResponse__query, 
    spEligibilityResponse__security, 
    spEligibilityResponse__tag, 
    spEligibilityResponse__text, 
    spEligibilityResponse_Identifier); 
{$ENDIF}

{$IFDEF FHIR_ENCOUNTER}
  // Search Parameters for Encounter
  TSearchParamsEncounter = (
    spEncounter__content, 
    spEncounter__id, 
    spEncounter__lastUpdated, 
    spEncounter__profile, 
    spEncounter__query, 
    spEncounter__security, 
    spEncounter__tag, 
    spEncounter__text, 
    spEncounter_Appointment, 
    spEncounter_Condition, 
    spEncounter_Date, 
    spEncounter_Episodeofcare, 
    spEncounter_Identifier, 
    spEncounter_Incomingreferral, 
    spEncounter_Indication, 
    spEncounter_Length, 
    spEncounter_Location, 
    spEncounter_Locationperiod, 
    spEncounter_Partof, 
    spEncounter_Participant, 
    spEncounter_Participanttype, 
    spEncounter_Patient, 
    spEncounter_Practitioner, 
    spEncounter_Procedure, 
    spEncounter_Reason, 
    spEncounter_Specialarrangement, 
    spEncounter_Status, 
    spEncounter_Type); 
{$ENDIF}

{$IFDEF FHIR_ENROLLMENTREQUEST}
  // Search Parameters for EnrollmentRequest
  TSearchParamsEnrollmentRequest = (
    spEnrollmentRequest__content, 
    spEnrollmentRequest__id, 
    spEnrollmentRequest__lastUpdated, 
    spEnrollmentRequest__profile, 
    spEnrollmentRequest__query, 
    spEnrollmentRequest__security, 
    spEnrollmentRequest__tag, 
    spEnrollmentRequest__text, 
    spEnrollmentRequest_Identifier, 
    spEnrollmentRequest_Patient, 
    spEnrollmentRequest_Subject); 
{$ENDIF}

{$IFDEF FHIR_ENROLLMENTRESPONSE}
  // Search Parameters for EnrollmentResponse
  TSearchParamsEnrollmentResponse = (
    spEnrollmentResponse__content, 
    spEnrollmentResponse__id, 
    spEnrollmentResponse__lastUpdated, 
    spEnrollmentResponse__profile, 
    spEnrollmentResponse__query, 
    spEnrollmentResponse__security, 
    spEnrollmentResponse__tag, 
    spEnrollmentResponse__text, 
    spEnrollmentResponse_Identifier); 
{$ENDIF}

{$IFDEF FHIR_EPISODEOFCARE}
  // Search Parameters for EpisodeOfCare
  TSearchParamsEpisodeOfCare = (
    spEpisodeOfCare__content, 
    spEpisodeOfCare__id, 
    spEpisodeOfCare__lastUpdated, 
    spEpisodeOfCare__profile, 
    spEpisodeOfCare__query, 
    spEpisodeOfCare__security, 
    spEpisodeOfCare__tag, 
    spEpisodeOfCare__text, 
    spEpisodeOfCare_Caremanager, 
    spEpisodeOfCare_Condition, 
    spEpisodeOfCare_Date, 
    spEpisodeOfCare_Identifier, 
    spEpisodeOfCare_Incomingreferral, 
    spEpisodeOfCare_Organization, 
    spEpisodeOfCare_Patient, 
    spEpisodeOfCare_Status, 
    spEpisodeOfCare_Teammember, 
    spEpisodeOfCare_Type); 
{$ENDIF}

{$IFDEF FHIR_EXPLANATIONOFBENEFIT}
  // Search Parameters for ExplanationOfBenefit
  TSearchParamsExplanationOfBenefit = (
    spExplanationOfBenefit__content, 
    spExplanationOfBenefit__id, 
    spExplanationOfBenefit__lastUpdated, 
    spExplanationOfBenefit__profile, 
    spExplanationOfBenefit__query, 
    spExplanationOfBenefit__security, 
    spExplanationOfBenefit__tag, 
    spExplanationOfBenefit__text, 
    spExplanationOfBenefit_Identifier); 
{$ENDIF}

{$IFDEF FHIR_FAMILYMEMBERHISTORY}
  // Search Parameters for FamilyMemberHistory
  TSearchParamsFamilyMemberHistory = (
    spFamilyMemberHistory__content, 
    spFamilyMemberHistory__id, 
    spFamilyMemberHistory__lastUpdated, 
    spFamilyMemberHistory__profile, 
    spFamilyMemberHistory__query, 
    spFamilyMemberHistory__security, 
    spFamilyMemberHistory__tag, 
    spFamilyMemberHistory__text, 
    spFamilyMemberHistory_Code, 
    spFamilyMemberHistory_Condition, 
    spFamilyMemberHistory_Date, 
    spFamilyMemberHistory_Gender, 
    spFamilyMemberHistory_Identifier, 
    spFamilyMemberHistory_Patient, 
    spFamilyMemberHistory_Relationship); 
{$ENDIF}

{$IFDEF FHIR_FLAG}
  // Search Parameters for Flag
  TSearchParamsFlag = (
    spFlag__content, 
    spFlag__id, 
    spFlag__lastUpdated, 
    spFlag__profile, 
    spFlag__query, 
    spFlag__security, 
    spFlag__tag, 
    spFlag__text, 
    spFlag_Author, 
    spFlag_Date, 
    spFlag_Encounter, 
    spFlag_Patient, 
    spFlag_Subject); 
{$ENDIF}

{$IFDEF FHIR_GOAL}
  // Search Parameters for Goal
  TSearchParamsGoal = (
    spGoal__content, 
    spGoal__id, 
    spGoal__lastUpdated, 
    spGoal__profile, 
    spGoal__query, 
    spGoal__security, 
    spGoal__tag, 
    spGoal__text, 
    spGoal_Category, 
    spGoal_Identifier, 
    spGoal_Patient, 
    spGoal_Status, 
    spGoal_Subject, 
    spGoal_Targetdate); 
{$ENDIF}

{$IFDEF FHIR_GROUP}
  // Search Parameters for Group
  TSearchParamsGroup = (
    spGroup__content, 
    spGroup__id, 
    spGroup__lastUpdated, 
    spGroup__profile, 
    spGroup__query, 
    spGroup__security, 
    spGroup__tag, 
    spGroup__text, 
    spGroup_Actual, 
    spGroup_Characteristic, 
    spGroup_Characteristicvalue, 
    spGroup_Code, 
    spGroup_Exclude, 
    spGroup_Identifier, 
    spGroup_Member, 
    spGroup_Type, 
    spGroup_Value); 
{$ENDIF}

{$IFDEF FHIR_HEALTHCARESERVICE}
  // Search Parameters for HealthcareService
  TSearchParamsHealthcareService = (
    spHealthcareService__content, 
    spHealthcareService__id, 
    spHealthcareService__lastUpdated, 
    spHealthcareService__profile, 
    spHealthcareService__query, 
    spHealthcareService__security, 
    spHealthcareService__tag, 
    spHealthcareService__text, 
    spHealthcareService_Characteristic, 
    spHealthcareService_Identifier, 
    spHealthcareService_Location, 
    spHealthcareService_Name, 
    spHealthcareService_Organization, 
    spHealthcareService_Programname, 
    spHealthcareService_Servicecategory, 
    spHealthcareService_Servicetype); 
{$ENDIF}

{$IFDEF FHIR_IMAGINGOBJECTSELECTION}
  // Search Parameters for ImagingObjectSelection
  TSearchParamsImagingObjectSelection = (
    spImagingObjectSelection__content, 
    spImagingObjectSelection__id, 
    spImagingObjectSelection__lastUpdated, 
    spImagingObjectSelection__profile, 
    spImagingObjectSelection__query, 
    spImagingObjectSelection__security, 
    spImagingObjectSelection__tag, 
    spImagingObjectSelection__text, 
    spImagingObjectSelection_Author, 
    spImagingObjectSelection_Authoringtime, 
    spImagingObjectSelection_Identifier, 
    spImagingObjectSelection_Patient, 
    spImagingObjectSelection_Selectedstudy, 
    spImagingObjectSelection_Title); 
{$ENDIF}

{$IFDEF FHIR_IMAGINGSTUDY}
  // Search Parameters for ImagingStudy
  TSearchParamsImagingStudy = (
    spImagingStudy__content, 
    spImagingStudy__id, 
    spImagingStudy__lastUpdated, 
    spImagingStudy__profile, 
    spImagingStudy__query, 
    spImagingStudy__security, 
    spImagingStudy__tag, 
    spImagingStudy__text, 
    spImagingStudy_Accession, 
    spImagingStudy_Bodysite, 
    spImagingStudy_Dicomclass, 
    spImagingStudy_Modality, 
    spImagingStudy_Order, 
    spImagingStudy_Patient, 
    spImagingStudy_Series, 
    spImagingStudy_Started, 
    spImagingStudy_Study, 
    spImagingStudy_Uid); 
{$ENDIF}

{$IFDEF FHIR_IMMUNIZATION}
  // Search Parameters for Immunization
  TSearchParamsImmunization = (
    spImmunization__content, 
    spImmunization__id, 
    spImmunization__lastUpdated, 
    spImmunization__profile, 
    spImmunization__query, 
    spImmunization__security, 
    spImmunization__tag, 
    spImmunization__text, 
    spImmunization_Date, 
    spImmunization_Dosesequence, 
    spImmunization_Identifier, 
    spImmunization_Location, 
    spImmunization_Lotnumber, 
    spImmunization_Manufacturer, 
    spImmunization_Notgiven, 
    spImmunization_Patient, 
    spImmunization_Performer, 
    spImmunization_Reaction, 
    spImmunization_Reactiondate, 
    spImmunization_Reason, 
    spImmunization_Reasonnotgiven, 
    spImmunization_Requester, 
    spImmunization_Status, 
    spImmunization_Vaccinecode); 
{$ENDIF}

{$IFDEF FHIR_IMMUNIZATIONRECOMMENDATION}
  // Search Parameters for ImmunizationRecommendation
  TSearchParamsImmunizationRecommendation = (
    spImmunizationRecommendation__content, 
    spImmunizationRecommendation__id, 
    spImmunizationRecommendation__lastUpdated, 
    spImmunizationRecommendation__profile, 
    spImmunizationRecommendation__query, 
    spImmunizationRecommendation__security, 
    spImmunizationRecommendation__tag, 
    spImmunizationRecommendation__text, 
    spImmunizationRecommendation_Date, 
    spImmunizationRecommendation_Dosenumber, 
    spImmunizationRecommendation_Dosesequence, 
    spImmunizationRecommendation_Identifier, 
    spImmunizationRecommendation_Information, 
    spImmunizationRecommendation_Patient, 
    spImmunizationRecommendation_Status, 
    spImmunizationRecommendation_Support, 
    spImmunizationRecommendation_Vaccinetype); 
{$ENDIF}

{$IFDEF FHIR_IMPLEMENTATIONGUIDE}
  // Search Parameters for ImplementationGuide
  TSearchParamsImplementationGuide = (
    spImplementationGuide__content, 
    spImplementationGuide__id, 
    spImplementationGuide__lastUpdated, 
    spImplementationGuide__profile, 
    spImplementationGuide__query, 
    spImplementationGuide__security, 
    spImplementationGuide__tag, 
    spImplementationGuide__text, 
    spImplementationGuide_Context, 
    spImplementationGuide_Date, 
    spImplementationGuide_Dependency, 
    spImplementationGuide_Description, 
    spImplementationGuide_Experimental, 
    spImplementationGuide_Name, 
    spImplementationGuide_Publisher, 
    spImplementationGuide_Status, 
    spImplementationGuide_Url, 
    spImplementationGuide_Version); 
{$ENDIF}

{$IFDEF FHIR_LIST}
  // Search Parameters for List
  TSearchParamsList = (
    spList__content, 
    spList__id, 
    spList__lastUpdated, 
    spList__profile, 
    spList__query, 
    spList__security, 
    spList__tag, 
    spList__text, 
    spList_Code, 
    spList_Date, 
    spList_Emptyreason, 
    spList_Encounter, 
    spList_Item, 
    spList_Notes, 
    spList_Patient, 
    spList_Source, 
    spList_Status, 
    spList_Subject, 
    spList_Title); 
{$ENDIF}

{$IFDEF FHIR_LOCATION}
  // Search Parameters for Location
  TSearchParamsLocation = (
    spLocation__content, 
    spLocation__id, 
    spLocation__lastUpdated, 
    spLocation__profile, 
    spLocation__query, 
    spLocation__security, 
    spLocation__tag, 
    spLocation__text, 
    spLocation_Address, 
    spLocation_Addresscity, 
    spLocation_Addresscountry, 
    spLocation_Addresspostalcode, 
    spLocation_Addressstate, 
    spLocation_Addressuse, 
    spLocation_Identifier, 
    spLocation_Name, 
    spLocation_Near, 
    spLocation_Neardistance, 
    spLocation_Organization, 
    spLocation_Partof, 
    spLocation_Status, 
    spLocation_Type); 
{$ENDIF}

{$IFDEF FHIR_MEDIA}
  // Search Parameters for Media
  TSearchParamsMedia = (
    spMedia__content, 
    spMedia__id, 
    spMedia__lastUpdated, 
    spMedia__profile, 
    spMedia__query, 
    spMedia__security, 
    spMedia__tag, 
    spMedia__text, 
    spMedia_Created, 
    spMedia_Identifier, 
    spMedia_Operator, 
    spMedia_Patient, 
    spMedia_Subject, 
    spMedia_Subtype, 
    spMedia_Type, 
    spMedia_View); 
{$ENDIF}

{$IFDEF FHIR_MEDICATION}
  // Search Parameters for Medication
  TSearchParamsMedication = (
    spMedication__content, 
    spMedication__id, 
    spMedication__lastUpdated, 
    spMedication__profile, 
    spMedication__query, 
    spMedication__security, 
    spMedication__tag, 
    spMedication__text, 
    spMedication_Code, 
    spMedication_Container, 
    spMedication_Content, 
    spMedication_Form, 
    spMedication_Ingredient, 
    spMedication_Manufacturer); 
{$ENDIF}

{$IFDEF FHIR_MEDICATIONADMINISTRATION}
  // Search Parameters for MedicationAdministration
  TSearchParamsMedicationAdministration = (
    spMedicationAdministration__content, 
    spMedicationAdministration__id, 
    spMedicationAdministration__lastUpdated, 
    spMedicationAdministration__profile, 
    spMedicationAdministration__query, 
    spMedicationAdministration__security, 
    spMedicationAdministration__tag, 
    spMedicationAdministration__text, 
    spMedicationAdministration_Code, 
    spMedicationAdministration_Device, 
    spMedicationAdministration_Effectivetime, 
    spMedicationAdministration_Encounter, 
    spMedicationAdministration_Identifier, 
    spMedicationAdministration_Medication, 
    spMedicationAdministration_Notgiven, 
    spMedicationAdministration_Patient, 
    spMedicationAdministration_Practitioner, 
    spMedicationAdministration_Prescription, 
    spMedicationAdministration_Status); 
{$ENDIF}

{$IFDEF FHIR_MEDICATIONDISPENSE}
  // Search Parameters for MedicationDispense
  TSearchParamsMedicationDispense = (
    spMedicationDispense__content, 
    spMedicationDispense__id, 
    spMedicationDispense__lastUpdated, 
    spMedicationDispense__profile, 
    spMedicationDispense__query, 
    spMedicationDispense__security, 
    spMedicationDispense__tag, 
    spMedicationDispense__text, 
    spMedicationDispense_Code, 
    spMedicationDispense_Destination, 
    spMedicationDispense_Dispenser, 
    spMedicationDispense_Identifier, 
    spMedicationDispense_Medication, 
    spMedicationDispense_Patient, 
    spMedicationDispense_Prescription, 
    spMedicationDispense_Receiver, 
    spMedicationDispense_Responsibleparty, 
    spMedicationDispense_Status, 
    spMedicationDispense_Type, 
    spMedicationDispense_Whenhandedover, 
    spMedicationDispense_Whenprepared); 
{$ENDIF}

{$IFDEF FHIR_MEDICATIONORDER}
  // Search Parameters for MedicationOrder
  TSearchParamsMedicationOrder = (
    spMedicationOrder__content, 
    spMedicationOrder__id, 
    spMedicationOrder__lastUpdated, 
    spMedicationOrder__profile, 
    spMedicationOrder__query, 
    spMedicationOrder__security, 
    spMedicationOrder__tag, 
    spMedicationOrder__text, 
    spMedicationOrder_Code, 
    spMedicationOrder_Datewritten, 
    spMedicationOrder_Encounter, 
    spMedicationOrder_Identifier, 
    spMedicationOrder_Medication, 
    spMedicationOrder_Patient, 
    spMedicationOrder_Prescriber, 
    spMedicationOrder_Status); 
{$ENDIF}

{$IFDEF FHIR_MEDICATIONSTATEMENT}
  // Search Parameters for MedicationStatement
  TSearchParamsMedicationStatement = (
    spMedicationStatement__content, 
    spMedicationStatement__id, 
    spMedicationStatement__lastUpdated, 
    spMedicationStatement__profile, 
    spMedicationStatement__query, 
    spMedicationStatement__security, 
    spMedicationStatement__tag, 
    spMedicationStatement__text, 
    spMedicationStatement_Code, 
    spMedicationStatement_Effectivedate, 
    spMedicationStatement_Identifier, 
    spMedicationStatement_Medication, 
    spMedicationStatement_Patient, 
    spMedicationStatement_Source, 
    spMedicationStatement_Status); 
{$ENDIF}

{$IFDEF FHIR_MESSAGEHEADER}
  // Search Parameters for MessageHeader
  TSearchParamsMessageHeader = (
    spMessageHeader__content, 
    spMessageHeader__id, 
    spMessageHeader__lastUpdated, 
    spMessageHeader__profile, 
    spMessageHeader__query, 
    spMessageHeader__security, 
    spMessageHeader__tag, 
    spMessageHeader__text, 
    spMessageHeader_Author, 
    spMessageHeader_Code, 
    spMessageHeader_Data, 
    spMessageHeader_Destination, 
    spMessageHeader_Destinationuri, 
    spMessageHeader_Enterer, 
    spMessageHeader_Event, 
    spMessageHeader_Receiver, 
    spMessageHeader_Responseid, 
    spMessageHeader_Responsible, 
    spMessageHeader_Source, 
    spMessageHeader_Sourceuri, 
    spMessageHeader_Target, 
    spMessageHeader_Timestamp); 
{$ENDIF}

{$IFDEF FHIR_NAMINGSYSTEM}
  // Search Parameters for NamingSystem
  TSearchParamsNamingSystem = (
    spNamingSystem__content, 
    spNamingSystem__id, 
    spNamingSystem__lastUpdated, 
    spNamingSystem__profile, 
    spNamingSystem__query, 
    spNamingSystem__security, 
    spNamingSystem__tag, 
    spNamingSystem__text, 
    spNamingSystem_Contact, 
    spNamingSystem_Context, 
    spNamingSystem_Date, 
    spNamingSystem_Idtype, 
    spNamingSystem_Kind, 
    spNamingSystem_Name, 
    spNamingSystem_Period, 
    spNamingSystem_Publisher, 
    spNamingSystem_Replacedby, 
    spNamingSystem_Responsible, 
    spNamingSystem_Status, 
    spNamingSystem_Telecom, 
    spNamingSystem_Type, 
    spNamingSystem_Value); 
{$ENDIF}

{$IFDEF FHIR_NUTRITIONORDER}
  // Search Parameters for NutritionOrder
  TSearchParamsNutritionOrder = (
    spNutritionOrder__content, 
    spNutritionOrder__id, 
    spNutritionOrder__lastUpdated, 
    spNutritionOrder__profile, 
    spNutritionOrder__query, 
    spNutritionOrder__security, 
    spNutritionOrder__tag, 
    spNutritionOrder__text, 
    spNutritionOrder_Additive, 
    spNutritionOrder_Datetime, 
    spNutritionOrder_Encounter, 
    spNutritionOrder_Formula, 
    spNutritionOrder_Identifier, 
    spNutritionOrder_Oraldiet, 
    spNutritionOrder_Patient, 
    spNutritionOrder_Provider, 
    spNutritionOrder_Status, 
    spNutritionOrder_Supplement); 
{$ENDIF}

{$IFDEF FHIR_OBSERVATION}
  // Search Parameters for Observation
  TSearchParamsObservation = (
    spObservation__content, 
    spObservation__id, 
    spObservation__lastUpdated, 
    spObservation__profile, 
    spObservation__query, 
    spObservation__security, 
    spObservation__tag, 
    spObservation__text, 
    spObservation_Category, 
    spObservation_Code, 
    spObservation_Codevaluex, 
    spObservation_Componentcode, 
    spObservation_Componentcodevaluex, 
    spObservation_Componentdataabsentreason, 
    spObservation_Componentvalueconcept, 
    spObservation_Componentvaluequantity, 
    spObservation_Componentvaluestring, 
    spObservation_Dataabsentreason, 
    spObservation_Date, 
    spObservation_Device, 
    spObservation_Dnavariant, 
    spObservation_Encounter, 
    spObservation_Genednavariant, 
    spObservation_Geneidentifier, 
    spObservation_Identifier, 
    spObservation_Patient, 
    spObservation_Performer, 
    spObservation_Related, 
    spObservation_Relatedtarget, 
    spObservation_Relatedtype, 
    spObservation_Specimen, 
    spObservation_Status, 
    spObservation_Subject, 
    spObservation_Valueconcept, 
    spObservation_Valuedate, 
    spObservation_Valuequantity, 
    spObservation_Valuestring); 
{$ENDIF}

{$IFDEF FHIR_OPERATIONDEFINITION}
  // Search Parameters for OperationDefinition
  TSearchParamsOperationDefinition = (
    spOperationDefinition__content, 
    spOperationDefinition__id, 
    spOperationDefinition__lastUpdated, 
    spOperationDefinition__profile, 
    spOperationDefinition__query, 
    spOperationDefinition__security, 
    spOperationDefinition__tag, 
    spOperationDefinition__text, 
    spOperationDefinition_Base, 
    spOperationDefinition_Code, 
    spOperationDefinition_Date, 
    spOperationDefinition_Instance, 
    spOperationDefinition_Kind, 
    spOperationDefinition_Name, 
    spOperationDefinition_Profile, 
    spOperationDefinition_Publisher, 
    spOperationDefinition_Status, 
    spOperationDefinition_System, 
    spOperationDefinition_Type, 
    spOperationDefinition_Url, 
    spOperationDefinition_Version); 
{$ENDIF}

{$IFDEF FHIR_OPERATIONOUTCOME}
  // Search Parameters for OperationOutcome
  TSearchParamsOperationOutcome = (
    spOperationOutcome__content, 
    spOperationOutcome__id, 
    spOperationOutcome__lastUpdated, 
    spOperationOutcome__profile, 
    spOperationOutcome__query, 
    spOperationOutcome__security, 
    spOperationOutcome__tag, 
    spOperationOutcome__text); 
{$ENDIF}

{$IFDEF FHIR_ORDER}
  // Search Parameters for Order
  TSearchParamsOrder = (
    spOrder__content, 
    spOrder__id, 
    spOrder__lastUpdated, 
    spOrder__profile, 
    spOrder__query, 
    spOrder__security, 
    spOrder__tag, 
    spOrder__text, 
    spOrder_Date, 
    spOrder_Detail, 
    spOrder_Identifier, 
    spOrder_Patient, 
    spOrder_Source, 
    spOrder_Subject, 
    spOrder_Target, 
    spOrder_When, 
    spOrder_When_code); 
{$ENDIF}

{$IFDEF FHIR_ORDERRESPONSE}
  // Search Parameters for OrderResponse
  TSearchParamsOrderResponse = (
    spOrderResponse__content, 
    spOrderResponse__id, 
    spOrderResponse__lastUpdated, 
    spOrderResponse__profile, 
    spOrderResponse__query, 
    spOrderResponse__security, 
    spOrderResponse__tag, 
    spOrderResponse__text, 
    spOrderResponse_Code, 
    spOrderResponse_Date, 
    spOrderResponse_Fulfillment, 
    spOrderResponse_Identifier, 
    spOrderResponse_Request, 
    spOrderResponse_Who); 
{$ENDIF}

{$IFDEF FHIR_ORGANIZATION}
  // Search Parameters for Organization
  TSearchParamsOrganization = (
    spOrganization__content, 
    spOrganization__id, 
    spOrganization__lastUpdated, 
    spOrganization__profile, 
    spOrganization__query, 
    spOrganization__security, 
    spOrganization__tag, 
    spOrganization__text, 
    spOrganization_Active, 
    spOrganization_Address, 
    spOrganization_Addresscity, 
    spOrganization_Addresscountry, 
    spOrganization_Addresspostalcode, 
    spOrganization_Addressstate, 
    spOrganization_Addressuse, 
    spOrganization_Identifier, 
    spOrganization_Name, 
    spOrganization_Partof, 
    spOrganization_Phonetic, 
    spOrganization_Type); 
{$ENDIF}

{$IFDEF FHIR_PATIENT}
  // Search Parameters for Patient
  TSearchParamsPatient = (
    spPatient__content, 
    spPatient__id, 
    spPatient__lastUpdated, 
    spPatient__profile, 
    spPatient__query, 
    spPatient__security, 
    spPatient__tag, 
    spPatient__text, 
    spPatient_Active, 
    spPatient_Address, 
    spPatient_Addresscity, 
    spPatient_Addresscountry, 
    spPatient_Addresspostalcode, 
    spPatient_Addressstate, 
    spPatient_Addressuse, 
    spPatient_Animalbreed, 
    spPatient_Animalspecies, 
    spPatient_Birthdate, 
    spPatient_Careprovider, 
    spPatient_Deathdate, 
    spPatient_Deceased, 
    spPatient_Email, 
    spPatient_Ethnicity, 
    spPatient_Family, 
    spPatient_Gender, 
    spPatient_Given, 
    spPatient_Identifier, 
    spPatient_Language, 
    spPatient_Link, 
    spPatient_Name, 
    spPatient_Organization, 
    spPatient_Phone, 
    spPatient_Phonetic, 
    spPatient_Race, 
    spPatient_Telecom); 
{$ENDIF}

{$IFDEF FHIR_PAYMENTNOTICE}
  // Search Parameters for PaymentNotice
  TSearchParamsPaymentNotice = (
    spPaymentNotice__content, 
    spPaymentNotice__id, 
    spPaymentNotice__lastUpdated, 
    spPaymentNotice__profile, 
    spPaymentNotice__query, 
    spPaymentNotice__security, 
    spPaymentNotice__tag, 
    spPaymentNotice__text, 
    spPaymentNotice_Identifier); 
{$ENDIF}

{$IFDEF FHIR_PAYMENTRECONCILIATION}
  // Search Parameters for PaymentReconciliation
  TSearchParamsPaymentReconciliation = (
    spPaymentReconciliation__content, 
    spPaymentReconciliation__id, 
    spPaymentReconciliation__lastUpdated, 
    spPaymentReconciliation__profile, 
    spPaymentReconciliation__query, 
    spPaymentReconciliation__security, 
    spPaymentReconciliation__tag, 
    spPaymentReconciliation__text, 
    spPaymentReconciliation_Identifier); 
{$ENDIF}

{$IFDEF FHIR_PERSON}
  // Search Parameters for Person
  TSearchParamsPerson = (
    spPerson__content, 
    spPerson__id, 
    spPerson__lastUpdated, 
    spPerson__profile, 
    spPerson__query, 
    spPerson__security, 
    spPerson__tag, 
    spPerson__text, 
    spPerson_Address, 
    spPerson_Addresscity, 
    spPerson_Addresscountry, 
    spPerson_Addresspostalcode, 
    spPerson_Addressstate, 
    spPerson_Addressuse, 
    spPerson_Birthdate, 
    spPerson_Email, 
    spPerson_Gender, 
    spPerson_Identifier, 
    spPerson_Link, 
    spPerson_Name, 
    spPerson_Organization, 
    spPerson_Patient, 
    spPerson_Phone, 
    spPerson_Phonetic, 
    spPerson_Practitioner, 
    spPerson_Relatedperson, 
    spPerson_Telecom); 
{$ENDIF}

{$IFDEF FHIR_PRACTITIONER}
  // Search Parameters for Practitioner
  TSearchParamsPractitioner = (
    spPractitioner__content, 
    spPractitioner__id, 
    spPractitioner__lastUpdated, 
    spPractitioner__profile, 
    spPractitioner__query, 
    spPractitioner__security, 
    spPractitioner__tag, 
    spPractitioner__text, 
    spPractitioner_Address, 
    spPractitioner_Addresscity, 
    spPractitioner_Addresscountry, 
    spPractitioner_Addresspostalcode, 
    spPractitioner_Addressstate, 
    spPractitioner_Addressuse, 
    spPractitioner_Communication, 
    spPractitioner_Email, 
    spPractitioner_Family, 
    spPractitioner_Gender, 
    spPractitioner_Given, 
    spPractitioner_Identifier, 
    spPractitioner_Location, 
    spPractitioner_Name, 
    spPractitioner_Organization, 
    spPractitioner_Phone, 
    spPractitioner_Phonetic, 
    spPractitioner_Role, 
    spPractitioner_Specialty, 
    spPractitioner_Telecom); 
{$ENDIF}

{$IFDEF FHIR_PROCEDURE}
  // Search Parameters for Procedure
  TSearchParamsProcedure = (
    spProcedure__content, 
    spProcedure__id, 
    spProcedure__lastUpdated, 
    spProcedure__profile, 
    spProcedure__query, 
    spProcedure__security, 
    spProcedure__tag, 
    spProcedure__text, 
    spProcedure_Code, 
    spProcedure_Date, 
    spProcedure_Encounter, 
    spProcedure_Identifier, 
    spProcedure_Location, 
    spProcedure_Patient, 
    spProcedure_Performer, 
    spProcedure_Subject); 
{$ENDIF}

{$IFDEF FHIR_PROCEDUREREQUEST}
  // Search Parameters for ProcedureRequest
  TSearchParamsProcedureRequest = (
    spProcedureRequest__content, 
    spProcedureRequest__id, 
    spProcedureRequest__lastUpdated, 
    spProcedureRequest__profile, 
    spProcedureRequest__query, 
    spProcedureRequest__security, 
    spProcedureRequest__tag, 
    spProcedureRequest__text, 
    spProcedureRequest_Encounter, 
    spProcedureRequest_Identifier, 
    spProcedureRequest_Orderer, 
    spProcedureRequest_Patient, 
    spProcedureRequest_Performer, 
    spProcedureRequest_Subject); 
{$ENDIF}

{$IFDEF FHIR_PROCESSREQUEST}
  // Search Parameters for ProcessRequest
  TSearchParamsProcessRequest = (
    spProcessRequest__content, 
    spProcessRequest__id, 
    spProcessRequest__lastUpdated, 
    spProcessRequest__profile, 
    spProcessRequest__query, 
    spProcessRequest__security, 
    spProcessRequest__tag, 
    spProcessRequest__text, 
    spProcessRequest_Action, 
    spProcessRequest_Identifier, 
    spProcessRequest_Organization, 
    spProcessRequest_Provider); 
{$ENDIF}

{$IFDEF FHIR_PROCESSRESPONSE}
  // Search Parameters for ProcessResponse
  TSearchParamsProcessResponse = (
    spProcessResponse__content, 
    spProcessResponse__id, 
    spProcessResponse__lastUpdated, 
    spProcessResponse__profile, 
    spProcessResponse__query, 
    spProcessResponse__security, 
    spProcessResponse__tag, 
    spProcessResponse__text, 
    spProcessResponse_Identifier, 
    spProcessResponse_Organization, 
    spProcessResponse_Request, 
    spProcessResponse_Requestorganization, 
    spProcessResponse_Requestprovider); 
{$ENDIF}

{$IFDEF FHIR_PROVENANCE}
  // Search Parameters for Provenance
  TSearchParamsProvenance = (
    spProvenance__content, 
    spProvenance__id, 
    spProvenance__lastUpdated, 
    spProvenance__profile, 
    spProvenance__query, 
    spProvenance__security, 
    spProvenance__tag, 
    spProvenance__text, 
    spProvenance_Agent, 
    spProvenance_End, 
    spProvenance_Entity, 
    spProvenance_Entitytype, 
    spProvenance_Location, 
    spProvenance_Patient, 
    spProvenance_Sigtype, 
    spProvenance_Start, 
    spProvenance_Target, 
    spProvenance_Userid); 
{$ENDIF}

{$IFDEF FHIR_QUESTIONNAIRE}
  // Search Parameters for Questionnaire
  TSearchParamsQuestionnaire = (
    spQuestionnaire__content, 
    spQuestionnaire__id, 
    spQuestionnaire__lastUpdated, 
    spQuestionnaire__profile, 
    spQuestionnaire__query, 
    spQuestionnaire__security, 
    spQuestionnaire__tag, 
    spQuestionnaire__text, 
    spQuestionnaire_Code, 
    spQuestionnaire_Date, 
    spQuestionnaire_Identifier, 
    spQuestionnaire_Publisher, 
    spQuestionnaire_Status, 
    spQuestionnaire_Title, 
    spQuestionnaire_Version); 
{$ENDIF}

{$IFDEF FHIR_QUESTIONNAIRERESPONSE}
  // Search Parameters for QuestionnaireResponse
  TSearchParamsQuestionnaireResponse = (
    spQuestionnaireResponse__content, 
    spQuestionnaireResponse__id, 
    spQuestionnaireResponse__lastUpdated, 
    spQuestionnaireResponse__profile, 
    spQuestionnaireResponse__query, 
    spQuestionnaireResponse__security, 
    spQuestionnaireResponse__tag, 
    spQuestionnaireResponse__text, 
    spQuestionnaireResponse_Author, 
    spQuestionnaireResponse_Authored, 
    spQuestionnaireResponse_Encounter, 
    spQuestionnaireResponse_Patient, 
    spQuestionnaireResponse_Questionnaire, 
    spQuestionnaireResponse_Source, 
    spQuestionnaireResponse_Status, 
    spQuestionnaireResponse_Subject); 
{$ENDIF}

{$IFDEF FHIR_REFERRALREQUEST}
  // Search Parameters for ReferralRequest
  TSearchParamsReferralRequest = (
    spReferralRequest__content, 
    spReferralRequest__id, 
    spReferralRequest__lastUpdated, 
    spReferralRequest__profile, 
    spReferralRequest__query, 
    spReferralRequest__security, 
    spReferralRequest__tag, 
    spReferralRequest__text, 
    spReferralRequest_Date, 
    spReferralRequest_Patient, 
    spReferralRequest_Priority, 
    spReferralRequest_Recipient, 
    spReferralRequest_Requester, 
    spReferralRequest_Specialty, 
    spReferralRequest_Status, 
    spReferralRequest_Type); 
{$ENDIF}

{$IFDEF FHIR_RELATEDPERSON}
  // Search Parameters for RelatedPerson
  TSearchParamsRelatedPerson = (
    spRelatedPerson__content, 
    spRelatedPerson__id, 
    spRelatedPerson__lastUpdated, 
    spRelatedPerson__profile, 
    spRelatedPerson__query, 
    spRelatedPerson__security, 
    spRelatedPerson__tag, 
    spRelatedPerson__text, 
    spRelatedPerson_Address, 
    spRelatedPerson_Addresscity, 
    spRelatedPerson_Addresscountry, 
    spRelatedPerson_Addresspostalcode, 
    spRelatedPerson_Addressstate, 
    spRelatedPerson_Addressuse, 
    spRelatedPerson_Birthdate, 
    spRelatedPerson_Email, 
    spRelatedPerson_Gender, 
    spRelatedPerson_Identifier, 
    spRelatedPerson_Name, 
    spRelatedPerson_Patient, 
    spRelatedPerson_Phone, 
    spRelatedPerson_Phonetic, 
    spRelatedPerson_Telecom); 
{$ENDIF}

{$IFDEF FHIR_RISKASSESSMENT}
  // Search Parameters for RiskAssessment
  TSearchParamsRiskAssessment = (
    spRiskAssessment__content, 
    spRiskAssessment__id, 
    spRiskAssessment__lastUpdated, 
    spRiskAssessment__profile, 
    spRiskAssessment__query, 
    spRiskAssessment__security, 
    spRiskAssessment__tag, 
    spRiskAssessment__text, 
    spRiskAssessment_Condition, 
    spRiskAssessment_Date, 
    spRiskAssessment_Encounter, 
    spRiskAssessment_Identifier, 
    spRiskAssessment_Method, 
    spRiskAssessment_Patient, 
    spRiskAssessment_Performer, 
    spRiskAssessment_Subject); 
{$ENDIF}

{$IFDEF FHIR_SCHEDULE}
  // Search Parameters for Schedule
  TSearchParamsSchedule = (
    spSchedule__content, 
    spSchedule__id, 
    spSchedule__lastUpdated, 
    spSchedule__profile, 
    spSchedule__query, 
    spSchedule__security, 
    spSchedule__tag, 
    spSchedule__text, 
    spSchedule_Actor, 
    spSchedule_Date, 
    spSchedule_Identifier, 
    spSchedule_Type); 
{$ENDIF}

{$IFDEF FHIR_SEARCHPARAMETER}
  // Search Parameters for SearchParameter
  TSearchParamsSearchParameter = (
    spSearchParameter__content, 
    spSearchParameter__id, 
    spSearchParameter__lastUpdated, 
    spSearchParameter__profile, 
    spSearchParameter__query, 
    spSearchParameter__security, 
    spSearchParameter__tag, 
    spSearchParameter__text, 
    spSearchParameter_Base, 
    spSearchParameter_Code, 
    spSearchParameter_Description, 
    spSearchParameter_Name, 
    spSearchParameter_Target, 
    spSearchParameter_Type, 
    spSearchParameter_Url); 
{$ENDIF}

{$IFDEF FHIR_SLOT}
  // Search Parameters for Slot
  TSearchParamsSlot = (
    spSlot__content, 
    spSlot__id, 
    spSlot__lastUpdated, 
    spSlot__profile, 
    spSlot__query, 
    spSlot__security, 
    spSlot__tag, 
    spSlot__text, 
    spSlot_Fbtype, 
    spSlot_Identifier, 
    spSlot_Schedule, 
    spSlot_Slottype, 
    spSlot_Start); 
{$ENDIF}

{$IFDEF FHIR_SPECIMEN}
  // Search Parameters for Specimen
  TSearchParamsSpecimen = (
    spSpecimen__content, 
    spSpecimen__id, 
    spSpecimen__lastUpdated, 
    spSpecimen__profile, 
    spSpecimen__query, 
    spSpecimen__security, 
    spSpecimen__tag, 
    spSpecimen__text, 
    spSpecimen_Accession, 
    spSpecimen_Bodysite, 
    spSpecimen_Collected, 
    spSpecimen_Collector, 
    spSpecimen_Container, 
    spSpecimen_Containerid, 
    spSpecimen_Identifier, 
    spSpecimen_Parent, 
    spSpecimen_Patient, 
    spSpecimen_Subject, 
    spSpecimen_Type); 
{$ENDIF}

{$IFDEF FHIR_STRUCTUREDEFINITION}
  // Search Parameters for StructureDefinition
  TSearchParamsStructureDefinition = (
    spStructureDefinition__content, 
    spStructureDefinition__id, 
    spStructureDefinition__lastUpdated, 
    spStructureDefinition__profile, 
    spStructureDefinition__query, 
    spStructureDefinition__security, 
    spStructureDefinition__tag, 
    spStructureDefinition__text, 
    spStructureDefinition_Abstract, 
    spStructureDefinition_Base, 
    spStructureDefinition_Basepath, 
    spStructureDefinition_Code, 
    spStructureDefinition_Context, 
    spStructureDefinition_Contexttype, 
    spStructureDefinition_Date, 
    spStructureDefinition_Description, 
    spStructureDefinition_Display, 
    spStructureDefinition_Experimental, 
    spStructureDefinition_Extcontext, 
    spStructureDefinition_Identifier, 
    spStructureDefinition_Kind, 
    spStructureDefinition_Name, 
    spStructureDefinition_Path, 
    spStructureDefinition_Publisher, 
    spStructureDefinition_Status, 
    spStructureDefinition_Type, 
    spStructureDefinition_Url, 
    spStructureDefinition_Valueset, 
    spStructureDefinition_Version); 
{$ENDIF}

{$IFDEF FHIR_SUBSCRIPTION}
  // Search Parameters for Subscription
  TSearchParamsSubscription = (
    spSubscription__content, 
    spSubscription__id, 
    spSubscription__lastUpdated, 
    spSubscription__profile, 
    spSubscription__query, 
    spSubscription__security, 
    spSubscription__tag, 
    spSubscription__text, 
    spSubscription_Contact, 
    spSubscription_Criteria, 
    spSubscription_Payload, 
    spSubscription_Status, 
    spSubscription_Tag, 
    spSubscription_Type, 
    spSubscription_Url); 
{$ENDIF}

{$IFDEF FHIR_SUBSTANCE}
  // Search Parameters for Substance
  TSearchParamsSubstance = (
    spSubstance__content, 
    spSubstance__id, 
    spSubstance__lastUpdated, 
    spSubstance__profile, 
    spSubstance__query, 
    spSubstance__security, 
    spSubstance__tag, 
    spSubstance__text, 
    spSubstance_Category, 
    spSubstance_Code, 
    spSubstance_Containeridentifier, 
    spSubstance_Expiry, 
    spSubstance_Identifier, 
    spSubstance_Quantity, 
    spSubstance_Substance); 
{$ENDIF}

{$IFDEF FHIR_SUPPLYDELIVERY}
  // Search Parameters for SupplyDelivery
  TSearchParamsSupplyDelivery = (
    spSupplyDelivery__content, 
    spSupplyDelivery__id, 
    spSupplyDelivery__lastUpdated, 
    spSupplyDelivery__profile, 
    spSupplyDelivery__query, 
    spSupplyDelivery__security, 
    spSupplyDelivery__tag, 
    spSupplyDelivery__text, 
    spSupplyDelivery_Identifier, 
    spSupplyDelivery_Patient, 
    spSupplyDelivery_Receiver, 
    spSupplyDelivery_Status, 
    spSupplyDelivery_Supplier); 
{$ENDIF}

{$IFDEF FHIR_SUPPLYREQUEST}
  // Search Parameters for SupplyRequest
  TSearchParamsSupplyRequest = (
    spSupplyRequest__content, 
    spSupplyRequest__id, 
    spSupplyRequest__lastUpdated, 
    spSupplyRequest__profile, 
    spSupplyRequest__query, 
    spSupplyRequest__security, 
    spSupplyRequest__tag, 
    spSupplyRequest__text, 
    spSupplyRequest_Date, 
    spSupplyRequest_Identifier, 
    spSupplyRequest_Kind, 
    spSupplyRequest_Patient, 
    spSupplyRequest_Source, 
    spSupplyRequest_Status, 
    spSupplyRequest_Supplier); 
{$ENDIF}

{$IFDEF FHIR_TESTSCRIPT}
  // Search Parameters for TestScript
  TSearchParamsTestScript = (
    spTestScript__content, 
    spTestScript__id, 
    spTestScript__lastUpdated, 
    spTestScript__profile, 
    spTestScript__query, 
    spTestScript__security, 
    spTestScript__tag, 
    spTestScript__text, 
    spTestScript_Description, 
    spTestScript_Identifier, 
    spTestScript_Name, 
    spTestScript_Testscriptcapability, 
    spTestScript_Testscriptsetupcapability, 
    spTestScript_Testscripttestcapability, 
    spTestScript_Url); 
{$ENDIF}

{$IFDEF FHIR_VALUESET}
  // Search Parameters for ValueSet
  TSearchParamsValueSet = (
    spValueSet__content, 
    spValueSet__id, 
    spValueSet__lastUpdated, 
    spValueSet__profile, 
    spValueSet__query, 
    spValueSet__security, 
    spValueSet__tag, 
    spValueSet__text, 
    spValueSet_Code, 
    spValueSet_Context, 
    spValueSet_Date, 
    spValueSet_Description, 
    spValueSet_Expansion, 
    spValueSet_Identifier, 
    spValueSet_Name, 
    spValueSet_Publisher, 
    spValueSet_Reference, 
    spValueSet_Status, 
    spValueSet_System, 
    spValueSet_Url, 
    spValueSet_Version); 
{$ENDIF}

{$IFDEF FHIR_VISIONPRESCRIPTION}
  // Search Parameters for VisionPrescription
  TSearchParamsVisionPrescription = (
    spVisionPrescription__content, 
    spVisionPrescription__id, 
    spVisionPrescription__lastUpdated, 
    spVisionPrescription__profile, 
    spVisionPrescription__query, 
    spVisionPrescription__security, 
    spVisionPrescription__tag, 
    spVisionPrescription__text, 
    spVisionPrescription_Datewritten, 
    spVisionPrescription_Encounter, 
    spVisionPrescription_Identifier, 
    spVisionPrescription_Patient, 
    spVisionPrescription_Prescriber); 
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
  CODES_TSearchParamsObservation : Array[TSearchParamsObservation] of String = ('_content', '_id', '_lastUpdated', '_profile', '_query', '_security', '_tag', '_text', 'category', 'code', 'code-value-[x]', 'component-code', 'component-code-value-[x]', 'component-data-absent-reason', 'component-value-concept', 'component-value-quantity', 'component-value-string', 'data-absent-reason', 'date', 'device', 'dna-variant', 'encounter', 'gene-dnavariant', 'gene-identifier', 'identifier', 'patient', 'performer', 'related', 'related-target', 'related-type', 'specimen', 'status', 'subject', 'value-concept', 'value-date', 'value-quantity', 'value-string');
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

