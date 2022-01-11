unit fhir4b_constants;

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
{$I fhir4b.inc}

interface

// Generated on Mon, Dec 27, 2021 21:46+1100 for FHIR v4.3.0



uses
  SysUtils, Classes,
  fsl_utilities, fsl_stream,
  fhir_objects, fhir4b_types, fhir4b_resources, fhir4b_resources_base;

const
  currentFHIRVersionRelease = fhirVersionRelease4b;
  FHIR_GENERATED_VERSION = '4.3.0';
  FHIR_GENERATED_VERSION_BASE = '4.3';
  FHIR_GENERATED_PUBLICATION = '4';
  FHIR_GENERATED_DATE = 'Mon, Dec 27, 2021 21:46+1100';

const
  CODES_TFhirResourceType : Array[TFhirResourceType] of String = (
    '',
    {$IFDEF FHIR_ACCOUNT} 'Account', {$ENDIF}
    {$IFDEF FHIR_ACTIVITYDEFINITION} 'ActivityDefinition', {$ENDIF}
    {$IFDEF FHIR_ADMINISTRABLEPRODUCTDEFINITION} 'AdministrableProductDefinition', {$ENDIF}
    {$IFDEF FHIR_ADVERSEEVENT} 'AdverseEvent', {$ENDIF}
    {$IFDEF FHIR_ALLERGYINTOLERANCE} 'AllergyIntolerance', {$ENDIF}
    {$IFDEF FHIR_APPOINTMENT} 'Appointment', {$ENDIF}
    {$IFDEF FHIR_APPOINTMENTRESPONSE} 'AppointmentResponse', {$ENDIF}
    {$IFDEF FHIR_AUDITEVENT} 'AuditEvent', {$ENDIF}
    {$IFDEF FHIR_BASIC} 'Basic', {$ENDIF}
    {$IFDEF FHIR_BINARY} 'Binary', {$ENDIF}
    {$IFDEF FHIR_BIOLOGICALLYDERIVEDPRODUCT} 'BiologicallyDerivedProduct', {$ENDIF}
    {$IFDEF FHIR_BODYSTRUCTURE} 'BodyStructure', {$ENDIF}
    {$IFDEF FHIR_BUNDLE} 'Bundle', {$ENDIF}
    {$IFDEF FHIR_CAPABILITYSTATEMENT} 'CapabilityStatement', {$ENDIF}
    {$IFDEF FHIR_CAREPLAN} 'CarePlan', {$ENDIF}
    {$IFDEF FHIR_CARETEAM} 'CareTeam', {$ENDIF}
    {$IFDEF FHIR_CATALOGENTRY} 'CatalogEntry', {$ENDIF}
    {$IFDEF FHIR_CHARGEITEM} 'ChargeItem', {$ENDIF}
    {$IFDEF FHIR_CHARGEITEMDEFINITION} 'ChargeItemDefinition', {$ENDIF}
    {$IFDEF FHIR_CITATION} 'Citation', {$ENDIF}
    {$IFDEF FHIR_CLAIM} 'Claim', {$ENDIF}
    {$IFDEF FHIR_CLAIMRESPONSE} 'ClaimResponse', {$ENDIF}
    {$IFDEF FHIR_CLINICALIMPRESSION} 'ClinicalImpression', {$ENDIF}
    {$IFDEF FHIR_CLINICALUSEDEFINITION} 'ClinicalUseDefinition', {$ENDIF}
    {$IFDEF FHIR_CODESYSTEM} 'CodeSystem', {$ENDIF}
    {$IFDEF FHIR_COMMUNICATION} 'Communication', {$ENDIF}
    {$IFDEF FHIR_COMMUNICATIONREQUEST} 'CommunicationRequest', {$ENDIF}
    {$IFDEF FHIR_COMPARTMENTDEFINITION} 'CompartmentDefinition', {$ENDIF}
    {$IFDEF FHIR_COMPOSITION} 'Composition', {$ENDIF}
    {$IFDEF FHIR_CONCEPTMAP} 'ConceptMap', {$ENDIF}
    {$IFDEF FHIR_CONDITION} 'Condition', {$ENDIF}
    {$IFDEF FHIR_CONSENT} 'Consent', {$ENDIF}
    {$IFDEF FHIR_CONTRACT} 'Contract', {$ENDIF}
    {$IFDEF FHIR_COVERAGE} 'Coverage', {$ENDIF}
    {$IFDEF FHIR_COVERAGEELIGIBILITYREQUEST} 'CoverageEligibilityRequest', {$ENDIF}
    {$IFDEF FHIR_COVERAGEELIGIBILITYRESPONSE} 'CoverageEligibilityResponse', {$ENDIF}
    {$IFDEF FHIR_DETECTEDISSUE} 'DetectedIssue', {$ENDIF}
    {$IFDEF FHIR_DEVICE} 'Device', {$ENDIF}
    {$IFDEF FHIR_DEVICEDEFINITION} 'DeviceDefinition', {$ENDIF}
    {$IFDEF FHIR_DEVICEMETRIC} 'DeviceMetric', {$ENDIF}
    {$IFDEF FHIR_DEVICEREQUEST} 'DeviceRequest', {$ENDIF}
    {$IFDEF FHIR_DEVICEUSESTATEMENT} 'DeviceUseStatement', {$ENDIF}
    {$IFDEF FHIR_DIAGNOSTICREPORT} 'DiagnosticReport', {$ENDIF}
    {$IFDEF FHIR_DOCUMENTMANIFEST} 'DocumentManifest', {$ENDIF}
    {$IFDEF FHIR_DOCUMENTREFERENCE} 'DocumentReference', {$ENDIF}
    {$IFDEF FHIR_ENCOUNTER} 'Encounter', {$ENDIF}
    {$IFDEF FHIR_ENDPOINT} 'Endpoint', {$ENDIF}
    {$IFDEF FHIR_ENROLLMENTREQUEST} 'EnrollmentRequest', {$ENDIF}
    {$IFDEF FHIR_ENROLLMENTRESPONSE} 'EnrollmentResponse', {$ENDIF}
    {$IFDEF FHIR_EPISODEOFCARE} 'EpisodeOfCare', {$ENDIF}
    {$IFDEF FHIR_EVENTDEFINITION} 'EventDefinition', {$ENDIF}
    {$IFDEF FHIR_EVIDENCE} 'Evidence', {$ENDIF}
    {$IFDEF FHIR_EVIDENCEREPORT} 'EvidenceReport', {$ENDIF}
    {$IFDEF FHIR_EVIDENCEVARIABLE} 'EvidenceVariable', {$ENDIF}
    {$IFDEF FHIR_EXAMPLESCENARIO} 'ExampleScenario', {$ENDIF}
    {$IFDEF FHIR_EXPLANATIONOFBENEFIT} 'ExplanationOfBenefit', {$ENDIF}
    {$IFDEF FHIR_FAMILYMEMBERHISTORY} 'FamilyMemberHistory', {$ENDIF}
    {$IFDEF FHIR_FLAG} 'Flag', {$ENDIF}
    {$IFDEF FHIR_GOAL} 'Goal', {$ENDIF}
    {$IFDEF FHIR_GRAPHDEFINITION} 'GraphDefinition', {$ENDIF}
    {$IFDEF FHIR_GROUP} 'Group', {$ENDIF}
    {$IFDEF FHIR_GUIDANCERESPONSE} 'GuidanceResponse', {$ENDIF}
    {$IFDEF FHIR_HEALTHCARESERVICE} 'HealthcareService', {$ENDIF}
    {$IFDEF FHIR_IMAGINGSTUDY} 'ImagingStudy', {$ENDIF}
    {$IFDEF FHIR_IMMUNIZATION} 'Immunization', {$ENDIF}
    {$IFDEF FHIR_IMMUNIZATIONEVALUATION} 'ImmunizationEvaluation', {$ENDIF}
    {$IFDEF FHIR_IMMUNIZATIONRECOMMENDATION} 'ImmunizationRecommendation', {$ENDIF}
    {$IFDEF FHIR_IMPLEMENTATIONGUIDE} 'ImplementationGuide', {$ENDIF}
    {$IFDEF FHIR_INGREDIENT} 'Ingredient', {$ENDIF}
    {$IFDEF FHIR_INSURANCEPLAN} 'InsurancePlan', {$ENDIF}
    {$IFDEF FHIR_INVOICE} 'Invoice', {$ENDIF}
    {$IFDEF FHIR_LIBRARY} 'Library', {$ENDIF}
    {$IFDEF FHIR_LINKAGE} 'Linkage', {$ENDIF}
    {$IFDEF FHIR_LIST} 'List', {$ENDIF}
    {$IFDEF FHIR_LOCATION} 'Location', {$ENDIF}
    {$IFDEF FHIR_MANUFACTUREDITEMDEFINITION} 'ManufacturedItemDefinition', {$ENDIF}
    {$IFDEF FHIR_MEASURE} 'Measure', {$ENDIF}
    {$IFDEF FHIR_MEASUREREPORT} 'MeasureReport', {$ENDIF}
    {$IFDEF FHIR_MEDIA} 'Media', {$ENDIF}
    {$IFDEF FHIR_MEDICATION} 'Medication', {$ENDIF}
    {$IFDEF FHIR_MEDICATIONADMINISTRATION} 'MedicationAdministration', {$ENDIF}
    {$IFDEF FHIR_MEDICATIONDISPENSE} 'MedicationDispense', {$ENDIF}
    {$IFDEF FHIR_MEDICATIONKNOWLEDGE} 'MedicationKnowledge', {$ENDIF}
    {$IFDEF FHIR_MEDICATIONREQUEST} 'MedicationRequest', {$ENDIF}
    {$IFDEF FHIR_MEDICATIONSTATEMENT} 'MedicationStatement', {$ENDIF}
    {$IFDEF FHIR_MEDICINALPRODUCTDEFINITION} 'MedicinalProductDefinition', {$ENDIF}
    {$IFDEF FHIR_MESSAGEDEFINITION} 'MessageDefinition', {$ENDIF}
    {$IFDEF FHIR_MESSAGEHEADER} 'MessageHeader', {$ENDIF}
    {$IFDEF FHIR_MOLECULARSEQUENCE} 'MolecularSequence', {$ENDIF}
    {$IFDEF FHIR_NAMINGSYSTEM} 'NamingSystem', {$ENDIF}
    {$IFDEF FHIR_NUTRITIONORDER} 'NutritionOrder', {$ENDIF}
    {$IFDEF FHIR_NUTRITIONPRODUCT} 'NutritionProduct', {$ENDIF}
    {$IFDEF FHIR_OBSERVATION} 'Observation', {$ENDIF}
    {$IFDEF FHIR_OBSERVATIONDEFINITION} 'ObservationDefinition', {$ENDIF}
    {$IFDEF FHIR_OPERATIONDEFINITION} 'OperationDefinition', {$ENDIF}
    {$IFDEF FHIR_OPERATIONOUTCOME} 'OperationOutcome', {$ENDIF}
    {$IFDEF FHIR_ORGANIZATION} 'Organization', {$ENDIF}
    {$IFDEF FHIR_ORGANIZATIONAFFILIATION} 'OrganizationAffiliation', {$ENDIF}
    {$IFDEF FHIR_PACKAGEDPRODUCTDEFINITION} 'PackagedProductDefinition', {$ENDIF}
    {$IFDEF FHIR_PARAMETERS} 'Parameters', {$ENDIF}
    {$IFDEF FHIR_PATIENT} 'Patient', {$ENDIF}
    {$IFDEF FHIR_PAYMENTNOTICE} 'PaymentNotice', {$ENDIF}
    {$IFDEF FHIR_PAYMENTRECONCILIATION} 'PaymentReconciliation', {$ENDIF}
    {$IFDEF FHIR_PERSON} 'Person', {$ENDIF}
    {$IFDEF FHIR_PLANDEFINITION} 'PlanDefinition', {$ENDIF}
    {$IFDEF FHIR_PRACTITIONER} 'Practitioner', {$ENDIF}
    {$IFDEF FHIR_PRACTITIONERROLE} 'PractitionerRole', {$ENDIF}
    {$IFDEF FHIR_PROCEDURE} 'Procedure', {$ENDIF}
    {$IFDEF FHIR_PROVENANCE} 'Provenance', {$ENDIF}
    {$IFDEF FHIR_QUESTIONNAIRE} 'Questionnaire', {$ENDIF}
    {$IFDEF FHIR_QUESTIONNAIRERESPONSE} 'QuestionnaireResponse', {$ENDIF}
    {$IFDEF FHIR_REGULATEDAUTHORIZATION} 'RegulatedAuthorization', {$ENDIF}
    {$IFDEF FHIR_RELATEDPERSON} 'RelatedPerson', {$ENDIF}
    {$IFDEF FHIR_REQUESTGROUP} 'RequestGroup', {$ENDIF}
    {$IFDEF FHIR_RESEARCHDEFINITION} 'ResearchDefinition', {$ENDIF}
    {$IFDEF FHIR_RESEARCHELEMENTDEFINITION} 'ResearchElementDefinition', {$ENDIF}
    {$IFDEF FHIR_RESEARCHSTUDY} 'ResearchStudy', {$ENDIF}
    {$IFDEF FHIR_RESEARCHSUBJECT} 'ResearchSubject', {$ENDIF}
    {$IFDEF FHIR_RISKASSESSMENT} 'RiskAssessment', {$ENDIF}
    {$IFDEF FHIR_SCHEDULE} 'Schedule', {$ENDIF}
    {$IFDEF FHIR_SEARCHPARAMETER} 'SearchParameter', {$ENDIF}
    {$IFDEF FHIR_SERVICEREQUEST} 'ServiceRequest', {$ENDIF}
    {$IFDEF FHIR_SLOT} 'Slot', {$ENDIF}
    {$IFDEF FHIR_SPECIMEN} 'Specimen', {$ENDIF}
    {$IFDEF FHIR_SPECIMENDEFINITION} 'SpecimenDefinition', {$ENDIF}
    {$IFDEF FHIR_STRUCTUREDEFINITION} 'StructureDefinition', {$ENDIF}
    {$IFDEF FHIR_STRUCTUREMAP} 'StructureMap', {$ENDIF}
    {$IFDEF FHIR_SUBSCRIPTION} 'Subscription', {$ENDIF}
    {$IFDEF FHIR_SUBSCRIPTIONSTATUS} 'SubscriptionStatus', {$ENDIF}
    {$IFDEF FHIR_SUBSCRIPTIONTOPIC} 'SubscriptionTopic', {$ENDIF}
    {$IFDEF FHIR_SUBSTANCE} 'Substance', {$ENDIF}
    {$IFDEF FHIR_SUBSTANCEDEFINITION} 'SubstanceDefinition', {$ENDIF}
    {$IFDEF FHIR_SUPPLYDELIVERY} 'SupplyDelivery', {$ENDIF}
    {$IFDEF FHIR_SUPPLYREQUEST} 'SupplyRequest', {$ENDIF}
    {$IFDEF FHIR_TASK} 'Task', {$ENDIF}
    {$IFDEF FHIR_TERMINOLOGYCAPABILITIES} 'TerminologyCapabilities', {$ENDIF}
    {$IFDEF FHIR_TESTREPORT} 'TestReport', {$ENDIF}
    {$IFDEF FHIR_TESTSCRIPT} 'TestScript', {$ENDIF}
    {$IFDEF FHIR_VALUESET} 'ValueSet', {$ENDIF}
    {$IFDEF FHIR_VERIFICATIONRESULT} 'VerificationResult', {$ENDIF}
    {$IFDEF FHIR_VISIONPRESCRIPTION} 'VisionPrescription', {$ENDIF}
    'Custom');

const
  LOWERCASE_CODES_TFhirResourceType : Array[TFhirResourceType] of String = (
    '',
    {$IFDEF FHIR_ACCOUNT} 'account', {$ENDIF}
    {$IFDEF FHIR_ACTIVITYDEFINITION} 'activitydefinition', {$ENDIF}
    {$IFDEF FHIR_ADMINISTRABLEPRODUCTDEFINITION} 'administrableproductdefinition', {$ENDIF}
    {$IFDEF FHIR_ADVERSEEVENT} 'adverseevent', {$ENDIF}
    {$IFDEF FHIR_ALLERGYINTOLERANCE} 'allergyintolerance', {$ENDIF}
    {$IFDEF FHIR_APPOINTMENT} 'appointment', {$ENDIF}
    {$IFDEF FHIR_APPOINTMENTRESPONSE} 'appointmentresponse', {$ENDIF}
    {$IFDEF FHIR_AUDITEVENT} 'auditevent', {$ENDIF}
    {$IFDEF FHIR_BASIC} 'basic', {$ENDIF}
    {$IFDEF FHIR_BINARY} 'binary', {$ENDIF}
    {$IFDEF FHIR_BIOLOGICALLYDERIVEDPRODUCT} 'biologicallyderivedproduct', {$ENDIF}
    {$IFDEF FHIR_BODYSTRUCTURE} 'bodystructure', {$ENDIF}
    {$IFDEF FHIR_BUNDLE} 'bundle', {$ENDIF}
    {$IFDEF FHIR_CAPABILITYSTATEMENT} 'capabilitystatement', {$ENDIF}
    {$IFDEF FHIR_CAREPLAN} 'careplan', {$ENDIF}
    {$IFDEF FHIR_CARETEAM} 'careteam', {$ENDIF}
    {$IFDEF FHIR_CATALOGENTRY} 'catalogentry', {$ENDIF}
    {$IFDEF FHIR_CHARGEITEM} 'chargeitem', {$ENDIF}
    {$IFDEF FHIR_CHARGEITEMDEFINITION} 'chargeitemdefinition', {$ENDIF}
    {$IFDEF FHIR_CITATION} 'citation', {$ENDIF}
    {$IFDEF FHIR_CLAIM} 'claim', {$ENDIF}
    {$IFDEF FHIR_CLAIMRESPONSE} 'claimresponse', {$ENDIF}
    {$IFDEF FHIR_CLINICALIMPRESSION} 'clinicalimpression', {$ENDIF}
    {$IFDEF FHIR_CLINICALUSEDEFINITION} 'clinicalusedefinition', {$ENDIF}
    {$IFDEF FHIR_CODESYSTEM} 'codesystem', {$ENDIF}
    {$IFDEF FHIR_COMMUNICATION} 'communication', {$ENDIF}
    {$IFDEF FHIR_COMMUNICATIONREQUEST} 'communicationrequest', {$ENDIF}
    {$IFDEF FHIR_COMPARTMENTDEFINITION} 'compartmentdefinition', {$ENDIF}
    {$IFDEF FHIR_COMPOSITION} 'composition', {$ENDIF}
    {$IFDEF FHIR_CONCEPTMAP} 'conceptmap', {$ENDIF}
    {$IFDEF FHIR_CONDITION} 'condition', {$ENDIF}
    {$IFDEF FHIR_CONSENT} 'consent', {$ENDIF}
    {$IFDEF FHIR_CONTRACT} 'contract', {$ENDIF}
    {$IFDEF FHIR_COVERAGE} 'coverage', {$ENDIF}
    {$IFDEF FHIR_COVERAGEELIGIBILITYREQUEST} 'coverageeligibilityrequest', {$ENDIF}
    {$IFDEF FHIR_COVERAGEELIGIBILITYRESPONSE} 'coverageeligibilityresponse', {$ENDIF}
    {$IFDEF FHIR_DETECTEDISSUE} 'detectedissue', {$ENDIF}
    {$IFDEF FHIR_DEVICE} 'device', {$ENDIF}
    {$IFDEF FHIR_DEVICEDEFINITION} 'devicedefinition', {$ENDIF}
    {$IFDEF FHIR_DEVICEMETRIC} 'devicemetric', {$ENDIF}
    {$IFDEF FHIR_DEVICEREQUEST} 'devicerequest', {$ENDIF}
    {$IFDEF FHIR_DEVICEUSESTATEMENT} 'deviceusestatement', {$ENDIF}
    {$IFDEF FHIR_DIAGNOSTICREPORT} 'diagnosticreport', {$ENDIF}
    {$IFDEF FHIR_DOCUMENTMANIFEST} 'documentmanifest', {$ENDIF}
    {$IFDEF FHIR_DOCUMENTREFERENCE} 'documentreference', {$ENDIF}
    {$IFDEF FHIR_ENCOUNTER} 'encounter', {$ENDIF}
    {$IFDEF FHIR_ENDPOINT} 'endpoint', {$ENDIF}
    {$IFDEF FHIR_ENROLLMENTREQUEST} 'enrollmentrequest', {$ENDIF}
    {$IFDEF FHIR_ENROLLMENTRESPONSE} 'enrollmentresponse', {$ENDIF}
    {$IFDEF FHIR_EPISODEOFCARE} 'episodeofcare', {$ENDIF}
    {$IFDEF FHIR_EVENTDEFINITION} 'eventdefinition', {$ENDIF}
    {$IFDEF FHIR_EVIDENCE} 'evidence', {$ENDIF}
    {$IFDEF FHIR_EVIDENCEREPORT} 'evidencereport', {$ENDIF}
    {$IFDEF FHIR_EVIDENCEVARIABLE} 'evidencevariable', {$ENDIF}
    {$IFDEF FHIR_EXAMPLESCENARIO} 'examplescenario', {$ENDIF}
    {$IFDEF FHIR_EXPLANATIONOFBENEFIT} 'explanationofbenefit', {$ENDIF}
    {$IFDEF FHIR_FAMILYMEMBERHISTORY} 'familymemberhistory', {$ENDIF}
    {$IFDEF FHIR_FLAG} 'flag', {$ENDIF}
    {$IFDEF FHIR_GOAL} 'goal', {$ENDIF}
    {$IFDEF FHIR_GRAPHDEFINITION} 'graphdefinition', {$ENDIF}
    {$IFDEF FHIR_GROUP} 'group', {$ENDIF}
    {$IFDEF FHIR_GUIDANCERESPONSE} 'guidanceresponse', {$ENDIF}
    {$IFDEF FHIR_HEALTHCARESERVICE} 'healthcareservice', {$ENDIF}
    {$IFDEF FHIR_IMAGINGSTUDY} 'imagingstudy', {$ENDIF}
    {$IFDEF FHIR_IMMUNIZATION} 'immunization', {$ENDIF}
    {$IFDEF FHIR_IMMUNIZATIONEVALUATION} 'immunizationevaluation', {$ENDIF}
    {$IFDEF FHIR_IMMUNIZATIONRECOMMENDATION} 'immunizationrecommendation', {$ENDIF}
    {$IFDEF FHIR_IMPLEMENTATIONGUIDE} 'implementationguide', {$ENDIF}
    {$IFDEF FHIR_INGREDIENT} 'ingredient', {$ENDIF}
    {$IFDEF FHIR_INSURANCEPLAN} 'insuranceplan', {$ENDIF}
    {$IFDEF FHIR_INVOICE} 'invoice', {$ENDIF}
    {$IFDEF FHIR_LIBRARY} 'library', {$ENDIF}
    {$IFDEF FHIR_LINKAGE} 'linkage', {$ENDIF}
    {$IFDEF FHIR_LIST} 'list', {$ENDIF}
    {$IFDEF FHIR_LOCATION} 'location', {$ENDIF}
    {$IFDEF FHIR_MANUFACTUREDITEMDEFINITION} 'manufactureditemdefinition', {$ENDIF}
    {$IFDEF FHIR_MEASURE} 'measure', {$ENDIF}
    {$IFDEF FHIR_MEASUREREPORT} 'measurereport', {$ENDIF}
    {$IFDEF FHIR_MEDIA} 'media', {$ENDIF}
    {$IFDEF FHIR_MEDICATION} 'medication', {$ENDIF}
    {$IFDEF FHIR_MEDICATIONADMINISTRATION} 'medicationadministration', {$ENDIF}
    {$IFDEF FHIR_MEDICATIONDISPENSE} 'medicationdispense', {$ENDIF}
    {$IFDEF FHIR_MEDICATIONKNOWLEDGE} 'medicationknowledge', {$ENDIF}
    {$IFDEF FHIR_MEDICATIONREQUEST} 'medicationrequest', {$ENDIF}
    {$IFDEF FHIR_MEDICATIONSTATEMENT} 'medicationstatement', {$ENDIF}
    {$IFDEF FHIR_MEDICINALPRODUCTDEFINITION} 'medicinalproductdefinition', {$ENDIF}
    {$IFDEF FHIR_MESSAGEDEFINITION} 'messagedefinition', {$ENDIF}
    {$IFDEF FHIR_MESSAGEHEADER} 'messageheader', {$ENDIF}
    {$IFDEF FHIR_MOLECULARSEQUENCE} 'molecularsequence', {$ENDIF}
    {$IFDEF FHIR_NAMINGSYSTEM} 'namingsystem', {$ENDIF}
    {$IFDEF FHIR_NUTRITIONORDER} 'nutritionorder', {$ENDIF}
    {$IFDEF FHIR_NUTRITIONPRODUCT} 'nutritionproduct', {$ENDIF}
    {$IFDEF FHIR_OBSERVATION} 'observation', {$ENDIF}
    {$IFDEF FHIR_OBSERVATIONDEFINITION} 'observationdefinition', {$ENDIF}
    {$IFDEF FHIR_OPERATIONDEFINITION} 'operationdefinition', {$ENDIF}
    {$IFDEF FHIR_OPERATIONOUTCOME} 'operationoutcome', {$ENDIF}
    {$IFDEF FHIR_ORGANIZATION} 'organization', {$ENDIF}
    {$IFDEF FHIR_ORGANIZATIONAFFILIATION} 'organizationaffiliation', {$ENDIF}
    {$IFDEF FHIR_PACKAGEDPRODUCTDEFINITION} 'packagedproductdefinition', {$ENDIF}
    {$IFDEF FHIR_PARAMETERS} 'parameters', {$ENDIF}
    {$IFDEF FHIR_PATIENT} 'patient', {$ENDIF}
    {$IFDEF FHIR_PAYMENTNOTICE} 'paymentnotice', {$ENDIF}
    {$IFDEF FHIR_PAYMENTRECONCILIATION} 'paymentreconciliation', {$ENDIF}
    {$IFDEF FHIR_PERSON} 'person', {$ENDIF}
    {$IFDEF FHIR_PLANDEFINITION} 'plandefinition', {$ENDIF}
    {$IFDEF FHIR_PRACTITIONER} 'practitioner', {$ENDIF}
    {$IFDEF FHIR_PRACTITIONERROLE} 'practitionerrole', {$ENDIF}
    {$IFDEF FHIR_PROCEDURE} 'procedure', {$ENDIF}
    {$IFDEF FHIR_PROVENANCE} 'provenance', {$ENDIF}
    {$IFDEF FHIR_QUESTIONNAIRE} 'questionnaire', {$ENDIF}
    {$IFDEF FHIR_QUESTIONNAIRERESPONSE} 'questionnaireresponse', {$ENDIF}
    {$IFDEF FHIR_REGULATEDAUTHORIZATION} 'regulatedauthorization', {$ENDIF}
    {$IFDEF FHIR_RELATEDPERSON} 'relatedperson', {$ENDIF}
    {$IFDEF FHIR_REQUESTGROUP} 'requestgroup', {$ENDIF}
    {$IFDEF FHIR_RESEARCHDEFINITION} 'researchdefinition', {$ENDIF}
    {$IFDEF FHIR_RESEARCHELEMENTDEFINITION} 'researchelementdefinition', {$ENDIF}
    {$IFDEF FHIR_RESEARCHSTUDY} 'researchstudy', {$ENDIF}
    {$IFDEF FHIR_RESEARCHSUBJECT} 'researchsubject', {$ENDIF}
    {$IFDEF FHIR_RISKASSESSMENT} 'riskassessment', {$ENDIF}
    {$IFDEF FHIR_SCHEDULE} 'schedule', {$ENDIF}
    {$IFDEF FHIR_SEARCHPARAMETER} 'searchparameter', {$ENDIF}
    {$IFDEF FHIR_SERVICEREQUEST} 'servicerequest', {$ENDIF}
    {$IFDEF FHIR_SLOT} 'slot', {$ENDIF}
    {$IFDEF FHIR_SPECIMEN} 'specimen', {$ENDIF}
    {$IFDEF FHIR_SPECIMENDEFINITION} 'specimendefinition', {$ENDIF}
    {$IFDEF FHIR_STRUCTUREDEFINITION} 'structuredefinition', {$ENDIF}
    {$IFDEF FHIR_STRUCTUREMAP} 'structuremap', {$ENDIF}
    {$IFDEF FHIR_SUBSCRIPTION} 'subscription', {$ENDIF}
    {$IFDEF FHIR_SUBSCRIPTIONSTATUS} 'subscriptionstatus', {$ENDIF}
    {$IFDEF FHIR_SUBSCRIPTIONTOPIC} 'subscriptiontopic', {$ENDIF}
    {$IFDEF FHIR_SUBSTANCE} 'substance', {$ENDIF}
    {$IFDEF FHIR_SUBSTANCEDEFINITION} 'substancedefinition', {$ENDIF}
    {$IFDEF FHIR_SUPPLYDELIVERY} 'supplydelivery', {$ENDIF}
    {$IFDEF FHIR_SUPPLYREQUEST} 'supplyrequest', {$ENDIF}
    {$IFDEF FHIR_TASK} 'task', {$ENDIF}
    {$IFDEF FHIR_TERMINOLOGYCAPABILITIES} 'terminologycapabilities', {$ENDIF}
    {$IFDEF FHIR_TESTREPORT} 'testreport', {$ENDIF}
    {$IFDEF FHIR_TESTSCRIPT} 'testscript', {$ENDIF}
    {$IFDEF FHIR_VALUESET} 'valueset', {$ENDIF}
    {$IFDEF FHIR_VERIFICATIONRESULT} 'verificationresult', {$ENDIF}
    {$IFDEF FHIR_VISIONPRESCRIPTION} 'visionprescription', {$ENDIF}
    'custom');

const
  CLASSES_TFhirResourceType : Array[TFhirResourceType] of TFhirResourceClass = (
    nil,
    {$IFDEF FHIR_ACCOUNT} TFhirAccount, {$ENDIF}
    {$IFDEF FHIR_ACTIVITYDEFINITION} TFhirActivityDefinition, {$ENDIF}
    {$IFDEF FHIR_ADMINISTRABLEPRODUCTDEFINITION} TFhirAdministrableProductDefinition, {$ENDIF}
    {$IFDEF FHIR_ADVERSEEVENT} TFhirAdverseEvent, {$ENDIF}
    {$IFDEF FHIR_ALLERGYINTOLERANCE} TFhirAllergyIntolerance, {$ENDIF}
    {$IFDEF FHIR_APPOINTMENT} TFhirAppointment, {$ENDIF}
    {$IFDEF FHIR_APPOINTMENTRESPONSE} TFhirAppointmentResponse, {$ENDIF}
    {$IFDEF FHIR_AUDITEVENT} TFhirAuditEvent, {$ENDIF}
    {$IFDEF FHIR_BASIC} TFhirBasic, {$ENDIF}
    {$IFDEF FHIR_BINARY} TFhirBinary, {$ENDIF}
    {$IFDEF FHIR_BIOLOGICALLYDERIVEDPRODUCT} TFhirBiologicallyDerivedProduct, {$ENDIF}
    {$IFDEF FHIR_BODYSTRUCTURE} TFhirBodyStructure, {$ENDIF}
    {$IFDEF FHIR_BUNDLE} TFhirBundle, {$ENDIF}
    {$IFDEF FHIR_CAPABILITYSTATEMENT} TFhirCapabilityStatement, {$ENDIF}
    {$IFDEF FHIR_CAREPLAN} TFhirCarePlan, {$ENDIF}
    {$IFDEF FHIR_CARETEAM} TFhirCareTeam, {$ENDIF}
    {$IFDEF FHIR_CATALOGENTRY} TFhirCatalogEntry, {$ENDIF}
    {$IFDEF FHIR_CHARGEITEM} TFhirChargeItem, {$ENDIF}
    {$IFDEF FHIR_CHARGEITEMDEFINITION} TFhirChargeItemDefinition, {$ENDIF}
    {$IFDEF FHIR_CITATION} TFhirCitation, {$ENDIF}
    {$IFDEF FHIR_CLAIM} TFhirClaim, {$ENDIF}
    {$IFDEF FHIR_CLAIMRESPONSE} TFhirClaimResponse, {$ENDIF}
    {$IFDEF FHIR_CLINICALIMPRESSION} TFhirClinicalImpression, {$ENDIF}
    {$IFDEF FHIR_CLINICALUSEDEFINITION} TFhirClinicalUseDefinition, {$ENDIF}
    {$IFDEF FHIR_CODESYSTEM} TFhirCodeSystem, {$ENDIF}
    {$IFDEF FHIR_COMMUNICATION} TFhirCommunication, {$ENDIF}
    {$IFDEF FHIR_COMMUNICATIONREQUEST} TFhirCommunicationRequest, {$ENDIF}
    {$IFDEF FHIR_COMPARTMENTDEFINITION} TFhirCompartmentDefinition, {$ENDIF}
    {$IFDEF FHIR_COMPOSITION} TFhirComposition, {$ENDIF}
    {$IFDEF FHIR_CONCEPTMAP} TFhirConceptMap, {$ENDIF}
    {$IFDEF FHIR_CONDITION} TFhirCondition, {$ENDIF}
    {$IFDEF FHIR_CONSENT} TFhirConsent, {$ENDIF}
    {$IFDEF FHIR_CONTRACT} TFhirContract, {$ENDIF}
    {$IFDEF FHIR_COVERAGE} TFhirCoverage, {$ENDIF}
    {$IFDEF FHIR_COVERAGEELIGIBILITYREQUEST} TFhirCoverageEligibilityRequest, {$ENDIF}
    {$IFDEF FHIR_COVERAGEELIGIBILITYRESPONSE} TFhirCoverageEligibilityResponse, {$ENDIF}
    {$IFDEF FHIR_DETECTEDISSUE} TFhirDetectedIssue, {$ENDIF}
    {$IFDEF FHIR_DEVICE} TFhirDevice, {$ENDIF}
    {$IFDEF FHIR_DEVICEDEFINITION} TFhirDeviceDefinition, {$ENDIF}
    {$IFDEF FHIR_DEVICEMETRIC} TFhirDeviceMetric, {$ENDIF}
    {$IFDEF FHIR_DEVICEREQUEST} TFhirDeviceRequest, {$ENDIF}
    {$IFDEF FHIR_DEVICEUSESTATEMENT} TFhirDeviceUseStatement, {$ENDIF}
    {$IFDEF FHIR_DIAGNOSTICREPORT} TFhirDiagnosticReport, {$ENDIF}
    {$IFDEF FHIR_DOCUMENTMANIFEST} TFhirDocumentManifest, {$ENDIF}
    {$IFDEF FHIR_DOCUMENTREFERENCE} TFhirDocumentReference, {$ENDIF}
    {$IFDEF FHIR_ENCOUNTER} TFhirEncounter, {$ENDIF}
    {$IFDEF FHIR_ENDPOINT} TFhirEndpoint, {$ENDIF}
    {$IFDEF FHIR_ENROLLMENTREQUEST} TFhirEnrollmentRequest, {$ENDIF}
    {$IFDEF FHIR_ENROLLMENTRESPONSE} TFhirEnrollmentResponse, {$ENDIF}
    {$IFDEF FHIR_EPISODEOFCARE} TFhirEpisodeOfCare, {$ENDIF}
    {$IFDEF FHIR_EVENTDEFINITION} TFhirEventDefinition, {$ENDIF}
    {$IFDEF FHIR_EVIDENCE} TFhirEvidence, {$ENDIF}
    {$IFDEF FHIR_EVIDENCEREPORT} TFhirEvidenceReport, {$ENDIF}
    {$IFDEF FHIR_EVIDENCEVARIABLE} TFhirEvidenceVariable, {$ENDIF}
    {$IFDEF FHIR_EXAMPLESCENARIO} TFhirExampleScenario, {$ENDIF}
    {$IFDEF FHIR_EXPLANATIONOFBENEFIT} TFhirExplanationOfBenefit, {$ENDIF}
    {$IFDEF FHIR_FAMILYMEMBERHISTORY} TFhirFamilyMemberHistory, {$ENDIF}
    {$IFDEF FHIR_FLAG} TFhirFlag, {$ENDIF}
    {$IFDEF FHIR_GOAL} TFhirGoal, {$ENDIF}
    {$IFDEF FHIR_GRAPHDEFINITION} TFhirGraphDefinition, {$ENDIF}
    {$IFDEF FHIR_GROUP} TFhirGroup, {$ENDIF}
    {$IFDEF FHIR_GUIDANCERESPONSE} TFhirGuidanceResponse, {$ENDIF}
    {$IFDEF FHIR_HEALTHCARESERVICE} TFhirHealthcareService, {$ENDIF}
    {$IFDEF FHIR_IMAGINGSTUDY} TFhirImagingStudy, {$ENDIF}
    {$IFDEF FHIR_IMMUNIZATION} TFhirImmunization, {$ENDIF}
    {$IFDEF FHIR_IMMUNIZATIONEVALUATION} TFhirImmunizationEvaluation, {$ENDIF}
    {$IFDEF FHIR_IMMUNIZATIONRECOMMENDATION} TFhirImmunizationRecommendation, {$ENDIF}
    {$IFDEF FHIR_IMPLEMENTATIONGUIDE} TFhirImplementationGuide, {$ENDIF}
    {$IFDEF FHIR_INGREDIENT} TFhirIngredient, {$ENDIF}
    {$IFDEF FHIR_INSURANCEPLAN} TFhirInsurancePlan, {$ENDIF}
    {$IFDEF FHIR_INVOICE} TFhirInvoice, {$ENDIF}
    {$IFDEF FHIR_LIBRARY} TFhirLibrary, {$ENDIF}
    {$IFDEF FHIR_LINKAGE} TFhirLinkage, {$ENDIF}
    {$IFDEF FHIR_LIST} TFhirList, {$ENDIF}
    {$IFDEF FHIR_LOCATION} TFhirLocation, {$ENDIF}
    {$IFDEF FHIR_MANUFACTUREDITEMDEFINITION} TFhirManufacturedItemDefinition, {$ENDIF}
    {$IFDEF FHIR_MEASURE} TFhirMeasure, {$ENDIF}
    {$IFDEF FHIR_MEASUREREPORT} TFhirMeasureReport, {$ENDIF}
    {$IFDEF FHIR_MEDIA} TFhirMedia, {$ENDIF}
    {$IFDEF FHIR_MEDICATION} TFhirMedication, {$ENDIF}
    {$IFDEF FHIR_MEDICATIONADMINISTRATION} TFhirMedicationAdministration, {$ENDIF}
    {$IFDEF FHIR_MEDICATIONDISPENSE} TFhirMedicationDispense, {$ENDIF}
    {$IFDEF FHIR_MEDICATIONKNOWLEDGE} TFhirMedicationKnowledge, {$ENDIF}
    {$IFDEF FHIR_MEDICATIONREQUEST} TFhirMedicationRequest, {$ENDIF}
    {$IFDEF FHIR_MEDICATIONSTATEMENT} TFhirMedicationStatement, {$ENDIF}
    {$IFDEF FHIR_MEDICINALPRODUCTDEFINITION} TFhirMedicinalProductDefinition, {$ENDIF}
    {$IFDEF FHIR_MESSAGEDEFINITION} TFhirMessageDefinition, {$ENDIF}
    {$IFDEF FHIR_MESSAGEHEADER} TFhirMessageHeader, {$ENDIF}
    {$IFDEF FHIR_MOLECULARSEQUENCE} TFhirMolecularSequence, {$ENDIF}
    {$IFDEF FHIR_NAMINGSYSTEM} TFhirNamingSystem, {$ENDIF}
    {$IFDEF FHIR_NUTRITIONORDER} TFhirNutritionOrder, {$ENDIF}
    {$IFDEF FHIR_NUTRITIONPRODUCT} TFhirNutritionProduct, {$ENDIF}
    {$IFDEF FHIR_OBSERVATION} TFhirObservation, {$ENDIF}
    {$IFDEF FHIR_OBSERVATIONDEFINITION} TFhirObservationDefinition, {$ENDIF}
    {$IFDEF FHIR_OPERATIONDEFINITION} TFhirOperationDefinition, {$ENDIF}
    {$IFDEF FHIR_OPERATIONOUTCOME} TFhirOperationOutcome, {$ENDIF}
    {$IFDEF FHIR_ORGANIZATION} TFhirOrganization, {$ENDIF}
    {$IFDEF FHIR_ORGANIZATIONAFFILIATION} TFhirOrganizationAffiliation, {$ENDIF}
    {$IFDEF FHIR_PACKAGEDPRODUCTDEFINITION} TFhirPackagedProductDefinition, {$ENDIF}
    {$IFDEF FHIR_PARAMETERS} TFhirParameters, {$ENDIF}
    {$IFDEF FHIR_PATIENT} TFhirPatient, {$ENDIF}
    {$IFDEF FHIR_PAYMENTNOTICE} TFhirPaymentNotice, {$ENDIF}
    {$IFDEF FHIR_PAYMENTRECONCILIATION} TFhirPaymentReconciliation, {$ENDIF}
    {$IFDEF FHIR_PERSON} TFhirPerson, {$ENDIF}
    {$IFDEF FHIR_PLANDEFINITION} TFhirPlanDefinition, {$ENDIF}
    {$IFDEF FHIR_PRACTITIONER} TFhirPractitioner, {$ENDIF}
    {$IFDEF FHIR_PRACTITIONERROLE} TFhirPractitionerRole, {$ENDIF}
    {$IFDEF FHIR_PROCEDURE} TFhirProcedure, {$ENDIF}
    {$IFDEF FHIR_PROVENANCE} TFhirProvenance, {$ENDIF}
    {$IFDEF FHIR_QUESTIONNAIRE} TFhirQuestionnaire, {$ENDIF}
    {$IFDEF FHIR_QUESTIONNAIRERESPONSE} TFhirQuestionnaireResponse, {$ENDIF}
    {$IFDEF FHIR_REGULATEDAUTHORIZATION} TFhirRegulatedAuthorization, {$ENDIF}
    {$IFDEF FHIR_RELATEDPERSON} TFhirRelatedPerson, {$ENDIF}
    {$IFDEF FHIR_REQUESTGROUP} TFhirRequestGroup, {$ENDIF}
    {$IFDEF FHIR_RESEARCHDEFINITION} TFhirResearchDefinition, {$ENDIF}
    {$IFDEF FHIR_RESEARCHELEMENTDEFINITION} TFhirResearchElementDefinition, {$ENDIF}
    {$IFDEF FHIR_RESEARCHSTUDY} TFhirResearchStudy, {$ENDIF}
    {$IFDEF FHIR_RESEARCHSUBJECT} TFhirResearchSubject, {$ENDIF}
    {$IFDEF FHIR_RISKASSESSMENT} TFhirRiskAssessment, {$ENDIF}
    {$IFDEF FHIR_SCHEDULE} TFhirSchedule, {$ENDIF}
    {$IFDEF FHIR_SEARCHPARAMETER} TFhirSearchParameter, {$ENDIF}
    {$IFDEF FHIR_SERVICEREQUEST} TFhirServiceRequest, {$ENDIF}
    {$IFDEF FHIR_SLOT} TFhirSlot, {$ENDIF}
    {$IFDEF FHIR_SPECIMEN} TFhirSpecimen, {$ENDIF}
    {$IFDEF FHIR_SPECIMENDEFINITION} TFhirSpecimenDefinition, {$ENDIF}
    {$IFDEF FHIR_STRUCTUREDEFINITION} TFhirStructureDefinition, {$ENDIF}
    {$IFDEF FHIR_STRUCTUREMAP} TFhirStructureMap, {$ENDIF}
    {$IFDEF FHIR_SUBSCRIPTION} TFhirSubscription, {$ENDIF}
    {$IFDEF FHIR_SUBSCRIPTIONSTATUS} TFhirSubscriptionStatus, {$ENDIF}
    {$IFDEF FHIR_SUBSCRIPTIONTOPIC} TFhirSubscriptionTopic, {$ENDIF}
    {$IFDEF FHIR_SUBSTANCE} TFhirSubstance, {$ENDIF}
    {$IFDEF FHIR_SUBSTANCEDEFINITION} TFhirSubstanceDefinition, {$ENDIF}
    {$IFDEF FHIR_SUPPLYDELIVERY} TFhirSupplyDelivery, {$ENDIF}
    {$IFDEF FHIR_SUPPLYREQUEST} TFhirSupplyRequest, {$ENDIF}
    {$IFDEF FHIR_TASK} TFhirTask, {$ENDIF}
    {$IFDEF FHIR_TERMINOLOGYCAPABILITIES} TFhirTerminologyCapabilities, {$ENDIF}
    {$IFDEF FHIR_TESTREPORT} TFhirTestReport, {$ENDIF}
    {$IFDEF FHIR_TESTSCRIPT} TFhirTestScript, {$ENDIF}
    {$IFDEF FHIR_VALUESET} TFhirValueSet, {$ENDIF}
    {$IFDEF FHIR_VERIFICATIONRESULT} TFhirVerificationResult, {$ENDIF}
    {$IFDEF FHIR_VISIONPRESCRIPTION} TFhirVisionPrescription, {$ENDIF}
    nil);

  ALL_RESOURCE_TYPES = [
    {$IFDEF FHIR_ACCOUNT} frtAccount, {$ENDIF}
    {$IFDEF FHIR_ACTIVITYDEFINITION} frtActivityDefinition, {$ENDIF}
    {$IFDEF FHIR_ADMINISTRABLEPRODUCTDEFINITION} frtAdministrableProductDefinition, {$ENDIF}
    {$IFDEF FHIR_ADVERSEEVENT} frtAdverseEvent, {$ENDIF}
    {$IFDEF FHIR_ALLERGYINTOLERANCE} frtAllergyIntolerance, {$ENDIF}
    {$IFDEF FHIR_APPOINTMENT} frtAppointment, {$ENDIF}
    {$IFDEF FHIR_APPOINTMENTRESPONSE} frtAppointmentResponse, {$ENDIF}
    {$IFDEF FHIR_AUDITEVENT} frtAuditEvent, {$ENDIF}
    {$IFDEF FHIR_BASIC} frtBasic, {$ENDIF}
    {$IFDEF FHIR_BINARY} frtBinary, {$ENDIF}
    {$IFDEF FHIR_BIOLOGICALLYDERIVEDPRODUCT} frtBiologicallyDerivedProduct, {$ENDIF}
    {$IFDEF FHIR_BODYSTRUCTURE} frtBodyStructure, {$ENDIF}
    {$IFDEF FHIR_BUNDLE} frtBundle, {$ENDIF}
    {$IFDEF FHIR_CAPABILITYSTATEMENT} frtCapabilityStatement, {$ENDIF}
    {$IFDEF FHIR_CAREPLAN} frtCarePlan, {$ENDIF}
    {$IFDEF FHIR_CARETEAM} frtCareTeam, {$ENDIF}
    {$IFDEF FHIR_CATALOGENTRY} frtCatalogEntry, {$ENDIF}
    {$IFDEF FHIR_CHARGEITEM} frtChargeItem, {$ENDIF}
    {$IFDEF FHIR_CHARGEITEMDEFINITION} frtChargeItemDefinition, {$ENDIF}
    {$IFDEF FHIR_CITATION} frtCitation, {$ENDIF}
    {$IFDEF FHIR_CLAIM} frtClaim, {$ENDIF}
    {$IFDEF FHIR_CLAIMRESPONSE} frtClaimResponse, {$ENDIF}
    {$IFDEF FHIR_CLINICALIMPRESSION} frtClinicalImpression, {$ENDIF}
    {$IFDEF FHIR_CLINICALUSEDEFINITION} frtClinicalUseDefinition, {$ENDIF}
    {$IFDEF FHIR_CODESYSTEM} frtCodeSystem, {$ENDIF}
    {$IFDEF FHIR_COMMUNICATION} frtCommunication, {$ENDIF}
    {$IFDEF FHIR_COMMUNICATIONREQUEST} frtCommunicationRequest, {$ENDIF}
    {$IFDEF FHIR_COMPARTMENTDEFINITION} frtCompartmentDefinition, {$ENDIF}
    {$IFDEF FHIR_COMPOSITION} frtComposition, {$ENDIF}
    {$IFDEF FHIR_CONCEPTMAP} frtConceptMap, {$ENDIF}
    {$IFDEF FHIR_CONDITION} frtCondition, {$ENDIF}
    {$IFDEF FHIR_CONSENT} frtConsent, {$ENDIF}
    {$IFDEF FHIR_CONTRACT} frtContract, {$ENDIF}
    {$IFDEF FHIR_COVERAGE} frtCoverage, {$ENDIF}
    {$IFDEF FHIR_COVERAGEELIGIBILITYREQUEST} frtCoverageEligibilityRequest, {$ENDIF}
    {$IFDEF FHIR_COVERAGEELIGIBILITYRESPONSE} frtCoverageEligibilityResponse, {$ENDIF}
    {$IFDEF FHIR_DETECTEDISSUE} frtDetectedIssue, {$ENDIF}
    {$IFDEF FHIR_DEVICE} frtDevice, {$ENDIF}
    {$IFDEF FHIR_DEVICEDEFINITION} frtDeviceDefinition, {$ENDIF}
    {$IFDEF FHIR_DEVICEMETRIC} frtDeviceMetric, {$ENDIF}
    {$IFDEF FHIR_DEVICEREQUEST} frtDeviceRequest, {$ENDIF}
    {$IFDEF FHIR_DEVICEUSESTATEMENT} frtDeviceUseStatement, {$ENDIF}
    {$IFDEF FHIR_DIAGNOSTICREPORT} frtDiagnosticReport, {$ENDIF}
    {$IFDEF FHIR_DOCUMENTMANIFEST} frtDocumentManifest, {$ENDIF}
    {$IFDEF FHIR_DOCUMENTREFERENCE} frtDocumentReference, {$ENDIF}
    {$IFDEF FHIR_ENCOUNTER} frtEncounter, {$ENDIF}
    {$IFDEF FHIR_ENDPOINT} frtEndpoint, {$ENDIF}
    {$IFDEF FHIR_ENROLLMENTREQUEST} frtEnrollmentRequest, {$ENDIF}
    {$IFDEF FHIR_ENROLLMENTRESPONSE} frtEnrollmentResponse, {$ENDIF}
    {$IFDEF FHIR_EPISODEOFCARE} frtEpisodeOfCare, {$ENDIF}
    {$IFDEF FHIR_EVENTDEFINITION} frtEventDefinition, {$ENDIF}
    {$IFDEF FHIR_EVIDENCE} frtEvidence, {$ENDIF}
    {$IFDEF FHIR_EVIDENCEREPORT} frtEvidenceReport, {$ENDIF}
    {$IFDEF FHIR_EVIDENCEVARIABLE} frtEvidenceVariable, {$ENDIF}
    {$IFDEF FHIR_EXAMPLESCENARIO} frtExampleScenario, {$ENDIF}
    {$IFDEF FHIR_EXPLANATIONOFBENEFIT} frtExplanationOfBenefit, {$ENDIF}
    {$IFDEF FHIR_FAMILYMEMBERHISTORY} frtFamilyMemberHistory, {$ENDIF}
    {$IFDEF FHIR_FLAG} frtFlag, {$ENDIF}
    {$IFDEF FHIR_GOAL} frtGoal, {$ENDIF}
    {$IFDEF FHIR_GRAPHDEFINITION} frtGraphDefinition, {$ENDIF}
    {$IFDEF FHIR_GROUP} frtGroup, {$ENDIF}
    {$IFDEF FHIR_GUIDANCERESPONSE} frtGuidanceResponse, {$ENDIF}
    {$IFDEF FHIR_HEALTHCARESERVICE} frtHealthcareService, {$ENDIF}
    {$IFDEF FHIR_IMAGINGSTUDY} frtImagingStudy, {$ENDIF}
    {$IFDEF FHIR_IMMUNIZATION} frtImmunization, {$ENDIF}
    {$IFDEF FHIR_IMMUNIZATIONEVALUATION} frtImmunizationEvaluation, {$ENDIF}
    {$IFDEF FHIR_IMMUNIZATIONRECOMMENDATION} frtImmunizationRecommendation, {$ENDIF}
    {$IFDEF FHIR_IMPLEMENTATIONGUIDE} frtImplementationGuide, {$ENDIF}
    {$IFDEF FHIR_INGREDIENT} frtIngredient, {$ENDIF}
    {$IFDEF FHIR_INSURANCEPLAN} frtInsurancePlan, {$ENDIF}
    {$IFDEF FHIR_INVOICE} frtInvoice, {$ENDIF}
    {$IFDEF FHIR_LIBRARY} frtLibrary, {$ENDIF}
    {$IFDEF FHIR_LINKAGE} frtLinkage, {$ENDIF}
    {$IFDEF FHIR_LIST} frtList, {$ENDIF}
    {$IFDEF FHIR_LOCATION} frtLocation, {$ENDIF}
    {$IFDEF FHIR_MANUFACTUREDITEMDEFINITION} frtManufacturedItemDefinition, {$ENDIF}
    {$IFDEF FHIR_MEASURE} frtMeasure, {$ENDIF}
    {$IFDEF FHIR_MEASUREREPORT} frtMeasureReport, {$ENDIF}
    {$IFDEF FHIR_MEDIA} frtMedia, {$ENDIF}
    {$IFDEF FHIR_MEDICATION} frtMedication, {$ENDIF}
    {$IFDEF FHIR_MEDICATIONADMINISTRATION} frtMedicationAdministration, {$ENDIF}
    {$IFDEF FHIR_MEDICATIONDISPENSE} frtMedicationDispense, {$ENDIF}
    {$IFDEF FHIR_MEDICATIONKNOWLEDGE} frtMedicationKnowledge, {$ENDIF}
    {$IFDEF FHIR_MEDICATIONREQUEST} frtMedicationRequest, {$ENDIF}
    {$IFDEF FHIR_MEDICATIONSTATEMENT} frtMedicationStatement, {$ENDIF}
    {$IFDEF FHIR_MEDICINALPRODUCTDEFINITION} frtMedicinalProductDefinition, {$ENDIF}
    {$IFDEF FHIR_MESSAGEDEFINITION} frtMessageDefinition, {$ENDIF}
    {$IFDEF FHIR_MESSAGEHEADER} frtMessageHeader, {$ENDIF}
    {$IFDEF FHIR_MOLECULARSEQUENCE} frtMolecularSequence, {$ENDIF}
    {$IFDEF FHIR_NAMINGSYSTEM} frtNamingSystem, {$ENDIF}
    {$IFDEF FHIR_NUTRITIONORDER} frtNutritionOrder, {$ENDIF}
    {$IFDEF FHIR_NUTRITIONPRODUCT} frtNutritionProduct, {$ENDIF}
    {$IFDEF FHIR_OBSERVATION} frtObservation, {$ENDIF}
    {$IFDEF FHIR_OBSERVATIONDEFINITION} frtObservationDefinition, {$ENDIF}
    {$IFDEF FHIR_OPERATIONDEFINITION} frtOperationDefinition, {$ENDIF}
    {$IFDEF FHIR_OPERATIONOUTCOME} frtOperationOutcome, {$ENDIF}
    {$IFDEF FHIR_ORGANIZATION} frtOrganization, {$ENDIF}
    {$IFDEF FHIR_ORGANIZATIONAFFILIATION} frtOrganizationAffiliation, {$ENDIF}
    {$IFDEF FHIR_PACKAGEDPRODUCTDEFINITION} frtPackagedProductDefinition, {$ENDIF}
    {$IFDEF FHIR_PARAMETERS} frtParameters, {$ENDIF}
    {$IFDEF FHIR_PATIENT} frtPatient, {$ENDIF}
    {$IFDEF FHIR_PAYMENTNOTICE} frtPaymentNotice, {$ENDIF}
    {$IFDEF FHIR_PAYMENTRECONCILIATION} frtPaymentReconciliation, {$ENDIF}
    {$IFDEF FHIR_PERSON} frtPerson, {$ENDIF}
    {$IFDEF FHIR_PLANDEFINITION} frtPlanDefinition, {$ENDIF}
    {$IFDEF FHIR_PRACTITIONER} frtPractitioner, {$ENDIF}
    {$IFDEF FHIR_PRACTITIONERROLE} frtPractitionerRole, {$ENDIF}
    {$IFDEF FHIR_PROCEDURE} frtProcedure, {$ENDIF}
    {$IFDEF FHIR_PROVENANCE} frtProvenance, {$ENDIF}
    {$IFDEF FHIR_QUESTIONNAIRE} frtQuestionnaire, {$ENDIF}
    {$IFDEF FHIR_QUESTIONNAIRERESPONSE} frtQuestionnaireResponse, {$ENDIF}
    {$IFDEF FHIR_REGULATEDAUTHORIZATION} frtRegulatedAuthorization, {$ENDIF}
    {$IFDEF FHIR_RELATEDPERSON} frtRelatedPerson, {$ENDIF}
    {$IFDEF FHIR_REQUESTGROUP} frtRequestGroup, {$ENDIF}
    {$IFDEF FHIR_RESEARCHDEFINITION} frtResearchDefinition, {$ENDIF}
    {$IFDEF FHIR_RESEARCHELEMENTDEFINITION} frtResearchElementDefinition, {$ENDIF}
    {$IFDEF FHIR_RESEARCHSTUDY} frtResearchStudy, {$ENDIF}
    {$IFDEF FHIR_RESEARCHSUBJECT} frtResearchSubject, {$ENDIF}
    {$IFDEF FHIR_RISKASSESSMENT} frtRiskAssessment, {$ENDIF}
    {$IFDEF FHIR_SCHEDULE} frtSchedule, {$ENDIF}
    {$IFDEF FHIR_SEARCHPARAMETER} frtSearchParameter, {$ENDIF}
    {$IFDEF FHIR_SERVICEREQUEST} frtServiceRequest, {$ENDIF}
    {$IFDEF FHIR_SLOT} frtSlot, {$ENDIF}
    {$IFDEF FHIR_SPECIMEN} frtSpecimen, {$ENDIF}
    {$IFDEF FHIR_SPECIMENDEFINITION} frtSpecimenDefinition, {$ENDIF}
    {$IFDEF FHIR_STRUCTUREDEFINITION} frtStructureDefinition, {$ENDIF}
    {$IFDEF FHIR_STRUCTUREMAP} frtStructureMap, {$ENDIF}
    {$IFDEF FHIR_SUBSCRIPTION} frtSubscription, {$ENDIF}
    {$IFDEF FHIR_SUBSCRIPTIONSTATUS} frtSubscriptionStatus, {$ENDIF}
    {$IFDEF FHIR_SUBSCRIPTIONTOPIC} frtSubscriptionTopic, {$ENDIF}
    {$IFDEF FHIR_SUBSTANCE} frtSubstance, {$ENDIF}
    {$IFDEF FHIR_SUBSTANCEDEFINITION} frtSubstanceDefinition, {$ENDIF}
    {$IFDEF FHIR_SUPPLYDELIVERY} frtSupplyDelivery, {$ENDIF}
    {$IFDEF FHIR_SUPPLYREQUEST} frtSupplyRequest, {$ENDIF}
    {$IFDEF FHIR_TASK} frtTask, {$ENDIF}
    {$IFDEF FHIR_TERMINOLOGYCAPABILITIES} frtTerminologyCapabilities, {$ENDIF}
    {$IFDEF FHIR_TESTREPORT} frtTestReport, {$ENDIF}
    {$IFDEF FHIR_TESTSCRIPT} frtTestScript, {$ENDIF}
    {$IFDEF FHIR_VALUESET} frtValueSet, {$ENDIF}
    {$IFDEF FHIR_VERIFICATIONRESULT} frtVerificationResult, {$ENDIF}
    {$IFDEF FHIR_VISIONPRESCRIPTION} frtVisionPrescription, {$ENDIF}
    frtCustom];

const
  ALL_RESOURCE_TYPE_NAMES : Array[TFhirResourceType] of String = (
    '--None--',
    {$IFDEF FHIR_ACCOUNT} 'Account', {$ENDIF}
    {$IFDEF FHIR_ACTIVITYDEFINITION} 'ActivityDefinition', {$ENDIF}
    {$IFDEF FHIR_ADMINISTRABLEPRODUCTDEFINITION} 'AdministrableProductDefinition', {$ENDIF}
    {$IFDEF FHIR_ADVERSEEVENT} 'AdverseEvent', {$ENDIF}
    {$IFDEF FHIR_ALLERGYINTOLERANCE} 'AllergyIntolerance', {$ENDIF}
    {$IFDEF FHIR_APPOINTMENT} 'Appointment', {$ENDIF}
    {$IFDEF FHIR_APPOINTMENTRESPONSE} 'AppointmentResponse', {$ENDIF}
    {$IFDEF FHIR_AUDITEVENT} 'AuditEvent', {$ENDIF}
    {$IFDEF FHIR_BASIC} 'Basic', {$ENDIF}
    {$IFDEF FHIR_BINARY} 'Binary', {$ENDIF}
    {$IFDEF FHIR_BIOLOGICALLYDERIVEDPRODUCT} 'BiologicallyDerivedProduct', {$ENDIF}
    {$IFDEF FHIR_BODYSTRUCTURE} 'BodyStructure', {$ENDIF}
    {$IFDEF FHIR_BUNDLE} 'Bundle', {$ENDIF}
    {$IFDEF FHIR_CAPABILITYSTATEMENT} 'CapabilityStatement', {$ENDIF}
    {$IFDEF FHIR_CAREPLAN} 'CarePlan', {$ENDIF}
    {$IFDEF FHIR_CARETEAM} 'CareTeam', {$ENDIF}
    {$IFDEF FHIR_CATALOGENTRY} 'CatalogEntry', {$ENDIF}
    {$IFDEF FHIR_CHARGEITEM} 'ChargeItem', {$ENDIF}
    {$IFDEF FHIR_CHARGEITEMDEFINITION} 'ChargeItemDefinition', {$ENDIF}
    {$IFDEF FHIR_CITATION} 'Citation', {$ENDIF}
    {$IFDEF FHIR_CLAIM} 'Claim', {$ENDIF}
    {$IFDEF FHIR_CLAIMRESPONSE} 'ClaimResponse', {$ENDIF}
    {$IFDEF FHIR_CLINICALIMPRESSION} 'ClinicalImpression', {$ENDIF}
    {$IFDEF FHIR_CLINICALUSEDEFINITION} 'ClinicalUseDefinition', {$ENDIF}
    {$IFDEF FHIR_CODESYSTEM} 'CodeSystem', {$ENDIF}
    {$IFDEF FHIR_COMMUNICATION} 'Communication', {$ENDIF}
    {$IFDEF FHIR_COMMUNICATIONREQUEST} 'CommunicationRequest', {$ENDIF}
    {$IFDEF FHIR_COMPARTMENTDEFINITION} 'CompartmentDefinition', {$ENDIF}
    {$IFDEF FHIR_COMPOSITION} 'Composition', {$ENDIF}
    {$IFDEF FHIR_CONCEPTMAP} 'ConceptMap', {$ENDIF}
    {$IFDEF FHIR_CONDITION} 'Condition', {$ENDIF}
    {$IFDEF FHIR_CONSENT} 'Consent', {$ENDIF}
    {$IFDEF FHIR_CONTRACT} 'Contract', {$ENDIF}
    {$IFDEF FHIR_COVERAGE} 'Coverage', {$ENDIF}
    {$IFDEF FHIR_COVERAGEELIGIBILITYREQUEST} 'CoverageEligibilityRequest', {$ENDIF}
    {$IFDEF FHIR_COVERAGEELIGIBILITYRESPONSE} 'CoverageEligibilityResponse', {$ENDIF}
    {$IFDEF FHIR_DETECTEDISSUE} 'DetectedIssue', {$ENDIF}
    {$IFDEF FHIR_DEVICE} 'Device', {$ENDIF}
    {$IFDEF FHIR_DEVICEDEFINITION} 'DeviceDefinition', {$ENDIF}
    {$IFDEF FHIR_DEVICEMETRIC} 'DeviceMetric', {$ENDIF}
    {$IFDEF FHIR_DEVICEREQUEST} 'DeviceRequest', {$ENDIF}
    {$IFDEF FHIR_DEVICEUSESTATEMENT} 'DeviceUseStatement', {$ENDIF}
    {$IFDEF FHIR_DIAGNOSTICREPORT} 'DiagnosticReport', {$ENDIF}
    {$IFDEF FHIR_DOCUMENTMANIFEST} 'DocumentManifest', {$ENDIF}
    {$IFDEF FHIR_DOCUMENTREFERENCE} 'DocumentReference', {$ENDIF}
    {$IFDEF FHIR_ENCOUNTER} 'Encounter', {$ENDIF}
    {$IFDEF FHIR_ENDPOINT} 'Endpoint', {$ENDIF}
    {$IFDEF FHIR_ENROLLMENTREQUEST} 'EnrollmentRequest', {$ENDIF}
    {$IFDEF FHIR_ENROLLMENTRESPONSE} 'EnrollmentResponse', {$ENDIF}
    {$IFDEF FHIR_EPISODEOFCARE} 'EpisodeOfCare', {$ENDIF}
    {$IFDEF FHIR_EVENTDEFINITION} 'EventDefinition', {$ENDIF}
    {$IFDEF FHIR_EVIDENCE} 'Evidence', {$ENDIF}
    {$IFDEF FHIR_EVIDENCEREPORT} 'EvidenceReport', {$ENDIF}
    {$IFDEF FHIR_EVIDENCEVARIABLE} 'EvidenceVariable', {$ENDIF}
    {$IFDEF FHIR_EXAMPLESCENARIO} 'ExampleScenario', {$ENDIF}
    {$IFDEF FHIR_EXPLANATIONOFBENEFIT} 'ExplanationOfBenefit', {$ENDIF}
    {$IFDEF FHIR_FAMILYMEMBERHISTORY} 'FamilyMemberHistory', {$ENDIF}
    {$IFDEF FHIR_FLAG} 'Flag', {$ENDIF}
    {$IFDEF FHIR_GOAL} 'Goal', {$ENDIF}
    {$IFDEF FHIR_GRAPHDEFINITION} 'GraphDefinition', {$ENDIF}
    {$IFDEF FHIR_GROUP} 'Group', {$ENDIF}
    {$IFDEF FHIR_GUIDANCERESPONSE} 'GuidanceResponse', {$ENDIF}
    {$IFDEF FHIR_HEALTHCARESERVICE} 'HealthcareService', {$ENDIF}
    {$IFDEF FHIR_IMAGINGSTUDY} 'ImagingStudy', {$ENDIF}
    {$IFDEF FHIR_IMMUNIZATION} 'Immunization', {$ENDIF}
    {$IFDEF FHIR_IMMUNIZATIONEVALUATION} 'ImmunizationEvaluation', {$ENDIF}
    {$IFDEF FHIR_IMMUNIZATIONRECOMMENDATION} 'ImmunizationRecommendation', {$ENDIF}
    {$IFDEF FHIR_IMPLEMENTATIONGUIDE} 'ImplementationGuide', {$ENDIF}
    {$IFDEF FHIR_INGREDIENT} 'Ingredient', {$ENDIF}
    {$IFDEF FHIR_INSURANCEPLAN} 'InsurancePlan', {$ENDIF}
    {$IFDEF FHIR_INVOICE} 'Invoice', {$ENDIF}
    {$IFDEF FHIR_LIBRARY} 'Library', {$ENDIF}
    {$IFDEF FHIR_LINKAGE} 'Linkage', {$ENDIF}
    {$IFDEF FHIR_LIST} 'List', {$ENDIF}
    {$IFDEF FHIR_LOCATION} 'Location', {$ENDIF}
    {$IFDEF FHIR_MANUFACTUREDITEMDEFINITION} 'ManufacturedItemDefinition', {$ENDIF}
    {$IFDEF FHIR_MEASURE} 'Measure', {$ENDIF}
    {$IFDEF FHIR_MEASUREREPORT} 'MeasureReport', {$ENDIF}
    {$IFDEF FHIR_MEDIA} 'Media', {$ENDIF}
    {$IFDEF FHIR_MEDICATION} 'Medication', {$ENDIF}
    {$IFDEF FHIR_MEDICATIONADMINISTRATION} 'MedicationAdministration', {$ENDIF}
    {$IFDEF FHIR_MEDICATIONDISPENSE} 'MedicationDispense', {$ENDIF}
    {$IFDEF FHIR_MEDICATIONKNOWLEDGE} 'MedicationKnowledge', {$ENDIF}
    {$IFDEF FHIR_MEDICATIONREQUEST} 'MedicationRequest', {$ENDIF}
    {$IFDEF FHIR_MEDICATIONSTATEMENT} 'MedicationStatement', {$ENDIF}
    {$IFDEF FHIR_MEDICINALPRODUCTDEFINITION} 'MedicinalProductDefinition', {$ENDIF}
    {$IFDEF FHIR_MESSAGEDEFINITION} 'MessageDefinition', {$ENDIF}
    {$IFDEF FHIR_MESSAGEHEADER} 'MessageHeader', {$ENDIF}
    {$IFDEF FHIR_MOLECULARSEQUENCE} 'MolecularSequence', {$ENDIF}
    {$IFDEF FHIR_NAMINGSYSTEM} 'NamingSystem', {$ENDIF}
    {$IFDEF FHIR_NUTRITIONORDER} 'NutritionOrder', {$ENDIF}
    {$IFDEF FHIR_NUTRITIONPRODUCT} 'NutritionProduct', {$ENDIF}
    {$IFDEF FHIR_OBSERVATION} 'Observation', {$ENDIF}
    {$IFDEF FHIR_OBSERVATIONDEFINITION} 'ObservationDefinition', {$ENDIF}
    {$IFDEF FHIR_OPERATIONDEFINITION} 'OperationDefinition', {$ENDIF}
    {$IFDEF FHIR_OPERATIONOUTCOME} 'OperationOutcome', {$ENDIF}
    {$IFDEF FHIR_ORGANIZATION} 'Organization', {$ENDIF}
    {$IFDEF FHIR_ORGANIZATIONAFFILIATION} 'OrganizationAffiliation', {$ENDIF}
    {$IFDEF FHIR_PACKAGEDPRODUCTDEFINITION} 'PackagedProductDefinition', {$ENDIF}
    {$IFDEF FHIR_PARAMETERS} 'Parameters', {$ENDIF}
    {$IFDEF FHIR_PATIENT} 'Patient', {$ENDIF}
    {$IFDEF FHIR_PAYMENTNOTICE} 'PaymentNotice', {$ENDIF}
    {$IFDEF FHIR_PAYMENTRECONCILIATION} 'PaymentReconciliation', {$ENDIF}
    {$IFDEF FHIR_PERSON} 'Person', {$ENDIF}
    {$IFDEF FHIR_PLANDEFINITION} 'PlanDefinition', {$ENDIF}
    {$IFDEF FHIR_PRACTITIONER} 'Practitioner', {$ENDIF}
    {$IFDEF FHIR_PRACTITIONERROLE} 'PractitionerRole', {$ENDIF}
    {$IFDEF FHIR_PROCEDURE} 'Procedure', {$ENDIF}
    {$IFDEF FHIR_PROVENANCE} 'Provenance', {$ENDIF}
    {$IFDEF FHIR_QUESTIONNAIRE} 'Questionnaire', {$ENDIF}
    {$IFDEF FHIR_QUESTIONNAIRERESPONSE} 'QuestionnaireResponse', {$ENDIF}
    {$IFDEF FHIR_REGULATEDAUTHORIZATION} 'RegulatedAuthorization', {$ENDIF}
    {$IFDEF FHIR_RELATEDPERSON} 'RelatedPerson', {$ENDIF}
    {$IFDEF FHIR_REQUESTGROUP} 'RequestGroup', {$ENDIF}
    {$IFDEF FHIR_RESEARCHDEFINITION} 'ResearchDefinition', {$ENDIF}
    {$IFDEF FHIR_RESEARCHELEMENTDEFINITION} 'ResearchElementDefinition', {$ENDIF}
    {$IFDEF FHIR_RESEARCHSTUDY} 'ResearchStudy', {$ENDIF}
    {$IFDEF FHIR_RESEARCHSUBJECT} 'ResearchSubject', {$ENDIF}
    {$IFDEF FHIR_RISKASSESSMENT} 'RiskAssessment', {$ENDIF}
    {$IFDEF FHIR_SCHEDULE} 'Schedule', {$ENDIF}
    {$IFDEF FHIR_SEARCHPARAMETER} 'SearchParameter', {$ENDIF}
    {$IFDEF FHIR_SERVICEREQUEST} 'ServiceRequest', {$ENDIF}
    {$IFDEF FHIR_SLOT} 'Slot', {$ENDIF}
    {$IFDEF FHIR_SPECIMEN} 'Specimen', {$ENDIF}
    {$IFDEF FHIR_SPECIMENDEFINITION} 'SpecimenDefinition', {$ENDIF}
    {$IFDEF FHIR_STRUCTUREDEFINITION} 'StructureDefinition', {$ENDIF}
    {$IFDEF FHIR_STRUCTUREMAP} 'StructureMap', {$ENDIF}
    {$IFDEF FHIR_SUBSCRIPTION} 'Subscription', {$ENDIF}
    {$IFDEF FHIR_SUBSCRIPTIONSTATUS} 'SubscriptionStatus', {$ENDIF}
    {$IFDEF FHIR_SUBSCRIPTIONTOPIC} 'SubscriptionTopic', {$ENDIF}
    {$IFDEF FHIR_SUBSTANCE} 'Substance', {$ENDIF}
    {$IFDEF FHIR_SUBSTANCEDEFINITION} 'SubstanceDefinition', {$ENDIF}
    {$IFDEF FHIR_SUPPLYDELIVERY} 'SupplyDelivery', {$ENDIF}
    {$IFDEF FHIR_SUPPLYREQUEST} 'SupplyRequest', {$ENDIF}
    {$IFDEF FHIR_TASK} 'Task', {$ENDIF}
    {$IFDEF FHIR_TERMINOLOGYCAPABILITIES} 'TerminologyCapabilities', {$ENDIF}
    {$IFDEF FHIR_TESTREPORT} 'TestReport', {$ENDIF}
    {$IFDEF FHIR_TESTSCRIPT} 'TestScript', {$ENDIF}
    {$IFDEF FHIR_VALUESET} 'ValueSet', {$ENDIF}
    {$IFDEF FHIR_VERIFICATIONRESULT} 'VerificationResult', {$ENDIF}
    {$IFDEF FHIR_VISIONPRESCRIPTION} 'VisionPrescription', {$ENDIF}
    'Custom');



type

{$IFDEF FHIR_ACCOUNT}
  // Search Parameters for Account
  TSearchParamsAccount = (
    spAccount__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_ACCOUNT}

{$IFDEF FHIR_ACTIVITYDEFINITION}
  // Search Parameters for ActivityDefinition
  TSearchParamsActivityDefinition = (
    spActivityDefinition__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_ACTIVITYDEFINITION}

{$IFDEF FHIR_ADMINISTRABLEPRODUCTDEFINITION}
  // Search Parameters for AdministrableProductDefinition
  TSearchParamsAdministrableProductDefinition = (
    spAdministrableProductDefinition__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_ADMINISTRABLEPRODUCTDEFINITION}

{$IFDEF FHIR_ADVERSEEVENT}
  // Search Parameters for AdverseEvent
  TSearchParamsAdverseEvent = (
    spAdverseEvent__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_ADVERSEEVENT}

{$IFDEF FHIR_ALLERGYINTOLERANCE}
  // Search Parameters for AllergyIntolerance
  TSearchParamsAllergyIntolerance = (
    spAllergyIntolerance__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_ALLERGYINTOLERANCE}

{$IFDEF FHIR_APPOINTMENT}
  // Search Parameters for Appointment
  TSearchParamsAppointment = (
    spAppointment__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_APPOINTMENT}

{$IFDEF FHIR_APPOINTMENTRESPONSE}
  // Search Parameters for AppointmentResponse
  TSearchParamsAppointmentResponse = (
    spAppointmentResponse__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_APPOINTMENTRESPONSE}

{$IFDEF FHIR_AUDITEVENT}
  // Search Parameters for AuditEvent
  TSearchParamsAuditEvent = (
    spAuditEvent__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_AUDITEVENT}

{$IFDEF FHIR_BASIC}
  // Search Parameters for Basic
  TSearchParamsBasic = (
    spBasic__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_BASIC}

{$IFDEF FHIR_BINARY}
  // Search Parameters for Binary
  TSearchParamsBinary = (
    spBinary__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_BINARY}

{$IFDEF FHIR_BIOLOGICALLYDERIVEDPRODUCT}
  // Search Parameters for BiologicallyDerivedProduct
  TSearchParamsBiologicallyDerivedProduct = (
    spBiologicallyDerivedProduct__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_BIOLOGICALLYDERIVEDPRODUCT}

{$IFDEF FHIR_BODYSTRUCTURE}
  // Search Parameters for BodyStructure
  TSearchParamsBodyStructure = (
    spBodyStructure__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_BODYSTRUCTURE}

{$IFDEF FHIR_BUNDLE}
  // Search Parameters for Bundle
  TSearchParamsBundle = (
    spBundle__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_BUNDLE}

{$IFDEF FHIR_CAPABILITYSTATEMENT}
  // Search Parameters for CapabilityStatement
  TSearchParamsCapabilityStatement = (
    spCapabilityStatement__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_CAPABILITYSTATEMENT}

{$IFDEF FHIR_CAREPLAN}
  // Search Parameters for CarePlan
  TSearchParamsCarePlan = (
    spCarePlan__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_CAREPLAN}

{$IFDEF FHIR_CARETEAM}
  // Search Parameters for CareTeam
  TSearchParamsCareTeam = (
    spCareTeam__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_CARETEAM}

{$IFDEF FHIR_CATALOGENTRY}
  // Search Parameters for CatalogEntry
  TSearchParamsCatalogEntry = (
    spCatalogEntry__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_CATALOGENTRY}

{$IFDEF FHIR_CHARGEITEM}
  // Search Parameters for ChargeItem
  TSearchParamsChargeItem = (
    spChargeItem__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_CHARGEITEM}

{$IFDEF FHIR_CHARGEITEMDEFINITION}
  // Search Parameters for ChargeItemDefinition
  TSearchParamsChargeItemDefinition = (
    spChargeItemDefinition__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_CHARGEITEMDEFINITION}

{$IFDEF FHIR_CITATION}
  // Search Parameters for Citation
  TSearchParamsCitation = (
    spCitation__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_CITATION}

{$IFDEF FHIR_CLAIM}
  // Search Parameters for Claim
  TSearchParamsClaim = (
    spClaim__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_CLAIM}

{$IFDEF FHIR_CLAIMRESPONSE}
  // Search Parameters for ClaimResponse
  TSearchParamsClaimResponse = (
    spClaimResponse__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_CLAIMRESPONSE}

{$IFDEF FHIR_CLINICALIMPRESSION}
  // Search Parameters for ClinicalImpression
  TSearchParamsClinicalImpression = (
    spClinicalImpression__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_CLINICALIMPRESSION}

{$IFDEF FHIR_CLINICALUSEDEFINITION}
  // Search Parameters for ClinicalUseDefinition
  TSearchParamsClinicalUseDefinition = (
    spClinicalUseDefinition__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_CLINICALUSEDEFINITION}

{$IFDEF FHIR_CODESYSTEM}
  // Search Parameters for CodeSystem
  TSearchParamsCodeSystem = (
    spCodeSystem__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_CODESYSTEM}

{$IFDEF FHIR_COMMUNICATION}
  // Search Parameters for Communication
  TSearchParamsCommunication = (
    spCommunication__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_COMMUNICATION}

{$IFDEF FHIR_COMMUNICATIONREQUEST}
  // Search Parameters for CommunicationRequest
  TSearchParamsCommunicationRequest = (
    spCommunicationRequest__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_COMMUNICATIONREQUEST}

{$IFDEF FHIR_COMPARTMENTDEFINITION}
  // Search Parameters for CompartmentDefinition
  TSearchParamsCompartmentDefinition = (
    spCompartmentDefinition__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_COMPARTMENTDEFINITION}

{$IFDEF FHIR_COMPOSITION}
  // Search Parameters for Composition
  TSearchParamsComposition = (
    spComposition__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_COMPOSITION}

{$IFDEF FHIR_CONCEPTMAP}
  // Search Parameters for ConceptMap
  TSearchParamsConceptMap = (
    spConceptMap__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_CONCEPTMAP}

{$IFDEF FHIR_CONDITION}
  // Search Parameters for Condition
  TSearchParamsCondition = (
    spCondition__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_CONDITION}

{$IFDEF FHIR_CONSENT}
  // Search Parameters for Consent
  TSearchParamsConsent = (
    spConsent__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_CONSENT}

{$IFDEF FHIR_CONTRACT}
  // Search Parameters for Contract
  TSearchParamsContract = (
    spContract__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_CONTRACT}

{$IFDEF FHIR_COVERAGE}
  // Search Parameters for Coverage
  TSearchParamsCoverage = (
    spCoverage__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_COVERAGE}

{$IFDEF FHIR_COVERAGEELIGIBILITYREQUEST}
  // Search Parameters for CoverageEligibilityRequest
  TSearchParamsCoverageEligibilityRequest = (
    spCoverageEligibilityRequest__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_COVERAGEELIGIBILITYREQUEST}

{$IFDEF FHIR_COVERAGEELIGIBILITYRESPONSE}
  // Search Parameters for CoverageEligibilityResponse
  TSearchParamsCoverageEligibilityResponse = (
    spCoverageEligibilityResponse__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_COVERAGEELIGIBILITYRESPONSE}

{$IFDEF FHIR_DETECTEDISSUE}
  // Search Parameters for DetectedIssue
  TSearchParamsDetectedIssue = (
    spDetectedIssue__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_DETECTEDISSUE}

{$IFDEF FHIR_DEVICE}
  // Search Parameters for Device
  TSearchParamsDevice = (
    spDevice__filter {http://hl7.org/fhir/SearchParameter/filter},
    spDevice_Din {http://hl7.org/fhir/SearchParameter/deviceextensionsDevicedin});
{$ENDIF FHIR_DEVICE}

{$IFDEF FHIR_DEVICEDEFINITION}
  // Search Parameters for DeviceDefinition
  TSearchParamsDeviceDefinition = (
    spDeviceDefinition__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_DEVICEDEFINITION}

{$IFDEF FHIR_DEVICEMETRIC}
  // Search Parameters for DeviceMetric
  TSearchParamsDeviceMetric = (
    spDeviceMetric__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_DEVICEMETRIC}

{$IFDEF FHIR_DEVICEREQUEST}
  // Search Parameters for DeviceRequest
  TSearchParamsDeviceRequest = (
    spDeviceRequest__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_DEVICEREQUEST}

{$IFDEF FHIR_DEVICEUSESTATEMENT}
  // Search Parameters for DeviceUseStatement
  TSearchParamsDeviceUseStatement = (
    spDeviceUseStatement__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_DEVICEUSESTATEMENT}

{$IFDEF FHIR_DIAGNOSTICREPORT}
  // Search Parameters for DiagnosticReport
  TSearchParamsDiagnosticReport = (
    spDiagnosticReport__filter {http://hl7.org/fhir/SearchParameter/filter},
    spDiagnosticReport_Assessedcondition {http://hl7.org/fhir/SearchParameter/diagnosticreportgeneticDiagnosticReportassessedcondition});
{$ENDIF FHIR_DIAGNOSTICREPORT}

{$IFDEF FHIR_DOCUMENTMANIFEST}
  // Search Parameters for DocumentManifest
  TSearchParamsDocumentManifest = (
    spDocumentManifest__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_DOCUMENTMANIFEST}

{$IFDEF FHIR_DOCUMENTREFERENCE}
  // Search Parameters for DocumentReference
  TSearchParamsDocumentReference = (
    spDocumentReference__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_DOCUMENTREFERENCE}

{$IFDEF FHIR_ENCOUNTER}
  // Search Parameters for Encounter
  TSearchParamsEncounter = (
    spEncounter__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_ENCOUNTER}

{$IFDEF FHIR_ENDPOINT}
  // Search Parameters for Endpoint
  TSearchParamsEndpoint = (
    spEndpoint__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_ENDPOINT}

{$IFDEF FHIR_ENROLLMENTREQUEST}
  // Search Parameters for EnrollmentRequest
  TSearchParamsEnrollmentRequest = (
    spEnrollmentRequest__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_ENROLLMENTREQUEST}

{$IFDEF FHIR_ENROLLMENTRESPONSE}
  // Search Parameters for EnrollmentResponse
  TSearchParamsEnrollmentResponse = (
    spEnrollmentResponse__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_ENROLLMENTRESPONSE}

{$IFDEF FHIR_EPISODEOFCARE}
  // Search Parameters for EpisodeOfCare
  TSearchParamsEpisodeOfCare = (
    spEpisodeOfCare__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_EPISODEOFCARE}

{$IFDEF FHIR_EVENTDEFINITION}
  // Search Parameters for EventDefinition
  TSearchParamsEventDefinition = (
    spEventDefinition__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_EVENTDEFINITION}

{$IFDEF FHIR_EVIDENCE}
  // Search Parameters for Evidence
  TSearchParamsEvidence = (
    spEvidence__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_EVIDENCE}

{$IFDEF FHIR_EVIDENCEREPORT}
  // Search Parameters for EvidenceReport
  TSearchParamsEvidenceReport = (
    spEvidenceReport__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_EVIDENCEREPORT}

{$IFDEF FHIR_EVIDENCEVARIABLE}
  // Search Parameters for EvidenceVariable
  TSearchParamsEvidenceVariable = (
    spEvidenceVariable__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_EVIDENCEVARIABLE}

{$IFDEF FHIR_EXAMPLESCENARIO}
  // Search Parameters for ExampleScenario
  TSearchParamsExampleScenario = (
    spExampleScenario__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_EXAMPLESCENARIO}

{$IFDEF FHIR_EXPLANATIONOFBENEFIT}
  // Search Parameters for ExplanationOfBenefit
  TSearchParamsExplanationOfBenefit = (
    spExplanationOfBenefit__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_EXPLANATIONOFBENEFIT}

{$IFDEF FHIR_FAMILYMEMBERHISTORY}
  // Search Parameters for FamilyMemberHistory
  TSearchParamsFamilyMemberHistory = (
    spFamilyMemberHistory__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_FAMILYMEMBERHISTORY}

{$IFDEF FHIR_FLAG}
  // Search Parameters for Flag
  TSearchParamsFlag = (
    spFlag__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_FLAG}

{$IFDEF FHIR_GOAL}
  // Search Parameters for Goal
  TSearchParamsGoal = (
    spGoal__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_GOAL}

{$IFDEF FHIR_GRAPHDEFINITION}
  // Search Parameters for GraphDefinition
  TSearchParamsGraphDefinition = (
    spGraphDefinition__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_GRAPHDEFINITION}

{$IFDEF FHIR_GROUP}
  // Search Parameters for Group
  TSearchParamsGroup = (
    spGroup__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_GROUP}

{$IFDEF FHIR_GUIDANCERESPONSE}
  // Search Parameters for GuidanceResponse
  TSearchParamsGuidanceResponse = (
    spGuidanceResponse__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_GUIDANCERESPONSE}

{$IFDEF FHIR_HEALTHCARESERVICE}
  // Search Parameters for HealthcareService
  TSearchParamsHealthcareService = (
    spHealthcareService__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_HEALTHCARESERVICE}

{$IFDEF FHIR_IMAGINGSTUDY}
  // Search Parameters for ImagingStudy
  TSearchParamsImagingStudy = (
    spImagingStudy__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_IMAGINGSTUDY}

{$IFDEF FHIR_IMMUNIZATION}
  // Search Parameters for Immunization
  TSearchParamsImmunization = (
    spImmunization__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_IMMUNIZATION}

{$IFDEF FHIR_IMMUNIZATIONEVALUATION}
  // Search Parameters for ImmunizationEvaluation
  TSearchParamsImmunizationEvaluation = (
    spImmunizationEvaluation__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_IMMUNIZATIONEVALUATION}

{$IFDEF FHIR_IMMUNIZATIONRECOMMENDATION}
  // Search Parameters for ImmunizationRecommendation
  TSearchParamsImmunizationRecommendation = (
    spImmunizationRecommendation__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_IMMUNIZATIONRECOMMENDATION}

{$IFDEF FHIR_IMPLEMENTATIONGUIDE}
  // Search Parameters for ImplementationGuide
  TSearchParamsImplementationGuide = (
    spImplementationGuide__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_IMPLEMENTATIONGUIDE}

{$IFDEF FHIR_INGREDIENT}
  // Search Parameters for Ingredient
  TSearchParamsIngredient = (
    spIngredient__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_INGREDIENT}

{$IFDEF FHIR_INSURANCEPLAN}
  // Search Parameters for InsurancePlan
  TSearchParamsInsurancePlan = (
    spInsurancePlan__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_INSURANCEPLAN}

{$IFDEF FHIR_INVOICE}
  // Search Parameters for Invoice
  TSearchParamsInvoice = (
    spInvoice__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_INVOICE}

{$IFDEF FHIR_LIBRARY}
  // Search Parameters for Library
  TSearchParamsLibrary = (
    spLibrary__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_LIBRARY}

{$IFDEF FHIR_LINKAGE}
  // Search Parameters for Linkage
  TSearchParamsLinkage = (
    spLinkage__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_LINKAGE}

{$IFDEF FHIR_LIST}
  // Search Parameters for List
  TSearchParamsList = (
    spList__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_LIST}

{$IFDEF FHIR_LOCATION}
  // Search Parameters for Location
  TSearchParamsLocation = (
    spLocation__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_LOCATION}

{$IFDEF FHIR_MANUFACTUREDITEMDEFINITION}
  // Search Parameters for ManufacturedItemDefinition
  TSearchParamsManufacturedItemDefinition = (
    spManufacturedItemDefinition__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_MANUFACTUREDITEMDEFINITION}

{$IFDEF FHIR_MEASURE}
  // Search Parameters for Measure
  TSearchParamsMeasure = (
    spMeasure__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_MEASURE}

{$IFDEF FHIR_MEASUREREPORT}
  // Search Parameters for MeasureReport
  TSearchParamsMeasureReport = (
    spMeasureReport__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_MEASUREREPORT}

{$IFDEF FHIR_MEDIA}
  // Search Parameters for Media
  TSearchParamsMedia = (
    spMedia__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_MEDIA}

{$IFDEF FHIR_MEDICATION}
  // Search Parameters for Medication
  TSearchParamsMedication = (
    spMedication__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_MEDICATION}

{$IFDEF FHIR_MEDICATIONADMINISTRATION}
  // Search Parameters for MedicationAdministration
  TSearchParamsMedicationAdministration = (
    spMedicationAdministration__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_MEDICATIONADMINISTRATION}

{$IFDEF FHIR_MEDICATIONDISPENSE}
  // Search Parameters for MedicationDispense
  TSearchParamsMedicationDispense = (
    spMedicationDispense__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_MEDICATIONDISPENSE}

{$IFDEF FHIR_MEDICATIONKNOWLEDGE}
  // Search Parameters for MedicationKnowledge
  TSearchParamsMedicationKnowledge = (
    spMedicationKnowledge__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_MEDICATIONKNOWLEDGE}

{$IFDEF FHIR_MEDICATIONREQUEST}
  // Search Parameters for MedicationRequest
  TSearchParamsMedicationRequest = (
    spMedicationRequest__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_MEDICATIONREQUEST}

{$IFDEF FHIR_MEDICATIONSTATEMENT}
  // Search Parameters for MedicationStatement
  TSearchParamsMedicationStatement = (
    spMedicationStatement__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_MEDICATIONSTATEMENT}

{$IFDEF FHIR_MEDICINALPRODUCTDEFINITION}
  // Search Parameters for MedicinalProductDefinition
  TSearchParamsMedicinalProductDefinition = (
    spMedicinalProductDefinition__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_MEDICINALPRODUCTDEFINITION}

{$IFDEF FHIR_MESSAGEDEFINITION}
  // Search Parameters for MessageDefinition
  TSearchParamsMessageDefinition = (
    spMessageDefinition__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_MESSAGEDEFINITION}

{$IFDEF FHIR_MESSAGEHEADER}
  // Search Parameters for MessageHeader
  TSearchParamsMessageHeader = (
    spMessageHeader__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_MESSAGEHEADER}

{$IFDEF FHIR_MOLECULARSEQUENCE}
  // Search Parameters for MolecularSequence
  TSearchParamsMolecularSequence = (
    spMolecularSequence__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_MOLECULARSEQUENCE}

{$IFDEF FHIR_NAMINGSYSTEM}
  // Search Parameters for NamingSystem
  TSearchParamsNamingSystem = (
    spNamingSystem__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_NAMINGSYSTEM}

{$IFDEF FHIR_NUTRITIONORDER}
  // Search Parameters for NutritionOrder
  TSearchParamsNutritionOrder = (
    spNutritionOrder__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_NUTRITIONORDER}

{$IFDEF FHIR_NUTRITIONPRODUCT}
  // Search Parameters for NutritionProduct
  TSearchParamsNutritionProduct = (
    spNutritionProduct__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_NUTRITIONPRODUCT}

{$IFDEF FHIR_OBSERVATION}
  // Search Parameters for Observation
  TSearchParamsObservation = (
    spObservation__filter {http://hl7.org/fhir/SearchParameter/filter},
    spObservation_Aminoacidchange {http://hl7.org/fhir/SearchParameter/observationgeneticObservationaminoacidchange},
    spObservation_Dnavariant {http://hl7.org/fhir/SearchParameter/observationgeneticObservationdnavariant},
    spObservation_Geneaminoacidchange {http://hl7.org/fhir/SearchParameter/observationgeneticObservationgeneaminoacidchange},
    spObservation_Genednavariant {http://hl7.org/fhir/SearchParameter/observationgeneticObservationgenednavariant},
    spObservation_Geneidentifier {http://hl7.org/fhir/SearchParameter/observationgeneticObservationgeneidentifier});
{$ENDIF FHIR_OBSERVATION}

{$IFDEF FHIR_OBSERVATIONDEFINITION}
  // Search Parameters for ObservationDefinition
  TSearchParamsObservationDefinition = (
    spObservationDefinition__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_OBSERVATIONDEFINITION}

{$IFDEF FHIR_OPERATIONDEFINITION}
  // Search Parameters for OperationDefinition
  TSearchParamsOperationDefinition = (
    spOperationDefinition__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_OPERATIONDEFINITION}

{$IFDEF FHIR_OPERATIONOUTCOME}
  // Search Parameters for OperationOutcome
  TSearchParamsOperationOutcome = (
    spOperationOutcome__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_OPERATIONOUTCOME}

{$IFDEF FHIR_ORGANIZATION}
  // Search Parameters for Organization
  TSearchParamsOrganization = (
    spOrganization__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_ORGANIZATION}

{$IFDEF FHIR_ORGANIZATIONAFFILIATION}
  // Search Parameters for OrganizationAffiliation
  TSearchParamsOrganizationAffiliation = (
    spOrganizationAffiliation__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_ORGANIZATIONAFFILIATION}

{$IFDEF FHIR_PACKAGEDPRODUCTDEFINITION}
  // Search Parameters for PackagedProductDefinition
  TSearchParamsPackagedProductDefinition = (
    spPackagedProductDefinition__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_PACKAGEDPRODUCTDEFINITION}

{$IFDEF FHIR_PARAMETERS}
  // Search Parameters for Parameters
  TSearchParamsParameters = (
    spParameters__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_PARAMETERS}

{$IFDEF FHIR_PATIENT}
  // Search Parameters for Patient
  TSearchParamsPatient = (
    spPatient__filter {http://hl7.org/fhir/SearchParameter/filter},
    spPatient_Age {http://hl7.org/fhir/SearchParameter/patientextensionsPatientage},
    spPatient_BirthOrderBoolean {http://hl7.org/fhir/SearchParameter/patientextensionsPatientbirthOrderBoolean},
    spPatient_MothersMaidenName {http://hl7.org/fhir/SearchParameter/patientextensionsPatientmothersMaidenName});
{$ENDIF FHIR_PATIENT}

{$IFDEF FHIR_PAYMENTNOTICE}
  // Search Parameters for PaymentNotice
  TSearchParamsPaymentNotice = (
    spPaymentNotice__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_PAYMENTNOTICE}

{$IFDEF FHIR_PAYMENTRECONCILIATION}
  // Search Parameters for PaymentReconciliation
  TSearchParamsPaymentReconciliation = (
    spPaymentReconciliation__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_PAYMENTRECONCILIATION}

{$IFDEF FHIR_PERSON}
  // Search Parameters for Person
  TSearchParamsPerson = (
    spPerson__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_PERSON}

{$IFDEF FHIR_PLANDEFINITION}
  // Search Parameters for PlanDefinition
  TSearchParamsPlanDefinition = (
    spPlanDefinition__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_PLANDEFINITION}

{$IFDEF FHIR_PRACTITIONER}
  // Search Parameters for Practitioner
  TSearchParamsPractitioner = (
    spPractitioner__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_PRACTITIONER}

{$IFDEF FHIR_PRACTITIONERROLE}
  // Search Parameters for PractitionerRole
  TSearchParamsPractitionerRole = (
    spPractitionerRole__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_PRACTITIONERROLE}

{$IFDEF FHIR_PROCEDURE}
  // Search Parameters for Procedure
  TSearchParamsProcedure = (
    spProcedure__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_PROCEDURE}

{$IFDEF FHIR_PROVENANCE}
  // Search Parameters for Provenance
  TSearchParamsProvenance = (
    spProvenance__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_PROVENANCE}

{$IFDEF FHIR_QUESTIONNAIRE}
  // Search Parameters for Questionnaire
  TSearchParamsQuestionnaire = (
    spQuestionnaire__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_QUESTIONNAIRE}

{$IFDEF FHIR_QUESTIONNAIRERESPONSE}
  // Search Parameters for QuestionnaireResponse
  TSearchParamsQuestionnaireResponse = (
    spQuestionnaireResponse__filter {http://hl7.org/fhir/SearchParameter/filter},
    spQuestionnaireResponse_Itemsubject {http://hl7.org/fhir/SearchParameter/questionnaireresponseextensionsQuestionnaireResponseitemsubject});
{$ENDIF FHIR_QUESTIONNAIRERESPONSE}

{$IFDEF FHIR_REGULATEDAUTHORIZATION}
  // Search Parameters for RegulatedAuthorization
  TSearchParamsRegulatedAuthorization = (
    spRegulatedAuthorization__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_REGULATEDAUTHORIZATION}

{$IFDEF FHIR_RELATEDPERSON}
  // Search Parameters for RelatedPerson
  TSearchParamsRelatedPerson = (
    spRelatedPerson__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_RELATEDPERSON}

{$IFDEF FHIR_REQUESTGROUP}
  // Search Parameters for RequestGroup
  TSearchParamsRequestGroup = (
    spRequestGroup__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_REQUESTGROUP}

{$IFDEF FHIR_RESEARCHDEFINITION}
  // Search Parameters for ResearchDefinition
  TSearchParamsResearchDefinition = (
    spResearchDefinition__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_RESEARCHDEFINITION}

{$IFDEF FHIR_RESEARCHELEMENTDEFINITION}
  // Search Parameters for ResearchElementDefinition
  TSearchParamsResearchElementDefinition = (
    spResearchElementDefinition__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_RESEARCHELEMENTDEFINITION}

{$IFDEF FHIR_RESEARCHSTUDY}
  // Search Parameters for ResearchStudy
  TSearchParamsResearchStudy = (
    spResearchStudy__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_RESEARCHSTUDY}

{$IFDEF FHIR_RESEARCHSUBJECT}
  // Search Parameters for ResearchSubject
  TSearchParamsResearchSubject = (
    spResearchSubject__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_RESEARCHSUBJECT}

{$IFDEF FHIR_RISKASSESSMENT}
  // Search Parameters for RiskAssessment
  TSearchParamsRiskAssessment = (
    spRiskAssessment__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_RISKASSESSMENT}

{$IFDEF FHIR_SCHEDULE}
  // Search Parameters for Schedule
  TSearchParamsSchedule = (
    spSchedule__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_SCHEDULE}

{$IFDEF FHIR_SEARCHPARAMETER}
  // Search Parameters for SearchParameter
  TSearchParamsSearchParameter = (
    spSearchParameter__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_SEARCHPARAMETER}

{$IFDEF FHIR_SERVICEREQUEST}
  // Search Parameters for ServiceRequest
  TSearchParamsServiceRequest = (
    spServiceRequest__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_SERVICEREQUEST}

{$IFDEF FHIR_SLOT}
  // Search Parameters for Slot
  TSearchParamsSlot = (
    spSlot__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_SLOT}

{$IFDEF FHIR_SPECIMEN}
  // Search Parameters for Specimen
  TSearchParamsSpecimen = (
    spSpecimen__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_SPECIMEN}

{$IFDEF FHIR_SPECIMENDEFINITION}
  // Search Parameters for SpecimenDefinition
  TSearchParamsSpecimenDefinition = (
    spSpecimenDefinition__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_SPECIMENDEFINITION}

{$IFDEF FHIR_STRUCTUREDEFINITION}
  // Search Parameters for StructureDefinition
  TSearchParamsStructureDefinition = (
    spStructureDefinition__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_STRUCTUREDEFINITION}

{$IFDEF FHIR_STRUCTUREMAP}
  // Search Parameters for StructureMap
  TSearchParamsStructureMap = (
    spStructureMap__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_STRUCTUREMAP}

{$IFDEF FHIR_SUBSCRIPTION}
  // Search Parameters for Subscription
  TSearchParamsSubscription = (
    spSubscription__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_SUBSCRIPTION}

{$IFDEF FHIR_SUBSCRIPTIONSTATUS}
  // Search Parameters for SubscriptionStatus
  TSearchParamsSubscriptionStatus = (
    spSubscriptionStatus__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_SUBSCRIPTIONSTATUS}

{$IFDEF FHIR_SUBSCRIPTIONTOPIC}
  // Search Parameters for SubscriptionTopic
  TSearchParamsSubscriptionTopic = (
    spSubscriptionTopic__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_SUBSCRIPTIONTOPIC}

{$IFDEF FHIR_SUBSTANCE}
  // Search Parameters for Substance
  TSearchParamsSubstance = (
    spSubstance__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_SUBSTANCE}

{$IFDEF FHIR_SUBSTANCEDEFINITION}
  // Search Parameters for SubstanceDefinition
  TSearchParamsSubstanceDefinition = (
    spSubstanceDefinition__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_SUBSTANCEDEFINITION}

{$IFDEF FHIR_SUPPLYDELIVERY}
  // Search Parameters for SupplyDelivery
  TSearchParamsSupplyDelivery = (
    spSupplyDelivery__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_SUPPLYDELIVERY}

{$IFDEF FHIR_SUPPLYREQUEST}
  // Search Parameters for SupplyRequest
  TSearchParamsSupplyRequest = (
    spSupplyRequest__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_SUPPLYREQUEST}

{$IFDEF FHIR_TASK}
  // Search Parameters for Task
  TSearchParamsTask = (
    spTask__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_TASK}

{$IFDEF FHIR_TERMINOLOGYCAPABILITIES}
  // Search Parameters for TerminologyCapabilities
  TSearchParamsTerminologyCapabilities = (
    spTerminologyCapabilities__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_TERMINOLOGYCAPABILITIES}

{$IFDEF FHIR_TESTREPORT}
  // Search Parameters for TestReport
  TSearchParamsTestReport = (
    spTestReport__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_TESTREPORT}

{$IFDEF FHIR_TESTSCRIPT}
  // Search Parameters for TestScript
  TSearchParamsTestScript = (
    spTestScript__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_TESTSCRIPT}

{$IFDEF FHIR_VALUESET}
  // Search Parameters for ValueSet
  TSearchParamsValueSet = (
    spValueSet__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_VALUESET}

{$IFDEF FHIR_VERIFICATIONRESULT}
  // Search Parameters for VerificationResult
  TSearchParamsVerificationResult = (
    spVerificationResult__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_VERIFICATIONRESULT}

{$IFDEF FHIR_VISIONPRESCRIPTION}
  // Search Parameters for VisionPrescription
  TSearchParamsVisionPrescription = (
    spVisionPrescription__filter {http://hl7.org/fhir/SearchParameter/filter});
{$ENDIF FHIR_VISIONPRESCRIPTION}



const

{$IFDEF FHIR_ACCOUNT}
  CODES_TSearchParamsAccount : Array[TSearchParamsAccount] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_ACTIVITYDEFINITION}
  CODES_TSearchParamsActivityDefinition : Array[TSearchParamsActivityDefinition] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_ADMINISTRABLEPRODUCTDEFINITION}
  CODES_TSearchParamsAdministrableProductDefinition : Array[TSearchParamsAdministrableProductDefinition] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_ADVERSEEVENT}
  CODES_TSearchParamsAdverseEvent : Array[TSearchParamsAdverseEvent] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_ALLERGYINTOLERANCE}
  CODES_TSearchParamsAllergyIntolerance : Array[TSearchParamsAllergyIntolerance] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_APPOINTMENT}
  CODES_TSearchParamsAppointment : Array[TSearchParamsAppointment] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_APPOINTMENTRESPONSE}
  CODES_TSearchParamsAppointmentResponse : Array[TSearchParamsAppointmentResponse] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_AUDITEVENT}
  CODES_TSearchParamsAuditEvent : Array[TSearchParamsAuditEvent] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_BASIC}
  CODES_TSearchParamsBasic : Array[TSearchParamsBasic] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_BINARY}
  CODES_TSearchParamsBinary : Array[TSearchParamsBinary] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_BIOLOGICALLYDERIVEDPRODUCT}
  CODES_TSearchParamsBiologicallyDerivedProduct : Array[TSearchParamsBiologicallyDerivedProduct] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_BODYSTRUCTURE}
  CODES_TSearchParamsBodyStructure : Array[TSearchParamsBodyStructure] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_BUNDLE}
  CODES_TSearchParamsBundle : Array[TSearchParamsBundle] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_CAPABILITYSTATEMENT}
  CODES_TSearchParamsCapabilityStatement : Array[TSearchParamsCapabilityStatement] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_CAREPLAN}
  CODES_TSearchParamsCarePlan : Array[TSearchParamsCarePlan] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_CARETEAM}
  CODES_TSearchParamsCareTeam : Array[TSearchParamsCareTeam] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_CATALOGENTRY}
  CODES_TSearchParamsCatalogEntry : Array[TSearchParamsCatalogEntry] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_CHARGEITEM}
  CODES_TSearchParamsChargeItem : Array[TSearchParamsChargeItem] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_CHARGEITEMDEFINITION}
  CODES_TSearchParamsChargeItemDefinition : Array[TSearchParamsChargeItemDefinition] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_CITATION}
  CODES_TSearchParamsCitation : Array[TSearchParamsCitation] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_CLAIM}
  CODES_TSearchParamsClaim : Array[TSearchParamsClaim] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_CLAIMRESPONSE}
  CODES_TSearchParamsClaimResponse : Array[TSearchParamsClaimResponse] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_CLINICALIMPRESSION}
  CODES_TSearchParamsClinicalImpression : Array[TSearchParamsClinicalImpression] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_CLINICALUSEDEFINITION}
  CODES_TSearchParamsClinicalUseDefinition : Array[TSearchParamsClinicalUseDefinition] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_CODESYSTEM}
  CODES_TSearchParamsCodeSystem : Array[TSearchParamsCodeSystem] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_COMMUNICATION}
  CODES_TSearchParamsCommunication : Array[TSearchParamsCommunication] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_COMMUNICATIONREQUEST}
  CODES_TSearchParamsCommunicationRequest : Array[TSearchParamsCommunicationRequest] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_COMPARTMENTDEFINITION}
  CODES_TSearchParamsCompartmentDefinition : Array[TSearchParamsCompartmentDefinition] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_COMPOSITION}
  CODES_TSearchParamsComposition : Array[TSearchParamsComposition] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_CONCEPTMAP}
  CODES_TSearchParamsConceptMap : Array[TSearchParamsConceptMap] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_CONDITION}
  CODES_TSearchParamsCondition : Array[TSearchParamsCondition] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_CONSENT}
  CODES_TSearchParamsConsent : Array[TSearchParamsConsent] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_CONTRACT}
  CODES_TSearchParamsContract : Array[TSearchParamsContract] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_COVERAGE}
  CODES_TSearchParamsCoverage : Array[TSearchParamsCoverage] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_COVERAGEELIGIBILITYREQUEST}
  CODES_TSearchParamsCoverageEligibilityRequest : Array[TSearchParamsCoverageEligibilityRequest] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_COVERAGEELIGIBILITYRESPONSE}
  CODES_TSearchParamsCoverageEligibilityResponse : Array[TSearchParamsCoverageEligibilityResponse] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_DETECTEDISSUE}
  CODES_TSearchParamsDetectedIssue : Array[TSearchParamsDetectedIssue] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_DEVICE}
  CODES_TSearchParamsDevice : Array[TSearchParamsDevice] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}', 'din {http://hl7.org/fhir/SearchParameter/device-extensions-Device-din}');
{$ENDIF}
{$IFDEF FHIR_DEVICEDEFINITION}
  CODES_TSearchParamsDeviceDefinition : Array[TSearchParamsDeviceDefinition] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_DEVICEMETRIC}
  CODES_TSearchParamsDeviceMetric : Array[TSearchParamsDeviceMetric] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_DEVICEREQUEST}
  CODES_TSearchParamsDeviceRequest : Array[TSearchParamsDeviceRequest] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_DEVICEUSESTATEMENT}
  CODES_TSearchParamsDeviceUseStatement : Array[TSearchParamsDeviceUseStatement] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_DIAGNOSTICREPORT}
  CODES_TSearchParamsDiagnosticReport : Array[TSearchParamsDiagnosticReport] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}', 'assessed-condition {http://hl7.org/fhir/SearchParameter/diagnosticreport-genetic-DiagnosticReport-assessed-condition}');
{$ENDIF}
{$IFDEF FHIR_DOCUMENTMANIFEST}
  CODES_TSearchParamsDocumentManifest : Array[TSearchParamsDocumentManifest] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_DOCUMENTREFERENCE}
  CODES_TSearchParamsDocumentReference : Array[TSearchParamsDocumentReference] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_ENCOUNTER}
  CODES_TSearchParamsEncounter : Array[TSearchParamsEncounter] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_ENDPOINT}
  CODES_TSearchParamsEndpoint : Array[TSearchParamsEndpoint] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_ENROLLMENTREQUEST}
  CODES_TSearchParamsEnrollmentRequest : Array[TSearchParamsEnrollmentRequest] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_ENROLLMENTRESPONSE}
  CODES_TSearchParamsEnrollmentResponse : Array[TSearchParamsEnrollmentResponse] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_EPISODEOFCARE}
  CODES_TSearchParamsEpisodeOfCare : Array[TSearchParamsEpisodeOfCare] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_EVENTDEFINITION}
  CODES_TSearchParamsEventDefinition : Array[TSearchParamsEventDefinition] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_EVIDENCE}
  CODES_TSearchParamsEvidence : Array[TSearchParamsEvidence] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_EVIDENCEREPORT}
  CODES_TSearchParamsEvidenceReport : Array[TSearchParamsEvidenceReport] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_EVIDENCEVARIABLE}
  CODES_TSearchParamsEvidenceVariable : Array[TSearchParamsEvidenceVariable] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_EXAMPLESCENARIO}
  CODES_TSearchParamsExampleScenario : Array[TSearchParamsExampleScenario] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_EXPLANATIONOFBENEFIT}
  CODES_TSearchParamsExplanationOfBenefit : Array[TSearchParamsExplanationOfBenefit] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_FAMILYMEMBERHISTORY}
  CODES_TSearchParamsFamilyMemberHistory : Array[TSearchParamsFamilyMemberHistory] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_FLAG}
  CODES_TSearchParamsFlag : Array[TSearchParamsFlag] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_GOAL}
  CODES_TSearchParamsGoal : Array[TSearchParamsGoal] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_GRAPHDEFINITION}
  CODES_TSearchParamsGraphDefinition : Array[TSearchParamsGraphDefinition] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_GROUP}
  CODES_TSearchParamsGroup : Array[TSearchParamsGroup] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_GUIDANCERESPONSE}
  CODES_TSearchParamsGuidanceResponse : Array[TSearchParamsGuidanceResponse] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_HEALTHCARESERVICE}
  CODES_TSearchParamsHealthcareService : Array[TSearchParamsHealthcareService] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_IMAGINGSTUDY}
  CODES_TSearchParamsImagingStudy : Array[TSearchParamsImagingStudy] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_IMMUNIZATION}
  CODES_TSearchParamsImmunization : Array[TSearchParamsImmunization] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_IMMUNIZATIONEVALUATION}
  CODES_TSearchParamsImmunizationEvaluation : Array[TSearchParamsImmunizationEvaluation] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_IMMUNIZATIONRECOMMENDATION}
  CODES_TSearchParamsImmunizationRecommendation : Array[TSearchParamsImmunizationRecommendation] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_IMPLEMENTATIONGUIDE}
  CODES_TSearchParamsImplementationGuide : Array[TSearchParamsImplementationGuide] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_INGREDIENT}
  CODES_TSearchParamsIngredient : Array[TSearchParamsIngredient] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_INSURANCEPLAN}
  CODES_TSearchParamsInsurancePlan : Array[TSearchParamsInsurancePlan] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_INVOICE}
  CODES_TSearchParamsInvoice : Array[TSearchParamsInvoice] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_LIBRARY}
  CODES_TSearchParamsLibrary : Array[TSearchParamsLibrary] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_LINKAGE}
  CODES_TSearchParamsLinkage : Array[TSearchParamsLinkage] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_LIST}
  CODES_TSearchParamsList : Array[TSearchParamsList] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_LOCATION}
  CODES_TSearchParamsLocation : Array[TSearchParamsLocation] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_MANUFACTUREDITEMDEFINITION}
  CODES_TSearchParamsManufacturedItemDefinition : Array[TSearchParamsManufacturedItemDefinition] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_MEASURE}
  CODES_TSearchParamsMeasure : Array[TSearchParamsMeasure] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_MEASUREREPORT}
  CODES_TSearchParamsMeasureReport : Array[TSearchParamsMeasureReport] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_MEDIA}
  CODES_TSearchParamsMedia : Array[TSearchParamsMedia] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_MEDICATION}
  CODES_TSearchParamsMedication : Array[TSearchParamsMedication] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_MEDICATIONADMINISTRATION}
  CODES_TSearchParamsMedicationAdministration : Array[TSearchParamsMedicationAdministration] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_MEDICATIONDISPENSE}
  CODES_TSearchParamsMedicationDispense : Array[TSearchParamsMedicationDispense] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_MEDICATIONKNOWLEDGE}
  CODES_TSearchParamsMedicationKnowledge : Array[TSearchParamsMedicationKnowledge] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_MEDICATIONREQUEST}
  CODES_TSearchParamsMedicationRequest : Array[TSearchParamsMedicationRequest] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_MEDICATIONSTATEMENT}
  CODES_TSearchParamsMedicationStatement : Array[TSearchParamsMedicationStatement] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_MEDICINALPRODUCTDEFINITION}
  CODES_TSearchParamsMedicinalProductDefinition : Array[TSearchParamsMedicinalProductDefinition] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_MESSAGEDEFINITION}
  CODES_TSearchParamsMessageDefinition : Array[TSearchParamsMessageDefinition] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_MESSAGEHEADER}
  CODES_TSearchParamsMessageHeader : Array[TSearchParamsMessageHeader] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_MOLECULARSEQUENCE}
  CODES_TSearchParamsMolecularSequence : Array[TSearchParamsMolecularSequence] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_NAMINGSYSTEM}
  CODES_TSearchParamsNamingSystem : Array[TSearchParamsNamingSystem] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_NUTRITIONORDER}
  CODES_TSearchParamsNutritionOrder : Array[TSearchParamsNutritionOrder] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_NUTRITIONPRODUCT}
  CODES_TSearchParamsNutritionProduct : Array[TSearchParamsNutritionProduct] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_OBSERVATION}
  CODES_TSearchParamsObservation : Array[TSearchParamsObservation] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}', 'amino-acid-change {http://hl7.org/fhir/SearchParameter/observation-genetic-Observation-amino-acid-change}', 'dna-variant {http://hl7.org/fhir/SearchParameter/observation-genetic-Observation-dna-variant}', 'gene-amino-acid-change {http://hl7.org/fhir/SearchParameter/observation-genetic-Observation-gene-amino-acid-change}', 'gene-dnavariant {http://hl7.org/fhir/SearchParameter/observation-genetic-Observation-gene-dnavariant}', 'gene-identifier {http://hl7.org/fhir/SearchParameter/observation-genetic-Observation-gene-identifier}');
{$ENDIF}
{$IFDEF FHIR_OBSERVATIONDEFINITION}
  CODES_TSearchParamsObservationDefinition : Array[TSearchParamsObservationDefinition] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_OPERATIONDEFINITION}
  CODES_TSearchParamsOperationDefinition : Array[TSearchParamsOperationDefinition] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_OPERATIONOUTCOME}
  CODES_TSearchParamsOperationOutcome : Array[TSearchParamsOperationOutcome] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_ORGANIZATION}
  CODES_TSearchParamsOrganization : Array[TSearchParamsOrganization] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_ORGANIZATIONAFFILIATION}
  CODES_TSearchParamsOrganizationAffiliation : Array[TSearchParamsOrganizationAffiliation] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_PACKAGEDPRODUCTDEFINITION}
  CODES_TSearchParamsPackagedProductDefinition : Array[TSearchParamsPackagedProductDefinition] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_PARAMETERS}
  CODES_TSearchParamsParameters : Array[TSearchParamsParameters] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_PATIENT}
  CODES_TSearchParamsPatient : Array[TSearchParamsPatient] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}', 'age {http://hl7.org/fhir/SearchParameter/patient-extensions-Patient-age}', 'birthOrderBoolean {http://hl7.org/fhir/SearchParameter/patient-extensions-Patient-birthOrderBoolean}', 'mothersMaidenName {http://hl7.org/fhir/SearchParameter/patient-extensions-Patient-mothersMaidenName}');
{$ENDIF}
{$IFDEF FHIR_PAYMENTNOTICE}
  CODES_TSearchParamsPaymentNotice : Array[TSearchParamsPaymentNotice] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_PAYMENTRECONCILIATION}
  CODES_TSearchParamsPaymentReconciliation : Array[TSearchParamsPaymentReconciliation] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_PERSON}
  CODES_TSearchParamsPerson : Array[TSearchParamsPerson] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_PLANDEFINITION}
  CODES_TSearchParamsPlanDefinition : Array[TSearchParamsPlanDefinition] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_PRACTITIONER}
  CODES_TSearchParamsPractitioner : Array[TSearchParamsPractitioner] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_PRACTITIONERROLE}
  CODES_TSearchParamsPractitionerRole : Array[TSearchParamsPractitionerRole] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_PROCEDURE}
  CODES_TSearchParamsProcedure : Array[TSearchParamsProcedure] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_PROVENANCE}
  CODES_TSearchParamsProvenance : Array[TSearchParamsProvenance] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_QUESTIONNAIRE}
  CODES_TSearchParamsQuestionnaire : Array[TSearchParamsQuestionnaire] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_QUESTIONNAIRERESPONSE}
  CODES_TSearchParamsQuestionnaireResponse : Array[TSearchParamsQuestionnaireResponse] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}', 'item-subject {http://hl7.org/fhir/SearchParameter/questionnaireresponse-extensions-QuestionnaireResponse-item-subject}');
{$ENDIF}
{$IFDEF FHIR_REGULATEDAUTHORIZATION}
  CODES_TSearchParamsRegulatedAuthorization : Array[TSearchParamsRegulatedAuthorization] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_RELATEDPERSON}
  CODES_TSearchParamsRelatedPerson : Array[TSearchParamsRelatedPerson] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_REQUESTGROUP}
  CODES_TSearchParamsRequestGroup : Array[TSearchParamsRequestGroup] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_RESEARCHDEFINITION}
  CODES_TSearchParamsResearchDefinition : Array[TSearchParamsResearchDefinition] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_RESEARCHELEMENTDEFINITION}
  CODES_TSearchParamsResearchElementDefinition : Array[TSearchParamsResearchElementDefinition] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_RESEARCHSTUDY}
  CODES_TSearchParamsResearchStudy : Array[TSearchParamsResearchStudy] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_RESEARCHSUBJECT}
  CODES_TSearchParamsResearchSubject : Array[TSearchParamsResearchSubject] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_RISKASSESSMENT}
  CODES_TSearchParamsRiskAssessment : Array[TSearchParamsRiskAssessment] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_SCHEDULE}
  CODES_TSearchParamsSchedule : Array[TSearchParamsSchedule] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_SEARCHPARAMETER}
  CODES_TSearchParamsSearchParameter : Array[TSearchParamsSearchParameter] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_SERVICEREQUEST}
  CODES_TSearchParamsServiceRequest : Array[TSearchParamsServiceRequest] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_SLOT}
  CODES_TSearchParamsSlot : Array[TSearchParamsSlot] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_SPECIMEN}
  CODES_TSearchParamsSpecimen : Array[TSearchParamsSpecimen] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_SPECIMENDEFINITION}
  CODES_TSearchParamsSpecimenDefinition : Array[TSearchParamsSpecimenDefinition] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_STRUCTUREDEFINITION}
  CODES_TSearchParamsStructureDefinition : Array[TSearchParamsStructureDefinition] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_STRUCTUREMAP}
  CODES_TSearchParamsStructureMap : Array[TSearchParamsStructureMap] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_SUBSCRIPTION}
  CODES_TSearchParamsSubscription : Array[TSearchParamsSubscription] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_SUBSCRIPTIONSTATUS}
  CODES_TSearchParamsSubscriptionStatus : Array[TSearchParamsSubscriptionStatus] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_SUBSCRIPTIONTOPIC}
  CODES_TSearchParamsSubscriptionTopic : Array[TSearchParamsSubscriptionTopic] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_SUBSTANCE}
  CODES_TSearchParamsSubstance : Array[TSearchParamsSubstance] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_SUBSTANCEDEFINITION}
  CODES_TSearchParamsSubstanceDefinition : Array[TSearchParamsSubstanceDefinition] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_SUPPLYDELIVERY}
  CODES_TSearchParamsSupplyDelivery : Array[TSearchParamsSupplyDelivery] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_SUPPLYREQUEST}
  CODES_TSearchParamsSupplyRequest : Array[TSearchParamsSupplyRequest] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_TASK}
  CODES_TSearchParamsTask : Array[TSearchParamsTask] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_TERMINOLOGYCAPABILITIES}
  CODES_TSearchParamsTerminologyCapabilities : Array[TSearchParamsTerminologyCapabilities] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_TESTREPORT}
  CODES_TSearchParamsTestReport : Array[TSearchParamsTestReport] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_TESTSCRIPT}
  CODES_TSearchParamsTestScript : Array[TSearchParamsTestScript] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_VALUESET}
  CODES_TSearchParamsValueSet : Array[TSearchParamsValueSet] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_VERIFICATIONRESULT}
  CODES_TSearchParamsVerificationResult : Array[TSearchParamsVerificationResult] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}
{$IFDEF FHIR_VISIONPRESCRIPTION}
  CODES_TSearchParamsVisionPrescription : Array[TSearchParamsVisionPrescription] of String = ('_filter {http://hl7.org/fhir/SearchParameter/filter}');
{$ENDIF}


implementation

end.

