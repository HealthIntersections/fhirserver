unit fhir5_constants;

{$I fhir5.inc}

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

interface

// Generated on Fri, Aug 21, 2020 11:27+1000 for FHIR v4.5.0

uses
  SysUtils, Classes,
  fsl_utilities, fsl_stream,
  fhir_objects, fhir5_types, fhir5_resources, fhir5_resources_base;

const
  currentFHIRVersionRelease = fhirVersionRelease5;
  FHIR_GENERATED_VERSION = '4.5.0';
  FHIR_GENERATED_VERSION_BASE = '4.5';
  FHIR_GENERATED_PUBLICATION = '5';
  FHIR_GENERATED_DATE = 'Fri, Aug 21, 2020 11:27+1000';

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
    {$IFDEF FHIR_CAPABILITYSTATEMENT2} 'CapabilityStatement2', {$ENDIF}
    {$IFDEF FHIR_CAREPLAN} 'CarePlan', {$ENDIF}
    {$IFDEF FHIR_CARETEAM} 'CareTeam', {$ENDIF}
    {$IFDEF FHIR_CATALOGENTRY} 'CatalogEntry', {$ENDIF}
    {$IFDEF FHIR_CHARGEITEM} 'ChargeItem', {$ENDIF}
    {$IFDEF FHIR_CHARGEITEMDEFINITION} 'ChargeItemDefinition', {$ENDIF}
    {$IFDEF FHIR_CITATION} 'Citation', {$ENDIF}
    {$IFDEF FHIR_CLAIM} 'Claim', {$ENDIF}
    {$IFDEF FHIR_CLAIMRESPONSE} 'ClaimResponse', {$ENDIF}
    {$IFDEF FHIR_CLINICALIMPRESSION} 'ClinicalImpression', {$ENDIF}
    {$IFDEF FHIR_CLINICALUSEISSUE} 'ClinicalUseIssue', {$ENDIF}
    {$IFDEF FHIR_CODESYSTEM} 'CodeSystem', {$ENDIF}
    {$IFDEF FHIR_COMMUNICATION} 'Communication', {$ENDIF}
    {$IFDEF FHIR_COMMUNICATIONREQUEST} 'CommunicationRequest', {$ENDIF}
    {$IFDEF FHIR_COMPARTMENTDEFINITION} 'CompartmentDefinition', {$ENDIF}
    {$IFDEF FHIR_COMPOSITION} 'Composition', {$ENDIF}
    {$IFDEF FHIR_CONCEPTMAP} 'ConceptMap', {$ENDIF}
    {$IFDEF FHIR_CONDITION} 'Condition', {$ENDIF}
    {$IFDEF FHIR_CONDITIONDEFINITION} 'ConditionDefinition', {$ENDIF}
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
    {$IFDEF FHIR_MEDICATION} 'Medication', {$ENDIF}
    {$IFDEF FHIR_MEDICATIONADMINISTRATION} 'MedicationAdministration', {$ENDIF}
    {$IFDEF FHIR_MEDICATIONDISPENSE} 'MedicationDispense', {$ENDIF}
    {$IFDEF FHIR_MEDICATIONKNOWLEDGE} 'MedicationKnowledge', {$ENDIF}
    {$IFDEF FHIR_MEDICATIONREQUEST} 'MedicationRequest', {$ENDIF}
    {$IFDEF FHIR_MEDICATIONUSAGE} 'MedicationUsage', {$ENDIF}
    {$IFDEF FHIR_MEDICINALPRODUCTDEFINITION} 'MedicinalProductDefinition', {$ENDIF}
    {$IFDEF FHIR_MESSAGEDEFINITION} 'MessageDefinition', {$ENDIF}
    {$IFDEF FHIR_MESSAGEHEADER} 'MessageHeader', {$ENDIF}
    {$IFDEF FHIR_MOLECULARSEQUENCE} 'MolecularSequence', {$ENDIF}
    {$IFDEF FHIR_NAMINGSYSTEM} 'NamingSystem', {$ENDIF}
    {$IFDEF FHIR_NUTRITIONINTAKE} 'NutritionIntake', {$ENDIF}
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
    {$IFDEF FHIR_PERMISSION} 'Permission', {$ENDIF}
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
    {$IFDEF FHIR_SUBSTANCENUCLEICACID} 'SubstanceNucleicAcid', {$ENDIF}
    {$IFDEF FHIR_SUBSTANCEPOLYMER} 'SubstancePolymer', {$ENDIF}
    {$IFDEF FHIR_SUBSTANCEPROTEIN} 'SubstanceProtein', {$ENDIF}
    {$IFDEF FHIR_SUBSTANCEREFERENCEINFORMATION} 'SubstanceReferenceInformation', {$ENDIF}
    {$IFDEF FHIR_SUBSTANCESOURCEMATERIAL} 'SubstanceSourceMaterial', {$ENDIF}
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
    {$IFDEF FHIR_CAPABILITYSTATEMENT2} 'capabilitystatement2', {$ENDIF}
    {$IFDEF FHIR_CAREPLAN} 'careplan', {$ENDIF}
    {$IFDEF FHIR_CARETEAM} 'careteam', {$ENDIF}
    {$IFDEF FHIR_CATALOGENTRY} 'catalogentry', {$ENDIF}
    {$IFDEF FHIR_CHARGEITEM} 'chargeitem', {$ENDIF}
    {$IFDEF FHIR_CHARGEITEMDEFINITION} 'chargeitemdefinition', {$ENDIF}
    {$IFDEF FHIR_CITATION} 'citation', {$ENDIF}
    {$IFDEF FHIR_CLAIM} 'claim', {$ENDIF}
    {$IFDEF FHIR_CLAIMRESPONSE} 'claimresponse', {$ENDIF}
    {$IFDEF FHIR_CLINICALIMPRESSION} 'clinicalimpression', {$ENDIF}
    {$IFDEF FHIR_CLINICALUSEISSUE} 'clinicaluseissue', {$ENDIF}
    {$IFDEF FHIR_CODESYSTEM} 'codesystem', {$ENDIF}
    {$IFDEF FHIR_COMMUNICATION} 'communication', {$ENDIF}
    {$IFDEF FHIR_COMMUNICATIONREQUEST} 'communicationrequest', {$ENDIF}
    {$IFDEF FHIR_COMPARTMENTDEFINITION} 'compartmentdefinition', {$ENDIF}
    {$IFDEF FHIR_COMPOSITION} 'composition', {$ENDIF}
    {$IFDEF FHIR_CONCEPTMAP} 'conceptmap', {$ENDIF}
    {$IFDEF FHIR_CONDITION} 'condition', {$ENDIF}
    {$IFDEF FHIR_CONDITIONDEFINITION} 'conditiondefinition', {$ENDIF}
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
    {$IFDEF FHIR_MEDICATION} 'medication', {$ENDIF}
    {$IFDEF FHIR_MEDICATIONADMINISTRATION} 'medicationadministration', {$ENDIF}
    {$IFDEF FHIR_MEDICATIONDISPENSE} 'medicationdispense', {$ENDIF}
    {$IFDEF FHIR_MEDICATIONKNOWLEDGE} 'medicationknowledge', {$ENDIF}
    {$IFDEF FHIR_MEDICATIONREQUEST} 'medicationrequest', {$ENDIF}
    {$IFDEF FHIR_MEDICATIONUSAGE} 'medicationusage', {$ENDIF}
    {$IFDEF FHIR_MEDICINALPRODUCTDEFINITION} 'medicinalproductdefinition', {$ENDIF}
    {$IFDEF FHIR_MESSAGEDEFINITION} 'messagedefinition', {$ENDIF}
    {$IFDEF FHIR_MESSAGEHEADER} 'messageheader', {$ENDIF}
    {$IFDEF FHIR_MOLECULARSEQUENCE} 'molecularsequence', {$ENDIF}
    {$IFDEF FHIR_NAMINGSYSTEM} 'namingsystem', {$ENDIF}
    {$IFDEF FHIR_NUTRITIONINTAKE} 'nutritionintake', {$ENDIF}
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
    {$IFDEF FHIR_PERMISSION} 'permission', {$ENDIF}
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
    {$IFDEF FHIR_SUBSTANCENUCLEICACID} 'substancenucleicacid', {$ENDIF}
    {$IFDEF FHIR_SUBSTANCEPOLYMER} 'substancepolymer', {$ENDIF}
    {$IFDEF FHIR_SUBSTANCEPROTEIN} 'substanceprotein', {$ENDIF}
    {$IFDEF FHIR_SUBSTANCEREFERENCEINFORMATION} 'substancereferenceinformation', {$ENDIF}
    {$IFDEF FHIR_SUBSTANCESOURCEMATERIAL} 'substancesourcematerial', {$ENDIF}
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
    {$IFDEF FHIR_CAPABILITYSTATEMENT2} TFhirCapabilityStatement2, {$ENDIF}
    {$IFDEF FHIR_CAREPLAN} TFhirCarePlan, {$ENDIF}
    {$IFDEF FHIR_CARETEAM} TFhirCareTeam, {$ENDIF}
    {$IFDEF FHIR_CATALOGENTRY} TFhirCatalogEntry, {$ENDIF}
    {$IFDEF FHIR_CHARGEITEM} TFhirChargeItem, {$ENDIF}
    {$IFDEF FHIR_CHARGEITEMDEFINITION} TFhirChargeItemDefinition, {$ENDIF}
    {$IFDEF FHIR_CITATION} TFhirCitation, {$ENDIF}
    {$IFDEF FHIR_CLAIM} TFhirClaim, {$ENDIF}
    {$IFDEF FHIR_CLAIMRESPONSE} TFhirClaimResponse, {$ENDIF}
    {$IFDEF FHIR_CLINICALIMPRESSION} TFhirClinicalImpression, {$ENDIF}
    {$IFDEF FHIR_CLINICALUSEISSUE} TFhirClinicalUseIssue, {$ENDIF}
    {$IFDEF FHIR_CODESYSTEM} TFhirCodeSystem, {$ENDIF}
    {$IFDEF FHIR_COMMUNICATION} TFhirCommunication, {$ENDIF}
    {$IFDEF FHIR_COMMUNICATIONREQUEST} TFhirCommunicationRequest, {$ENDIF}
    {$IFDEF FHIR_COMPARTMENTDEFINITION} TFhirCompartmentDefinition, {$ENDIF}
    {$IFDEF FHIR_COMPOSITION} TFhirComposition, {$ENDIF}
    {$IFDEF FHIR_CONCEPTMAP} TFhirConceptMap, {$ENDIF}
    {$IFDEF FHIR_CONDITION} TFhirCondition, {$ENDIF}
    {$IFDEF FHIR_CONDITIONDEFINITION} TFhirConditionDefinition, {$ENDIF}
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
    {$IFDEF FHIR_MEDICATION} TFhirMedication, {$ENDIF}
    {$IFDEF FHIR_MEDICATIONADMINISTRATION} TFhirMedicationAdministration, {$ENDIF}
    {$IFDEF FHIR_MEDICATIONDISPENSE} TFhirMedicationDispense, {$ENDIF}
    {$IFDEF FHIR_MEDICATIONKNOWLEDGE} TFhirMedicationKnowledge, {$ENDIF}
    {$IFDEF FHIR_MEDICATIONREQUEST} TFhirMedicationRequest, {$ENDIF}
    {$IFDEF FHIR_MEDICATIONUSAGE} TFhirMedicationUsage, {$ENDIF}
    {$IFDEF FHIR_MEDICINALPRODUCTDEFINITION} TFhirMedicinalProductDefinition, {$ENDIF}
    {$IFDEF FHIR_MESSAGEDEFINITION} TFhirMessageDefinition, {$ENDIF}
    {$IFDEF FHIR_MESSAGEHEADER} TFhirMessageHeader, {$ENDIF}
    {$IFDEF FHIR_MOLECULARSEQUENCE} TFhirMolecularSequence, {$ENDIF}
    {$IFDEF FHIR_NAMINGSYSTEM} TFhirNamingSystem, {$ENDIF}
    {$IFDEF FHIR_NUTRITIONINTAKE} TFhirNutritionIntake, {$ENDIF}
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
    {$IFDEF FHIR_PERMISSION} TFhirPermission, {$ENDIF}
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
    {$IFDEF FHIR_SUBSTANCENUCLEICACID} TFhirSubstanceNucleicAcid, {$ENDIF}
    {$IFDEF FHIR_SUBSTANCEPOLYMER} TFhirSubstancePolymer, {$ENDIF}
    {$IFDEF FHIR_SUBSTANCEPROTEIN} TFhirSubstanceProtein, {$ENDIF}
    {$IFDEF FHIR_SUBSTANCEREFERENCEINFORMATION} TFhirSubstanceReferenceInformation, {$ENDIF}
    {$IFDEF FHIR_SUBSTANCESOURCEMATERIAL} TFhirSubstanceSourceMaterial, {$ENDIF}
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
    {$IFDEF FHIR_CAPABILITYSTATEMENT2} frtCapabilityStatement2, {$ENDIF}
    {$IFDEF FHIR_CAREPLAN} frtCarePlan, {$ENDIF}
    {$IFDEF FHIR_CARETEAM} frtCareTeam, {$ENDIF}
    {$IFDEF FHIR_CATALOGENTRY} frtCatalogEntry, {$ENDIF}
    {$IFDEF FHIR_CHARGEITEM} frtChargeItem, {$ENDIF}
    {$IFDEF FHIR_CHARGEITEMDEFINITION} frtChargeItemDefinition, {$ENDIF}
    {$IFDEF FHIR_CITATION} frtCitation, {$ENDIF}
    {$IFDEF FHIR_CLAIM} frtClaim, {$ENDIF}
    {$IFDEF FHIR_CLAIMRESPONSE} frtClaimResponse, {$ENDIF}
    {$IFDEF FHIR_CLINICALIMPRESSION} frtClinicalImpression, {$ENDIF}
    {$IFDEF FHIR_CLINICALUSEISSUE} frtClinicalUseIssue, {$ENDIF}
    {$IFDEF FHIR_CODESYSTEM} frtCodeSystem, {$ENDIF}
    {$IFDEF FHIR_COMMUNICATION} frtCommunication, {$ENDIF}
    {$IFDEF FHIR_COMMUNICATIONREQUEST} frtCommunicationRequest, {$ENDIF}
    {$IFDEF FHIR_COMPARTMENTDEFINITION} frtCompartmentDefinition, {$ENDIF}
    {$IFDEF FHIR_COMPOSITION} frtComposition, {$ENDIF}
    {$IFDEF FHIR_CONCEPTMAP} frtConceptMap, {$ENDIF}
    {$IFDEF FHIR_CONDITION} frtCondition, {$ENDIF}
    {$IFDEF FHIR_CONDITIONDEFINITION} frtConditionDefinition, {$ENDIF}
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
    {$IFDEF FHIR_MEDICATION} frtMedication, {$ENDIF}
    {$IFDEF FHIR_MEDICATIONADMINISTRATION} frtMedicationAdministration, {$ENDIF}
    {$IFDEF FHIR_MEDICATIONDISPENSE} frtMedicationDispense, {$ENDIF}
    {$IFDEF FHIR_MEDICATIONKNOWLEDGE} frtMedicationKnowledge, {$ENDIF}
    {$IFDEF FHIR_MEDICATIONREQUEST} frtMedicationRequest, {$ENDIF}
    {$IFDEF FHIR_MEDICATIONUSAGE} frtMedicationUsage, {$ENDIF}
    {$IFDEF FHIR_MEDICINALPRODUCTDEFINITION} frtMedicinalProductDefinition, {$ENDIF}
    {$IFDEF FHIR_MESSAGEDEFINITION} frtMessageDefinition, {$ENDIF}
    {$IFDEF FHIR_MESSAGEHEADER} frtMessageHeader, {$ENDIF}
    {$IFDEF FHIR_MOLECULARSEQUENCE} frtMolecularSequence, {$ENDIF}
    {$IFDEF FHIR_NAMINGSYSTEM} frtNamingSystem, {$ENDIF}
    {$IFDEF FHIR_NUTRITIONINTAKE} frtNutritionIntake, {$ENDIF}
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
    {$IFDEF FHIR_PERMISSION} frtPermission, {$ENDIF}
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
    {$IFDEF FHIR_SUBSTANCENUCLEICACID} frtSubstanceNucleicAcid, {$ENDIF}
    {$IFDEF FHIR_SUBSTANCEPOLYMER} frtSubstancePolymer, {$ENDIF}
    {$IFDEF FHIR_SUBSTANCEPROTEIN} frtSubstanceProtein, {$ENDIF}
    {$IFDEF FHIR_SUBSTANCEREFERENCEINFORMATION} frtSubstanceReferenceInformation, {$ENDIF}
    {$IFDEF FHIR_SUBSTANCESOURCEMATERIAL} frtSubstanceSourceMaterial, {$ENDIF}
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
    {$IFDEF FHIR_CAPABILITYSTATEMENT2} 'CapabilityStatement2', {$ENDIF}
    {$IFDEF FHIR_CAREPLAN} 'CarePlan', {$ENDIF}
    {$IFDEF FHIR_CARETEAM} 'CareTeam', {$ENDIF}
    {$IFDEF FHIR_CATALOGENTRY} 'CatalogEntry', {$ENDIF}
    {$IFDEF FHIR_CHARGEITEM} 'ChargeItem', {$ENDIF}
    {$IFDEF FHIR_CHARGEITEMDEFINITION} 'ChargeItemDefinition', {$ENDIF}
    {$IFDEF FHIR_CITATION} 'Citation', {$ENDIF}
    {$IFDEF FHIR_CLAIM} 'Claim', {$ENDIF}
    {$IFDEF FHIR_CLAIMRESPONSE} 'ClaimResponse', {$ENDIF}
    {$IFDEF FHIR_CLINICALIMPRESSION} 'ClinicalImpression', {$ENDIF}
    {$IFDEF FHIR_CLINICALUSEISSUE} 'ClinicalUseIssue', {$ENDIF}
    {$IFDEF FHIR_CODESYSTEM} 'CodeSystem', {$ENDIF}
    {$IFDEF FHIR_COMMUNICATION} 'Communication', {$ENDIF}
    {$IFDEF FHIR_COMMUNICATIONREQUEST} 'CommunicationRequest', {$ENDIF}
    {$IFDEF FHIR_COMPARTMENTDEFINITION} 'CompartmentDefinition', {$ENDIF}
    {$IFDEF FHIR_COMPOSITION} 'Composition', {$ENDIF}
    {$IFDEF FHIR_CONCEPTMAP} 'ConceptMap', {$ENDIF}
    {$IFDEF FHIR_CONDITION} 'Condition', {$ENDIF}
    {$IFDEF FHIR_CONDITIONDEFINITION} 'ConditionDefinition', {$ENDIF}
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
    {$IFDEF FHIR_MEDICATION} 'Medication', {$ENDIF}
    {$IFDEF FHIR_MEDICATIONADMINISTRATION} 'MedicationAdministration', {$ENDIF}
    {$IFDEF FHIR_MEDICATIONDISPENSE} 'MedicationDispense', {$ENDIF}
    {$IFDEF FHIR_MEDICATIONKNOWLEDGE} 'MedicationKnowledge', {$ENDIF}
    {$IFDEF FHIR_MEDICATIONREQUEST} 'MedicationRequest', {$ENDIF}
    {$IFDEF FHIR_MEDICATIONUSAGE} 'MedicationUsage', {$ENDIF}
    {$IFDEF FHIR_MEDICINALPRODUCTDEFINITION} 'MedicinalProductDefinition', {$ENDIF}
    {$IFDEF FHIR_MESSAGEDEFINITION} 'MessageDefinition', {$ENDIF}
    {$IFDEF FHIR_MESSAGEHEADER} 'MessageHeader', {$ENDIF}
    {$IFDEF FHIR_MOLECULARSEQUENCE} 'MolecularSequence', {$ENDIF}
    {$IFDEF FHIR_NAMINGSYSTEM} 'NamingSystem', {$ENDIF}
    {$IFDEF FHIR_NUTRITIONINTAKE} 'NutritionIntake', {$ENDIF}
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
    {$IFDEF FHIR_PERMISSION} 'Permission', {$ENDIF}
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
    {$IFDEF FHIR_SUBSTANCENUCLEICACID} 'SubstanceNucleicAcid', {$ENDIF}
    {$IFDEF FHIR_SUBSTANCEPOLYMER} 'SubstancePolymer', {$ENDIF}
    {$IFDEF FHIR_SUBSTANCEPROTEIN} 'SubstanceProtein', {$ENDIF}
    {$IFDEF FHIR_SUBSTANCEREFERENCEINFORMATION} 'SubstanceReferenceInformation', {$ENDIF}
    {$IFDEF FHIR_SUBSTANCESOURCEMATERIAL} 'SubstanceSourceMaterial', {$ENDIF}
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
    spAccount__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spAccount__filter {http://hl7.org/fhir/SearchParameter/filter},
    spAccount__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spAccount__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spAccount__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spAccount__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spAccount__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spAccount__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spAccount__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spAccount__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spAccount_Identifier {http://hl7.org/fhir/SearchParameter/Accountidentifier},
    spAccount_Name {http://hl7.org/fhir/SearchParameter/Accountname},
    spAccount_Owner {http://hl7.org/fhir/SearchParameter/Accountowner},
    spAccount_Patient {http://hl7.org/fhir/SearchParameter/Accountpatient},
    spAccount_Period {http://hl7.org/fhir/SearchParameter/Accountperiod},
    spAccount_Status {http://hl7.org/fhir/SearchParameter/Accountstatus},
    spAccount_Subject {http://hl7.org/fhir/SearchParameter/Accountsubject},
    spAccount_Type {http://hl7.org/fhir/SearchParameter/Accounttype});
{$ENDIF FHIR_ACCOUNT}

{$IFDEF FHIR_ACTIVITYDEFINITION}
  // Search Parameters for ActivityDefinition
  TSearchParamsActivityDefinition = (
    spActivityDefinition__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spActivityDefinition__filter {http://hl7.org/fhir/SearchParameter/filter},
    spActivityDefinition__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spActivityDefinition__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spActivityDefinition__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spActivityDefinition__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spActivityDefinition__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spActivityDefinition__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spActivityDefinition__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spActivityDefinition__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spActivityDefinition_Composedof {http://hl7.org/fhir/SearchParameter/ActivityDefinitioncomposedof},
    spActivityDefinition_Context {http://hl7.org/fhir/SearchParameter/ActivityDefinitioncontext},
    spActivityDefinition_Contextquantity {http://hl7.org/fhir/SearchParameter/ActivityDefinitioncontextquantity},
    spActivityDefinition_Contexttype {http://hl7.org/fhir/SearchParameter/ActivityDefinitioncontexttype},
    spActivityDefinition_Contexttypequantity {http://hl7.org/fhir/SearchParameter/ActivityDefinitioncontexttypequantity},
    spActivityDefinition_Contexttypevalue {http://hl7.org/fhir/SearchParameter/ActivityDefinitioncontexttypevalue},
    spActivityDefinition_Date {http://hl7.org/fhir/SearchParameter/ActivityDefinitiondate},
    spActivityDefinition_Dependson {http://hl7.org/fhir/SearchParameter/ActivityDefinitiondependson},
    spActivityDefinition_Derivedfrom {http://hl7.org/fhir/SearchParameter/ActivityDefinitionderivedfrom},
    spActivityDefinition_Description {http://hl7.org/fhir/SearchParameter/ActivityDefinitiondescription},
    spActivityDefinition_Effective {http://hl7.org/fhir/SearchParameter/ActivityDefinitioneffective},
    spActivityDefinition_Identifier {http://hl7.org/fhir/SearchParameter/ActivityDefinitionidentifier},
    spActivityDefinition_Jurisdiction {http://hl7.org/fhir/SearchParameter/ActivityDefinitionjurisdiction},
    spActivityDefinition_Name {http://hl7.org/fhir/SearchParameter/ActivityDefinitionname},
    spActivityDefinition_Predecessor {http://hl7.org/fhir/SearchParameter/ActivityDefinitionpredecessor},
    spActivityDefinition_Publisher {http://hl7.org/fhir/SearchParameter/ActivityDefinitionpublisher},
    spActivityDefinition_Status {http://hl7.org/fhir/SearchParameter/ActivityDefinitionstatus},
    spActivityDefinition_Successor {http://hl7.org/fhir/SearchParameter/ActivityDefinitionsuccessor},
    spActivityDefinition_Title {http://hl7.org/fhir/SearchParameter/ActivityDefinitiontitle},
    spActivityDefinition_Topic {http://hl7.org/fhir/SearchParameter/ActivityDefinitiontopic},
    spActivityDefinition_Url {http://hl7.org/fhir/SearchParameter/ActivityDefinitionurl},
    spActivityDefinition_Version {http://hl7.org/fhir/SearchParameter/ActivityDefinitionversion});
{$ENDIF FHIR_ACTIVITYDEFINITION}

{$IFDEF FHIR_ADMINISTRABLEPRODUCTDEFINITION}
  // Search Parameters for AdministrableProductDefinition
  TSearchParamsAdministrableProductDefinition = (
    spAdministrableProductDefinition__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spAdministrableProductDefinition__filter {http://hl7.org/fhir/SearchParameter/filter},
    spAdministrableProductDefinition__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spAdministrableProductDefinition__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spAdministrableProductDefinition__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spAdministrableProductDefinition__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spAdministrableProductDefinition__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spAdministrableProductDefinition__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spAdministrableProductDefinition__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spAdministrableProductDefinition__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spAdministrableProductDefinition_Device {http://hl7.org/fhir/SearchParameter/AdministrableProductDefinitiondevice},
    spAdministrableProductDefinition_Doseform {http://hl7.org/fhir/SearchParameter/AdministrableProductDefinitiondoseform},
    spAdministrableProductDefinition_Identifier {http://hl7.org/fhir/SearchParameter/AdministrableProductDefinitionidentifier},
    spAdministrableProductDefinition_Ingredient {http://hl7.org/fhir/SearchParameter/AdministrableProductDefinitioningredient},
    spAdministrableProductDefinition_Manufactureditem {http://hl7.org/fhir/SearchParameter/AdministrableProductDefinitionmanufactureditem},
    spAdministrableProductDefinition_Route {http://hl7.org/fhir/SearchParameter/AdministrableProductDefinitionroute},
    spAdministrableProductDefinition_Subject {http://hl7.org/fhir/SearchParameter/AdministrableProductDefinitionsubject},
    spAdministrableProductDefinition_Targetspecies {http://hl7.org/fhir/SearchParameter/AdministrableProductDefinitiontargetspecies});
{$ENDIF FHIR_ADMINISTRABLEPRODUCTDEFINITION}

{$IFDEF FHIR_ADVERSEEVENT}
  // Search Parameters for AdverseEvent
  TSearchParamsAdverseEvent = (
    spAdverseEvent__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spAdverseEvent__filter {http://hl7.org/fhir/SearchParameter/filter},
    spAdverseEvent__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spAdverseEvent__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spAdverseEvent__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spAdverseEvent__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spAdverseEvent__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spAdverseEvent__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spAdverseEvent__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spAdverseEvent__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spAdverseEvent_Actuality {http://hl7.org/fhir/SearchParameter/AdverseEventactuality},
    spAdverseEvent_Category {http://hl7.org/fhir/SearchParameter/AdverseEventcategory},
    spAdverseEvent_Code {http://hl7.org/fhir/SearchParameter/AdverseEventcode},
    spAdverseEvent_Date {http://hl7.org/fhir/SearchParameter/AdverseEventdate},
    spAdverseEvent_Identifier {http://hl7.org/fhir/SearchParameter/AdverseEventidentifier},
    spAdverseEvent_Location {http://hl7.org/fhir/SearchParameter/AdverseEventlocation},
    spAdverseEvent_Patient {http://hl7.org/fhir/SearchParameter/AdverseEventpatient},
    spAdverseEvent_Recorder {http://hl7.org/fhir/SearchParameter/AdverseEventrecorder},
    spAdverseEvent_Resultingcondition {http://hl7.org/fhir/SearchParameter/AdverseEventresultingcondition},
    spAdverseEvent_Seriousness {http://hl7.org/fhir/SearchParameter/AdverseEventseriousness},
    spAdverseEvent_Status {http://hl7.org/fhir/SearchParameter/AdverseEventstatus},
    spAdverseEvent_Study {http://hl7.org/fhir/SearchParameter/AdverseEventstudy},
    spAdverseEvent_Subject {http://hl7.org/fhir/SearchParameter/AdverseEventsubject},
    spAdverseEvent_Substance {http://hl7.org/fhir/SearchParameter/AdverseEventsubstance});
{$ENDIF FHIR_ADVERSEEVENT}

{$IFDEF FHIR_ALLERGYINTOLERANCE}
  // Search Parameters for AllergyIntolerance
  TSearchParamsAllergyIntolerance = (
    spAllergyIntolerance__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spAllergyIntolerance__filter {http://hl7.org/fhir/SearchParameter/filter},
    spAllergyIntolerance__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spAllergyIntolerance__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spAllergyIntolerance__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spAllergyIntolerance__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spAllergyIntolerance__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spAllergyIntolerance__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spAllergyIntolerance__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spAllergyIntolerance__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spAllergyIntolerance_Asserter {http://hl7.org/fhir/SearchParameter/AllergyIntoleranceasserter},
    spAllergyIntolerance_Category {http://hl7.org/fhir/SearchParameter/AllergyIntolerancecategory},
    spAllergyIntolerance_Clinicalstatus {http://hl7.org/fhir/SearchParameter/AllergyIntoleranceclinicalstatus},
    spAllergyIntolerance_Code {http://hl7.org/fhir/SearchParameter/clinicalcode},
    spAllergyIntolerance_Criticality {http://hl7.org/fhir/SearchParameter/AllergyIntolerancecriticality},
    spAllergyIntolerance_Date {http://hl7.org/fhir/SearchParameter/clinicaldate},
    spAllergyIntolerance_Identifier {http://hl7.org/fhir/SearchParameter/clinicalidentifier},
    spAllergyIntolerance_Lastdate {http://hl7.org/fhir/SearchParameter/AllergyIntolerancelastdate},
    spAllergyIntolerance_Manifestation {http://hl7.org/fhir/SearchParameter/AllergyIntolerancemanifestation},
    spAllergyIntolerance_Patient {http://hl7.org/fhir/SearchParameter/clinicalpatient},
    spAllergyIntolerance_Recorder {http://hl7.org/fhir/SearchParameter/AllergyIntolerancerecorder},
    spAllergyIntolerance_Route {http://hl7.org/fhir/SearchParameter/AllergyIntoleranceroute},
    spAllergyIntolerance_Severity {http://hl7.org/fhir/SearchParameter/AllergyIntoleranceseverity},
    spAllergyIntolerance_Type {http://hl7.org/fhir/SearchParameter/clinicaltype},
    spAllergyIntolerance_Verificationstatus {http://hl7.org/fhir/SearchParameter/AllergyIntoleranceverificationstatus});
{$ENDIF FHIR_ALLERGYINTOLERANCE}

{$IFDEF FHIR_APPOINTMENT}
  // Search Parameters for Appointment
  TSearchParamsAppointment = (
    spAppointment__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spAppointment__filter {http://hl7.org/fhir/SearchParameter/filter},
    spAppointment__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spAppointment__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spAppointment__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spAppointment__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spAppointment__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spAppointment__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spAppointment__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spAppointment__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spAppointment_Actor {http://hl7.org/fhir/SearchParameter/Appointmentactor},
    spAppointment_Appointmenttype {http://hl7.org/fhir/SearchParameter/Appointmentappointmenttype},
    spAppointment_Basedon {http://hl7.org/fhir/SearchParameter/Appointmentbasedon},
    spAppointment_Date {http://hl7.org/fhir/SearchParameter/Appointmentdate},
    spAppointment_Identifier {http://hl7.org/fhir/SearchParameter/Appointmentidentifier},
    spAppointment_Location {http://hl7.org/fhir/SearchParameter/Appointmentlocation},
    spAppointment_Partstatus {http://hl7.org/fhir/SearchParameter/Appointmentpartstatus},
    spAppointment_Patient {http://hl7.org/fhir/SearchParameter/Appointmentpatient},
    spAppointment_Practitioner {http://hl7.org/fhir/SearchParameter/Appointmentpractitioner},
    spAppointment_Reasoncode {http://hl7.org/fhir/SearchParameter/Appointmentreasoncode},
    spAppointment_Reasonreference {http://hl7.org/fhir/SearchParameter/Appointmentreasonreference},
    spAppointment_Servicecategory {http://hl7.org/fhir/SearchParameter/Appointmentservicecategory},
    spAppointment_Servicetype {http://hl7.org/fhir/SearchParameter/Appointmentservicetype},
    spAppointment_Slot {http://hl7.org/fhir/SearchParameter/Appointmentslot},
    spAppointment_Specialty {http://hl7.org/fhir/SearchParameter/Appointmentspecialty},
    spAppointment_Status {http://hl7.org/fhir/SearchParameter/Appointmentstatus},
    spAppointment_Supportinginfo {http://hl7.org/fhir/SearchParameter/Appointmentsupportinginfo});
{$ENDIF FHIR_APPOINTMENT}

{$IFDEF FHIR_APPOINTMENTRESPONSE}
  // Search Parameters for AppointmentResponse
  TSearchParamsAppointmentResponse = (
    spAppointmentResponse__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spAppointmentResponse__filter {http://hl7.org/fhir/SearchParameter/filter},
    spAppointmentResponse__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spAppointmentResponse__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spAppointmentResponse__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spAppointmentResponse__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spAppointmentResponse__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spAppointmentResponse__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spAppointmentResponse__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spAppointmentResponse__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spAppointmentResponse_Actor {http://hl7.org/fhir/SearchParameter/AppointmentResponseactor},
    spAppointmentResponse_Appointment {http://hl7.org/fhir/SearchParameter/AppointmentResponseappointment},
    spAppointmentResponse_Identifier {http://hl7.org/fhir/SearchParameter/AppointmentResponseidentifier},
    spAppointmentResponse_Location {http://hl7.org/fhir/SearchParameter/AppointmentResponselocation},
    spAppointmentResponse_Partstatus {http://hl7.org/fhir/SearchParameter/AppointmentResponsepartstatus},
    spAppointmentResponse_Patient {http://hl7.org/fhir/SearchParameter/AppointmentResponsepatient},
    spAppointmentResponse_Practitioner {http://hl7.org/fhir/SearchParameter/AppointmentResponsepractitioner});
{$ENDIF FHIR_APPOINTMENTRESPONSE}

{$IFDEF FHIR_AUDITEVENT}
  // Search Parameters for AuditEvent
  TSearchParamsAuditEvent = (
    spAuditEvent__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spAuditEvent__filter {http://hl7.org/fhir/SearchParameter/filter},
    spAuditEvent__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spAuditEvent__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spAuditEvent__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spAuditEvent__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spAuditEvent__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spAuditEvent__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spAuditEvent__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spAuditEvent__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spAuditEvent_Action {http://hl7.org/fhir/SearchParameter/AuditEventaction},
    spAuditEvent_Address {http://hl7.org/fhir/SearchParameter/AuditEventaddress},
    spAuditEvent_Agent {http://hl7.org/fhir/SearchParameter/AuditEventagent},
    spAuditEvent_Agentname {http://hl7.org/fhir/SearchParameter/AuditEventagentname},
    spAuditEvent_Agentrole {http://hl7.org/fhir/SearchParameter/AuditEventagentrole},
    spAuditEvent_Altid {http://hl7.org/fhir/SearchParameter/AuditEventaltid},
    spAuditEvent_Date {http://hl7.org/fhir/SearchParameter/AuditEventdate},
    spAuditEvent_Entity {http://hl7.org/fhir/SearchParameter/AuditEvententity},
    spAuditEvent_Entityname {http://hl7.org/fhir/SearchParameter/AuditEvententityname},
    spAuditEvent_Entityrole {http://hl7.org/fhir/SearchParameter/AuditEvententityrole},
    spAuditEvent_Entitytype {http://hl7.org/fhir/SearchParameter/AuditEvententitytype},
    spAuditEvent_Outcome {http://hl7.org/fhir/SearchParameter/AuditEventoutcome},
    spAuditEvent_Patient {http://hl7.org/fhir/SearchParameter/AuditEventpatient},
    spAuditEvent_Policy {http://hl7.org/fhir/SearchParameter/AuditEventpolicy},
    spAuditEvent_Purpose {http://hl7.org/fhir/SearchParameter/AuditEventpurpose},
    spAuditEvent_Site {http://hl7.org/fhir/SearchParameter/AuditEventsite},
    spAuditEvent_Source {http://hl7.org/fhir/SearchParameter/AuditEventsource},
    spAuditEvent_Subtype {http://hl7.org/fhir/SearchParameter/AuditEventsubtype},
    spAuditEvent_Type {http://hl7.org/fhir/SearchParameter/AuditEventtype});
{$ENDIF FHIR_AUDITEVENT}

{$IFDEF FHIR_BASIC}
  // Search Parameters for Basic
  TSearchParamsBasic = (
    spBasic__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spBasic__filter {http://hl7.org/fhir/SearchParameter/filter},
    spBasic__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spBasic__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spBasic__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spBasic__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spBasic__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spBasic__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spBasic__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spBasic__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spBasic_Author {http://hl7.org/fhir/SearchParameter/Basicauthor},
    spBasic_Code {http://hl7.org/fhir/SearchParameter/Basiccode},
    spBasic_Created {http://hl7.org/fhir/SearchParameter/Basiccreated},
    spBasic_Identifier {http://hl7.org/fhir/SearchParameter/Basicidentifier},
    spBasic_Patient {http://hl7.org/fhir/SearchParameter/Basicpatient},
    spBasic_Subject {http://hl7.org/fhir/SearchParameter/Basicsubject});
{$ENDIF FHIR_BASIC}

{$IFDEF FHIR_BINARY}
  // Search Parameters for Binary
  TSearchParamsBinary = (
    spBinary__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spBinary__filter {http://hl7.org/fhir/SearchParameter/filter},
    spBinary__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spBinary__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spBinary__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spBinary__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spBinary__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spBinary__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spBinary__tag {http://hl7.org/fhir/SearchParameter/Resourcetag});
{$ENDIF FHIR_BINARY}

{$IFDEF FHIR_BIOLOGICALLYDERIVEDPRODUCT}
  // Search Parameters for BiologicallyDerivedProduct
  TSearchParamsBiologicallyDerivedProduct = (
    spBiologicallyDerivedProduct__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spBiologicallyDerivedProduct__filter {http://hl7.org/fhir/SearchParameter/filter},
    spBiologicallyDerivedProduct__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spBiologicallyDerivedProduct__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spBiologicallyDerivedProduct__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spBiologicallyDerivedProduct__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spBiologicallyDerivedProduct__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spBiologicallyDerivedProduct__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spBiologicallyDerivedProduct__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spBiologicallyDerivedProduct__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext});
{$ENDIF FHIR_BIOLOGICALLYDERIVEDPRODUCT}

{$IFDEF FHIR_BODYSTRUCTURE}
  // Search Parameters for BodyStructure
  TSearchParamsBodyStructure = (
    spBodyStructure__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spBodyStructure__filter {http://hl7.org/fhir/SearchParameter/filter},
    spBodyStructure__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spBodyStructure__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spBodyStructure__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spBodyStructure__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spBodyStructure__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spBodyStructure__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spBodyStructure__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spBodyStructure__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spBodyStructure_Identifier {http://hl7.org/fhir/SearchParameter/BodyStructureidentifier},
    spBodyStructure_Location {http://hl7.org/fhir/SearchParameter/BodyStructurelocation},
    spBodyStructure_Morphology {http://hl7.org/fhir/SearchParameter/BodyStructuremorphology},
    spBodyStructure_Patient {http://hl7.org/fhir/SearchParameter/BodyStructurepatient});
{$ENDIF FHIR_BODYSTRUCTURE}

{$IFDEF FHIR_BUNDLE}
  // Search Parameters for Bundle
  TSearchParamsBundle = (
    spBundle__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spBundle__filter {http://hl7.org/fhir/SearchParameter/filter},
    spBundle__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spBundle__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spBundle__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spBundle__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spBundle__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spBundle__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spBundle__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spBundle_Composition {http://hl7.org/fhir/SearchParameter/Bundlecomposition},
    spBundle_Identifier {http://hl7.org/fhir/SearchParameter/Bundleidentifier},
    spBundle_Message {http://hl7.org/fhir/SearchParameter/Bundlemessage},
    spBundle_Timestamp {http://hl7.org/fhir/SearchParameter/Bundletimestamp},
    spBundle_Type {http://hl7.org/fhir/SearchParameter/Bundletype});
{$ENDIF FHIR_BUNDLE}

{$IFDEF FHIR_CAPABILITYSTATEMENT}
  // Search Parameters for CapabilityStatement
  TSearchParamsCapabilityStatement = (
    spCapabilityStatement__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spCapabilityStatement__filter {http://hl7.org/fhir/SearchParameter/filter},
    spCapabilityStatement__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spCapabilityStatement__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spCapabilityStatement__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spCapabilityStatement__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spCapabilityStatement__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spCapabilityStatement__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spCapabilityStatement__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spCapabilityStatement__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spCapabilityStatement_Context {http://hl7.org/fhir/SearchParameter/conformancecontext},
    spCapabilityStatement_Contextquantity {http://hl7.org/fhir/SearchParameter/conformancecontextquantity},
    spCapabilityStatement_Contexttype {http://hl7.org/fhir/SearchParameter/conformancecontexttype},
    spCapabilityStatement_Contexttypequantity {http://hl7.org/fhir/SearchParameter/conformancecontexttypequantity},
    spCapabilityStatement_Contexttypevalue {http://hl7.org/fhir/SearchParameter/conformancecontexttypevalue},
    spCapabilityStatement_Date {http://hl7.org/fhir/SearchParameter/conformancedate},
    spCapabilityStatement_Description {http://hl7.org/fhir/SearchParameter/conformancedescription},
    spCapabilityStatement_Fhirversion {http://hl7.org/fhir/SearchParameter/CapabilityStatementfhirversion},
    spCapabilityStatement_Format {http://hl7.org/fhir/SearchParameter/CapabilityStatementformat},
    spCapabilityStatement_Guide {http://hl7.org/fhir/SearchParameter/CapabilityStatementguide},
    spCapabilityStatement_Jurisdiction {http://hl7.org/fhir/SearchParameter/conformancejurisdiction},
    spCapabilityStatement_Mode {http://hl7.org/fhir/SearchParameter/CapabilityStatementmode},
    spCapabilityStatement_Name {http://hl7.org/fhir/SearchParameter/conformancename},
    spCapabilityStatement_Publisher {http://hl7.org/fhir/SearchParameter/conformancepublisher},
    spCapabilityStatement_Resource {http://hl7.org/fhir/SearchParameter/CapabilityStatementresource},
    spCapabilityStatement_Resourceprofile {http://hl7.org/fhir/SearchParameter/CapabilityStatementresourceprofile},
    spCapabilityStatement_Securityservice {http://hl7.org/fhir/SearchParameter/CapabilityStatementsecurityservice},
    spCapabilityStatement_Software {http://hl7.org/fhir/SearchParameter/CapabilityStatementsoftware},
    spCapabilityStatement_Status {http://hl7.org/fhir/SearchParameter/conformancestatus},
    spCapabilityStatement_Supportedprofile {http://hl7.org/fhir/SearchParameter/CapabilityStatementsupportedprofile},
    spCapabilityStatement_Title {http://hl7.org/fhir/SearchParameter/conformancetitle},
    spCapabilityStatement_Url {http://hl7.org/fhir/SearchParameter/conformanceurl},
    spCapabilityStatement_Version {http://hl7.org/fhir/SearchParameter/conformanceversion});
{$ENDIF FHIR_CAPABILITYSTATEMENT}

{$IFDEF FHIR_CAPABILITYSTATEMENT2}
  // Search Parameters for CapabilityStatement2
  TSearchParamsCapabilityStatement2 = (
    spCapabilityStatement2__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spCapabilityStatement2__filter {http://hl7.org/fhir/SearchParameter/filter},
    spCapabilityStatement2__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spCapabilityStatement2__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spCapabilityStatement2__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spCapabilityStatement2__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spCapabilityStatement2__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spCapabilityStatement2__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spCapabilityStatement2__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spCapabilityStatement2__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spCapabilityStatement2_Context {http://hl7.org/fhir/SearchParameter/CapabilityStatement2context},
    spCapabilityStatement2_Contextquantity {http://hl7.org/fhir/SearchParameter/CapabilityStatement2contextquantity},
    spCapabilityStatement2_Contexttype {http://hl7.org/fhir/SearchParameter/CapabilityStatement2contexttype},
    spCapabilityStatement2_Contexttypequantity {http://hl7.org/fhir/SearchParameter/CapabilityStatement2contexttypequantity},
    spCapabilityStatement2_Contexttypevalue {http://hl7.org/fhir/SearchParameter/CapabilityStatement2contexttypevalue},
    spCapabilityStatement2_Date {http://hl7.org/fhir/SearchParameter/CapabilityStatement2date},
    spCapabilityStatement2_Description {http://hl7.org/fhir/SearchParameter/CapabilityStatement2description},
    spCapabilityStatement2_Fhirversion {http://hl7.org/fhir/SearchParameter/CapabilityStatement2fhirversion},
    spCapabilityStatement2_Format {http://hl7.org/fhir/SearchParameter/CapabilityStatement2format},
    spCapabilityStatement2_Guide {http://hl7.org/fhir/SearchParameter/CapabilityStatement2guide},
    spCapabilityStatement2_Jurisdiction {http://hl7.org/fhir/SearchParameter/CapabilityStatement2jurisdiction},
    spCapabilityStatement2_Mode {http://hl7.org/fhir/SearchParameter/CapabilityStatement2mode},
    spCapabilityStatement2_Name {http://hl7.org/fhir/SearchParameter/CapabilityStatement2name},
    spCapabilityStatement2_Publisher {http://hl7.org/fhir/SearchParameter/CapabilityStatement2publisher},
    spCapabilityStatement2_Resource {http://hl7.org/fhir/SearchParameter/CapabilityStatement2resource},
    spCapabilityStatement2_Resourceprofile {http://hl7.org/fhir/SearchParameter/CapabilityStatement2resourceprofile},
    spCapabilityStatement2_Software {http://hl7.org/fhir/SearchParameter/CapabilityStatement2software},
    spCapabilityStatement2_Status {http://hl7.org/fhir/SearchParameter/CapabilityStatement2status},
    spCapabilityStatement2_Supportedprofile {http://hl7.org/fhir/SearchParameter/CapabilityStatement2supportedprofile},
    spCapabilityStatement2_Title {http://hl7.org/fhir/SearchParameter/CapabilityStatement2title},
    spCapabilityStatement2_Url {http://hl7.org/fhir/SearchParameter/CapabilityStatement2url},
    spCapabilityStatement2_Version {http://hl7.org/fhir/SearchParameter/CapabilityStatement2version});
{$ENDIF FHIR_CAPABILITYSTATEMENT2}

{$IFDEF FHIR_CAREPLAN}
  // Search Parameters for CarePlan
  TSearchParamsCarePlan = (
    spCarePlan__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spCarePlan__filter {http://hl7.org/fhir/SearchParameter/filter},
    spCarePlan__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spCarePlan__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spCarePlan__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spCarePlan__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spCarePlan__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spCarePlan__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spCarePlan__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spCarePlan__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spCarePlan_Activitycode {http://hl7.org/fhir/SearchParameter/CarePlanactivitycode},
    spCarePlan_Activityreference {http://hl7.org/fhir/SearchParameter/CarePlanactivityreference},
    spCarePlan_Activityscheduleddate {http://hl7.org/fhir/SearchParameter/CarePlanactivityscheduleddate},
    spCarePlan_Activityscheduledstring {http://hl7.org/fhir/SearchParameter/CarePlanactivityscheduledstring},
    spCarePlan_Basedon {http://hl7.org/fhir/SearchParameter/CarePlanbasedon},
    spCarePlan_Careteam {http://hl7.org/fhir/SearchParameter/CarePlancareteam},
    spCarePlan_Category {http://hl7.org/fhir/SearchParameter/CarePlancategory},
    spCarePlan_Condition {http://hl7.org/fhir/SearchParameter/CarePlancondition},
    spCarePlan_Date {http://hl7.org/fhir/SearchParameter/clinicaldate},
    spCarePlan_Encounter {http://hl7.org/fhir/SearchParameter/CarePlanencounter},
    spCarePlan_Goal {http://hl7.org/fhir/SearchParameter/CarePlangoal},
    spCarePlan_Identifier {http://hl7.org/fhir/SearchParameter/clinicalidentifier},
    spCarePlan_Instantiatescanonical {http://hl7.org/fhir/SearchParameter/CarePlaninstantiatescanonical},
    spCarePlan_Instantiatesuri {http://hl7.org/fhir/SearchParameter/CarePlaninstantiatesuri},
    spCarePlan_Intent {http://hl7.org/fhir/SearchParameter/CarePlanintent},
    spCarePlan_Partof {http://hl7.org/fhir/SearchParameter/CarePlanpartof},
    spCarePlan_Patient {http://hl7.org/fhir/SearchParameter/clinicalpatient},
    spCarePlan_Performer {http://hl7.org/fhir/SearchParameter/CarePlanperformer},
    spCarePlan_Replaces {http://hl7.org/fhir/SearchParameter/CarePlanreplaces},
    spCarePlan_Status {http://hl7.org/fhir/SearchParameter/CarePlanstatus},
    spCarePlan_Subject {http://hl7.org/fhir/SearchParameter/CarePlansubject});
{$ENDIF FHIR_CAREPLAN}

{$IFDEF FHIR_CARETEAM}
  // Search Parameters for CareTeam
  TSearchParamsCareTeam = (
    spCareTeam__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spCareTeam__filter {http://hl7.org/fhir/SearchParameter/filter},
    spCareTeam__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spCareTeam__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spCareTeam__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spCareTeam__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spCareTeam__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spCareTeam__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spCareTeam__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spCareTeam__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spCareTeam_Category {http://hl7.org/fhir/SearchParameter/CareTeamcategory},
    spCareTeam_Date {http://hl7.org/fhir/SearchParameter/clinicaldate},
    spCareTeam_Identifier {http://hl7.org/fhir/SearchParameter/clinicalidentifier},
    spCareTeam_Name {http://hl7.org/fhir/SearchParameter/CareTeamname},
    spCareTeam_Participant {http://hl7.org/fhir/SearchParameter/CareTeamparticipant},
    spCareTeam_Patient {http://hl7.org/fhir/SearchParameter/clinicalpatient},
    spCareTeam_Status {http://hl7.org/fhir/SearchParameter/CareTeamstatus},
    spCareTeam_Subject {http://hl7.org/fhir/SearchParameter/CareTeamsubject});
{$ENDIF FHIR_CARETEAM}

{$IFDEF FHIR_CATALOGENTRY}
  // Search Parameters for CatalogEntry
  TSearchParamsCatalogEntry = (
    spCatalogEntry__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spCatalogEntry__filter {http://hl7.org/fhir/SearchParameter/filter},
    spCatalogEntry__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spCatalogEntry__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spCatalogEntry__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spCatalogEntry__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spCatalogEntry__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spCatalogEntry__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spCatalogEntry__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spCatalogEntry__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spCatalogEntry_Identifier {http://hl7.org/fhir/SearchParameter/CatalogEntryidentifier},
    spCatalogEntry_Name {http://hl7.org/fhir/SearchParameter/CatalogEntryname},
    spCatalogEntry_Orderable {http://hl7.org/fhir/SearchParameter/CatalogEntryorderable},
    spCatalogEntry_Referenceditem {http://hl7.org/fhir/SearchParameter/CatalogEntryreferenceditem},
    spCatalogEntry_Relatedentry {http://hl7.org/fhir/SearchParameter/CatalogEntryrelatedentry},
    spCatalogEntry_Status {http://hl7.org/fhir/SearchParameter/CatalogEntrystatus},
    spCatalogEntry_Type {http://hl7.org/fhir/SearchParameter/CatalogEntrytype});
{$ENDIF FHIR_CATALOGENTRY}

{$IFDEF FHIR_CHARGEITEM}
  // Search Parameters for ChargeItem
  TSearchParamsChargeItem = (
    spChargeItem__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spChargeItem__filter {http://hl7.org/fhir/SearchParameter/filter},
    spChargeItem__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spChargeItem__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spChargeItem__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spChargeItem__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spChargeItem__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spChargeItem__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spChargeItem__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spChargeItem__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spChargeItem_Account {http://hl7.org/fhir/SearchParameter/ChargeItemaccount},
    spChargeItem_Code {http://hl7.org/fhir/SearchParameter/ChargeItemcode},
    spChargeItem_Context {http://hl7.org/fhir/SearchParameter/ChargeItemcontext},
    spChargeItem_Entereddate {http://hl7.org/fhir/SearchParameter/ChargeItementereddate},
    spChargeItem_Enterer {http://hl7.org/fhir/SearchParameter/ChargeItementerer},
    spChargeItem_Factoroverride {http://hl7.org/fhir/SearchParameter/ChargeItemfactoroverride},
    spChargeItem_Identifier {http://hl7.org/fhir/SearchParameter/ChargeItemidentifier},
    spChargeItem_Occurrence {http://hl7.org/fhir/SearchParameter/ChargeItemoccurrence},
    spChargeItem_Patient {http://hl7.org/fhir/SearchParameter/ChargeItempatient},
    spChargeItem_Performeractor {http://hl7.org/fhir/SearchParameter/ChargeItemperformeractor},
    spChargeItem_Performerfunction {http://hl7.org/fhir/SearchParameter/ChargeItemperformerfunction},
    spChargeItem_Performingorganization {http://hl7.org/fhir/SearchParameter/ChargeItemperformingorganization},
    spChargeItem_Priceoverride {http://hl7.org/fhir/SearchParameter/ChargeItempriceoverride},
    spChargeItem_Quantity {http://hl7.org/fhir/SearchParameter/ChargeItemquantity},
    spChargeItem_Requestingorganization {http://hl7.org/fhir/SearchParameter/ChargeItemrequestingorganization},
    spChargeItem_Service {http://hl7.org/fhir/SearchParameter/ChargeItemservice},
    spChargeItem_Subject {http://hl7.org/fhir/SearchParameter/ChargeItemsubject});
{$ENDIF FHIR_CHARGEITEM}

{$IFDEF FHIR_CHARGEITEMDEFINITION}
  // Search Parameters for ChargeItemDefinition
  TSearchParamsChargeItemDefinition = (
    spChargeItemDefinition__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spChargeItemDefinition__filter {http://hl7.org/fhir/SearchParameter/filter},
    spChargeItemDefinition__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spChargeItemDefinition__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spChargeItemDefinition__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spChargeItemDefinition__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spChargeItemDefinition__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spChargeItemDefinition__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spChargeItemDefinition__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spChargeItemDefinition__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spChargeItemDefinition_Context {http://hl7.org/fhir/SearchParameter/ChargeItemDefinitioncontext},
    spChargeItemDefinition_Contextquantity {http://hl7.org/fhir/SearchParameter/ChargeItemDefinitioncontextquantity},
    spChargeItemDefinition_Contexttype {http://hl7.org/fhir/SearchParameter/ChargeItemDefinitioncontexttype},
    spChargeItemDefinition_Contexttypequantity {http://hl7.org/fhir/SearchParameter/ChargeItemDefinitioncontexttypequantity},
    spChargeItemDefinition_Contexttypevalue {http://hl7.org/fhir/SearchParameter/ChargeItemDefinitioncontexttypevalue},
    spChargeItemDefinition_Date {http://hl7.org/fhir/SearchParameter/ChargeItemDefinitiondate},
    spChargeItemDefinition_Description {http://hl7.org/fhir/SearchParameter/ChargeItemDefinitiondescription},
    spChargeItemDefinition_Effective {http://hl7.org/fhir/SearchParameter/ChargeItemDefinitioneffective},
    spChargeItemDefinition_Identifier {http://hl7.org/fhir/SearchParameter/ChargeItemDefinitionidentifier},
    spChargeItemDefinition_Jurisdiction {http://hl7.org/fhir/SearchParameter/ChargeItemDefinitionjurisdiction},
    spChargeItemDefinition_Publisher {http://hl7.org/fhir/SearchParameter/ChargeItemDefinitionpublisher},
    spChargeItemDefinition_Status {http://hl7.org/fhir/SearchParameter/ChargeItemDefinitionstatus},
    spChargeItemDefinition_Title {http://hl7.org/fhir/SearchParameter/ChargeItemDefinitiontitle},
    spChargeItemDefinition_Url {http://hl7.org/fhir/SearchParameter/ChargeItemDefinitionurl},
    spChargeItemDefinition_Version {http://hl7.org/fhir/SearchParameter/ChargeItemDefinitionversion});
{$ENDIF FHIR_CHARGEITEMDEFINITION}

{$IFDEF FHIR_CITATION}
  // Search Parameters for Citation
  TSearchParamsCitation = (
    spCitation__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spCitation__filter {http://hl7.org/fhir/SearchParameter/filter},
    spCitation__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spCitation__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spCitation__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spCitation__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spCitation__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spCitation__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spCitation__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spCitation__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spCitation_Context {http://hl7.org/fhir/SearchParameter/Citationcontext},
    spCitation_Contextquantity {http://hl7.org/fhir/SearchParameter/Citationcontextquantity},
    spCitation_Contexttype {http://hl7.org/fhir/SearchParameter/Citationcontexttype},
    spCitation_Contexttypequantity {http://hl7.org/fhir/SearchParameter/Citationcontexttypequantity},
    spCitation_Contexttypevalue {http://hl7.org/fhir/SearchParameter/Citationcontexttypevalue},
    spCitation_Identifier {http://hl7.org/fhir/SearchParameter/Citationidentifier},
    spCitation_Status {http://hl7.org/fhir/SearchParameter/Citationstatus},
    spCitation_Url {http://hl7.org/fhir/SearchParameter/Citationurl});
{$ENDIF FHIR_CITATION}

{$IFDEF FHIR_CLAIM}
  // Search Parameters for Claim
  TSearchParamsClaim = (
    spClaim__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spClaim__filter {http://hl7.org/fhir/SearchParameter/filter},
    spClaim__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spClaim__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spClaim__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spClaim__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spClaim__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spClaim__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spClaim__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spClaim__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spClaim_Careteam {http://hl7.org/fhir/SearchParameter/Claimcareteam},
    spClaim_Created {http://hl7.org/fhir/SearchParameter/Claimcreated},
    spClaim_Detailudi {http://hl7.org/fhir/SearchParameter/Claimdetailudi},
    spClaim_Encounter {http://hl7.org/fhir/SearchParameter/Claimencounter},
    spClaim_Enterer {http://hl7.org/fhir/SearchParameter/Claimenterer},
    spClaim_Facility {http://hl7.org/fhir/SearchParameter/Claimfacility},
    spClaim_Identifier {http://hl7.org/fhir/SearchParameter/Claimidentifier},
    spClaim_Insurer {http://hl7.org/fhir/SearchParameter/Claiminsurer},
    spClaim_Itemudi {http://hl7.org/fhir/SearchParameter/Claimitemudi},
    spClaim_Patient {http://hl7.org/fhir/SearchParameter/Claimpatient},
    spClaim_Payee {http://hl7.org/fhir/SearchParameter/Claimpayee},
    spClaim_Priority {http://hl7.org/fhir/SearchParameter/Claimpriority},
    spClaim_Procedureudi {http://hl7.org/fhir/SearchParameter/Claimprocedureudi},
    spClaim_Provider {http://hl7.org/fhir/SearchParameter/Claimprovider},
    spClaim_Status {http://hl7.org/fhir/SearchParameter/Claimstatus},
    spClaim_Subdetailudi {http://hl7.org/fhir/SearchParameter/Claimsubdetailudi},
    spClaim_Use {http://hl7.org/fhir/SearchParameter/Claimuse});
{$ENDIF FHIR_CLAIM}

{$IFDEF FHIR_CLAIMRESPONSE}
  // Search Parameters for ClaimResponse
  TSearchParamsClaimResponse = (
    spClaimResponse__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spClaimResponse__filter {http://hl7.org/fhir/SearchParameter/filter},
    spClaimResponse__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spClaimResponse__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spClaimResponse__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spClaimResponse__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spClaimResponse__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spClaimResponse__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spClaimResponse__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spClaimResponse__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spClaimResponse_Created {http://hl7.org/fhir/SearchParameter/ClaimResponsecreated},
    spClaimResponse_Disposition {http://hl7.org/fhir/SearchParameter/ClaimResponsedisposition},
    spClaimResponse_Identifier {http://hl7.org/fhir/SearchParameter/ClaimResponseidentifier},
    spClaimResponse_Insurer {http://hl7.org/fhir/SearchParameter/ClaimResponseinsurer},
    spClaimResponse_Outcome {http://hl7.org/fhir/SearchParameter/ClaimResponseoutcome},
    spClaimResponse_Patient {http://hl7.org/fhir/SearchParameter/ClaimResponsepatient},
    spClaimResponse_Paymentdate {http://hl7.org/fhir/SearchParameter/ClaimResponsepaymentdate},
    spClaimResponse_Request {http://hl7.org/fhir/SearchParameter/ClaimResponserequest},
    spClaimResponse_Requestor {http://hl7.org/fhir/SearchParameter/ClaimResponserequestor},
    spClaimResponse_Status {http://hl7.org/fhir/SearchParameter/ClaimResponsestatus},
    spClaimResponse_Use {http://hl7.org/fhir/SearchParameter/ClaimResponseuse});
{$ENDIF FHIR_CLAIMRESPONSE}

{$IFDEF FHIR_CLINICALIMPRESSION}
  // Search Parameters for ClinicalImpression
  TSearchParamsClinicalImpression = (
    spClinicalImpression__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spClinicalImpression__filter {http://hl7.org/fhir/SearchParameter/filter},
    spClinicalImpression__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spClinicalImpression__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spClinicalImpression__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spClinicalImpression__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spClinicalImpression__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spClinicalImpression__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spClinicalImpression__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spClinicalImpression__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spClinicalImpression_Date {http://hl7.org/fhir/SearchParameter/clinicaldate},
    spClinicalImpression_Encounter {http://hl7.org/fhir/SearchParameter/ClinicalImpressionencounter},
    spClinicalImpression_Findingcode {http://hl7.org/fhir/SearchParameter/ClinicalImpressionfindingcode},
    spClinicalImpression_Findingref {http://hl7.org/fhir/SearchParameter/ClinicalImpressionfindingref},
    spClinicalImpression_Identifier {http://hl7.org/fhir/SearchParameter/ClinicalImpressionidentifier},
    spClinicalImpression_Patient {http://hl7.org/fhir/SearchParameter/clinicalpatient},
    spClinicalImpression_Performer {http://hl7.org/fhir/SearchParameter/ClinicalImpressionperformer},
    spClinicalImpression_Previous {http://hl7.org/fhir/SearchParameter/ClinicalImpressionprevious},
    spClinicalImpression_Problem {http://hl7.org/fhir/SearchParameter/ClinicalImpressionproblem},
    spClinicalImpression_Status {http://hl7.org/fhir/SearchParameter/ClinicalImpressionstatus},
    spClinicalImpression_Subject {http://hl7.org/fhir/SearchParameter/ClinicalImpressionsubject},
    spClinicalImpression_Supportinginfo {http://hl7.org/fhir/SearchParameter/ClinicalImpressionsupportinginfo});
{$ENDIF FHIR_CLINICALIMPRESSION}

{$IFDEF FHIR_CLINICALUSEISSUE}
  // Search Parameters for ClinicalUseIssue
  TSearchParamsClinicalUseIssue = (
    spClinicalUseIssue__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spClinicalUseIssue__filter {http://hl7.org/fhir/SearchParameter/filter},
    spClinicalUseIssue__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spClinicalUseIssue__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spClinicalUseIssue__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spClinicalUseIssue__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spClinicalUseIssue__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spClinicalUseIssue__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spClinicalUseIssue__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spClinicalUseIssue__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spClinicalUseIssue_Contraindication {http://hl7.org/fhir/SearchParameter/ClinicalUseIssuecontraindication},
    spClinicalUseIssue_Effect {http://hl7.org/fhir/SearchParameter/ClinicalUseIssueeffect},
    spClinicalUseIssue_Identifier {http://hl7.org/fhir/SearchParameter/ClinicalUseIssueidentifier},
    spClinicalUseIssue_Indication {http://hl7.org/fhir/SearchParameter/ClinicalUseIssueindication},
    spClinicalUseIssue_Interaction {http://hl7.org/fhir/SearchParameter/ClinicalUseIssueinteraction},
    spClinicalUseIssue_Product {http://hl7.org/fhir/SearchParameter/ClinicalUseIssueproduct},
    spClinicalUseIssue_Subject {http://hl7.org/fhir/SearchParameter/ClinicalUseIssuesubject},
    spClinicalUseIssue_Type {http://hl7.org/fhir/SearchParameter/ClinicalUseIssuetype});
{$ENDIF FHIR_CLINICALUSEISSUE}

{$IFDEF FHIR_CODESYSTEM}
  // Search Parameters for CodeSystem
  TSearchParamsCodeSystem = (
    spCodeSystem__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spCodeSystem__filter {http://hl7.org/fhir/SearchParameter/filter},
    spCodeSystem__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spCodeSystem__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spCodeSystem__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spCodeSystem__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spCodeSystem__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spCodeSystem__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spCodeSystem__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spCodeSystem__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spCodeSystem_Code {http://hl7.org/fhir/SearchParameter/CodeSystemcode},
    spCodeSystem_Contentmode {http://hl7.org/fhir/SearchParameter/CodeSystemcontentmode},
    spCodeSystem_Context {http://hl7.org/fhir/SearchParameter/conformancecontext},
    spCodeSystem_Contextquantity {http://hl7.org/fhir/SearchParameter/conformancecontextquantity},
    spCodeSystem_Contexttype {http://hl7.org/fhir/SearchParameter/conformancecontexttype},
    spCodeSystem_Contexttypequantity {http://hl7.org/fhir/SearchParameter/conformancecontexttypequantity},
    spCodeSystem_Contexttypevalue {http://hl7.org/fhir/SearchParameter/conformancecontexttypevalue},
    spCodeSystem_Date {http://hl7.org/fhir/SearchParameter/conformancedate},
    spCodeSystem_Description {http://hl7.org/fhir/SearchParameter/conformancedescription},
    spCodeSystem_Identifier {http://hl7.org/fhir/SearchParameter/conformanceidentifier},
    spCodeSystem_Jurisdiction {http://hl7.org/fhir/SearchParameter/conformancejurisdiction},
    spCodeSystem_Language {http://hl7.org/fhir/SearchParameter/CodeSystemlanguage},
    spCodeSystem_Name {http://hl7.org/fhir/SearchParameter/conformancename},
    spCodeSystem_Publisher {http://hl7.org/fhir/SearchParameter/conformancepublisher},
    spCodeSystem_Status {http://hl7.org/fhir/SearchParameter/conformancestatus},
    spCodeSystem_Supplements {http://hl7.org/fhir/SearchParameter/CodeSystemsupplements},
    spCodeSystem_System {http://hl7.org/fhir/SearchParameter/CodeSystemsystem},
    spCodeSystem_Title {http://hl7.org/fhir/SearchParameter/conformancetitle},
    spCodeSystem_Url {http://hl7.org/fhir/SearchParameter/conformanceurl},
    spCodeSystem_Version {http://hl7.org/fhir/SearchParameter/conformanceversion});
{$ENDIF FHIR_CODESYSTEM}

{$IFDEF FHIR_COMMUNICATION}
  // Search Parameters for Communication
  TSearchParamsCommunication = (
    spCommunication__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spCommunication__filter {http://hl7.org/fhir/SearchParameter/filter},
    spCommunication__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spCommunication__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spCommunication__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spCommunication__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spCommunication__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spCommunication__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spCommunication__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spCommunication__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spCommunication_Basedon {http://hl7.org/fhir/SearchParameter/Communicationbasedon},
    spCommunication_Category {http://hl7.org/fhir/SearchParameter/Communicationcategory},
    spCommunication_Encounter {http://hl7.org/fhir/SearchParameter/Communicationencounter},
    spCommunication_Identifier {http://hl7.org/fhir/SearchParameter/Communicationidentifier},
    spCommunication_Instantiatescanonical {http://hl7.org/fhir/SearchParameter/Communicationinstantiatescanonical},
    spCommunication_Instantiatesuri {http://hl7.org/fhir/SearchParameter/Communicationinstantiatesuri},
    spCommunication_Medium {http://hl7.org/fhir/SearchParameter/Communicationmedium},
    spCommunication_Partof {http://hl7.org/fhir/SearchParameter/Communicationpartof},
    spCommunication_Patient {http://hl7.org/fhir/SearchParameter/Communicationpatient},
    spCommunication_Received {http://hl7.org/fhir/SearchParameter/Communicationreceived},
    spCommunication_Recipient {http://hl7.org/fhir/SearchParameter/Communicationrecipient},
    spCommunication_Sender {http://hl7.org/fhir/SearchParameter/Communicationsender},
    spCommunication_Sent {http://hl7.org/fhir/SearchParameter/Communicationsent},
    spCommunication_Status {http://hl7.org/fhir/SearchParameter/Communicationstatus},
    spCommunication_Subject {http://hl7.org/fhir/SearchParameter/Communicationsubject},
    spCommunication_Topic {http://hl7.org/fhir/SearchParameter/Communicationtopic});
{$ENDIF FHIR_COMMUNICATION}

{$IFDEF FHIR_COMMUNICATIONREQUEST}
  // Search Parameters for CommunicationRequest
  TSearchParamsCommunicationRequest = (
    spCommunicationRequest__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spCommunicationRequest__filter {http://hl7.org/fhir/SearchParameter/filter},
    spCommunicationRequest__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spCommunicationRequest__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spCommunicationRequest__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spCommunicationRequest__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spCommunicationRequest__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spCommunicationRequest__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spCommunicationRequest__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spCommunicationRequest__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spCommunicationRequest_Authored {http://hl7.org/fhir/SearchParameter/CommunicationRequestauthored},
    spCommunicationRequest_Basedon {http://hl7.org/fhir/SearchParameter/CommunicationRequestbasedon},
    spCommunicationRequest_Category {http://hl7.org/fhir/SearchParameter/CommunicationRequestcategory},
    spCommunicationRequest_Encounter {http://hl7.org/fhir/SearchParameter/CommunicationRequestencounter},
    spCommunicationRequest_Groupidentifier {http://hl7.org/fhir/SearchParameter/CommunicationRequestgroupidentifier},
    spCommunicationRequest_Identifier {http://hl7.org/fhir/SearchParameter/CommunicationRequestidentifier},
    spCommunicationRequest_Informationprovider {http://hl7.org/fhir/SearchParameter/CommunicationRequestinformationprovider},
    spCommunicationRequest_Medium {http://hl7.org/fhir/SearchParameter/CommunicationRequestmedium},
    spCommunicationRequest_Occurrence {http://hl7.org/fhir/SearchParameter/CommunicationRequestoccurrence},
    spCommunicationRequest_Patient {http://hl7.org/fhir/SearchParameter/CommunicationRequestpatient},
    spCommunicationRequest_Priority {http://hl7.org/fhir/SearchParameter/CommunicationRequestpriority},
    spCommunicationRequest_Recipient {http://hl7.org/fhir/SearchParameter/CommunicationRequestrecipient},
    spCommunicationRequest_Replaces {http://hl7.org/fhir/SearchParameter/CommunicationRequestreplaces},
    spCommunicationRequest_Requester {http://hl7.org/fhir/SearchParameter/CommunicationRequestrequester},
    spCommunicationRequest_Status {http://hl7.org/fhir/SearchParameter/CommunicationRequeststatus},
    spCommunicationRequest_Subject {http://hl7.org/fhir/SearchParameter/CommunicationRequestsubject});
{$ENDIF FHIR_COMMUNICATIONREQUEST}

{$IFDEF FHIR_COMPARTMENTDEFINITION}
  // Search Parameters for CompartmentDefinition
  TSearchParamsCompartmentDefinition = (
    spCompartmentDefinition__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spCompartmentDefinition__filter {http://hl7.org/fhir/SearchParameter/filter},
    spCompartmentDefinition__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spCompartmentDefinition__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spCompartmentDefinition__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spCompartmentDefinition__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spCompartmentDefinition__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spCompartmentDefinition__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spCompartmentDefinition__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spCompartmentDefinition__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spCompartmentDefinition_Code {http://hl7.org/fhir/SearchParameter/CompartmentDefinitioncode},
    spCompartmentDefinition_Context {http://hl7.org/fhir/SearchParameter/conformancecontext},
    spCompartmentDefinition_Contextquantity {http://hl7.org/fhir/SearchParameter/conformancecontextquantity},
    spCompartmentDefinition_Contexttype {http://hl7.org/fhir/SearchParameter/conformancecontexttype},
    spCompartmentDefinition_Contexttypequantity {http://hl7.org/fhir/SearchParameter/conformancecontexttypequantity},
    spCompartmentDefinition_Contexttypevalue {http://hl7.org/fhir/SearchParameter/conformancecontexttypevalue},
    spCompartmentDefinition_Date {http://hl7.org/fhir/SearchParameter/conformancedate},
    spCompartmentDefinition_Description {http://hl7.org/fhir/SearchParameter/conformancedescription},
    spCompartmentDefinition_Name {http://hl7.org/fhir/SearchParameter/conformancename},
    spCompartmentDefinition_Publisher {http://hl7.org/fhir/SearchParameter/conformancepublisher},
    spCompartmentDefinition_Resource {http://hl7.org/fhir/SearchParameter/CompartmentDefinitionresource},
    spCompartmentDefinition_Status {http://hl7.org/fhir/SearchParameter/conformancestatus},
    spCompartmentDefinition_Url {http://hl7.org/fhir/SearchParameter/conformanceurl},
    spCompartmentDefinition_Version {http://hl7.org/fhir/SearchParameter/conformanceversion});
{$ENDIF FHIR_COMPARTMENTDEFINITION}

{$IFDEF FHIR_COMPOSITION}
  // Search Parameters for Composition
  TSearchParamsComposition = (
    spComposition__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spComposition__filter {http://hl7.org/fhir/SearchParameter/filter},
    spComposition__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spComposition__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spComposition__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spComposition__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spComposition__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spComposition__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spComposition__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spComposition__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spComposition_Attester {http://hl7.org/fhir/SearchParameter/Compositionattester},
    spComposition_Author {http://hl7.org/fhir/SearchParameter/Compositionauthor},
    spComposition_Category {http://hl7.org/fhir/SearchParameter/Compositioncategory},
    spComposition_Confidentiality {http://hl7.org/fhir/SearchParameter/Compositionconfidentiality},
    spComposition_Context {http://hl7.org/fhir/SearchParameter/Compositioncontext},
    spComposition_Date {http://hl7.org/fhir/SearchParameter/clinicaldate},
    spComposition_Encounter {http://hl7.org/fhir/SearchParameter/clinicalencounter},
    spComposition_Entry {http://hl7.org/fhir/SearchParameter/Compositionentry},
    spComposition_Identifier {http://hl7.org/fhir/SearchParameter/clinicalidentifier},
    spComposition_Patient {http://hl7.org/fhir/SearchParameter/clinicalpatient},
    spComposition_Period {http://hl7.org/fhir/SearchParameter/Compositionperiod},
    spComposition_Relatedid {http://hl7.org/fhir/SearchParameter/Compositionrelatedid},
    spComposition_Relatedref {http://hl7.org/fhir/SearchParameter/Compositionrelatedref},
    spComposition_Section {http://hl7.org/fhir/SearchParameter/Compositionsection},
    spComposition_Status {http://hl7.org/fhir/SearchParameter/Compositionstatus},
    spComposition_Subject {http://hl7.org/fhir/SearchParameter/Compositionsubject},
    spComposition_Title {http://hl7.org/fhir/SearchParameter/Compositiontitle},
    spComposition_Type {http://hl7.org/fhir/SearchParameter/clinicaltype});
{$ENDIF FHIR_COMPOSITION}

{$IFDEF FHIR_CONCEPTMAP}
  // Search Parameters for ConceptMap
  TSearchParamsConceptMap = (
    spConceptMap__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spConceptMap__filter {http://hl7.org/fhir/SearchParameter/filter},
    spConceptMap__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spConceptMap__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spConceptMap__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spConceptMap__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spConceptMap__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spConceptMap__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spConceptMap__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spConceptMap__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spConceptMap_Context {http://hl7.org/fhir/SearchParameter/conformancecontext},
    spConceptMap_Contextquantity {http://hl7.org/fhir/SearchParameter/conformancecontextquantity},
    spConceptMap_Contexttype {http://hl7.org/fhir/SearchParameter/conformancecontexttype},
    spConceptMap_Contexttypequantity {http://hl7.org/fhir/SearchParameter/conformancecontexttypequantity},
    spConceptMap_Contexttypevalue {http://hl7.org/fhir/SearchParameter/conformancecontexttypevalue},
    spConceptMap_Date {http://hl7.org/fhir/SearchParameter/conformancedate},
    spConceptMap_Dependson {http://hl7.org/fhir/SearchParameter/ConceptMapdependson},
    spConceptMap_Description {http://hl7.org/fhir/SearchParameter/conformancedescription},
    spConceptMap_Identifier {http://hl7.org/fhir/SearchParameter/conformanceidentifier},
    spConceptMap_Jurisdiction {http://hl7.org/fhir/SearchParameter/conformancejurisdiction},
    spConceptMap_Name {http://hl7.org/fhir/SearchParameter/conformancename},
    spConceptMap_Other {http://hl7.org/fhir/SearchParameter/ConceptMapother},
    spConceptMap_Product {http://hl7.org/fhir/SearchParameter/ConceptMapproduct},
    spConceptMap_Publisher {http://hl7.org/fhir/SearchParameter/conformancepublisher},
    spConceptMap_Source {http://hl7.org/fhir/SearchParameter/ConceptMapsource},
    spConceptMap_Sourcecode {http://hl7.org/fhir/SearchParameter/ConceptMapsourcecode},
    spConceptMap_Sourcesystem {http://hl7.org/fhir/SearchParameter/ConceptMapsourcesystem},
    spConceptMap_Sourceuri {http://hl7.org/fhir/SearchParameter/ConceptMapsourceuri},
    spConceptMap_Status {http://hl7.org/fhir/SearchParameter/conformancestatus},
    spConceptMap_Target {http://hl7.org/fhir/SearchParameter/ConceptMaptarget},
    spConceptMap_Targetcode {http://hl7.org/fhir/SearchParameter/ConceptMaptargetcode},
    spConceptMap_Targetsystem {http://hl7.org/fhir/SearchParameter/ConceptMaptargetsystem},
    spConceptMap_Targeturi {http://hl7.org/fhir/SearchParameter/ConceptMaptargeturi},
    spConceptMap_Title {http://hl7.org/fhir/SearchParameter/conformancetitle},
    spConceptMap_Url {http://hl7.org/fhir/SearchParameter/conformanceurl},
    spConceptMap_Version {http://hl7.org/fhir/SearchParameter/conformanceversion});
{$ENDIF FHIR_CONCEPTMAP}

{$IFDEF FHIR_CONDITION}
  // Search Parameters for Condition
  TSearchParamsCondition = (
    spCondition__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spCondition__filter {http://hl7.org/fhir/SearchParameter/filter},
    spCondition__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spCondition__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spCondition__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spCondition__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spCondition__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spCondition__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spCondition__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spCondition__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spCondition_Abatementage {http://hl7.org/fhir/SearchParameter/Conditionabatementage},
    spCondition_Abatementdate {http://hl7.org/fhir/SearchParameter/Conditionabatementdate},
    spCondition_Abatementstring {http://hl7.org/fhir/SearchParameter/Conditionabatementstring},
    spCondition_Asserter {http://hl7.org/fhir/SearchParameter/Conditionasserter},
    spCondition_Bodysite {http://hl7.org/fhir/SearchParameter/Conditionbodysite},
    spCondition_Category {http://hl7.org/fhir/SearchParameter/Conditioncategory},
    spCondition_Clinicalstatus {http://hl7.org/fhir/SearchParameter/Conditionclinicalstatus},
    spCondition_Code {http://hl7.org/fhir/SearchParameter/clinicalcode},
    spCondition_Encounter {http://hl7.org/fhir/SearchParameter/Conditionencounter},
    spCondition_Evidence {http://hl7.org/fhir/SearchParameter/Conditionevidence},
    spCondition_Evidencedetail {http://hl7.org/fhir/SearchParameter/Conditionevidencedetail},
    spCondition_Identifier {http://hl7.org/fhir/SearchParameter/clinicalidentifier},
    spCondition_Onsetage {http://hl7.org/fhir/SearchParameter/Conditiononsetage},
    spCondition_Onsetdate {http://hl7.org/fhir/SearchParameter/Conditiononsetdate},
    spCondition_Onsetinfo {http://hl7.org/fhir/SearchParameter/Conditiononsetinfo},
    spCondition_Patient {http://hl7.org/fhir/SearchParameter/clinicalpatient},
    spCondition_Recordeddate {http://hl7.org/fhir/SearchParameter/Conditionrecordeddate},
    spCondition_Severity {http://hl7.org/fhir/SearchParameter/Conditionseverity},
    spCondition_Stage {http://hl7.org/fhir/SearchParameter/Conditionstage},
    spCondition_Subject {http://hl7.org/fhir/SearchParameter/Conditionsubject},
    spCondition_Verificationstatus {http://hl7.org/fhir/SearchParameter/Conditionverificationstatus});
{$ENDIF FHIR_CONDITION}

{$IFDEF FHIR_CONDITIONDEFINITION}
  // Search Parameters for ConditionDefinition
  TSearchParamsConditionDefinition = (
    spConditionDefinition__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spConditionDefinition__filter {http://hl7.org/fhir/SearchParameter/filter},
    spConditionDefinition__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spConditionDefinition__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spConditionDefinition__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spConditionDefinition__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spConditionDefinition__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spConditionDefinition__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spConditionDefinition__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spConditionDefinition__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spConditionDefinition_Context {http://hl7.org/fhir/SearchParameter/ConditionDefinitioncontext},
    spConditionDefinition_Contextquantity {http://hl7.org/fhir/SearchParameter/ConditionDefinitioncontextquantity},
    spConditionDefinition_Contexttype {http://hl7.org/fhir/SearchParameter/ConditionDefinitioncontexttype},
    spConditionDefinition_Contexttypequantity {http://hl7.org/fhir/SearchParameter/ConditionDefinitioncontexttypequantity},
    spConditionDefinition_Contexttypevalue {http://hl7.org/fhir/SearchParameter/ConditionDefinitioncontexttypevalue},
    spConditionDefinition_Date {http://hl7.org/fhir/SearchParameter/ConditionDefinitiondate},
    spConditionDefinition_Description {http://hl7.org/fhir/SearchParameter/ConditionDefinitiondescription},
    spConditionDefinition_Identifier {http://hl7.org/fhir/SearchParameter/ConditionDefinitionidentifier},
    spConditionDefinition_Jurisdiction {http://hl7.org/fhir/SearchParameter/ConditionDefinitionjurisdiction},
    spConditionDefinition_Name {http://hl7.org/fhir/SearchParameter/ConditionDefinitionname},
    spConditionDefinition_Publisher {http://hl7.org/fhir/SearchParameter/ConditionDefinitionpublisher},
    spConditionDefinition_Status {http://hl7.org/fhir/SearchParameter/ConditionDefinitionstatus},
    spConditionDefinition_Title {http://hl7.org/fhir/SearchParameter/ConditionDefinitiontitle},
    spConditionDefinition_Url {http://hl7.org/fhir/SearchParameter/ConditionDefinitionurl},
    spConditionDefinition_Version {http://hl7.org/fhir/SearchParameter/ConditionDefinitionversion});
{$ENDIF FHIR_CONDITIONDEFINITION}

{$IFDEF FHIR_CONSENT}
  // Search Parameters for Consent
  TSearchParamsConsent = (
    spConsent__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spConsent__filter {http://hl7.org/fhir/SearchParameter/filter},
    spConsent__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spConsent__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spConsent__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spConsent__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spConsent__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spConsent__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spConsent__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spConsent__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spConsent_Action {http://hl7.org/fhir/SearchParameter/Consentaction},
    spConsent_Actor {http://hl7.org/fhir/SearchParameter/Consentactor},
    spConsent_Category {http://hl7.org/fhir/SearchParameter/Consentcategory},
    spConsent_Consentor {http://hl7.org/fhir/SearchParameter/Consentconsentor},
    spConsent_Controller {http://hl7.org/fhir/SearchParameter/Consentcontroller},
    spConsent_Data {http://hl7.org/fhir/SearchParameter/Consentdata},
    spConsent_Date {http://hl7.org/fhir/SearchParameter/clinicaldate},
    spConsent_Identifier {http://hl7.org/fhir/SearchParameter/clinicalidentifier},
    spConsent_Manager {http://hl7.org/fhir/SearchParameter/Consentmanager},
    spConsent_Patient {http://hl7.org/fhir/SearchParameter/clinicalpatient},
    spConsent_Period {http://hl7.org/fhir/SearchParameter/Consentperiod},
    spConsent_Policyuri {http://hl7.org/fhir/SearchParameter/Consentpolicyuri},
    spConsent_Purpose {http://hl7.org/fhir/SearchParameter/Consentpurpose},
    spConsent_Scope {http://hl7.org/fhir/SearchParameter/Consentscope},
    spConsent_Securitylabel {http://hl7.org/fhir/SearchParameter/Consentsecuritylabel},
    spConsent_Sourcereference {http://hl7.org/fhir/SearchParameter/Consentsourcereference},
    spConsent_Status {http://hl7.org/fhir/SearchParameter/Consentstatus},
    spConsent_Subject {http://hl7.org/fhir/SearchParameter/Consentsubject},
    spConsent_Verified {http://hl7.org/fhir/SearchParameter/Consentverified},
    spConsent_Verifieddate {http://hl7.org/fhir/SearchParameter/Consentverifieddate});
{$ENDIF FHIR_CONSENT}

{$IFDEF FHIR_CONTRACT}
  // Search Parameters for Contract
  TSearchParamsContract = (
    spContract__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spContract__filter {http://hl7.org/fhir/SearchParameter/filter},
    spContract__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spContract__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spContract__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spContract__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spContract__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spContract__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spContract__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spContract__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spContract_Authority {http://hl7.org/fhir/SearchParameter/Contractauthority},
    spContract_Domain {http://hl7.org/fhir/SearchParameter/Contractdomain},
    spContract_Identifier {http://hl7.org/fhir/SearchParameter/Contractidentifier},
    spContract_Instantiates {http://hl7.org/fhir/SearchParameter/Contractinstantiates},
    spContract_Issued {http://hl7.org/fhir/SearchParameter/Contractissued},
    spContract_Patient {http://hl7.org/fhir/SearchParameter/Contractpatient},
    spContract_Signer {http://hl7.org/fhir/SearchParameter/Contractsigner},
    spContract_Status {http://hl7.org/fhir/SearchParameter/Contractstatus},
    spContract_Subject {http://hl7.org/fhir/SearchParameter/Contractsubject},
    spContract_Url {http://hl7.org/fhir/SearchParameter/Contracturl});
{$ENDIF FHIR_CONTRACT}

{$IFDEF FHIR_COVERAGE}
  // Search Parameters for Coverage
  TSearchParamsCoverage = (
    spCoverage__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spCoverage__filter {http://hl7.org/fhir/SearchParameter/filter},
    spCoverage__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spCoverage__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spCoverage__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spCoverage__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spCoverage__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spCoverage__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spCoverage__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spCoverage__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spCoverage_Beneficiary {http://hl7.org/fhir/SearchParameter/Coveragebeneficiary},
    spCoverage_Classtype {http://hl7.org/fhir/SearchParameter/Coverageclasstype},
    spCoverage_Classvalue {http://hl7.org/fhir/SearchParameter/Coverageclassvalue},
    spCoverage_Dependent {http://hl7.org/fhir/SearchParameter/Coveragedependent},
    spCoverage_Identifier {http://hl7.org/fhir/SearchParameter/Coverageidentifier},
    spCoverage_Patient {http://hl7.org/fhir/SearchParameter/Coveragepatient},
    spCoverage_Payor {http://hl7.org/fhir/SearchParameter/Coveragepayor},
    spCoverage_Policyholder {http://hl7.org/fhir/SearchParameter/Coveragepolicyholder},
    spCoverage_Status {http://hl7.org/fhir/SearchParameter/Coveragestatus},
    spCoverage_Subscriber {http://hl7.org/fhir/SearchParameter/Coveragesubscriber},
    spCoverage_Type {http://hl7.org/fhir/SearchParameter/Coveragetype});
{$ENDIF FHIR_COVERAGE}

{$IFDEF FHIR_COVERAGEELIGIBILITYREQUEST}
  // Search Parameters for CoverageEligibilityRequest
  TSearchParamsCoverageEligibilityRequest = (
    spCoverageEligibilityRequest__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spCoverageEligibilityRequest__filter {http://hl7.org/fhir/SearchParameter/filter},
    spCoverageEligibilityRequest__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spCoverageEligibilityRequest__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spCoverageEligibilityRequest__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spCoverageEligibilityRequest__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spCoverageEligibilityRequest__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spCoverageEligibilityRequest__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spCoverageEligibilityRequest__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spCoverageEligibilityRequest__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spCoverageEligibilityRequest_Created {http://hl7.org/fhir/SearchParameter/CoverageEligibilityRequestcreated},
    spCoverageEligibilityRequest_Enterer {http://hl7.org/fhir/SearchParameter/CoverageEligibilityRequestenterer},
    spCoverageEligibilityRequest_Facility {http://hl7.org/fhir/SearchParameter/CoverageEligibilityRequestfacility},
    spCoverageEligibilityRequest_Identifier {http://hl7.org/fhir/SearchParameter/CoverageEligibilityRequestidentifier},
    spCoverageEligibilityRequest_Patient {http://hl7.org/fhir/SearchParameter/CoverageEligibilityRequestpatient},
    spCoverageEligibilityRequest_Provider {http://hl7.org/fhir/SearchParameter/CoverageEligibilityRequestprovider},
    spCoverageEligibilityRequest_Status {http://hl7.org/fhir/SearchParameter/CoverageEligibilityRequeststatus});
{$ENDIF FHIR_COVERAGEELIGIBILITYREQUEST}

{$IFDEF FHIR_COVERAGEELIGIBILITYRESPONSE}
  // Search Parameters for CoverageEligibilityResponse
  TSearchParamsCoverageEligibilityResponse = (
    spCoverageEligibilityResponse__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spCoverageEligibilityResponse__filter {http://hl7.org/fhir/SearchParameter/filter},
    spCoverageEligibilityResponse__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spCoverageEligibilityResponse__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spCoverageEligibilityResponse__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spCoverageEligibilityResponse__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spCoverageEligibilityResponse__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spCoverageEligibilityResponse__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spCoverageEligibilityResponse__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spCoverageEligibilityResponse__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spCoverageEligibilityResponse_Created {http://hl7.org/fhir/SearchParameter/CoverageEligibilityResponsecreated},
    spCoverageEligibilityResponse_Disposition {http://hl7.org/fhir/SearchParameter/CoverageEligibilityResponsedisposition},
    spCoverageEligibilityResponse_Identifier {http://hl7.org/fhir/SearchParameter/CoverageEligibilityResponseidentifier},
    spCoverageEligibilityResponse_Insurer {http://hl7.org/fhir/SearchParameter/CoverageEligibilityResponseinsurer},
    spCoverageEligibilityResponse_Outcome {http://hl7.org/fhir/SearchParameter/CoverageEligibilityResponseoutcome},
    spCoverageEligibilityResponse_Patient {http://hl7.org/fhir/SearchParameter/CoverageEligibilityResponsepatient},
    spCoverageEligibilityResponse_Request {http://hl7.org/fhir/SearchParameter/CoverageEligibilityResponserequest},
    spCoverageEligibilityResponse_Requestor {http://hl7.org/fhir/SearchParameter/CoverageEligibilityResponserequestor},
    spCoverageEligibilityResponse_Status {http://hl7.org/fhir/SearchParameter/CoverageEligibilityResponsestatus});
{$ENDIF FHIR_COVERAGEELIGIBILITYRESPONSE}

{$IFDEF FHIR_DETECTEDISSUE}
  // Search Parameters for DetectedIssue
  TSearchParamsDetectedIssue = (
    spDetectedIssue__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spDetectedIssue__filter {http://hl7.org/fhir/SearchParameter/filter},
    spDetectedIssue__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spDetectedIssue__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spDetectedIssue__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spDetectedIssue__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spDetectedIssue__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spDetectedIssue__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spDetectedIssue__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spDetectedIssue__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spDetectedIssue_Author {http://hl7.org/fhir/SearchParameter/DetectedIssueauthor},
    spDetectedIssue_Code {http://hl7.org/fhir/SearchParameter/DetectedIssuecode},
    spDetectedIssue_Identified {http://hl7.org/fhir/SearchParameter/DetectedIssueidentified},
    spDetectedIssue_Identifier {http://hl7.org/fhir/SearchParameter/clinicalidentifier},
    spDetectedIssue_Implicated {http://hl7.org/fhir/SearchParameter/DetectedIssueimplicated},
    spDetectedIssue_Patient {http://hl7.org/fhir/SearchParameter/clinicalpatient});
{$ENDIF FHIR_DETECTEDISSUE}

{$IFDEF FHIR_DEVICE}
  // Search Parameters for Device
  TSearchParamsDevice = (
    spDevice__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spDevice__filter {http://hl7.org/fhir/SearchParameter/filter},
    spDevice__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spDevice__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spDevice__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spDevice__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spDevice__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spDevice__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spDevice__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spDevice__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spDevice_Definition {http://hl7.org/fhir/SearchParameter/Devicedefinition},
    spDevice_Devicename {http://hl7.org/fhir/SearchParameter/Devicedevicename},
    spDevice_Din {http://hl7.org/fhir/SearchParameter/deviceextensionsDevicedin},
    spDevice_Expirationdate {http://hl7.org/fhir/SearchParameter/Deviceexpirationdate},
    spDevice_Identifier {http://hl7.org/fhir/SearchParameter/Deviceidentifier},
    spDevice_Location {http://hl7.org/fhir/SearchParameter/Devicelocation},
    spDevice_Lotnumber {http://hl7.org/fhir/SearchParameter/Devicelotnumber},
    spDevice_Manufacturedate {http://hl7.org/fhir/SearchParameter/Devicemanufacturedate},
    spDevice_Manufacturer {http://hl7.org/fhir/SearchParameter/Devicemanufacturer},
    spDevice_Model {http://hl7.org/fhir/SearchParameter/Devicemodel},
    spDevice_Organization {http://hl7.org/fhir/SearchParameter/Deviceorganization},
    spDevice_Parent {http://hl7.org/fhir/SearchParameter/Deviceparent},
    spDevice_Patient {http://hl7.org/fhir/SearchParameter/Devicepatient},
    spDevice_Serialnumber {http://hl7.org/fhir/SearchParameter/Deviceserialnumber},
    spDevice_Status {http://hl7.org/fhir/SearchParameter/Devicestatus},
    spDevice_Type {http://hl7.org/fhir/SearchParameter/Devicetype},
    spDevice_Udicarrier {http://hl7.org/fhir/SearchParameter/Deviceudicarrier},
    spDevice_Udidi {http://hl7.org/fhir/SearchParameter/Deviceudidi},
    spDevice_Url {http://hl7.org/fhir/SearchParameter/Deviceurl},
    spDevice_Version {http://hl7.org/fhir/SearchParameter/Deviceversion});
{$ENDIF FHIR_DEVICE}

{$IFDEF FHIR_DEVICEDEFINITION}
  // Search Parameters for DeviceDefinition
  TSearchParamsDeviceDefinition = (
    spDeviceDefinition__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spDeviceDefinition__filter {http://hl7.org/fhir/SearchParameter/filter},
    spDeviceDefinition__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spDeviceDefinition__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spDeviceDefinition__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spDeviceDefinition__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spDeviceDefinition__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spDeviceDefinition__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spDeviceDefinition__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spDeviceDefinition__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spDeviceDefinition_Identifier {http://hl7.org/fhir/SearchParameter/DeviceDefinitionidentifier},
    spDeviceDefinition_Parent {http://hl7.org/fhir/SearchParameter/DeviceDefinitionparent},
    spDeviceDefinition_Type {http://hl7.org/fhir/SearchParameter/DeviceDefinitiontype});
{$ENDIF FHIR_DEVICEDEFINITION}

{$IFDEF FHIR_DEVICEMETRIC}
  // Search Parameters for DeviceMetric
  TSearchParamsDeviceMetric = (
    spDeviceMetric__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spDeviceMetric__filter {http://hl7.org/fhir/SearchParameter/filter},
    spDeviceMetric__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spDeviceMetric__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spDeviceMetric__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spDeviceMetric__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spDeviceMetric__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spDeviceMetric__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spDeviceMetric__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spDeviceMetric__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spDeviceMetric_Category {http://hl7.org/fhir/SearchParameter/DeviceMetriccategory},
    spDeviceMetric_Identifier {http://hl7.org/fhir/SearchParameter/DeviceMetricidentifier},
    spDeviceMetric_Parent {http://hl7.org/fhir/SearchParameter/DeviceMetricparent},
    spDeviceMetric_Source {http://hl7.org/fhir/SearchParameter/DeviceMetricsource},
    spDeviceMetric_Type {http://hl7.org/fhir/SearchParameter/DeviceMetrictype});
{$ENDIF FHIR_DEVICEMETRIC}

{$IFDEF FHIR_DEVICEREQUEST}
  // Search Parameters for DeviceRequest
  TSearchParamsDeviceRequest = (
    spDeviceRequest__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spDeviceRequest__filter {http://hl7.org/fhir/SearchParameter/filter},
    spDeviceRequest__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spDeviceRequest__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spDeviceRequest__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spDeviceRequest__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spDeviceRequest__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spDeviceRequest__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spDeviceRequest__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spDeviceRequest__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spDeviceRequest_Authoredon {http://hl7.org/fhir/SearchParameter/DeviceRequestauthoredon},
    spDeviceRequest_Basedon {http://hl7.org/fhir/SearchParameter/DeviceRequestbasedon},
    spDeviceRequest_Code {http://hl7.org/fhir/SearchParameter/clinicalcode},
    spDeviceRequest_Device {http://hl7.org/fhir/SearchParameter/DeviceRequestdevice},
    spDeviceRequest_Encounter {http://hl7.org/fhir/SearchParameter/clinicalencounter},
    spDeviceRequest_Eventdate {http://hl7.org/fhir/SearchParameter/DeviceRequesteventdate},
    spDeviceRequest_Groupidentifier {http://hl7.org/fhir/SearchParameter/DeviceRequestgroupidentifier},
    spDeviceRequest_Identifier {http://hl7.org/fhir/SearchParameter/clinicalidentifier},
    spDeviceRequest_Instantiatescanonical {http://hl7.org/fhir/SearchParameter/DeviceRequestinstantiatescanonical},
    spDeviceRequest_Instantiatesuri {http://hl7.org/fhir/SearchParameter/DeviceRequestinstantiatesuri},
    spDeviceRequest_Insurance {http://hl7.org/fhir/SearchParameter/DeviceRequestinsurance},
    spDeviceRequest_Intent {http://hl7.org/fhir/SearchParameter/DeviceRequestintent},
    spDeviceRequest_Patient {http://hl7.org/fhir/SearchParameter/clinicalpatient},
    spDeviceRequest_Performer {http://hl7.org/fhir/SearchParameter/DeviceRequestperformer},
    spDeviceRequest_Priorrequest {http://hl7.org/fhir/SearchParameter/DeviceRequestpriorrequest},
    spDeviceRequest_Requester {http://hl7.org/fhir/SearchParameter/DeviceRequestrequester},
    spDeviceRequest_Status {http://hl7.org/fhir/SearchParameter/DeviceRequeststatus},
    spDeviceRequest_Subject {http://hl7.org/fhir/SearchParameter/DeviceRequestsubject});
{$ENDIF FHIR_DEVICEREQUEST}

{$IFDEF FHIR_DEVICEUSESTATEMENT}
  // Search Parameters for DeviceUseStatement
  TSearchParamsDeviceUseStatement = (
    spDeviceUseStatement__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spDeviceUseStatement__filter {http://hl7.org/fhir/SearchParameter/filter},
    spDeviceUseStatement__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spDeviceUseStatement__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spDeviceUseStatement__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spDeviceUseStatement__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spDeviceUseStatement__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spDeviceUseStatement__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spDeviceUseStatement__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spDeviceUseStatement__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spDeviceUseStatement_Device {http://hl7.org/fhir/SearchParameter/DeviceUseStatementdevice},
    spDeviceUseStatement_Identifier {http://hl7.org/fhir/SearchParameter/DeviceUseStatementidentifier},
    spDeviceUseStatement_Patient {http://hl7.org/fhir/SearchParameter/clinicalpatient},
    spDeviceUseStatement_Subject {http://hl7.org/fhir/SearchParameter/DeviceUseStatementsubject});
{$ENDIF FHIR_DEVICEUSESTATEMENT}

{$IFDEF FHIR_DIAGNOSTICREPORT}
  // Search Parameters for DiagnosticReport
  TSearchParamsDiagnosticReport = (
    spDiagnosticReport__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spDiagnosticReport__filter {http://hl7.org/fhir/SearchParameter/filter},
    spDiagnosticReport__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spDiagnosticReport__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spDiagnosticReport__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spDiagnosticReport__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spDiagnosticReport__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spDiagnosticReport__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spDiagnosticReport__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spDiagnosticReport__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spDiagnosticReport_Basedon {http://hl7.org/fhir/SearchParameter/DiagnosticReportbasedon},
    spDiagnosticReport_Category {http://hl7.org/fhir/SearchParameter/DiagnosticReportcategory},
    spDiagnosticReport_Code {http://hl7.org/fhir/SearchParameter/clinicalcode},
    spDiagnosticReport_Conclusion {http://hl7.org/fhir/SearchParameter/DiagnosticReportconclusion},
    spDiagnosticReport_Date {http://hl7.org/fhir/SearchParameter/clinicaldate},
    spDiagnosticReport_Encounter {http://hl7.org/fhir/SearchParameter/clinicalencounter},
    spDiagnosticReport_Identifier {http://hl7.org/fhir/SearchParameter/clinicalidentifier},
    spDiagnosticReport_Issued {http://hl7.org/fhir/SearchParameter/DiagnosticReportissued},
    spDiagnosticReport_Media {http://hl7.org/fhir/SearchParameter/DiagnosticReportmedia},
    spDiagnosticReport_Patient {http://hl7.org/fhir/SearchParameter/clinicalpatient},
    spDiagnosticReport_Performer {http://hl7.org/fhir/SearchParameter/DiagnosticReportperformer},
    spDiagnosticReport_Result {http://hl7.org/fhir/SearchParameter/DiagnosticReportresult},
    spDiagnosticReport_Resultsinterpreter {http://hl7.org/fhir/SearchParameter/DiagnosticReportresultsinterpreter},
    spDiagnosticReport_Specimen {http://hl7.org/fhir/SearchParameter/DiagnosticReportspecimen},
    spDiagnosticReport_Status {http://hl7.org/fhir/SearchParameter/DiagnosticReportstatus},
    spDiagnosticReport_Subject {http://hl7.org/fhir/SearchParameter/DiagnosticReportsubject});
{$ENDIF FHIR_DIAGNOSTICREPORT}

{$IFDEF FHIR_DOCUMENTMANIFEST}
  // Search Parameters for DocumentManifest
  TSearchParamsDocumentManifest = (
    spDocumentManifest__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spDocumentManifest__filter {http://hl7.org/fhir/SearchParameter/filter},
    spDocumentManifest__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spDocumentManifest__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spDocumentManifest__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spDocumentManifest__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spDocumentManifest__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spDocumentManifest__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spDocumentManifest__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spDocumentManifest__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spDocumentManifest_Author {http://hl7.org/fhir/SearchParameter/DocumentManifestauthor},
    spDocumentManifest_Created {http://hl7.org/fhir/SearchParameter/DocumentManifestcreated},
    spDocumentManifest_Description {http://hl7.org/fhir/SearchParameter/DocumentManifestdescription},
    spDocumentManifest_Identifier {http://hl7.org/fhir/SearchParameter/clinicalidentifier},
    spDocumentManifest_Item {http://hl7.org/fhir/SearchParameter/DocumentManifestitem},
    spDocumentManifest_Patient {http://hl7.org/fhir/SearchParameter/clinicalpatient},
    spDocumentManifest_Recipient {http://hl7.org/fhir/SearchParameter/DocumentManifestrecipient},
    spDocumentManifest_Relatedid {http://hl7.org/fhir/SearchParameter/DocumentManifestrelatedid},
    spDocumentManifest_Relatedref {http://hl7.org/fhir/SearchParameter/DocumentManifestrelatedref},
    spDocumentManifest_Source {http://hl7.org/fhir/SearchParameter/DocumentManifestsource},
    spDocumentManifest_Status {http://hl7.org/fhir/SearchParameter/DocumentManifeststatus},
    spDocumentManifest_Subject {http://hl7.org/fhir/SearchParameter/DocumentManifestsubject},
    spDocumentManifest_Type {http://hl7.org/fhir/SearchParameter/clinicaltype});
{$ENDIF FHIR_DOCUMENTMANIFEST}

{$IFDEF FHIR_DOCUMENTREFERENCE}
  // Search Parameters for DocumentReference
  TSearchParamsDocumentReference = (
    spDocumentReference__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spDocumentReference__filter {http://hl7.org/fhir/SearchParameter/filter},
    spDocumentReference__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spDocumentReference__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spDocumentReference__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spDocumentReference__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spDocumentReference__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spDocumentReference__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spDocumentReference__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spDocumentReference__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spDocumentReference_Attester {http://hl7.org/fhir/SearchParameter/DocumentReferenceattester},
    spDocumentReference_Author {http://hl7.org/fhir/SearchParameter/DocumentReferenceauthor},
    spDocumentReference_Basedon {http://hl7.org/fhir/SearchParameter/DocumentReferencebasedon},
    spDocumentReference_Category {http://hl7.org/fhir/SearchParameter/DocumentReferencecategory},
    spDocumentReference_Contenttype {http://hl7.org/fhir/SearchParameter/DocumentReferencecontenttype},
    spDocumentReference_Creation {http://hl7.org/fhir/SearchParameter/DocumentReferencecreation},
    spDocumentReference_Custodian {http://hl7.org/fhir/SearchParameter/DocumentReferencecustodian},
    spDocumentReference_Date {http://hl7.org/fhir/SearchParameter/DocumentReferencedate},
    spDocumentReference_Description {http://hl7.org/fhir/SearchParameter/DocumentReferencedescription},
    spDocumentReference_Docstatus {http://hl7.org/fhir/SearchParameter/DocumentReferencedocstatus},
    spDocumentReference_Encounter {http://hl7.org/fhir/SearchParameter/clinicalencounter},
    spDocumentReference_Event {http://hl7.org/fhir/SearchParameter/DocumentReferenceevent},
    spDocumentReference_Facility {http://hl7.org/fhir/SearchParameter/DocumentReferencefacility},
    spDocumentReference_Format {http://hl7.org/fhir/SearchParameter/DocumentReferenceformat},
    spDocumentReference_Identifier {http://hl7.org/fhir/SearchParameter/clinicalidentifier},
    spDocumentReference_Language {http://hl7.org/fhir/SearchParameter/DocumentReferencelanguage},
    spDocumentReference_Location {http://hl7.org/fhir/SearchParameter/DocumentReferencelocation},
    spDocumentReference_Patient {http://hl7.org/fhir/SearchParameter/clinicalpatient},
    spDocumentReference_Period {http://hl7.org/fhir/SearchParameter/DocumentReferenceperiod},
    spDocumentReference_Related {http://hl7.org/fhir/SearchParameter/DocumentReferencerelated},
    spDocumentReference_Relatesto {http://hl7.org/fhir/SearchParameter/DocumentReferencerelatesto},
    spDocumentReference_Relation {http://hl7.org/fhir/SearchParameter/DocumentReferencerelation},
    spDocumentReference_Relationship {http://hl7.org/fhir/SearchParameter/DocumentReferencerelationship},
    spDocumentReference_Securitylabel {http://hl7.org/fhir/SearchParameter/DocumentReferencesecuritylabel},
    spDocumentReference_Setting {http://hl7.org/fhir/SearchParameter/DocumentReferencesetting},
    spDocumentReference_Status {http://hl7.org/fhir/SearchParameter/DocumentReferencestatus},
    spDocumentReference_Subject {http://hl7.org/fhir/SearchParameter/DocumentReferencesubject},
    spDocumentReference_Type {http://hl7.org/fhir/SearchParameter/clinicaltype});
{$ENDIF FHIR_DOCUMENTREFERENCE}

{$IFDEF FHIR_ENCOUNTER}
  // Search Parameters for Encounter
  TSearchParamsEncounter = (
    spEncounter__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spEncounter__filter {http://hl7.org/fhir/SearchParameter/filter},
    spEncounter__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spEncounter__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spEncounter__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spEncounter__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spEncounter__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spEncounter__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spEncounter__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spEncounter__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spEncounter_Account {http://hl7.org/fhir/SearchParameter/Encounteraccount},
    spEncounter_Appointment {http://hl7.org/fhir/SearchParameter/Encounterappointment},
    spEncounter_Basedon {http://hl7.org/fhir/SearchParameter/Encounterbasedon},
    spEncounter_Class {http://hl7.org/fhir/SearchParameter/Encounterclass},
    spEncounter_Date {http://hl7.org/fhir/SearchParameter/clinicaldate},
    spEncounter_Diagnosis {http://hl7.org/fhir/SearchParameter/Encounterdiagnosis},
    spEncounter_Episodeofcare {http://hl7.org/fhir/SearchParameter/Encounterepisodeofcare},
    spEncounter_Identifier {http://hl7.org/fhir/SearchParameter/clinicalidentifier},
    spEncounter_Length {http://hl7.org/fhir/SearchParameter/Encounterlength},
    spEncounter_Location {http://hl7.org/fhir/SearchParameter/Encounterlocation},
    spEncounter_Locationperiod {http://hl7.org/fhir/SearchParameter/Encounterlocationperiod},
    spEncounter_Partof {http://hl7.org/fhir/SearchParameter/Encounterpartof},
    spEncounter_Participant {http://hl7.org/fhir/SearchParameter/Encounterparticipant},
    spEncounter_Participanttype {http://hl7.org/fhir/SearchParameter/Encounterparticipanttype},
    spEncounter_Patient {http://hl7.org/fhir/SearchParameter/clinicalpatient},
    spEncounter_Practitioner {http://hl7.org/fhir/SearchParameter/Encounterpractitioner},
    spEncounter_Reasoncode {http://hl7.org/fhir/SearchParameter/Encounterreasoncode},
    spEncounter_Reasonreference {http://hl7.org/fhir/SearchParameter/Encounterreasonreference},
    spEncounter_Serviceprovider {http://hl7.org/fhir/SearchParameter/Encounterserviceprovider},
    spEncounter_Specialarrangement {http://hl7.org/fhir/SearchParameter/Encounterspecialarrangement},
    spEncounter_Status {http://hl7.org/fhir/SearchParameter/Encounterstatus},
    spEncounter_Subject {http://hl7.org/fhir/SearchParameter/Encountersubject},
    spEncounter_Subjectstatus {http://hl7.org/fhir/SearchParameter/Encountersubjectstatus},
    spEncounter_Type {http://hl7.org/fhir/SearchParameter/clinicaltype});
{$ENDIF FHIR_ENCOUNTER}

{$IFDEF FHIR_ENDPOINT}
  // Search Parameters for Endpoint
  TSearchParamsEndpoint = (
    spEndpoint__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spEndpoint__filter {http://hl7.org/fhir/SearchParameter/filter},
    spEndpoint__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spEndpoint__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spEndpoint__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spEndpoint__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spEndpoint__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spEndpoint__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spEndpoint__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spEndpoint__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spEndpoint_Connectiontype {http://hl7.org/fhir/SearchParameter/Endpointconnectiontype},
    spEndpoint_Identifier {http://hl7.org/fhir/SearchParameter/Endpointidentifier},
    spEndpoint_Name {http://hl7.org/fhir/SearchParameter/Endpointname},
    spEndpoint_Organization {http://hl7.org/fhir/SearchParameter/Endpointorganization},
    spEndpoint_Payloadtype {http://hl7.org/fhir/SearchParameter/Endpointpayloadtype},
    spEndpoint_Status {http://hl7.org/fhir/SearchParameter/Endpointstatus});
{$ENDIF FHIR_ENDPOINT}

{$IFDEF FHIR_ENROLLMENTREQUEST}
  // Search Parameters for EnrollmentRequest
  TSearchParamsEnrollmentRequest = (
    spEnrollmentRequest__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spEnrollmentRequest__filter {http://hl7.org/fhir/SearchParameter/filter},
    spEnrollmentRequest__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spEnrollmentRequest__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spEnrollmentRequest__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spEnrollmentRequest__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spEnrollmentRequest__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spEnrollmentRequest__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spEnrollmentRequest__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spEnrollmentRequest__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spEnrollmentRequest_Identifier {http://hl7.org/fhir/SearchParameter/EnrollmentRequestidentifier},
    spEnrollmentRequest_Patient {http://hl7.org/fhir/SearchParameter/EnrollmentRequestpatient},
    spEnrollmentRequest_Status {http://hl7.org/fhir/SearchParameter/EnrollmentRequeststatus},
    spEnrollmentRequest_Subject {http://hl7.org/fhir/SearchParameter/EnrollmentRequestsubject});
{$ENDIF FHIR_ENROLLMENTREQUEST}

{$IFDEF FHIR_ENROLLMENTRESPONSE}
  // Search Parameters for EnrollmentResponse
  TSearchParamsEnrollmentResponse = (
    spEnrollmentResponse__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spEnrollmentResponse__filter {http://hl7.org/fhir/SearchParameter/filter},
    spEnrollmentResponse__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spEnrollmentResponse__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spEnrollmentResponse__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spEnrollmentResponse__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spEnrollmentResponse__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spEnrollmentResponse__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spEnrollmentResponse__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spEnrollmentResponse__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spEnrollmentResponse_Identifier {http://hl7.org/fhir/SearchParameter/EnrollmentResponseidentifier},
    spEnrollmentResponse_Request {http://hl7.org/fhir/SearchParameter/EnrollmentResponserequest},
    spEnrollmentResponse_Status {http://hl7.org/fhir/SearchParameter/EnrollmentResponsestatus});
{$ENDIF FHIR_ENROLLMENTRESPONSE}

{$IFDEF FHIR_EPISODEOFCARE}
  // Search Parameters for EpisodeOfCare
  TSearchParamsEpisodeOfCare = (
    spEpisodeOfCare__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spEpisodeOfCare__filter {http://hl7.org/fhir/SearchParameter/filter},
    spEpisodeOfCare__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spEpisodeOfCare__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spEpisodeOfCare__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spEpisodeOfCare__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spEpisodeOfCare__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spEpisodeOfCare__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spEpisodeOfCare__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spEpisodeOfCare__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spEpisodeOfCare_Caremanager {http://hl7.org/fhir/SearchParameter/EpisodeOfCarecaremanager},
    spEpisodeOfCare_Condition {http://hl7.org/fhir/SearchParameter/EpisodeOfCarecondition},
    spEpisodeOfCare_Date {http://hl7.org/fhir/SearchParameter/clinicaldate},
    spEpisodeOfCare_Identifier {http://hl7.org/fhir/SearchParameter/clinicalidentifier},
    spEpisodeOfCare_Incomingreferral {http://hl7.org/fhir/SearchParameter/EpisodeOfCareincomingreferral},
    spEpisodeOfCare_Organization {http://hl7.org/fhir/SearchParameter/EpisodeOfCareorganization},
    spEpisodeOfCare_Patient {http://hl7.org/fhir/SearchParameter/clinicalpatient},
    spEpisodeOfCare_Status {http://hl7.org/fhir/SearchParameter/EpisodeOfCarestatus},
    spEpisodeOfCare_Type {http://hl7.org/fhir/SearchParameter/clinicaltype});
{$ENDIF FHIR_EPISODEOFCARE}

{$IFDEF FHIR_EVENTDEFINITION}
  // Search Parameters for EventDefinition
  TSearchParamsEventDefinition = (
    spEventDefinition__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spEventDefinition__filter {http://hl7.org/fhir/SearchParameter/filter},
    spEventDefinition__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spEventDefinition__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spEventDefinition__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spEventDefinition__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spEventDefinition__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spEventDefinition__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spEventDefinition__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spEventDefinition__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spEventDefinition_Composedof {http://hl7.org/fhir/SearchParameter/EventDefinitioncomposedof},
    spEventDefinition_Context {http://hl7.org/fhir/SearchParameter/EventDefinitioncontext},
    spEventDefinition_Contextquantity {http://hl7.org/fhir/SearchParameter/EventDefinitioncontextquantity},
    spEventDefinition_Contexttype {http://hl7.org/fhir/SearchParameter/EventDefinitioncontexttype},
    spEventDefinition_Contexttypequantity {http://hl7.org/fhir/SearchParameter/EventDefinitioncontexttypequantity},
    spEventDefinition_Contexttypevalue {http://hl7.org/fhir/SearchParameter/EventDefinitioncontexttypevalue},
    spEventDefinition_Date {http://hl7.org/fhir/SearchParameter/EventDefinitiondate},
    spEventDefinition_Dependson {http://hl7.org/fhir/SearchParameter/EventDefinitiondependson},
    spEventDefinition_Derivedfrom {http://hl7.org/fhir/SearchParameter/EventDefinitionderivedfrom},
    spEventDefinition_Description {http://hl7.org/fhir/SearchParameter/EventDefinitiondescription},
    spEventDefinition_Effective {http://hl7.org/fhir/SearchParameter/EventDefinitioneffective},
    spEventDefinition_Identifier {http://hl7.org/fhir/SearchParameter/EventDefinitionidentifier},
    spEventDefinition_Jurisdiction {http://hl7.org/fhir/SearchParameter/EventDefinitionjurisdiction},
    spEventDefinition_Name {http://hl7.org/fhir/SearchParameter/EventDefinitionname},
    spEventDefinition_Predecessor {http://hl7.org/fhir/SearchParameter/EventDefinitionpredecessor},
    spEventDefinition_Publisher {http://hl7.org/fhir/SearchParameter/EventDefinitionpublisher},
    spEventDefinition_Status {http://hl7.org/fhir/SearchParameter/EventDefinitionstatus},
    spEventDefinition_Successor {http://hl7.org/fhir/SearchParameter/EventDefinitionsuccessor},
    spEventDefinition_Title {http://hl7.org/fhir/SearchParameter/EventDefinitiontitle},
    spEventDefinition_Topic {http://hl7.org/fhir/SearchParameter/EventDefinitiontopic},
    spEventDefinition_Url {http://hl7.org/fhir/SearchParameter/EventDefinitionurl},
    spEventDefinition_Version {http://hl7.org/fhir/SearchParameter/EventDefinitionversion});
{$ENDIF FHIR_EVENTDEFINITION}

{$IFDEF FHIR_EVIDENCE}
  // Search Parameters for Evidence
  TSearchParamsEvidence = (
    spEvidence__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spEvidence__filter {http://hl7.org/fhir/SearchParameter/filter},
    spEvidence__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spEvidence__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spEvidence__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spEvidence__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spEvidence__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spEvidence__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spEvidence__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spEvidence__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spEvidence_Context {http://hl7.org/fhir/SearchParameter/Evidencecontext},
    spEvidence_Contextquantity {http://hl7.org/fhir/SearchParameter/Evidencecontextquantity},
    spEvidence_Contexttype {http://hl7.org/fhir/SearchParameter/Evidencecontexttype},
    spEvidence_Contexttypequantity {http://hl7.org/fhir/SearchParameter/Evidencecontexttypequantity},
    spEvidence_Contexttypevalue {http://hl7.org/fhir/SearchParameter/Evidencecontexttypevalue},
    spEvidence_Date {http://hl7.org/fhir/SearchParameter/Evidencedate},
    spEvidence_Description {http://hl7.org/fhir/SearchParameter/Evidencedescription},
    spEvidence_Identifier {http://hl7.org/fhir/SearchParameter/Evidenceidentifier},
    spEvidence_Publisher {http://hl7.org/fhir/SearchParameter/Evidencepublisher},
    spEvidence_Status {http://hl7.org/fhir/SearchParameter/Evidencestatus},
    spEvidence_Title {http://hl7.org/fhir/SearchParameter/Evidencetitle},
    spEvidence_Url {http://hl7.org/fhir/SearchParameter/Evidenceurl},
    spEvidence_Version {http://hl7.org/fhir/SearchParameter/Evidenceversion});
{$ENDIF FHIR_EVIDENCE}

{$IFDEF FHIR_EVIDENCEREPORT}
  // Search Parameters for EvidenceReport
  TSearchParamsEvidenceReport = (
    spEvidenceReport__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spEvidenceReport__filter {http://hl7.org/fhir/SearchParameter/filter},
    spEvidenceReport__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spEvidenceReport__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spEvidenceReport__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spEvidenceReport__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spEvidenceReport__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spEvidenceReport__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spEvidenceReport__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spEvidenceReport__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spEvidenceReport_Context {http://hl7.org/fhir/SearchParameter/EvidenceReportcontext},
    spEvidenceReport_Contextquantity {http://hl7.org/fhir/SearchParameter/EvidenceReportcontextquantity},
    spEvidenceReport_Contexttype {http://hl7.org/fhir/SearchParameter/EvidenceReportcontexttype},
    spEvidenceReport_Contexttypequantity {http://hl7.org/fhir/SearchParameter/EvidenceReportcontexttypequantity},
    spEvidenceReport_Contexttypevalue {http://hl7.org/fhir/SearchParameter/EvidenceReportcontexttypevalue},
    spEvidenceReport_Identifier {http://hl7.org/fhir/SearchParameter/EvidenceReportidentifier},
    spEvidenceReport_Publisher {http://hl7.org/fhir/SearchParameter/EvidenceReportpublisher},
    spEvidenceReport_Status {http://hl7.org/fhir/SearchParameter/EvidenceReportstatus},
    spEvidenceReport_Url {http://hl7.org/fhir/SearchParameter/EvidenceReporturl});
{$ENDIF FHIR_EVIDENCEREPORT}

{$IFDEF FHIR_EVIDENCEVARIABLE}
  // Search Parameters for EvidenceVariable
  TSearchParamsEvidenceVariable = (
    spEvidenceVariable__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spEvidenceVariable__filter {http://hl7.org/fhir/SearchParameter/filter},
    spEvidenceVariable__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spEvidenceVariable__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spEvidenceVariable__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spEvidenceVariable__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spEvidenceVariable__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spEvidenceVariable__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spEvidenceVariable__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spEvidenceVariable__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spEvidenceVariable_Composedof {http://hl7.org/fhir/SearchParameter/EvidenceVariablecomposedof},
    spEvidenceVariable_Context {http://hl7.org/fhir/SearchParameter/EvidenceVariablecontext},
    spEvidenceVariable_Contextquantity {http://hl7.org/fhir/SearchParameter/EvidenceVariablecontextquantity},
    spEvidenceVariable_Contexttype {http://hl7.org/fhir/SearchParameter/EvidenceVariablecontexttype},
    spEvidenceVariable_Contexttypequantity {http://hl7.org/fhir/SearchParameter/EvidenceVariablecontexttypequantity},
    spEvidenceVariable_Contexttypevalue {http://hl7.org/fhir/SearchParameter/EvidenceVariablecontexttypevalue},
    spEvidenceVariable_Date {http://hl7.org/fhir/SearchParameter/EvidenceVariabledate},
    spEvidenceVariable_Dependson {http://hl7.org/fhir/SearchParameter/EvidenceVariabledependson},
    spEvidenceVariable_Derivedfrom {http://hl7.org/fhir/SearchParameter/EvidenceVariablederivedfrom},
    spEvidenceVariable_Description {http://hl7.org/fhir/SearchParameter/EvidenceVariabledescription},
    spEvidenceVariable_Identifier {http://hl7.org/fhir/SearchParameter/EvidenceVariableidentifier},
    spEvidenceVariable_Name {http://hl7.org/fhir/SearchParameter/EvidenceVariablename},
    spEvidenceVariable_Predecessor {http://hl7.org/fhir/SearchParameter/EvidenceVariablepredecessor},
    spEvidenceVariable_Publisher {http://hl7.org/fhir/SearchParameter/EvidenceVariablepublisher},
    spEvidenceVariable_Status {http://hl7.org/fhir/SearchParameter/EvidenceVariablestatus},
    spEvidenceVariable_Successor {http://hl7.org/fhir/SearchParameter/EvidenceVariablesuccessor},
    spEvidenceVariable_Title {http://hl7.org/fhir/SearchParameter/EvidenceVariabletitle},
    spEvidenceVariable_Topic {http://hl7.org/fhir/SearchParameter/EvidenceVariabletopic},
    spEvidenceVariable_Url {http://hl7.org/fhir/SearchParameter/EvidenceVariableurl},
    spEvidenceVariable_Version {http://hl7.org/fhir/SearchParameter/EvidenceVariableversion});
{$ENDIF FHIR_EVIDENCEVARIABLE}

{$IFDEF FHIR_EXAMPLESCENARIO}
  // Search Parameters for ExampleScenario
  TSearchParamsExampleScenario = (
    spExampleScenario__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spExampleScenario__filter {http://hl7.org/fhir/SearchParameter/filter},
    spExampleScenario__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spExampleScenario__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spExampleScenario__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spExampleScenario__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spExampleScenario__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spExampleScenario__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spExampleScenario__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spExampleScenario__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spExampleScenario_Context {http://hl7.org/fhir/SearchParameter/ExampleScenariocontext},
    spExampleScenario_Contextquantity {http://hl7.org/fhir/SearchParameter/ExampleScenariocontextquantity},
    spExampleScenario_Contexttype {http://hl7.org/fhir/SearchParameter/ExampleScenariocontexttype},
    spExampleScenario_Contexttypequantity {http://hl7.org/fhir/SearchParameter/ExampleScenariocontexttypequantity},
    spExampleScenario_Contexttypevalue {http://hl7.org/fhir/SearchParameter/ExampleScenariocontexttypevalue},
    spExampleScenario_Date {http://hl7.org/fhir/SearchParameter/ExampleScenariodate},
    spExampleScenario_Identifier {http://hl7.org/fhir/SearchParameter/ExampleScenarioidentifier},
    spExampleScenario_Jurisdiction {http://hl7.org/fhir/SearchParameter/ExampleScenariojurisdiction},
    spExampleScenario_Name {http://hl7.org/fhir/SearchParameter/ExampleScenarioname},
    spExampleScenario_Publisher {http://hl7.org/fhir/SearchParameter/ExampleScenariopublisher},
    spExampleScenario_Status {http://hl7.org/fhir/SearchParameter/ExampleScenariostatus},
    spExampleScenario_Url {http://hl7.org/fhir/SearchParameter/ExampleScenariourl},
    spExampleScenario_Version {http://hl7.org/fhir/SearchParameter/ExampleScenarioversion});
{$ENDIF FHIR_EXAMPLESCENARIO}

{$IFDEF FHIR_EXPLANATIONOFBENEFIT}
  // Search Parameters for ExplanationOfBenefit
  TSearchParamsExplanationOfBenefit = (
    spExplanationOfBenefit__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spExplanationOfBenefit__filter {http://hl7.org/fhir/SearchParameter/filter},
    spExplanationOfBenefit__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spExplanationOfBenefit__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spExplanationOfBenefit__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spExplanationOfBenefit__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spExplanationOfBenefit__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spExplanationOfBenefit__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spExplanationOfBenefit__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spExplanationOfBenefit__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spExplanationOfBenefit_Careteam {http://hl7.org/fhir/SearchParameter/ExplanationOfBenefitcareteam},
    spExplanationOfBenefit_Claim {http://hl7.org/fhir/SearchParameter/ExplanationOfBenefitclaim},
    spExplanationOfBenefit_Coverage {http://hl7.org/fhir/SearchParameter/ExplanationOfBenefitcoverage},
    spExplanationOfBenefit_Created {http://hl7.org/fhir/SearchParameter/ExplanationOfBenefitcreated},
    spExplanationOfBenefit_Detailudi {http://hl7.org/fhir/SearchParameter/ExplanationOfBenefitdetailudi},
    spExplanationOfBenefit_Disposition {http://hl7.org/fhir/SearchParameter/ExplanationOfBenefitdisposition},
    spExplanationOfBenefit_Encounter {http://hl7.org/fhir/SearchParameter/ExplanationOfBenefitencounter},
    spExplanationOfBenefit_Enterer {http://hl7.org/fhir/SearchParameter/ExplanationOfBenefitenterer},
    spExplanationOfBenefit_Facility {http://hl7.org/fhir/SearchParameter/ExplanationOfBenefitfacility},
    spExplanationOfBenefit_Identifier {http://hl7.org/fhir/SearchParameter/ExplanationOfBenefitidentifier},
    spExplanationOfBenefit_Itemudi {http://hl7.org/fhir/SearchParameter/ExplanationOfBenefititemudi},
    spExplanationOfBenefit_Patient {http://hl7.org/fhir/SearchParameter/ExplanationOfBenefitpatient},
    spExplanationOfBenefit_Payee {http://hl7.org/fhir/SearchParameter/ExplanationOfBenefitpayee},
    spExplanationOfBenefit_Procedureudi {http://hl7.org/fhir/SearchParameter/ExplanationOfBenefitprocedureudi},
    spExplanationOfBenefit_Provider {http://hl7.org/fhir/SearchParameter/ExplanationOfBenefitprovider},
    spExplanationOfBenefit_Status {http://hl7.org/fhir/SearchParameter/ExplanationOfBenefitstatus},
    spExplanationOfBenefit_Subdetailudi {http://hl7.org/fhir/SearchParameter/ExplanationOfBenefitsubdetailudi});
{$ENDIF FHIR_EXPLANATIONOFBENEFIT}

{$IFDEF FHIR_FAMILYMEMBERHISTORY}
  // Search Parameters for FamilyMemberHistory
  TSearchParamsFamilyMemberHistory = (
    spFamilyMemberHistory__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spFamilyMemberHistory__filter {http://hl7.org/fhir/SearchParameter/filter},
    spFamilyMemberHistory__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spFamilyMemberHistory__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spFamilyMemberHistory__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spFamilyMemberHistory__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spFamilyMemberHistory__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spFamilyMemberHistory__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spFamilyMemberHistory__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spFamilyMemberHistory__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spFamilyMemberHistory_Code {http://hl7.org/fhir/SearchParameter/clinicalcode},
    spFamilyMemberHistory_Date {http://hl7.org/fhir/SearchParameter/clinicaldate},
    spFamilyMemberHistory_Identifier {http://hl7.org/fhir/SearchParameter/clinicalidentifier},
    spFamilyMemberHistory_Instantiatescanonical {http://hl7.org/fhir/SearchParameter/FamilyMemberHistoryinstantiatescanonical},
    spFamilyMemberHistory_Instantiatesuri {http://hl7.org/fhir/SearchParameter/FamilyMemberHistoryinstantiatesuri},
    spFamilyMemberHistory_Patient {http://hl7.org/fhir/SearchParameter/clinicalpatient},
    spFamilyMemberHistory_Relationship {http://hl7.org/fhir/SearchParameter/FamilyMemberHistoryrelationship},
    spFamilyMemberHistory_Sex {http://hl7.org/fhir/SearchParameter/FamilyMemberHistorysex},
    spFamilyMemberHistory_Status {http://hl7.org/fhir/SearchParameter/FamilyMemberHistorystatus});
{$ENDIF FHIR_FAMILYMEMBERHISTORY}

{$IFDEF FHIR_FLAG}
  // Search Parameters for Flag
  TSearchParamsFlag = (
    spFlag__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spFlag__filter {http://hl7.org/fhir/SearchParameter/filter},
    spFlag__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spFlag__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spFlag__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spFlag__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spFlag__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spFlag__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spFlag__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spFlag__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spFlag_Author {http://hl7.org/fhir/SearchParameter/Flagauthor},
    spFlag_Date {http://hl7.org/fhir/SearchParameter/clinicaldate},
    spFlag_Encounter {http://hl7.org/fhir/SearchParameter/clinicalencounter},
    spFlag_Identifier {http://hl7.org/fhir/SearchParameter/Flagidentifier},
    spFlag_Patient {http://hl7.org/fhir/SearchParameter/clinicalpatient},
    spFlag_Status {http://hl7.org/fhir/SearchParameter/Flagstatus},
    spFlag_Subject {http://hl7.org/fhir/SearchParameter/Flagsubject});
{$ENDIF FHIR_FLAG}

{$IFDEF FHIR_GOAL}
  // Search Parameters for Goal
  TSearchParamsGoal = (
    spGoal__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spGoal__filter {http://hl7.org/fhir/SearchParameter/filter},
    spGoal__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spGoal__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spGoal__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spGoal__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spGoal__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spGoal__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spGoal__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spGoal__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spGoal_Achievementstatus {http://hl7.org/fhir/SearchParameter/Goalachievementstatus},
    spGoal_Category {http://hl7.org/fhir/SearchParameter/Goalcategory},
    spGoal_Identifier {http://hl7.org/fhir/SearchParameter/clinicalidentifier},
    spGoal_Lifecyclestatus {http://hl7.org/fhir/SearchParameter/Goallifecyclestatus},
    spGoal_Patient {http://hl7.org/fhir/SearchParameter/clinicalpatient},
    spGoal_Startdate {http://hl7.org/fhir/SearchParameter/Goalstartdate},
    spGoal_Subject {http://hl7.org/fhir/SearchParameter/Goalsubject},
    spGoal_Targetdate {http://hl7.org/fhir/SearchParameter/Goaltargetdate});
{$ENDIF FHIR_GOAL}

{$IFDEF FHIR_GRAPHDEFINITION}
  // Search Parameters for GraphDefinition
  TSearchParamsGraphDefinition = (
    spGraphDefinition__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spGraphDefinition__filter {http://hl7.org/fhir/SearchParameter/filter},
    spGraphDefinition__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spGraphDefinition__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spGraphDefinition__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spGraphDefinition__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spGraphDefinition__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spGraphDefinition__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spGraphDefinition__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spGraphDefinition__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spGraphDefinition_Context {http://hl7.org/fhir/SearchParameter/conformancecontext},
    spGraphDefinition_Contextquantity {http://hl7.org/fhir/SearchParameter/conformancecontextquantity},
    spGraphDefinition_Contexttype {http://hl7.org/fhir/SearchParameter/conformancecontexttype},
    spGraphDefinition_Contexttypequantity {http://hl7.org/fhir/SearchParameter/conformancecontexttypequantity},
    spGraphDefinition_Contexttypevalue {http://hl7.org/fhir/SearchParameter/conformancecontexttypevalue},
    spGraphDefinition_Date {http://hl7.org/fhir/SearchParameter/conformancedate},
    spGraphDefinition_Description {http://hl7.org/fhir/SearchParameter/conformancedescription},
    spGraphDefinition_Jurisdiction {http://hl7.org/fhir/SearchParameter/conformancejurisdiction},
    spGraphDefinition_Name {http://hl7.org/fhir/SearchParameter/conformancename},
    spGraphDefinition_Publisher {http://hl7.org/fhir/SearchParameter/conformancepublisher},
    spGraphDefinition_Start {http://hl7.org/fhir/SearchParameter/GraphDefinitionstart},
    spGraphDefinition_Status {http://hl7.org/fhir/SearchParameter/conformancestatus},
    spGraphDefinition_Url {http://hl7.org/fhir/SearchParameter/conformanceurl},
    spGraphDefinition_Version {http://hl7.org/fhir/SearchParameter/conformanceversion});
{$ENDIF FHIR_GRAPHDEFINITION}

{$IFDEF FHIR_GROUP}
  // Search Parameters for Group
  TSearchParamsGroup = (
    spGroup__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spGroup__filter {http://hl7.org/fhir/SearchParameter/filter},
    spGroup__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spGroup__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spGroup__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spGroup__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spGroup__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spGroup__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spGroup__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spGroup__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spGroup_Actual {http://hl7.org/fhir/SearchParameter/Groupactual},
    spGroup_Characteristic {http://hl7.org/fhir/SearchParameter/Groupcharacteristic},
    spGroup_Characteristicvalue {http://hl7.org/fhir/SearchParameter/Groupcharacteristicvalue},
    spGroup_Code {http://hl7.org/fhir/SearchParameter/Groupcode},
    spGroup_Exclude {http://hl7.org/fhir/SearchParameter/Groupexclude},
    spGroup_Identifier {http://hl7.org/fhir/SearchParameter/Groupidentifier},
    spGroup_Managingentity {http://hl7.org/fhir/SearchParameter/Groupmanagingentity},
    spGroup_Member {http://hl7.org/fhir/SearchParameter/Groupmember},
    spGroup_Type {http://hl7.org/fhir/SearchParameter/Grouptype},
    spGroup_Value {http://hl7.org/fhir/SearchParameter/Groupvalue});
{$ENDIF FHIR_GROUP}

{$IFDEF FHIR_GUIDANCERESPONSE}
  // Search Parameters for GuidanceResponse
  TSearchParamsGuidanceResponse = (
    spGuidanceResponse__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spGuidanceResponse__filter {http://hl7.org/fhir/SearchParameter/filter},
    spGuidanceResponse__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spGuidanceResponse__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spGuidanceResponse__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spGuidanceResponse__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spGuidanceResponse__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spGuidanceResponse__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spGuidanceResponse__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spGuidanceResponse__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spGuidanceResponse_Identifier {http://hl7.org/fhir/SearchParameter/GuidanceResponseidentifier},
    spGuidanceResponse_Patient {http://hl7.org/fhir/SearchParameter/GuidanceResponsepatient},
    spGuidanceResponse_Request {http://hl7.org/fhir/SearchParameter/GuidanceResponserequest},
    spGuidanceResponse_Subject {http://hl7.org/fhir/SearchParameter/GuidanceResponsesubject});
{$ENDIF FHIR_GUIDANCERESPONSE}

{$IFDEF FHIR_HEALTHCARESERVICE}
  // Search Parameters for HealthcareService
  TSearchParamsHealthcareService = (
    spHealthcareService__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spHealthcareService__filter {http://hl7.org/fhir/SearchParameter/filter},
    spHealthcareService__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spHealthcareService__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spHealthcareService__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spHealthcareService__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spHealthcareService__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spHealthcareService__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spHealthcareService__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spHealthcareService__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spHealthcareService_Active {http://hl7.org/fhir/SearchParameter/HealthcareServiceactive},
    spHealthcareService_Characteristic {http://hl7.org/fhir/SearchParameter/HealthcareServicecharacteristic},
    spHealthcareService_Coveragearea {http://hl7.org/fhir/SearchParameter/HealthcareServicecoveragearea},
    spHealthcareService_Endpoint {http://hl7.org/fhir/SearchParameter/HealthcareServiceendpoint},
    spHealthcareService_Identifier {http://hl7.org/fhir/SearchParameter/HealthcareServiceidentifier},
    spHealthcareService_Location {http://hl7.org/fhir/SearchParameter/HealthcareServicelocation},
    spHealthcareService_Name {http://hl7.org/fhir/SearchParameter/HealthcareServicename},
    spHealthcareService_Organization {http://hl7.org/fhir/SearchParameter/HealthcareServiceorganization},
    spHealthcareService_Program {http://hl7.org/fhir/SearchParameter/HealthcareServiceprogram},
    spHealthcareService_Servicecategory {http://hl7.org/fhir/SearchParameter/HealthcareServiceservicecategory},
    spHealthcareService_Servicetype {http://hl7.org/fhir/SearchParameter/HealthcareServiceservicetype},
    spHealthcareService_Specialty {http://hl7.org/fhir/SearchParameter/HealthcareServicespecialty});
{$ENDIF FHIR_HEALTHCARESERVICE}

{$IFDEF FHIR_IMAGINGSTUDY}
  // Search Parameters for ImagingStudy
  TSearchParamsImagingStudy = (
    spImagingStudy__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spImagingStudy__filter {http://hl7.org/fhir/SearchParameter/filter},
    spImagingStudy__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spImagingStudy__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spImagingStudy__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spImagingStudy__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spImagingStudy__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spImagingStudy__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spImagingStudy__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spImagingStudy__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spImagingStudy_Basedon {http://hl7.org/fhir/SearchParameter/ImagingStudybasedon},
    spImagingStudy_Bodysite {http://hl7.org/fhir/SearchParameter/ImagingStudybodysite},
    spImagingStudy_Dicomclass {http://hl7.org/fhir/SearchParameter/ImagingStudydicomclass},
    spImagingStudy_Encounter {http://hl7.org/fhir/SearchParameter/ImagingStudyencounter},
    spImagingStudy_Endpoint {http://hl7.org/fhir/SearchParameter/ImagingStudyendpoint},
    spImagingStudy_Identifier {http://hl7.org/fhir/SearchParameter/clinicalidentifier},
    spImagingStudy_Instance {http://hl7.org/fhir/SearchParameter/ImagingStudyinstance},
    spImagingStudy_Interpreter {http://hl7.org/fhir/SearchParameter/ImagingStudyinterpreter},
    spImagingStudy_Modality {http://hl7.org/fhir/SearchParameter/ImagingStudymodality},
    spImagingStudy_Patient {http://hl7.org/fhir/SearchParameter/clinicalpatient},
    spImagingStudy_Performer {http://hl7.org/fhir/SearchParameter/ImagingStudyperformer},
    spImagingStudy_Reason {http://hl7.org/fhir/SearchParameter/ImagingStudyreason},
    spImagingStudy_Referrer {http://hl7.org/fhir/SearchParameter/ImagingStudyreferrer},
    spImagingStudy_Series {http://hl7.org/fhir/SearchParameter/ImagingStudyseries},
    spImagingStudy_Started {http://hl7.org/fhir/SearchParameter/ImagingStudystarted},
    spImagingStudy_Status {http://hl7.org/fhir/SearchParameter/ImagingStudystatus},
    spImagingStudy_Subject {http://hl7.org/fhir/SearchParameter/ImagingStudysubject});
{$ENDIF FHIR_IMAGINGSTUDY}

{$IFDEF FHIR_IMMUNIZATION}
  // Search Parameters for Immunization
  TSearchParamsImmunization = (
    spImmunization__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spImmunization__filter {http://hl7.org/fhir/SearchParameter/filter},
    spImmunization__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spImmunization__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spImmunization__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spImmunization__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spImmunization__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spImmunization__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spImmunization__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spImmunization__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spImmunization_Date {http://hl7.org/fhir/SearchParameter/clinicaldate},
    spImmunization_Identifier {http://hl7.org/fhir/SearchParameter/clinicalidentifier},
    spImmunization_Location {http://hl7.org/fhir/SearchParameter/Immunizationlocation},
    spImmunization_Lotnumber {http://hl7.org/fhir/SearchParameter/Immunizationlotnumber},
    spImmunization_Manufacturer {http://hl7.org/fhir/SearchParameter/Immunizationmanufacturer},
    spImmunization_Patient {http://hl7.org/fhir/SearchParameter/clinicalpatient},
    spImmunization_Performer {http://hl7.org/fhir/SearchParameter/Immunizationperformer},
    spImmunization_Reaction {http://hl7.org/fhir/SearchParameter/Immunizationreaction},
    spImmunization_Reactiondate {http://hl7.org/fhir/SearchParameter/Immunizationreactiondate},
    spImmunization_Reasoncode {http://hl7.org/fhir/SearchParameter/Immunizationreasoncode},
    spImmunization_Reasonreference {http://hl7.org/fhir/SearchParameter/Immunizationreasonreference},
    spImmunization_Series {http://hl7.org/fhir/SearchParameter/Immunizationseries},
    spImmunization_Status {http://hl7.org/fhir/SearchParameter/Immunizationstatus},
    spImmunization_Statusreason {http://hl7.org/fhir/SearchParameter/Immunizationstatusreason},
    spImmunization_Targetdisease {http://hl7.org/fhir/SearchParameter/Immunizationtargetdisease},
    spImmunization_Vaccinecode {http://hl7.org/fhir/SearchParameter/Immunizationvaccinecode});
{$ENDIF FHIR_IMMUNIZATION}

{$IFDEF FHIR_IMMUNIZATIONEVALUATION}
  // Search Parameters for ImmunizationEvaluation
  TSearchParamsImmunizationEvaluation = (
    spImmunizationEvaluation__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spImmunizationEvaluation__filter {http://hl7.org/fhir/SearchParameter/filter},
    spImmunizationEvaluation__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spImmunizationEvaluation__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spImmunizationEvaluation__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spImmunizationEvaluation__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spImmunizationEvaluation__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spImmunizationEvaluation__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spImmunizationEvaluation__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spImmunizationEvaluation__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spImmunizationEvaluation_Date {http://hl7.org/fhir/SearchParameter/ImmunizationEvaluationdate},
    spImmunizationEvaluation_Dosestatus {http://hl7.org/fhir/SearchParameter/ImmunizationEvaluationdosestatus},
    spImmunizationEvaluation_Identifier {http://hl7.org/fhir/SearchParameter/ImmunizationEvaluationidentifier},
    spImmunizationEvaluation_Immunizationevent {http://hl7.org/fhir/SearchParameter/ImmunizationEvaluationimmunizationevent},
    spImmunizationEvaluation_Patient {http://hl7.org/fhir/SearchParameter/ImmunizationEvaluationpatient},
    spImmunizationEvaluation_Status {http://hl7.org/fhir/SearchParameter/ImmunizationEvaluationstatus},
    spImmunizationEvaluation_Targetdisease {http://hl7.org/fhir/SearchParameter/ImmunizationEvaluationtargetdisease});
{$ENDIF FHIR_IMMUNIZATIONEVALUATION}

{$IFDEF FHIR_IMMUNIZATIONRECOMMENDATION}
  // Search Parameters for ImmunizationRecommendation
  TSearchParamsImmunizationRecommendation = (
    spImmunizationRecommendation__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spImmunizationRecommendation__filter {http://hl7.org/fhir/SearchParameter/filter},
    spImmunizationRecommendation__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spImmunizationRecommendation__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spImmunizationRecommendation__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spImmunizationRecommendation__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spImmunizationRecommendation__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spImmunizationRecommendation__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spImmunizationRecommendation__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spImmunizationRecommendation__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spImmunizationRecommendation_Date {http://hl7.org/fhir/SearchParameter/ImmunizationRecommendationdate},
    spImmunizationRecommendation_Identifier {http://hl7.org/fhir/SearchParameter/ImmunizationRecommendationidentifier},
    spImmunizationRecommendation_Information {http://hl7.org/fhir/SearchParameter/ImmunizationRecommendationinformation},
    spImmunizationRecommendation_Patient {http://hl7.org/fhir/SearchParameter/ImmunizationRecommendationpatient},
    spImmunizationRecommendation_Status {http://hl7.org/fhir/SearchParameter/ImmunizationRecommendationstatus},
    spImmunizationRecommendation_Support {http://hl7.org/fhir/SearchParameter/ImmunizationRecommendationsupport},
    spImmunizationRecommendation_Targetdisease {http://hl7.org/fhir/SearchParameter/ImmunizationRecommendationtargetdisease},
    spImmunizationRecommendation_Vaccinetype {http://hl7.org/fhir/SearchParameter/ImmunizationRecommendationvaccinetype});
{$ENDIF FHIR_IMMUNIZATIONRECOMMENDATION}

{$IFDEF FHIR_IMPLEMENTATIONGUIDE}
  // Search Parameters for ImplementationGuide
  TSearchParamsImplementationGuide = (
    spImplementationGuide__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spImplementationGuide__filter {http://hl7.org/fhir/SearchParameter/filter},
    spImplementationGuide__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spImplementationGuide__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spImplementationGuide__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spImplementationGuide__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spImplementationGuide__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spImplementationGuide__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spImplementationGuide__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spImplementationGuide__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spImplementationGuide_Context {http://hl7.org/fhir/SearchParameter/conformancecontext},
    spImplementationGuide_Contextquantity {http://hl7.org/fhir/SearchParameter/conformancecontextquantity},
    spImplementationGuide_Contexttype {http://hl7.org/fhir/SearchParameter/conformancecontexttype},
    spImplementationGuide_Contexttypequantity {http://hl7.org/fhir/SearchParameter/conformancecontexttypequantity},
    spImplementationGuide_Contexttypevalue {http://hl7.org/fhir/SearchParameter/conformancecontexttypevalue},
    spImplementationGuide_Date {http://hl7.org/fhir/SearchParameter/conformancedate},
    spImplementationGuide_Dependson {http://hl7.org/fhir/SearchParameter/ImplementationGuidedependson},
    spImplementationGuide_Description {http://hl7.org/fhir/SearchParameter/conformancedescription},
    spImplementationGuide_Experimental {http://hl7.org/fhir/SearchParameter/ImplementationGuideexperimental},
    spImplementationGuide_Global {http://hl7.org/fhir/SearchParameter/ImplementationGuideglobal},
    spImplementationGuide_Jurisdiction {http://hl7.org/fhir/SearchParameter/conformancejurisdiction},
    spImplementationGuide_Name {http://hl7.org/fhir/SearchParameter/conformancename},
    spImplementationGuide_Publisher {http://hl7.org/fhir/SearchParameter/conformancepublisher},
    spImplementationGuide_Resource {http://hl7.org/fhir/SearchParameter/ImplementationGuideresource},
    spImplementationGuide_Status {http://hl7.org/fhir/SearchParameter/conformancestatus},
    spImplementationGuide_Title {http://hl7.org/fhir/SearchParameter/conformancetitle},
    spImplementationGuide_Url {http://hl7.org/fhir/SearchParameter/conformanceurl},
    spImplementationGuide_Version {http://hl7.org/fhir/SearchParameter/conformanceversion});
{$ENDIF FHIR_IMPLEMENTATIONGUIDE}

{$IFDEF FHIR_INGREDIENT}
  // Search Parameters for Ingredient
  TSearchParamsIngredient = (
    spIngredient__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spIngredient__filter {http://hl7.org/fhir/SearchParameter/filter},
    spIngredient__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spIngredient__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spIngredient__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spIngredient__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spIngredient__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spIngredient__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spIngredient__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spIngredient__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spIngredient_Function {http://hl7.org/fhir/SearchParameter/Ingredientfunction},
    spIngredient_Identifier {http://hl7.org/fhir/SearchParameter/Ingredientidentifier},
    spIngredient_Manufacturer {http://hl7.org/fhir/SearchParameter/Ingredientmanufacturer},
    spIngredient_Role {http://hl7.org/fhir/SearchParameter/Ingredientrole},
    spIngredient_Specifiedsubstance {http://hl7.org/fhir/SearchParameter/Ingredientspecifiedsubstance},
    spIngredient_Specifiedsubstancecode {http://hl7.org/fhir/SearchParameter/Ingredientspecifiedsubstancecode},
    spIngredient_Specifiedsubstancedefinition {http://hl7.org/fhir/SearchParameter/Ingredientspecifiedsubstancedefinition},
    spIngredient_Substance {http://hl7.org/fhir/SearchParameter/Ingredientsubstance},
    spIngredient_Substancecode {http://hl7.org/fhir/SearchParameter/Ingredientsubstancecode},
    spIngredient_Substancedefinition {http://hl7.org/fhir/SearchParameter/Ingredientsubstancedefinition});
{$ENDIF FHIR_INGREDIENT}

{$IFDEF FHIR_INSURANCEPLAN}
  // Search Parameters for InsurancePlan
  TSearchParamsInsurancePlan = (
    spInsurancePlan__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spInsurancePlan__filter {http://hl7.org/fhir/SearchParameter/filter},
    spInsurancePlan__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spInsurancePlan__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spInsurancePlan__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spInsurancePlan__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spInsurancePlan__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spInsurancePlan__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spInsurancePlan__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spInsurancePlan__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spInsurancePlan_Address {http://hl7.org/fhir/SearchParameter/InsurancePlanaddress},
    spInsurancePlan_Addresscity {http://hl7.org/fhir/SearchParameter/InsurancePlanaddresscity},
    spInsurancePlan_Addresscountry {http://hl7.org/fhir/SearchParameter/InsurancePlanaddresscountry},
    spInsurancePlan_Addresspostalcode {http://hl7.org/fhir/SearchParameter/InsurancePlanaddresspostalcode},
    spInsurancePlan_Addressstate {http://hl7.org/fhir/SearchParameter/InsurancePlanaddressstate},
    spInsurancePlan_Addressuse {http://hl7.org/fhir/SearchParameter/InsurancePlanaddressuse},
    spInsurancePlan_Administeredby {http://hl7.org/fhir/SearchParameter/InsurancePlanadministeredby},
    spInsurancePlan_Endpoint {http://hl7.org/fhir/SearchParameter/InsurancePlanendpoint},
    spInsurancePlan_Identifier {http://hl7.org/fhir/SearchParameter/InsurancePlanidentifier},
    spInsurancePlan_Name {http://hl7.org/fhir/SearchParameter/InsurancePlanname},
    spInsurancePlan_Ownedby {http://hl7.org/fhir/SearchParameter/InsurancePlanownedby},
    spInsurancePlan_Phonetic {http://hl7.org/fhir/SearchParameter/InsurancePlanphonetic},
    spInsurancePlan_Status {http://hl7.org/fhir/SearchParameter/InsurancePlanstatus},
    spInsurancePlan_Type {http://hl7.org/fhir/SearchParameter/InsurancePlantype});
{$ENDIF FHIR_INSURANCEPLAN}

{$IFDEF FHIR_INVOICE}
  // Search Parameters for Invoice
  TSearchParamsInvoice = (
    spInvoice__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spInvoice__filter {http://hl7.org/fhir/SearchParameter/filter},
    spInvoice__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spInvoice__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spInvoice__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spInvoice__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spInvoice__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spInvoice__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spInvoice__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spInvoice__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spInvoice_Account {http://hl7.org/fhir/SearchParameter/Invoiceaccount},
    spInvoice_Date {http://hl7.org/fhir/SearchParameter/Invoicedate},
    spInvoice_Identifier {http://hl7.org/fhir/SearchParameter/Invoiceidentifier},
    spInvoice_Issuer {http://hl7.org/fhir/SearchParameter/Invoiceissuer},
    spInvoice_Participant {http://hl7.org/fhir/SearchParameter/Invoiceparticipant},
    spInvoice_Participantrole {http://hl7.org/fhir/SearchParameter/Invoiceparticipantrole},
    spInvoice_Patient {http://hl7.org/fhir/SearchParameter/Invoicepatient},
    spInvoice_Recipient {http://hl7.org/fhir/SearchParameter/Invoicerecipient},
    spInvoice_Status {http://hl7.org/fhir/SearchParameter/Invoicestatus},
    spInvoice_Subject {http://hl7.org/fhir/SearchParameter/Invoicesubject},
    spInvoice_Totalgross {http://hl7.org/fhir/SearchParameter/Invoicetotalgross},
    spInvoice_Totalnet {http://hl7.org/fhir/SearchParameter/Invoicetotalnet},
    spInvoice_Type {http://hl7.org/fhir/SearchParameter/Invoicetype});
{$ENDIF FHIR_INVOICE}

{$IFDEF FHIR_LIBRARY}
  // Search Parameters for Library
  TSearchParamsLibrary = (
    spLibrary__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spLibrary__filter {http://hl7.org/fhir/SearchParameter/filter},
    spLibrary__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spLibrary__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spLibrary__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spLibrary__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spLibrary__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spLibrary__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spLibrary__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spLibrary__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spLibrary_Composedof {http://hl7.org/fhir/SearchParameter/Librarycomposedof},
    spLibrary_Contenttype {http://hl7.org/fhir/SearchParameter/Librarycontenttype},
    spLibrary_Context {http://hl7.org/fhir/SearchParameter/Librarycontext},
    spLibrary_Contextquantity {http://hl7.org/fhir/SearchParameter/Librarycontextquantity},
    spLibrary_Contexttype {http://hl7.org/fhir/SearchParameter/Librarycontexttype},
    spLibrary_Contexttypequantity {http://hl7.org/fhir/SearchParameter/Librarycontexttypequantity},
    spLibrary_Contexttypevalue {http://hl7.org/fhir/SearchParameter/Librarycontexttypevalue},
    spLibrary_Date {http://hl7.org/fhir/SearchParameter/Librarydate},
    spLibrary_Dependson {http://hl7.org/fhir/SearchParameter/Librarydependson},
    spLibrary_Derivedfrom {http://hl7.org/fhir/SearchParameter/Libraryderivedfrom},
    spLibrary_Description {http://hl7.org/fhir/SearchParameter/Librarydescription},
    spLibrary_Effective {http://hl7.org/fhir/SearchParameter/Libraryeffective},
    spLibrary_Identifier {http://hl7.org/fhir/SearchParameter/Libraryidentifier},
    spLibrary_Jurisdiction {http://hl7.org/fhir/SearchParameter/Libraryjurisdiction},
    spLibrary_Name {http://hl7.org/fhir/SearchParameter/Libraryname},
    spLibrary_Predecessor {http://hl7.org/fhir/SearchParameter/Librarypredecessor},
    spLibrary_Publisher {http://hl7.org/fhir/SearchParameter/Librarypublisher},
    spLibrary_Status {http://hl7.org/fhir/SearchParameter/Librarystatus},
    spLibrary_Successor {http://hl7.org/fhir/SearchParameter/Librarysuccessor},
    spLibrary_Title {http://hl7.org/fhir/SearchParameter/Librarytitle},
    spLibrary_Topic {http://hl7.org/fhir/SearchParameter/Librarytopic},
    spLibrary_Type {http://hl7.org/fhir/SearchParameter/Librarytype},
    spLibrary_Url {http://hl7.org/fhir/SearchParameter/Libraryurl},
    spLibrary_Version {http://hl7.org/fhir/SearchParameter/Libraryversion});
{$ENDIF FHIR_LIBRARY}

{$IFDEF FHIR_LINKAGE}
  // Search Parameters for Linkage
  TSearchParamsLinkage = (
    spLinkage__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spLinkage__filter {http://hl7.org/fhir/SearchParameter/filter},
    spLinkage__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spLinkage__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spLinkage__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spLinkage__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spLinkage__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spLinkage__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spLinkage__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spLinkage__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spLinkage_Author {http://hl7.org/fhir/SearchParameter/Linkageauthor},
    spLinkage_Item {http://hl7.org/fhir/SearchParameter/Linkageitem},
    spLinkage_Source {http://hl7.org/fhir/SearchParameter/Linkagesource});
{$ENDIF FHIR_LINKAGE}

{$IFDEF FHIR_LIST}
  // Search Parameters for List
  TSearchParamsList = (
    spList__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spList__filter {http://hl7.org/fhir/SearchParameter/filter},
    spList__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spList__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spList__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spList__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spList__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spList__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spList__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spList__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spList_Code {http://hl7.org/fhir/SearchParameter/clinicalcode},
    spList_Date {http://hl7.org/fhir/SearchParameter/clinicaldate},
    spList_Emptyreason {http://hl7.org/fhir/SearchParameter/Listemptyreason},
    spList_Encounter {http://hl7.org/fhir/SearchParameter/clinicalencounter},
    spList_Identifier {http://hl7.org/fhir/SearchParameter/clinicalidentifier},
    spList_Item {http://hl7.org/fhir/SearchParameter/Listitem},
    spList_Notes {http://hl7.org/fhir/SearchParameter/Listnotes},
    spList_Patient {http://hl7.org/fhir/SearchParameter/clinicalpatient},
    spList_Source {http://hl7.org/fhir/SearchParameter/Listsource},
    spList_Status {http://hl7.org/fhir/SearchParameter/Liststatus},
    spList_Subject {http://hl7.org/fhir/SearchParameter/Listsubject},
    spList_Title {http://hl7.org/fhir/SearchParameter/Listtitle});
{$ENDIF FHIR_LIST}

{$IFDEF FHIR_LOCATION}
  // Search Parameters for Location
  TSearchParamsLocation = (
    spLocation__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spLocation__filter {http://hl7.org/fhir/SearchParameter/filter},
    spLocation__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spLocation__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spLocation__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spLocation__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spLocation__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spLocation__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spLocation__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spLocation__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spLocation_Address {http://hl7.org/fhir/SearchParameter/Locationaddress},
    spLocation_Addresscity {http://hl7.org/fhir/SearchParameter/Locationaddresscity},
    spLocation_Addresscountry {http://hl7.org/fhir/SearchParameter/Locationaddresscountry},
    spLocation_Addresspostalcode {http://hl7.org/fhir/SearchParameter/Locationaddresspostalcode},
    spLocation_Addressstate {http://hl7.org/fhir/SearchParameter/Locationaddressstate},
    spLocation_Addressuse {http://hl7.org/fhir/SearchParameter/Locationaddressuse},
    spLocation_Endpoint {http://hl7.org/fhir/SearchParameter/Locationendpoint},
    spLocation_Identifier {http://hl7.org/fhir/SearchParameter/Locationidentifier},
    spLocation_Name {http://hl7.org/fhir/SearchParameter/Locationname},
    spLocation_Near {http://hl7.org/fhir/SearchParameter/Locationnear},
    spLocation_Operationalstatus {http://hl7.org/fhir/SearchParameter/Locationoperationalstatus},
    spLocation_Organization {http://hl7.org/fhir/SearchParameter/Locationorganization},
    spLocation_Partof {http://hl7.org/fhir/SearchParameter/Locationpartof},
    spLocation_Status {http://hl7.org/fhir/SearchParameter/Locationstatus},
    spLocation_Type {http://hl7.org/fhir/SearchParameter/Locationtype});
{$ENDIF FHIR_LOCATION}

{$IFDEF FHIR_MANUFACTUREDITEMDEFINITION}
  // Search Parameters for ManufacturedItemDefinition
  TSearchParamsManufacturedItemDefinition = (
    spManufacturedItemDefinition__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spManufacturedItemDefinition__filter {http://hl7.org/fhir/SearchParameter/filter},
    spManufacturedItemDefinition__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spManufacturedItemDefinition__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spManufacturedItemDefinition__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spManufacturedItemDefinition__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spManufacturedItemDefinition__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spManufacturedItemDefinition__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spManufacturedItemDefinition__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spManufacturedItemDefinition__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spManufacturedItemDefinition_Doseform {http://hl7.org/fhir/SearchParameter/ManufacturedItemDefinitiondoseform},
    spManufacturedItemDefinition_Identifier {http://hl7.org/fhir/SearchParameter/ManufacturedItemDefinitionidentifier},
    spManufacturedItemDefinition_Ingredient {http://hl7.org/fhir/SearchParameter/ManufacturedItemDefinitioningredient});
{$ENDIF FHIR_MANUFACTUREDITEMDEFINITION}

{$IFDEF FHIR_MEASURE}
  // Search Parameters for Measure
  TSearchParamsMeasure = (
    spMeasure__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spMeasure__filter {http://hl7.org/fhir/SearchParameter/filter},
    spMeasure__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spMeasure__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spMeasure__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spMeasure__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spMeasure__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spMeasure__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spMeasure__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spMeasure__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spMeasure_Composedof {http://hl7.org/fhir/SearchParameter/Measurecomposedof},
    spMeasure_Context {http://hl7.org/fhir/SearchParameter/Measurecontext},
    spMeasure_Contextquantity {http://hl7.org/fhir/SearchParameter/Measurecontextquantity},
    spMeasure_Contexttype {http://hl7.org/fhir/SearchParameter/Measurecontexttype},
    spMeasure_Contexttypequantity {http://hl7.org/fhir/SearchParameter/Measurecontexttypequantity},
    spMeasure_Contexttypevalue {http://hl7.org/fhir/SearchParameter/Measurecontexttypevalue},
    spMeasure_Date {http://hl7.org/fhir/SearchParameter/Measuredate},
    spMeasure_Dependson {http://hl7.org/fhir/SearchParameter/Measuredependson},
    spMeasure_Derivedfrom {http://hl7.org/fhir/SearchParameter/Measurederivedfrom},
    spMeasure_Description {http://hl7.org/fhir/SearchParameter/Measuredescription},
    spMeasure_Effective {http://hl7.org/fhir/SearchParameter/Measureeffective},
    spMeasure_Identifier {http://hl7.org/fhir/SearchParameter/Measureidentifier},
    spMeasure_Jurisdiction {http://hl7.org/fhir/SearchParameter/Measurejurisdiction},
    spMeasure_Name {http://hl7.org/fhir/SearchParameter/Measurename},
    spMeasure_Predecessor {http://hl7.org/fhir/SearchParameter/Measurepredecessor},
    spMeasure_Publisher {http://hl7.org/fhir/SearchParameter/Measurepublisher},
    spMeasure_Status {http://hl7.org/fhir/SearchParameter/Measurestatus},
    spMeasure_Successor {http://hl7.org/fhir/SearchParameter/Measuresuccessor},
    spMeasure_Title {http://hl7.org/fhir/SearchParameter/Measuretitle},
    spMeasure_Topic {http://hl7.org/fhir/SearchParameter/Measuretopic},
    spMeasure_Url {http://hl7.org/fhir/SearchParameter/Measureurl},
    spMeasure_Version {http://hl7.org/fhir/SearchParameter/Measureversion});
{$ENDIF FHIR_MEASURE}

{$IFDEF FHIR_MEASUREREPORT}
  // Search Parameters for MeasureReport
  TSearchParamsMeasureReport = (
    spMeasureReport__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spMeasureReport__filter {http://hl7.org/fhir/SearchParameter/filter},
    spMeasureReport__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spMeasureReport__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spMeasureReport__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spMeasureReport__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spMeasureReport__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spMeasureReport__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spMeasureReport__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spMeasureReport__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spMeasureReport_Date {http://hl7.org/fhir/SearchParameter/MeasureReportdate},
    spMeasureReport_Evaluatedresource {http://hl7.org/fhir/SearchParameter/MeasureReportevaluatedresource},
    spMeasureReport_Identifier {http://hl7.org/fhir/SearchParameter/MeasureReportidentifier},
    spMeasureReport_Measure {http://hl7.org/fhir/SearchParameter/MeasureReportmeasure},
    spMeasureReport_Patient {http://hl7.org/fhir/SearchParameter/MeasureReportpatient},
    spMeasureReport_Period {http://hl7.org/fhir/SearchParameter/MeasureReportperiod},
    spMeasureReport_Reporter {http://hl7.org/fhir/SearchParameter/MeasureReportreporter},
    spMeasureReport_Status {http://hl7.org/fhir/SearchParameter/MeasureReportstatus},
    spMeasureReport_Subject {http://hl7.org/fhir/SearchParameter/MeasureReportsubject});
{$ENDIF FHIR_MEASUREREPORT}

{$IFDEF FHIR_MEDICATION}
  // Search Parameters for Medication
  TSearchParamsMedication = (
    spMedication__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spMedication__filter {http://hl7.org/fhir/SearchParameter/filter},
    spMedication__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spMedication__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spMedication__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spMedication__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spMedication__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spMedication__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spMedication__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spMedication__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spMedication_Code {http://hl7.org/fhir/SearchParameter/clinicalcode},
    spMedication_Expirationdate {http://hl7.org/fhir/SearchParameter/Medicationexpirationdate},
    spMedication_Form {http://hl7.org/fhir/SearchParameter/Medicationform},
    spMedication_Identifier {http://hl7.org/fhir/SearchParameter/Medicationidentifier},
    spMedication_Ingredient {http://hl7.org/fhir/SearchParameter/Medicationingredient},
    spMedication_Ingredientcode {http://hl7.org/fhir/SearchParameter/Medicationingredientcode},
    spMedication_Lotnumber {http://hl7.org/fhir/SearchParameter/Medicationlotnumber},
    spMedication_Manufacturer {http://hl7.org/fhir/SearchParameter/Medicationmanufacturer},
    spMedication_Status {http://hl7.org/fhir/SearchParameter/Medicationstatus});
{$ENDIF FHIR_MEDICATION}

{$IFDEF FHIR_MEDICATIONADMINISTRATION}
  // Search Parameters for MedicationAdministration
  TSearchParamsMedicationAdministration = (
    spMedicationAdministration__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spMedicationAdministration__filter {http://hl7.org/fhir/SearchParameter/filter},
    spMedicationAdministration__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spMedicationAdministration__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spMedicationAdministration__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spMedicationAdministration__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spMedicationAdministration__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spMedicationAdministration__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spMedicationAdministration__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spMedicationAdministration__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spMedicationAdministration_Code {http://hl7.org/fhir/SearchParameter/clinicalcode},
    spMedicationAdministration_Date {http://hl7.org/fhir/SearchParameter/medicationsdate},
    spMedicationAdministration_Device {http://hl7.org/fhir/SearchParameter/MedicationAdministrationdevice},
    spMedicationAdministration_Encounter {http://hl7.org/fhir/SearchParameter/medicationsencounter},
    spMedicationAdministration_Identifier {http://hl7.org/fhir/SearchParameter/clinicalidentifier},
    spMedicationAdministration_Medication {http://hl7.org/fhir/SearchParameter/medicationsmedication},
    spMedicationAdministration_Patient {http://hl7.org/fhir/SearchParameter/clinicalpatient},
    spMedicationAdministration_Performer {http://hl7.org/fhir/SearchParameter/MedicationAdministrationperformer},
    spMedicationAdministration_Reasongiven {http://hl7.org/fhir/SearchParameter/MedicationAdministrationreasongiven},
    spMedicationAdministration_Reasongivencode {http://hl7.org/fhir/SearchParameter/MedicationAdministrationreasongivencode},
    spMedicationAdministration_Reasonnotgiven {http://hl7.org/fhir/SearchParameter/MedicationAdministrationreasonnotgiven},
    spMedicationAdministration_Request {http://hl7.org/fhir/SearchParameter/MedicationAdministrationrequest},
    spMedicationAdministration_Status {http://hl7.org/fhir/SearchParameter/medicationsstatus},
    spMedicationAdministration_Subject {http://hl7.org/fhir/SearchParameter/MedicationAdministrationsubject});
{$ENDIF FHIR_MEDICATIONADMINISTRATION}

{$IFDEF FHIR_MEDICATIONDISPENSE}
  // Search Parameters for MedicationDispense
  TSearchParamsMedicationDispense = (
    spMedicationDispense__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spMedicationDispense__filter {http://hl7.org/fhir/SearchParameter/filter},
    spMedicationDispense__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spMedicationDispense__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spMedicationDispense__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spMedicationDispense__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spMedicationDispense__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spMedicationDispense__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spMedicationDispense__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spMedicationDispense__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spMedicationDispense_Code {http://hl7.org/fhir/SearchParameter/clinicalcode},
    spMedicationDispense_Destination {http://hl7.org/fhir/SearchParameter/MedicationDispensedestination},
    spMedicationDispense_Encounter {http://hl7.org/fhir/SearchParameter/MedicationDispenseencounter},
    spMedicationDispense_Identifier {http://hl7.org/fhir/SearchParameter/clinicalidentifier},
    spMedicationDispense_Medication {http://hl7.org/fhir/SearchParameter/medicationsmedication},
    spMedicationDispense_Patient {http://hl7.org/fhir/SearchParameter/clinicalpatient},
    spMedicationDispense_Performer {http://hl7.org/fhir/SearchParameter/MedicationDispenseperformer},
    spMedicationDispense_Prescription {http://hl7.org/fhir/SearchParameter/medicationsprescription},
    spMedicationDispense_Receiver {http://hl7.org/fhir/SearchParameter/MedicationDispensereceiver},
    spMedicationDispense_Responsibleparty {http://hl7.org/fhir/SearchParameter/MedicationDispenseresponsibleparty},
    spMedicationDispense_Status {http://hl7.org/fhir/SearchParameter/medicationsstatus},
    spMedicationDispense_Subject {http://hl7.org/fhir/SearchParameter/MedicationDispensesubject},
    spMedicationDispense_Type {http://hl7.org/fhir/SearchParameter/MedicationDispensetype},
    spMedicationDispense_Whenhandedover {http://hl7.org/fhir/SearchParameter/MedicationDispensewhenhandedover},
    spMedicationDispense_Whenprepared {http://hl7.org/fhir/SearchParameter/MedicationDispensewhenprepared});
{$ENDIF FHIR_MEDICATIONDISPENSE}

{$IFDEF FHIR_MEDICATIONKNOWLEDGE}
  // Search Parameters for MedicationKnowledge
  TSearchParamsMedicationKnowledge = (
    spMedicationKnowledge__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spMedicationKnowledge__filter {http://hl7.org/fhir/SearchParameter/filter},
    spMedicationKnowledge__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spMedicationKnowledge__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spMedicationKnowledge__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spMedicationKnowledge__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spMedicationKnowledge__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spMedicationKnowledge__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spMedicationKnowledge__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spMedicationKnowledge__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spMedicationKnowledge_Classification {http://hl7.org/fhir/SearchParameter/MedicationKnowledgeclassification},
    spMedicationKnowledge_Classificationtype {http://hl7.org/fhir/SearchParameter/MedicationKnowledgeclassificationtype},
    spMedicationKnowledge_Code {http://hl7.org/fhir/SearchParameter/MedicationKnowledgecode},
    spMedicationKnowledge_Doseform {http://hl7.org/fhir/SearchParameter/MedicationKnowledgedoseform},
    spMedicationKnowledge_Identifier {http://hl7.org/fhir/SearchParameter/MedicationKnowledgeidentifier},
    spMedicationKnowledge_Ingredient {http://hl7.org/fhir/SearchParameter/MedicationKnowledgeingredient},
    spMedicationKnowledge_Ingredientcode {http://hl7.org/fhir/SearchParameter/MedicationKnowledgeingredientcode},
    spMedicationKnowledge_Manufacturer {http://hl7.org/fhir/SearchParameter/MedicationKnowledgemanufacturer},
    spMedicationKnowledge_Monitoringprogramname {http://hl7.org/fhir/SearchParameter/MedicationKnowledgemonitoringprogramname},
    spMedicationKnowledge_Monitoringprogramtype {http://hl7.org/fhir/SearchParameter/MedicationKnowledgemonitoringprogramtype},
    spMedicationKnowledge_Monograph {http://hl7.org/fhir/SearchParameter/MedicationKnowledgemonograph},
    spMedicationKnowledge_Monographtype {http://hl7.org/fhir/SearchParameter/MedicationKnowledgemonographtype},
    spMedicationKnowledge_Packagingcost {http://hl7.org/fhir/SearchParameter/MedicationKnowledgepackagingcost},
    spMedicationKnowledge_Packagingcostconcept {http://hl7.org/fhir/SearchParameter/MedicationKnowledgepackagingcostconcept},
    spMedicationKnowledge_Producttype {http://hl7.org/fhir/SearchParameter/MedicationKnowledgeproducttype},
    spMedicationKnowledge_Sourcecost {http://hl7.org/fhir/SearchParameter/MedicationKnowledgesourcecost},
    spMedicationKnowledge_Status {http://hl7.org/fhir/SearchParameter/MedicationKnowledgestatus});
{$ENDIF FHIR_MEDICATIONKNOWLEDGE}

{$IFDEF FHIR_MEDICATIONREQUEST}
  // Search Parameters for MedicationRequest
  TSearchParamsMedicationRequest = (
    spMedicationRequest__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spMedicationRequest__filter {http://hl7.org/fhir/SearchParameter/filter},
    spMedicationRequest__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spMedicationRequest__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spMedicationRequest__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spMedicationRequest__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spMedicationRequest__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spMedicationRequest__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spMedicationRequest__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spMedicationRequest__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spMedicationRequest_Authoredon {http://hl7.org/fhir/SearchParameter/MedicationRequestauthoredon},
    spMedicationRequest_Category {http://hl7.org/fhir/SearchParameter/MedicationRequestcategory},
    spMedicationRequest_Code {http://hl7.org/fhir/SearchParameter/clinicalcode},
    spMedicationRequest_Combodate {http://hl7.org/fhir/SearchParameter/MedicationRequestcombodate},
    spMedicationRequest_Encounter {http://hl7.org/fhir/SearchParameter/medicationsencounter},
    spMedicationRequest_Identifier {http://hl7.org/fhir/SearchParameter/clinicalidentifier},
    spMedicationRequest_Intendeddispenser {http://hl7.org/fhir/SearchParameter/MedicationRequestintendeddispenser},
    spMedicationRequest_Intendedperformer {http://hl7.org/fhir/SearchParameter/MedicationRequestintendedperformer},
    spMedicationRequest_Intendedperformertype {http://hl7.org/fhir/SearchParameter/MedicationRequestintendedperformertype},
    spMedicationRequest_Intent {http://hl7.org/fhir/SearchParameter/MedicationRequestintent},
    spMedicationRequest_Medication {http://hl7.org/fhir/SearchParameter/medicationsmedication},
    spMedicationRequest_Patient {http://hl7.org/fhir/SearchParameter/clinicalpatient},
    spMedicationRequest_Priority {http://hl7.org/fhir/SearchParameter/MedicationRequestpriority},
    spMedicationRequest_Requester {http://hl7.org/fhir/SearchParameter/MedicationRequestrequester},
    spMedicationRequest_Status {http://hl7.org/fhir/SearchParameter/medicationsstatus},
    spMedicationRequest_Subject {http://hl7.org/fhir/SearchParameter/MedicationRequestsubject});
{$ENDIF FHIR_MEDICATIONREQUEST}

{$IFDEF FHIR_MEDICATIONUSAGE}
  // Search Parameters for MedicationUsage
  TSearchParamsMedicationUsage = (
    spMedicationUsage__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spMedicationUsage__filter {http://hl7.org/fhir/SearchParameter/filter},
    spMedicationUsage__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spMedicationUsage__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spMedicationUsage__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spMedicationUsage__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spMedicationUsage__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spMedicationUsage__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spMedicationUsage__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spMedicationUsage__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spMedicationUsage_Adherence {http://hl7.org/fhir/SearchParameter/MedicationUsageadherence},
    spMedicationUsage_Category {http://hl7.org/fhir/SearchParameter/MedicationUsagecategory},
    spMedicationUsage_Code {http://hl7.org/fhir/SearchParameter/clinicalcode},
    spMedicationUsage_Effective {http://hl7.org/fhir/SearchParameter/MedicationUsageeffective},
    spMedicationUsage_Encounter {http://hl7.org/fhir/SearchParameter/MedicationUsageencounter},
    spMedicationUsage_Identifier {http://hl7.org/fhir/SearchParameter/clinicalidentifier},
    spMedicationUsage_Medication {http://hl7.org/fhir/SearchParameter/medicationsmedication},
    spMedicationUsage_Partof {http://hl7.org/fhir/SearchParameter/MedicationUsagepartof},
    spMedicationUsage_Patient {http://hl7.org/fhir/SearchParameter/clinicalpatient},
    spMedicationUsage_Source {http://hl7.org/fhir/SearchParameter/MedicationUsagesource},
    spMedicationUsage_Status {http://hl7.org/fhir/SearchParameter/medicationsstatus},
    spMedicationUsage_Subject {http://hl7.org/fhir/SearchParameter/MedicationUsagesubject});
{$ENDIF FHIR_MEDICATIONUSAGE}

{$IFDEF FHIR_MEDICINALPRODUCTDEFINITION}
  // Search Parameters for MedicinalProductDefinition
  TSearchParamsMedicinalProductDefinition = (
    spMedicinalProductDefinition__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spMedicinalProductDefinition__filter {http://hl7.org/fhir/SearchParameter/filter},
    spMedicinalProductDefinition__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spMedicinalProductDefinition__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spMedicinalProductDefinition__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spMedicinalProductDefinition__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spMedicinalProductDefinition__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spMedicinalProductDefinition__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spMedicinalProductDefinition__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spMedicinalProductDefinition__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spMedicinalProductDefinition_Characteristic {http://hl7.org/fhir/SearchParameter/MedicinalProductDefinitioncharacteristic},
    spMedicinalProductDefinition_Contact {http://hl7.org/fhir/SearchParameter/MedicinalProductDefinitioncontact},
    spMedicinalProductDefinition_Domain {http://hl7.org/fhir/SearchParameter/MedicinalProductDefinitiondomain},
    spMedicinalProductDefinition_Identifier {http://hl7.org/fhir/SearchParameter/MedicinalProductDefinitionidentifier},
    spMedicinalProductDefinition_Ingredient {http://hl7.org/fhir/SearchParameter/MedicinalProductDefinitioningredient},
    spMedicinalProductDefinition_Masterfile {http://hl7.org/fhir/SearchParameter/MedicinalProductDefinitionmasterfile},
    spMedicinalProductDefinition_Name {http://hl7.org/fhir/SearchParameter/MedicinalProductDefinitionname},
    spMedicinalProductDefinition_Namelanguage {http://hl7.org/fhir/SearchParameter/MedicinalProductDefinitionnamelanguage},
    spMedicinalProductDefinition_Productclassification {http://hl7.org/fhir/SearchParameter/MedicinalProductDefinitionproductclassification},
    spMedicinalProductDefinition_Status {http://hl7.org/fhir/SearchParameter/MedicinalProductDefinitionstatus},
    spMedicinalProductDefinition_Type {http://hl7.org/fhir/SearchParameter/MedicinalProductDefinitiontype});
{$ENDIF FHIR_MEDICINALPRODUCTDEFINITION}

{$IFDEF FHIR_MESSAGEDEFINITION}
  // Search Parameters for MessageDefinition
  TSearchParamsMessageDefinition = (
    spMessageDefinition__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spMessageDefinition__filter {http://hl7.org/fhir/SearchParameter/filter},
    spMessageDefinition__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spMessageDefinition__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spMessageDefinition__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spMessageDefinition__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spMessageDefinition__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spMessageDefinition__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spMessageDefinition__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spMessageDefinition__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spMessageDefinition_Category {http://hl7.org/fhir/SearchParameter/MessageDefinitioncategory},
    spMessageDefinition_Context {http://hl7.org/fhir/SearchParameter/conformancecontext},
    spMessageDefinition_Contextquantity {http://hl7.org/fhir/SearchParameter/conformancecontextquantity},
    spMessageDefinition_Contexttype {http://hl7.org/fhir/SearchParameter/conformancecontexttype},
    spMessageDefinition_Contexttypequantity {http://hl7.org/fhir/SearchParameter/conformancecontexttypequantity},
    spMessageDefinition_Contexttypevalue {http://hl7.org/fhir/SearchParameter/conformancecontexttypevalue},
    spMessageDefinition_Date {http://hl7.org/fhir/SearchParameter/conformancedate},
    spMessageDefinition_Description {http://hl7.org/fhir/SearchParameter/conformancedescription},
    spMessageDefinition_Event {http://hl7.org/fhir/SearchParameter/MessageDefinitionevent},
    spMessageDefinition_Focus {http://hl7.org/fhir/SearchParameter/MessageDefinitionfocus},
    spMessageDefinition_Identifier {http://hl7.org/fhir/SearchParameter/conformanceidentifier},
    spMessageDefinition_Jurisdiction {http://hl7.org/fhir/SearchParameter/conformancejurisdiction},
    spMessageDefinition_Name {http://hl7.org/fhir/SearchParameter/conformancename},
    spMessageDefinition_Parent {http://hl7.org/fhir/SearchParameter/MessageDefinitionparent},
    spMessageDefinition_Publisher {http://hl7.org/fhir/SearchParameter/conformancepublisher},
    spMessageDefinition_Status {http://hl7.org/fhir/SearchParameter/conformancestatus},
    spMessageDefinition_Title {http://hl7.org/fhir/SearchParameter/conformancetitle},
    spMessageDefinition_Url {http://hl7.org/fhir/SearchParameter/conformanceurl},
    spMessageDefinition_Version {http://hl7.org/fhir/SearchParameter/conformanceversion});
{$ENDIF FHIR_MESSAGEDEFINITION}

{$IFDEF FHIR_MESSAGEHEADER}
  // Search Parameters for MessageHeader
  TSearchParamsMessageHeader = (
    spMessageHeader__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spMessageHeader__filter {http://hl7.org/fhir/SearchParameter/filter},
    spMessageHeader__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spMessageHeader__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spMessageHeader__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spMessageHeader__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spMessageHeader__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spMessageHeader__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spMessageHeader__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spMessageHeader__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spMessageHeader_Author {http://hl7.org/fhir/SearchParameter/MessageHeaderauthor},
    spMessageHeader_Code {http://hl7.org/fhir/SearchParameter/MessageHeadercode},
    spMessageHeader_Destination {http://hl7.org/fhir/SearchParameter/MessageHeaderdestination},
    spMessageHeader_Destinationuri {http://hl7.org/fhir/SearchParameter/MessageHeaderdestinationuri},
    spMessageHeader_Enterer {http://hl7.org/fhir/SearchParameter/MessageHeaderenterer},
    spMessageHeader_Event {http://hl7.org/fhir/SearchParameter/MessageHeaderevent},
    spMessageHeader_Focus {http://hl7.org/fhir/SearchParameter/MessageHeaderfocus},
    spMessageHeader_Receiver {http://hl7.org/fhir/SearchParameter/MessageHeaderreceiver},
    spMessageHeader_Responseid {http://hl7.org/fhir/SearchParameter/MessageHeaderresponseid},
    spMessageHeader_Responsible {http://hl7.org/fhir/SearchParameter/MessageHeaderresponsible},
    spMessageHeader_Sender {http://hl7.org/fhir/SearchParameter/MessageHeadersender},
    spMessageHeader_Source {http://hl7.org/fhir/SearchParameter/MessageHeadersource},
    spMessageHeader_Sourceuri {http://hl7.org/fhir/SearchParameter/MessageHeadersourceuri},
    spMessageHeader_Target {http://hl7.org/fhir/SearchParameter/MessageHeadertarget});
{$ENDIF FHIR_MESSAGEHEADER}

{$IFDEF FHIR_MOLECULARSEQUENCE}
  // Search Parameters for MolecularSequence
  TSearchParamsMolecularSequence = (
    spMolecularSequence__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spMolecularSequence__filter {http://hl7.org/fhir/SearchParameter/filter},
    spMolecularSequence__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spMolecularSequence__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spMolecularSequence__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spMolecularSequence__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spMolecularSequence__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spMolecularSequence__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spMolecularSequence__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spMolecularSequence__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spMolecularSequence_Chromosome {http://hl7.org/fhir/SearchParameter/MolecularSequencechromosome},
    spMolecularSequence_Chromosomevariantcoordinate {http://hl7.org/fhir/SearchParameter/MolecularSequencechromosomevariantcoordinate},
    spMolecularSequence_Chromosomewindowcoordinate {http://hl7.org/fhir/SearchParameter/MolecularSequencechromosomewindowcoordinate},
    spMolecularSequence_Identifier {http://hl7.org/fhir/SearchParameter/MolecularSequenceidentifier},
    spMolecularSequence_Patient {http://hl7.org/fhir/SearchParameter/MolecularSequencepatient},
    spMolecularSequence_Referenceseqid {http://hl7.org/fhir/SearchParameter/MolecularSequencereferenceseqid},
    spMolecularSequence_Referenceseqidvariantcoordinate {http://hl7.org/fhir/SearchParameter/MolecularSequencereferenceseqidvariantcoordinate},
    spMolecularSequence_Referenceseqidwindowcoordinate {http://hl7.org/fhir/SearchParameter/MolecularSequencereferenceseqidwindowcoordinate},
    spMolecularSequence_Type {http://hl7.org/fhir/SearchParameter/MolecularSequencetype},
    spMolecularSequence_Variantend {http://hl7.org/fhir/SearchParameter/MolecularSequencevariantend},
    spMolecularSequence_Variantstart {http://hl7.org/fhir/SearchParameter/MolecularSequencevariantstart},
    spMolecularSequence_Windowend {http://hl7.org/fhir/SearchParameter/MolecularSequencewindowend},
    spMolecularSequence_Windowstart {http://hl7.org/fhir/SearchParameter/MolecularSequencewindowstart});
{$ENDIF FHIR_MOLECULARSEQUENCE}

{$IFDEF FHIR_NAMINGSYSTEM}
  // Search Parameters for NamingSystem
  TSearchParamsNamingSystem = (
    spNamingSystem__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spNamingSystem__filter {http://hl7.org/fhir/SearchParameter/filter},
    spNamingSystem__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spNamingSystem__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spNamingSystem__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spNamingSystem__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spNamingSystem__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spNamingSystem__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spNamingSystem__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spNamingSystem__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spNamingSystem_Contact {http://hl7.org/fhir/SearchParameter/NamingSystemcontact},
    spNamingSystem_Context {http://hl7.org/fhir/SearchParameter/conformancecontext},
    spNamingSystem_Contextquantity {http://hl7.org/fhir/SearchParameter/conformancecontextquantity},
    spNamingSystem_Contexttype {http://hl7.org/fhir/SearchParameter/conformancecontexttype},
    spNamingSystem_Contexttypequantity {http://hl7.org/fhir/SearchParameter/conformancecontexttypequantity},
    spNamingSystem_Contexttypevalue {http://hl7.org/fhir/SearchParameter/conformancecontexttypevalue},
    spNamingSystem_Date {http://hl7.org/fhir/SearchParameter/conformancedate},
    spNamingSystem_Description {http://hl7.org/fhir/SearchParameter/conformancedescription},
    spNamingSystem_Idtype {http://hl7.org/fhir/SearchParameter/NamingSystemidtype},
    spNamingSystem_Jurisdiction {http://hl7.org/fhir/SearchParameter/conformancejurisdiction},
    spNamingSystem_Kind {http://hl7.org/fhir/SearchParameter/NamingSystemkind},
    spNamingSystem_Name {http://hl7.org/fhir/SearchParameter/conformancename},
    spNamingSystem_Period {http://hl7.org/fhir/SearchParameter/NamingSystemperiod},
    spNamingSystem_Publisher {http://hl7.org/fhir/SearchParameter/conformancepublisher},
    spNamingSystem_Responsible {http://hl7.org/fhir/SearchParameter/NamingSystemresponsible},
    spNamingSystem_Status {http://hl7.org/fhir/SearchParameter/conformancestatus},
    spNamingSystem_Telecom {http://hl7.org/fhir/SearchParameter/NamingSystemtelecom},
    spNamingSystem_Type {http://hl7.org/fhir/SearchParameter/NamingSystemtype},
    spNamingSystem_Url {http://hl7.org/fhir/SearchParameter/conformanceurl},
    spNamingSystem_Value {http://hl7.org/fhir/SearchParameter/NamingSystemvalue},
    spNamingSystem_Version {http://hl7.org/fhir/SearchParameter/conformanceversion});
{$ENDIF FHIR_NAMINGSYSTEM}

{$IFDEF FHIR_NUTRITIONINTAKE}
  // Search Parameters for NutritionIntake
  TSearchParamsNutritionIntake = (
    spNutritionIntake__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spNutritionIntake__filter {http://hl7.org/fhir/SearchParameter/filter},
    spNutritionIntake__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spNutritionIntake__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spNutritionIntake__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spNutritionIntake__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spNutritionIntake__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spNutritionIntake__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spNutritionIntake__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spNutritionIntake__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spNutritionIntake_Code {http://hl7.org/fhir/SearchParameter/NutritionIntakecode},
    spNutritionIntake_Date {http://hl7.org/fhir/SearchParameter/NutritionIntakedate},
    spNutritionIntake_Encounter {http://hl7.org/fhir/SearchParameter/NutritionIntakeencounter},
    spNutritionIntake_Identifier {http://hl7.org/fhir/SearchParameter/NutritionIntakeidentifier},
    spNutritionIntake_Nutrition {http://hl7.org/fhir/SearchParameter/NutritionIntakenutrition},
    spNutritionIntake_Partof {http://hl7.org/fhir/SearchParameter/NutritionIntakepartof},
    spNutritionIntake_Patient {http://hl7.org/fhir/SearchParameter/NutritionIntakepatient},
    spNutritionIntake_Source {http://hl7.org/fhir/SearchParameter/NutritionIntakesource},
    spNutritionIntake_Status {http://hl7.org/fhir/SearchParameter/NutritionIntakestatus},
    spNutritionIntake_Subject {http://hl7.org/fhir/SearchParameter/NutritionIntakesubject});
{$ENDIF FHIR_NUTRITIONINTAKE}

{$IFDEF FHIR_NUTRITIONORDER}
  // Search Parameters for NutritionOrder
  TSearchParamsNutritionOrder = (
    spNutritionOrder__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spNutritionOrder__filter {http://hl7.org/fhir/SearchParameter/filter},
    spNutritionOrder__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spNutritionOrder__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spNutritionOrder__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spNutritionOrder__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spNutritionOrder__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spNutritionOrder__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spNutritionOrder__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spNutritionOrder__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spNutritionOrder_Additive {http://hl7.org/fhir/SearchParameter/NutritionOrderadditive},
    spNutritionOrder_Datetime {http://hl7.org/fhir/SearchParameter/NutritionOrderdatetime},
    spNutritionOrder_Encounter {http://hl7.org/fhir/SearchParameter/clinicalencounter},
    spNutritionOrder_Formula {http://hl7.org/fhir/SearchParameter/NutritionOrderformula},
    spNutritionOrder_Identifier {http://hl7.org/fhir/SearchParameter/clinicalidentifier},
    spNutritionOrder_Instantiatescanonical {http://hl7.org/fhir/SearchParameter/NutritionOrderinstantiatescanonical},
    spNutritionOrder_Instantiatesuri {http://hl7.org/fhir/SearchParameter/NutritionOrderinstantiatesuri},
    spNutritionOrder_Oraldiet {http://hl7.org/fhir/SearchParameter/NutritionOrderoraldiet},
    spNutritionOrder_Patient {http://hl7.org/fhir/SearchParameter/clinicalpatient},
    spNutritionOrder_Provider {http://hl7.org/fhir/SearchParameter/NutritionOrderprovider},
    spNutritionOrder_Status {http://hl7.org/fhir/SearchParameter/NutritionOrderstatus},
    spNutritionOrder_Supplement {http://hl7.org/fhir/SearchParameter/NutritionOrdersupplement});
{$ENDIF FHIR_NUTRITIONORDER}

{$IFDEF FHIR_NUTRITIONPRODUCT}
  // Search Parameters for NutritionProduct
  TSearchParamsNutritionProduct = (
    spNutritionProduct__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spNutritionProduct__filter {http://hl7.org/fhir/SearchParameter/filter},
    spNutritionProduct__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spNutritionProduct__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spNutritionProduct__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spNutritionProduct__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spNutritionProduct__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spNutritionProduct__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spNutritionProduct__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spNutritionProduct__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spNutritionProduct_Identifier {http://hl7.org/fhir/SearchParameter/NutritionProductidentifier},
    spNutritionProduct_Status {http://hl7.org/fhir/SearchParameter/NutritionProductstatus});
{$ENDIF FHIR_NUTRITIONPRODUCT}

{$IFDEF FHIR_OBSERVATION}
  // Search Parameters for Observation
  TSearchParamsObservation = (
    spObservation__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spObservation__filter {http://hl7.org/fhir/SearchParameter/filter},
    spObservation__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spObservation__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spObservation__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spObservation__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spObservation__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spObservation__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spObservation__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spObservation__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spObservation_Basedon {http://hl7.org/fhir/SearchParameter/Observationbasedon},
    spObservation_Category {http://hl7.org/fhir/SearchParameter/Observationcategory},
    spObservation_Code {http://hl7.org/fhir/SearchParameter/clinicalcode},
    spObservation_Codevalueconcept {http://hl7.org/fhir/SearchParameter/Observationcodevalueconcept},
    spObservation_Codevaluedate {http://hl7.org/fhir/SearchParameter/Observationcodevaluedate},
    spObservation_Codevaluequantity {http://hl7.org/fhir/SearchParameter/Observationcodevaluequantity},
    spObservation_Codevaluestring {http://hl7.org/fhir/SearchParameter/Observationcodevaluestring},
    spObservation_Combocode {http://hl7.org/fhir/SearchParameter/Observationcombocode},
    spObservation_Combocodevalueconcept {http://hl7.org/fhir/SearchParameter/Observationcombocodevalueconcept},
    spObservation_Combocodevaluequantity {http://hl7.org/fhir/SearchParameter/Observationcombocodevaluequantity},
    spObservation_Combodataabsentreason {http://hl7.org/fhir/SearchParameter/Observationcombodataabsentreason},
    spObservation_Combovalueconcept {http://hl7.org/fhir/SearchParameter/Observationcombovalueconcept},
    spObservation_Combovaluequantity {http://hl7.org/fhir/SearchParameter/Observationcombovaluequantity},
    spObservation_Componentcode {http://hl7.org/fhir/SearchParameter/Observationcomponentcode},
    spObservation_Componentcodevalueconcept {http://hl7.org/fhir/SearchParameter/Observationcomponentcodevalueconcept},
    spObservation_Componentcodevaluequantity {http://hl7.org/fhir/SearchParameter/Observationcomponentcodevaluequantity},
    spObservation_Componentdataabsentreason {http://hl7.org/fhir/SearchParameter/Observationcomponentdataabsentreason},
    spObservation_Componentvalueconcept {http://hl7.org/fhir/SearchParameter/Observationcomponentvalueconcept},
    spObservation_Componentvaluequantity {http://hl7.org/fhir/SearchParameter/Observationcomponentvaluequantity},
    spObservation_Dataabsentreason {http://hl7.org/fhir/SearchParameter/Observationdataabsentreason},
    spObservation_Date {http://hl7.org/fhir/SearchParameter/clinicaldate},
    spObservation_Derivedfrom {http://hl7.org/fhir/SearchParameter/Observationderivedfrom},
    spObservation_Device {http://hl7.org/fhir/SearchParameter/Observationdevice},
    spObservation_Encounter {http://hl7.org/fhir/SearchParameter/clinicalencounter},
    spObservation_Focus {http://hl7.org/fhir/SearchParameter/Observationfocus},
    spObservation_Hasmember {http://hl7.org/fhir/SearchParameter/Observationhasmember},
    spObservation_Identifier {http://hl7.org/fhir/SearchParameter/clinicalidentifier},
    spObservation_Method {http://hl7.org/fhir/SearchParameter/Observationmethod},
    spObservation_Partof {http://hl7.org/fhir/SearchParameter/Observationpartof},
    spObservation_Patient {http://hl7.org/fhir/SearchParameter/clinicalpatient},
    spObservation_Performer {http://hl7.org/fhir/SearchParameter/Observationperformer},
    spObservation_Specimen {http://hl7.org/fhir/SearchParameter/Observationspecimen},
    spObservation_Status {http://hl7.org/fhir/SearchParameter/Observationstatus},
    spObservation_Subject {http://hl7.org/fhir/SearchParameter/Observationsubject},
    spObservation_Valueconcept {http://hl7.org/fhir/SearchParameter/Observationvalueconcept},
    spObservation_Valuedate {http://hl7.org/fhir/SearchParameter/Observationvaluedate},
    spObservation_Valuequantity {http://hl7.org/fhir/SearchParameter/Observationvaluequantity},
    spObservation_Valuestring {http://hl7.org/fhir/SearchParameter/Observationvaluestring});
{$ENDIF FHIR_OBSERVATION}

{$IFDEF FHIR_OBSERVATIONDEFINITION}
  // Search Parameters for ObservationDefinition
  TSearchParamsObservationDefinition = (
    spObservationDefinition__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spObservationDefinition__filter {http://hl7.org/fhir/SearchParameter/filter},
    spObservationDefinition__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spObservationDefinition__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spObservationDefinition__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spObservationDefinition__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spObservationDefinition__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spObservationDefinition__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spObservationDefinition__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spObservationDefinition__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spObservationDefinition_Category {http://hl7.org/fhir/SearchParameter/ObservationDefinitioncategory},
    spObservationDefinition_Code {http://hl7.org/fhir/SearchParameter/ObservationDefinitioncode},
    spObservationDefinition_Experimental {http://hl7.org/fhir/SearchParameter/ObservationDefinitionexperimental},
    spObservationDefinition_Identifier {http://hl7.org/fhir/SearchParameter/ObservationDefinitionidentifier},
    spObservationDefinition_Method {http://hl7.org/fhir/SearchParameter/ObservationDefinitionmethod},
    spObservationDefinition_Status {http://hl7.org/fhir/SearchParameter/ObservationDefinitionstatus},
    spObservationDefinition_Title {http://hl7.org/fhir/SearchParameter/ObservationDefinitiontitle},
    spObservationDefinition_Url {http://hl7.org/fhir/SearchParameter/ObservationDefinitionurl});
{$ENDIF FHIR_OBSERVATIONDEFINITION}

{$IFDEF FHIR_OPERATIONDEFINITION}
  // Search Parameters for OperationDefinition
  TSearchParamsOperationDefinition = (
    spOperationDefinition__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spOperationDefinition__filter {http://hl7.org/fhir/SearchParameter/filter},
    spOperationDefinition__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spOperationDefinition__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spOperationDefinition__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spOperationDefinition__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spOperationDefinition__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spOperationDefinition__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spOperationDefinition__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spOperationDefinition__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spOperationDefinition_Base {http://hl7.org/fhir/SearchParameter/OperationDefinitionbase},
    spOperationDefinition_Code {http://hl7.org/fhir/SearchParameter/OperationDefinitioncode},
    spOperationDefinition_Context {http://hl7.org/fhir/SearchParameter/conformancecontext},
    spOperationDefinition_Contextquantity {http://hl7.org/fhir/SearchParameter/conformancecontextquantity},
    spOperationDefinition_Contexttype {http://hl7.org/fhir/SearchParameter/conformancecontexttype},
    spOperationDefinition_Contexttypequantity {http://hl7.org/fhir/SearchParameter/conformancecontexttypequantity},
    spOperationDefinition_Contexttypevalue {http://hl7.org/fhir/SearchParameter/conformancecontexttypevalue},
    spOperationDefinition_Date {http://hl7.org/fhir/SearchParameter/conformancedate},
    spOperationDefinition_Description {http://hl7.org/fhir/SearchParameter/conformancedescription},
    spOperationDefinition_Inputprofile {http://hl7.org/fhir/SearchParameter/OperationDefinitioninputprofile},
    spOperationDefinition_Instance {http://hl7.org/fhir/SearchParameter/OperationDefinitioninstance},
    spOperationDefinition_Jurisdiction {http://hl7.org/fhir/SearchParameter/conformancejurisdiction},
    spOperationDefinition_Kind {http://hl7.org/fhir/SearchParameter/OperationDefinitionkind},
    spOperationDefinition_Name {http://hl7.org/fhir/SearchParameter/conformancename},
    spOperationDefinition_Outputprofile {http://hl7.org/fhir/SearchParameter/OperationDefinitionoutputprofile},
    spOperationDefinition_Publisher {http://hl7.org/fhir/SearchParameter/conformancepublisher},
    spOperationDefinition_Status {http://hl7.org/fhir/SearchParameter/conformancestatus},
    spOperationDefinition_System {http://hl7.org/fhir/SearchParameter/OperationDefinitionsystem},
    spOperationDefinition_Title {http://hl7.org/fhir/SearchParameter/conformancetitle},
    spOperationDefinition_Type {http://hl7.org/fhir/SearchParameter/OperationDefinitiontype},
    spOperationDefinition_Url {http://hl7.org/fhir/SearchParameter/conformanceurl},
    spOperationDefinition_Version {http://hl7.org/fhir/SearchParameter/conformanceversion});
{$ENDIF FHIR_OPERATIONDEFINITION}

{$IFDEF FHIR_OPERATIONOUTCOME}
  // Search Parameters for OperationOutcome
  TSearchParamsOperationOutcome = (
    spOperationOutcome__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spOperationOutcome__filter {http://hl7.org/fhir/SearchParameter/filter},
    spOperationOutcome__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spOperationOutcome__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spOperationOutcome__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spOperationOutcome__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spOperationOutcome__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spOperationOutcome__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spOperationOutcome__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spOperationOutcome__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext});
{$ENDIF FHIR_OPERATIONOUTCOME}

{$IFDEF FHIR_ORGANIZATION}
  // Search Parameters for Organization
  TSearchParamsOrganization = (
    spOrganization__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spOrganization__filter {http://hl7.org/fhir/SearchParameter/filter},
    spOrganization__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spOrganization__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spOrganization__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spOrganization__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spOrganization__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spOrganization__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spOrganization__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spOrganization__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spOrganization_Active {http://hl7.org/fhir/SearchParameter/Organizationactive},
    spOrganization_Address {http://hl7.org/fhir/SearchParameter/Organizationaddress},
    spOrganization_Addresscity {http://hl7.org/fhir/SearchParameter/Organizationaddresscity},
    spOrganization_Addresscountry {http://hl7.org/fhir/SearchParameter/Organizationaddresscountry},
    spOrganization_Addresspostalcode {http://hl7.org/fhir/SearchParameter/Organizationaddresspostalcode},
    spOrganization_Addressstate {http://hl7.org/fhir/SearchParameter/Organizationaddressstate},
    spOrganization_Addressuse {http://hl7.org/fhir/SearchParameter/Organizationaddressuse},
    spOrganization_Endpoint {http://hl7.org/fhir/SearchParameter/Organizationendpoint},
    spOrganization_Identifier {http://hl7.org/fhir/SearchParameter/Organizationidentifier},
    spOrganization_Name {http://hl7.org/fhir/SearchParameter/Organizationname},
    spOrganization_Partof {http://hl7.org/fhir/SearchParameter/Organizationpartof},
    spOrganization_Phonetic {http://hl7.org/fhir/SearchParameter/Organizationphonetic},
    spOrganization_Type {http://hl7.org/fhir/SearchParameter/Organizationtype});
{$ENDIF FHIR_ORGANIZATION}

{$IFDEF FHIR_ORGANIZATIONAFFILIATION}
  // Search Parameters for OrganizationAffiliation
  TSearchParamsOrganizationAffiliation = (
    spOrganizationAffiliation__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spOrganizationAffiliation__filter {http://hl7.org/fhir/SearchParameter/filter},
    spOrganizationAffiliation__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spOrganizationAffiliation__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spOrganizationAffiliation__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spOrganizationAffiliation__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spOrganizationAffiliation__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spOrganizationAffiliation__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spOrganizationAffiliation__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spOrganizationAffiliation__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spOrganizationAffiliation_Active {http://hl7.org/fhir/SearchParameter/OrganizationAffiliationactive},
    spOrganizationAffiliation_Date {http://hl7.org/fhir/SearchParameter/OrganizationAffiliationdate},
    spOrganizationAffiliation_Email {http://hl7.org/fhir/SearchParameter/OrganizationAffiliationemail},
    spOrganizationAffiliation_Endpoint {http://hl7.org/fhir/SearchParameter/OrganizationAffiliationendpoint},
    spOrganizationAffiliation_Identifier {http://hl7.org/fhir/SearchParameter/OrganizationAffiliationidentifier},
    spOrganizationAffiliation_Location {http://hl7.org/fhir/SearchParameter/OrganizationAffiliationlocation},
    spOrganizationAffiliation_Network {http://hl7.org/fhir/SearchParameter/OrganizationAffiliationnetwork},
    spOrganizationAffiliation_Participatingorganization {http://hl7.org/fhir/SearchParameter/OrganizationAffiliationparticipatingorganization},
    spOrganizationAffiliation_Phone {http://hl7.org/fhir/SearchParameter/OrganizationAffiliationphone},
    spOrganizationAffiliation_Primaryorganization {http://hl7.org/fhir/SearchParameter/OrganizationAffiliationprimaryorganization},
    spOrganizationAffiliation_Role {http://hl7.org/fhir/SearchParameter/OrganizationAffiliationrole},
    spOrganizationAffiliation_Service {http://hl7.org/fhir/SearchParameter/OrganizationAffiliationservice},
    spOrganizationAffiliation_Specialty {http://hl7.org/fhir/SearchParameter/OrganizationAffiliationspecialty},
    spOrganizationAffiliation_Telecom {http://hl7.org/fhir/SearchParameter/OrganizationAffiliationtelecom});
{$ENDIF FHIR_ORGANIZATIONAFFILIATION}

{$IFDEF FHIR_PACKAGEDPRODUCTDEFINITION}
  // Search Parameters for PackagedProductDefinition
  TSearchParamsPackagedProductDefinition = (
    spPackagedProductDefinition__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spPackagedProductDefinition__filter {http://hl7.org/fhir/SearchParameter/filter},
    spPackagedProductDefinition__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spPackagedProductDefinition__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spPackagedProductDefinition__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spPackagedProductDefinition__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spPackagedProductDefinition__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spPackagedProductDefinition__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spPackagedProductDefinition__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spPackagedProductDefinition__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spPackagedProductDefinition_Biological {http://hl7.org/fhir/SearchParameter/PackagedProductDefinitionbiological},
    spPackagedProductDefinition_Containeditem {http://hl7.org/fhir/SearchParameter/PackagedProductDefinitioncontaineditem},
    spPackagedProductDefinition_Device {http://hl7.org/fhir/SearchParameter/PackagedProductDefinitiondevice},
    spPackagedProductDefinition_Identifier {http://hl7.org/fhir/SearchParameter/PackagedProductDefinitionidentifier},
    spPackagedProductDefinition_Manufactureditem {http://hl7.org/fhir/SearchParameter/PackagedProductDefinitionmanufactureditem},
    spPackagedProductDefinition_Medication {http://hl7.org/fhir/SearchParameter/PackagedProductDefinitionmedication},
    spPackagedProductDefinition_Name {http://hl7.org/fhir/SearchParameter/PackagedProductDefinitionname},
    spPackagedProductDefinition_Nutrition {http://hl7.org/fhir/SearchParameter/PackagedProductDefinitionnutrition},
    spPackagedProductDefinition_Status {http://hl7.org/fhir/SearchParameter/PackagedProductDefinitionstatus},
    spPackagedProductDefinition_Subject {http://hl7.org/fhir/SearchParameter/PackagedProductDefinitionsubject});
{$ENDIF FHIR_PACKAGEDPRODUCTDEFINITION}

{$IFDEF FHIR_PARAMETERS}
  // Search Parameters for Parameters
  TSearchParamsParameters = (
    spParameters__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spParameters__filter {http://hl7.org/fhir/SearchParameter/filter},
    spParameters__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spParameters__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spParameters__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spParameters__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spParameters__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spParameters__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spParameters__tag {http://hl7.org/fhir/SearchParameter/Resourcetag});
{$ENDIF FHIR_PARAMETERS}

{$IFDEF FHIR_PATIENT}
  // Search Parameters for Patient
  TSearchParamsPatient = (
    spPatient__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spPatient__filter {http://hl7.org/fhir/SearchParameter/filter},
    spPatient__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spPatient__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spPatient__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spPatient__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spPatient__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spPatient__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spPatient__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spPatient__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spPatient_Active {http://hl7.org/fhir/SearchParameter/Patientactive},
    spPatient_Address {http://hl7.org/fhir/SearchParameter/individualaddress},
    spPatient_Addresscity {http://hl7.org/fhir/SearchParameter/individualaddresscity},
    spPatient_Addresscountry {http://hl7.org/fhir/SearchParameter/individualaddresscountry},
    spPatient_Addresspostalcode {http://hl7.org/fhir/SearchParameter/individualaddresspostalcode},
    spPatient_Addressstate {http://hl7.org/fhir/SearchParameter/individualaddressstate},
    spPatient_Addressuse {http://hl7.org/fhir/SearchParameter/individualaddressuse},
    spPatient_Age {http://hl7.org/fhir/SearchParameter/patientextensionsPatientage},
    spPatient_BirthOrderBoolean {http://hl7.org/fhir/SearchParameter/patientextensionsPatientbirthOrderBoolean},
    spPatient_Birthdate {http://hl7.org/fhir/SearchParameter/individualbirthdate},
    spPatient_Deathdate {http://hl7.org/fhir/SearchParameter/Patientdeathdate},
    spPatient_Deceased {http://hl7.org/fhir/SearchParameter/Patientdeceased},
    spPatient_Email {http://hl7.org/fhir/SearchParameter/individualemail},
    spPatient_Family {http://hl7.org/fhir/SearchParameter/individualfamily},
    spPatient_Gender {http://hl7.org/fhir/SearchParameter/individualgender},
    spPatient_Generalpractitioner {http://hl7.org/fhir/SearchParameter/Patientgeneralpractitioner},
    spPatient_Given {http://hl7.org/fhir/SearchParameter/individualgiven},
    spPatient_Identifier {http://hl7.org/fhir/SearchParameter/Patientidentifier},
    spPatient_Language {http://hl7.org/fhir/SearchParameter/Patientlanguage},
    spPatient_Link {http://hl7.org/fhir/SearchParameter/Patientlink},
    spPatient_MothersMaidenName {http://hl7.org/fhir/SearchParameter/patientextensionsPatientmothersMaidenName},
    spPatient_Name {http://hl7.org/fhir/SearchParameter/Patientname},
    spPatient_Organization {http://hl7.org/fhir/SearchParameter/Patientorganization},
    spPatient_Phone {http://hl7.org/fhir/SearchParameter/individualphone},
    spPatient_Phonetic {http://hl7.org/fhir/SearchParameter/individualphonetic},
    spPatient_Telecom {http://hl7.org/fhir/SearchParameter/individualtelecom});
{$ENDIF FHIR_PATIENT}

{$IFDEF FHIR_PAYMENTNOTICE}
  // Search Parameters for PaymentNotice
  TSearchParamsPaymentNotice = (
    spPaymentNotice__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spPaymentNotice__filter {http://hl7.org/fhir/SearchParameter/filter},
    spPaymentNotice__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spPaymentNotice__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spPaymentNotice__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spPaymentNotice__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spPaymentNotice__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spPaymentNotice__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spPaymentNotice__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spPaymentNotice__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spPaymentNotice_Created {http://hl7.org/fhir/SearchParameter/PaymentNoticecreated},
    spPaymentNotice_Identifier {http://hl7.org/fhir/SearchParameter/PaymentNoticeidentifier},
    spPaymentNotice_Paymentstatus {http://hl7.org/fhir/SearchParameter/PaymentNoticepaymentstatus},
    spPaymentNotice_Provider {http://hl7.org/fhir/SearchParameter/PaymentNoticeprovider},
    spPaymentNotice_Request {http://hl7.org/fhir/SearchParameter/PaymentNoticerequest},
    spPaymentNotice_Response {http://hl7.org/fhir/SearchParameter/PaymentNoticeresponse},
    spPaymentNotice_Status {http://hl7.org/fhir/SearchParameter/PaymentNoticestatus});
{$ENDIF FHIR_PAYMENTNOTICE}

{$IFDEF FHIR_PAYMENTRECONCILIATION}
  // Search Parameters for PaymentReconciliation
  TSearchParamsPaymentReconciliation = (
    spPaymentReconciliation__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spPaymentReconciliation__filter {http://hl7.org/fhir/SearchParameter/filter},
    spPaymentReconciliation__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spPaymentReconciliation__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spPaymentReconciliation__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spPaymentReconciliation__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spPaymentReconciliation__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spPaymentReconciliation__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spPaymentReconciliation__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spPaymentReconciliation__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spPaymentReconciliation_Created {http://hl7.org/fhir/SearchParameter/PaymentReconciliationcreated},
    spPaymentReconciliation_Disposition {http://hl7.org/fhir/SearchParameter/PaymentReconciliationdisposition},
    spPaymentReconciliation_Identifier {http://hl7.org/fhir/SearchParameter/PaymentReconciliationidentifier},
    spPaymentReconciliation_Outcome {http://hl7.org/fhir/SearchParameter/PaymentReconciliationoutcome},
    spPaymentReconciliation_Paymentissuer {http://hl7.org/fhir/SearchParameter/PaymentReconciliationpaymentissuer},
    spPaymentReconciliation_Request {http://hl7.org/fhir/SearchParameter/PaymentReconciliationrequest},
    spPaymentReconciliation_Requestor {http://hl7.org/fhir/SearchParameter/PaymentReconciliationrequestor},
    spPaymentReconciliation_Status {http://hl7.org/fhir/SearchParameter/PaymentReconciliationstatus});
{$ENDIF FHIR_PAYMENTRECONCILIATION}

{$IFDEF FHIR_PERMISSION}
  // Search Parameters for Permission
  TSearchParamsPermission = (
    spPermission__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spPermission__filter {http://hl7.org/fhir/SearchParameter/filter},
    spPermission__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spPermission__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spPermission__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spPermission__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spPermission__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spPermission__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spPermission__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spPermission__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spPermission_Status {http://hl7.org/fhir/SearchParameter/Permissionstatus});
{$ENDIF FHIR_PERMISSION}

{$IFDEF FHIR_PERSON}
  // Search Parameters for Person
  TSearchParamsPerson = (
    spPerson__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spPerson__filter {http://hl7.org/fhir/SearchParameter/filter},
    spPerson__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spPerson__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spPerson__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spPerson__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spPerson__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spPerson__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spPerson__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spPerson__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spPerson_Address {http://hl7.org/fhir/SearchParameter/individualaddress},
    spPerson_Addresscity {http://hl7.org/fhir/SearchParameter/individualaddresscity},
    spPerson_Addresscountry {http://hl7.org/fhir/SearchParameter/individualaddresscountry},
    spPerson_Addresspostalcode {http://hl7.org/fhir/SearchParameter/individualaddresspostalcode},
    spPerson_Addressstate {http://hl7.org/fhir/SearchParameter/individualaddressstate},
    spPerson_Addressuse {http://hl7.org/fhir/SearchParameter/individualaddressuse},
    spPerson_Birthdate {http://hl7.org/fhir/SearchParameter/individualbirthdate},
    spPerson_Email {http://hl7.org/fhir/SearchParameter/individualemail},
    spPerson_Gender {http://hl7.org/fhir/SearchParameter/individualgender},
    spPerson_Identifier {http://hl7.org/fhir/SearchParameter/Personidentifier},
    spPerson_Link {http://hl7.org/fhir/SearchParameter/Personlink},
    spPerson_Name {http://hl7.org/fhir/SearchParameter/Personname},
    spPerson_Organization {http://hl7.org/fhir/SearchParameter/Personorganization},
    spPerson_Patient {http://hl7.org/fhir/SearchParameter/Personpatient},
    spPerson_Phone {http://hl7.org/fhir/SearchParameter/individualphone},
    spPerson_Phonetic {http://hl7.org/fhir/SearchParameter/individualphonetic},
    spPerson_Practitioner {http://hl7.org/fhir/SearchParameter/Personpractitioner},
    spPerson_Relatedperson {http://hl7.org/fhir/SearchParameter/Personrelatedperson},
    spPerson_Telecom {http://hl7.org/fhir/SearchParameter/individualtelecom});
{$ENDIF FHIR_PERSON}

{$IFDEF FHIR_PLANDEFINITION}
  // Search Parameters for PlanDefinition
  TSearchParamsPlanDefinition = (
    spPlanDefinition__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spPlanDefinition__filter {http://hl7.org/fhir/SearchParameter/filter},
    spPlanDefinition__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spPlanDefinition__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spPlanDefinition__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spPlanDefinition__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spPlanDefinition__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spPlanDefinition__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spPlanDefinition__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spPlanDefinition__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spPlanDefinition_Composedof {http://hl7.org/fhir/SearchParameter/PlanDefinitioncomposedof},
    spPlanDefinition_Context {http://hl7.org/fhir/SearchParameter/PlanDefinitioncontext},
    spPlanDefinition_Contextquantity {http://hl7.org/fhir/SearchParameter/PlanDefinitioncontextquantity},
    spPlanDefinition_Contexttype {http://hl7.org/fhir/SearchParameter/PlanDefinitioncontexttype},
    spPlanDefinition_Contexttypequantity {http://hl7.org/fhir/SearchParameter/PlanDefinitioncontexttypequantity},
    spPlanDefinition_Contexttypevalue {http://hl7.org/fhir/SearchParameter/PlanDefinitioncontexttypevalue},
    spPlanDefinition_Date {http://hl7.org/fhir/SearchParameter/PlanDefinitiondate},
    spPlanDefinition_Definition {http://hl7.org/fhir/SearchParameter/PlanDefinitiondefinition},
    spPlanDefinition_Dependson {http://hl7.org/fhir/SearchParameter/PlanDefinitiondependson},
    spPlanDefinition_Derivedfrom {http://hl7.org/fhir/SearchParameter/PlanDefinitionderivedfrom},
    spPlanDefinition_Description {http://hl7.org/fhir/SearchParameter/PlanDefinitiondescription},
    spPlanDefinition_Effective {http://hl7.org/fhir/SearchParameter/PlanDefinitioneffective},
    spPlanDefinition_Identifier {http://hl7.org/fhir/SearchParameter/PlanDefinitionidentifier},
    spPlanDefinition_Jurisdiction {http://hl7.org/fhir/SearchParameter/PlanDefinitionjurisdiction},
    spPlanDefinition_Name {http://hl7.org/fhir/SearchParameter/PlanDefinitionname},
    spPlanDefinition_Predecessor {http://hl7.org/fhir/SearchParameter/PlanDefinitionpredecessor},
    spPlanDefinition_Publisher {http://hl7.org/fhir/SearchParameter/PlanDefinitionpublisher},
    spPlanDefinition_Status {http://hl7.org/fhir/SearchParameter/PlanDefinitionstatus},
    spPlanDefinition_Successor {http://hl7.org/fhir/SearchParameter/PlanDefinitionsuccessor},
    spPlanDefinition_Title {http://hl7.org/fhir/SearchParameter/PlanDefinitiontitle},
    spPlanDefinition_Topic {http://hl7.org/fhir/SearchParameter/PlanDefinitiontopic},
    spPlanDefinition_Type {http://hl7.org/fhir/SearchParameter/PlanDefinitiontype},
    spPlanDefinition_Url {http://hl7.org/fhir/SearchParameter/PlanDefinitionurl},
    spPlanDefinition_Version {http://hl7.org/fhir/SearchParameter/PlanDefinitionversion});
{$ENDIF FHIR_PLANDEFINITION}

{$IFDEF FHIR_PRACTITIONER}
  // Search Parameters for Practitioner
  TSearchParamsPractitioner = (
    spPractitioner__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spPractitioner__filter {http://hl7.org/fhir/SearchParameter/filter},
    spPractitioner__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spPractitioner__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spPractitioner__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spPractitioner__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spPractitioner__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spPractitioner__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spPractitioner__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spPractitioner__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spPractitioner_Active {http://hl7.org/fhir/SearchParameter/Practitioneractive},
    spPractitioner_Address {http://hl7.org/fhir/SearchParameter/individualaddress},
    spPractitioner_Addresscity {http://hl7.org/fhir/SearchParameter/individualaddresscity},
    spPractitioner_Addresscountry {http://hl7.org/fhir/SearchParameter/individualaddresscountry},
    spPractitioner_Addresspostalcode {http://hl7.org/fhir/SearchParameter/individualaddresspostalcode},
    spPractitioner_Addressstate {http://hl7.org/fhir/SearchParameter/individualaddressstate},
    spPractitioner_Addressuse {http://hl7.org/fhir/SearchParameter/individualaddressuse},
    spPractitioner_Communication {http://hl7.org/fhir/SearchParameter/Practitionercommunication},
    spPractitioner_Email {http://hl7.org/fhir/SearchParameter/individualemail},
    spPractitioner_Family {http://hl7.org/fhir/SearchParameter/individualfamily},
    spPractitioner_Gender {http://hl7.org/fhir/SearchParameter/individualgender},
    spPractitioner_Given {http://hl7.org/fhir/SearchParameter/individualgiven},
    spPractitioner_Identifier {http://hl7.org/fhir/SearchParameter/Practitioneridentifier},
    spPractitioner_Name {http://hl7.org/fhir/SearchParameter/Practitionername},
    spPractitioner_Phone {http://hl7.org/fhir/SearchParameter/individualphone},
    spPractitioner_Phonetic {http://hl7.org/fhir/SearchParameter/individualphonetic},
    spPractitioner_Telecom {http://hl7.org/fhir/SearchParameter/individualtelecom});
{$ENDIF FHIR_PRACTITIONER}

{$IFDEF FHIR_PRACTITIONERROLE}
  // Search Parameters for PractitionerRole
  TSearchParamsPractitionerRole = (
    spPractitionerRole__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spPractitionerRole__filter {http://hl7.org/fhir/SearchParameter/filter},
    spPractitionerRole__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spPractitionerRole__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spPractitionerRole__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spPractitionerRole__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spPractitionerRole__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spPractitionerRole__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spPractitionerRole__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spPractitionerRole__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spPractitionerRole_Active {http://hl7.org/fhir/SearchParameter/PractitionerRoleactive},
    spPractitionerRole_Date {http://hl7.org/fhir/SearchParameter/PractitionerRoledate},
    spPractitionerRole_Email {http://hl7.org/fhir/SearchParameter/individualemail},
    spPractitionerRole_Endpoint {http://hl7.org/fhir/SearchParameter/PractitionerRoleendpoint},
    spPractitionerRole_Identifier {http://hl7.org/fhir/SearchParameter/PractitionerRoleidentifier},
    spPractitionerRole_Location {http://hl7.org/fhir/SearchParameter/PractitionerRolelocation},
    spPractitionerRole_Organization {http://hl7.org/fhir/SearchParameter/PractitionerRoleorganization},
    spPractitionerRole_Phone {http://hl7.org/fhir/SearchParameter/individualphone},
    spPractitionerRole_Practitioner {http://hl7.org/fhir/SearchParameter/PractitionerRolepractitioner},
    spPractitionerRole_Role {http://hl7.org/fhir/SearchParameter/PractitionerRolerole},
    spPractitionerRole_Service {http://hl7.org/fhir/SearchParameter/PractitionerRoleservice},
    spPractitionerRole_Specialty {http://hl7.org/fhir/SearchParameter/PractitionerRolespecialty},
    spPractitionerRole_Telecom {http://hl7.org/fhir/SearchParameter/individualtelecom});
{$ENDIF FHIR_PRACTITIONERROLE}

{$IFDEF FHIR_PROCEDURE}
  // Search Parameters for Procedure
  TSearchParamsProcedure = (
    spProcedure__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spProcedure__filter {http://hl7.org/fhir/SearchParameter/filter},
    spProcedure__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spProcedure__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spProcedure__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spProcedure__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spProcedure__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spProcedure__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spProcedure__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spProcedure__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spProcedure_Basedon {http://hl7.org/fhir/SearchParameter/Procedurebasedon},
    spProcedure_Category {http://hl7.org/fhir/SearchParameter/Procedurecategory},
    spProcedure_Code {http://hl7.org/fhir/SearchParameter/clinicalcode},
    spProcedure_Date {http://hl7.org/fhir/SearchParameter/clinicaldate},
    spProcedure_Encounter {http://hl7.org/fhir/SearchParameter/clinicalencounter},
    spProcedure_Identifier {http://hl7.org/fhir/SearchParameter/clinicalidentifier},
    spProcedure_Instantiatescanonical {http://hl7.org/fhir/SearchParameter/Procedureinstantiatescanonical},
    spProcedure_Instantiatesuri {http://hl7.org/fhir/SearchParameter/Procedureinstantiatesuri},
    spProcedure_Location {http://hl7.org/fhir/SearchParameter/Procedurelocation},
    spProcedure_Partof {http://hl7.org/fhir/SearchParameter/Procedurepartof},
    spProcedure_Patient {http://hl7.org/fhir/SearchParameter/clinicalpatient},
    spProcedure_Performer {http://hl7.org/fhir/SearchParameter/Procedureperformer},
    spProcedure_Reasoncode {http://hl7.org/fhir/SearchParameter/Procedurereasoncode},
    spProcedure_Reasonreference {http://hl7.org/fhir/SearchParameter/Procedurereasonreference},
    spProcedure_Status {http://hl7.org/fhir/SearchParameter/Procedurestatus},
    spProcedure_Subject {http://hl7.org/fhir/SearchParameter/Proceduresubject});
{$ENDIF FHIR_PROCEDURE}

{$IFDEF FHIR_PROVENANCE}
  // Search Parameters for Provenance
  TSearchParamsProvenance = (
    spProvenance__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spProvenance__filter {http://hl7.org/fhir/SearchParameter/filter},
    spProvenance__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spProvenance__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spProvenance__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spProvenance__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spProvenance__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spProvenance__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spProvenance__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spProvenance__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spProvenance_Activity {http://hl7.org/fhir/SearchParameter/Provenanceactivity},
    spProvenance_Agent {http://hl7.org/fhir/SearchParameter/Provenanceagent},
    spProvenance_Agentrole {http://hl7.org/fhir/SearchParameter/Provenanceagentrole},
    spProvenance_Agenttype {http://hl7.org/fhir/SearchParameter/Provenanceagenttype},
    spProvenance_Entity {http://hl7.org/fhir/SearchParameter/Provenanceentity},
    spProvenance_Location {http://hl7.org/fhir/SearchParameter/Provenancelocation},
    spProvenance_Patient {http://hl7.org/fhir/SearchParameter/Provenancepatient},
    spProvenance_Recorded {http://hl7.org/fhir/SearchParameter/Provenancerecorded},
    spProvenance_Signaturetype {http://hl7.org/fhir/SearchParameter/Provenancesignaturetype},
    spProvenance_Target {http://hl7.org/fhir/SearchParameter/Provenancetarget},
    spProvenance_When {http://hl7.org/fhir/SearchParameter/Provenancewhen});
{$ENDIF FHIR_PROVENANCE}

{$IFDEF FHIR_QUESTIONNAIRE}
  // Search Parameters for Questionnaire
  TSearchParamsQuestionnaire = (
    spQuestionnaire__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spQuestionnaire__filter {http://hl7.org/fhir/SearchParameter/filter},
    spQuestionnaire__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spQuestionnaire__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spQuestionnaire__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spQuestionnaire__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spQuestionnaire__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spQuestionnaire__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spQuestionnaire__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spQuestionnaire__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spQuestionnaire_Code {http://hl7.org/fhir/SearchParameter/Questionnairecode},
    spQuestionnaire_Context {http://hl7.org/fhir/SearchParameter/Questionnairecontext},
    spQuestionnaire_Contextquantity {http://hl7.org/fhir/SearchParameter/Questionnairecontextquantity},
    spQuestionnaire_Contexttype {http://hl7.org/fhir/SearchParameter/Questionnairecontexttype},
    spQuestionnaire_Contexttypequantity {http://hl7.org/fhir/SearchParameter/Questionnairecontexttypequantity},
    spQuestionnaire_Contexttypevalue {http://hl7.org/fhir/SearchParameter/Questionnairecontexttypevalue},
    spQuestionnaire_Date {http://hl7.org/fhir/SearchParameter/Questionnairedate},
    spQuestionnaire_Definition {http://hl7.org/fhir/SearchParameter/Questionnairedefinition},
    spQuestionnaire_Description {http://hl7.org/fhir/SearchParameter/Questionnairedescription},
    spQuestionnaire_Effective {http://hl7.org/fhir/SearchParameter/Questionnaireeffective},
    spQuestionnaire_Identifier {http://hl7.org/fhir/SearchParameter/Questionnaireidentifier},
    spQuestionnaire_Jurisdiction {http://hl7.org/fhir/SearchParameter/Questionnairejurisdiction},
    spQuestionnaire_Name {http://hl7.org/fhir/SearchParameter/Questionnairename},
    spQuestionnaire_Publisher {http://hl7.org/fhir/SearchParameter/Questionnairepublisher},
    spQuestionnaire_Status {http://hl7.org/fhir/SearchParameter/Questionnairestatus},
    spQuestionnaire_Subjecttype {http://hl7.org/fhir/SearchParameter/Questionnairesubjecttype},
    spQuestionnaire_Title {http://hl7.org/fhir/SearchParameter/Questionnairetitle},
    spQuestionnaire_Url {http://hl7.org/fhir/SearchParameter/Questionnaireurl},
    spQuestionnaire_Version {http://hl7.org/fhir/SearchParameter/Questionnaireversion});
{$ENDIF FHIR_QUESTIONNAIRE}

{$IFDEF FHIR_QUESTIONNAIRERESPONSE}
  // Search Parameters for QuestionnaireResponse
  TSearchParamsQuestionnaireResponse = (
    spQuestionnaireResponse__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spQuestionnaireResponse__filter {http://hl7.org/fhir/SearchParameter/filter},
    spQuestionnaireResponse__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spQuestionnaireResponse__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spQuestionnaireResponse__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spQuestionnaireResponse__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spQuestionnaireResponse__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spQuestionnaireResponse__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spQuestionnaireResponse__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spQuestionnaireResponse__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spQuestionnaireResponse_Author {http://hl7.org/fhir/SearchParameter/QuestionnaireResponseauthor},
    spQuestionnaireResponse_Authored {http://hl7.org/fhir/SearchParameter/QuestionnaireResponseauthored},
    spQuestionnaireResponse_Basedon {http://hl7.org/fhir/SearchParameter/QuestionnaireResponsebasedon},
    spQuestionnaireResponse_Encounter {http://hl7.org/fhir/SearchParameter/QuestionnaireResponseencounter},
    spQuestionnaireResponse_Identifier {http://hl7.org/fhir/SearchParameter/QuestionnaireResponseidentifier},
    spQuestionnaireResponse_Itemsubject {http://hl7.org/fhir/SearchParameter/questionnaireresponseextensionsQuestionnaireResponseitemsubject},
    spQuestionnaireResponse_Partof {http://hl7.org/fhir/SearchParameter/QuestionnaireResponsepartof},
    spQuestionnaireResponse_Patient {http://hl7.org/fhir/SearchParameter/QuestionnaireResponsepatient},
    spQuestionnaireResponse_Questionnaire {http://hl7.org/fhir/SearchParameter/QuestionnaireResponsequestionnaire},
    spQuestionnaireResponse_Source {http://hl7.org/fhir/SearchParameter/QuestionnaireResponsesource},
    spQuestionnaireResponse_Status {http://hl7.org/fhir/SearchParameter/QuestionnaireResponsestatus},
    spQuestionnaireResponse_Subject {http://hl7.org/fhir/SearchParameter/QuestionnaireResponsesubject});
{$ENDIF FHIR_QUESTIONNAIRERESPONSE}

{$IFDEF FHIR_REGULATEDAUTHORIZATION}
  // Search Parameters for RegulatedAuthorization
  TSearchParamsRegulatedAuthorization = (
    spRegulatedAuthorization__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spRegulatedAuthorization__filter {http://hl7.org/fhir/SearchParameter/filter},
    spRegulatedAuthorization__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spRegulatedAuthorization__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spRegulatedAuthorization__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spRegulatedAuthorization__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spRegulatedAuthorization__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spRegulatedAuthorization__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spRegulatedAuthorization__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spRegulatedAuthorization__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spRegulatedAuthorization_Case {http://hl7.org/fhir/SearchParameter/RegulatedAuthorizationcase},
    spRegulatedAuthorization_Casetype {http://hl7.org/fhir/SearchParameter/RegulatedAuthorizationcasetype},
    spRegulatedAuthorization_Holder {http://hl7.org/fhir/SearchParameter/RegulatedAuthorizationholder},
    spRegulatedAuthorization_Identifier {http://hl7.org/fhir/SearchParameter/RegulatedAuthorizationidentifier},
    spRegulatedAuthorization_Region {http://hl7.org/fhir/SearchParameter/RegulatedAuthorizationregion},
    spRegulatedAuthorization_Status {http://hl7.org/fhir/SearchParameter/RegulatedAuthorizationstatus},
    spRegulatedAuthorization_Subject {http://hl7.org/fhir/SearchParameter/RegulatedAuthorizationsubject});
{$ENDIF FHIR_REGULATEDAUTHORIZATION}

{$IFDEF FHIR_RELATEDPERSON}
  // Search Parameters for RelatedPerson
  TSearchParamsRelatedPerson = (
    spRelatedPerson__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spRelatedPerson__filter {http://hl7.org/fhir/SearchParameter/filter},
    spRelatedPerson__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spRelatedPerson__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spRelatedPerson__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spRelatedPerson__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spRelatedPerson__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spRelatedPerson__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spRelatedPerson__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spRelatedPerson__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spRelatedPerson_Active {http://hl7.org/fhir/SearchParameter/RelatedPersonactive},
    spRelatedPerson_Address {http://hl7.org/fhir/SearchParameter/individualaddress},
    spRelatedPerson_Addresscity {http://hl7.org/fhir/SearchParameter/individualaddresscity},
    spRelatedPerson_Addresscountry {http://hl7.org/fhir/SearchParameter/individualaddresscountry},
    spRelatedPerson_Addresspostalcode {http://hl7.org/fhir/SearchParameter/individualaddresspostalcode},
    spRelatedPerson_Addressstate {http://hl7.org/fhir/SearchParameter/individualaddressstate},
    spRelatedPerson_Addressuse {http://hl7.org/fhir/SearchParameter/individualaddressuse},
    spRelatedPerson_Birthdate {http://hl7.org/fhir/SearchParameter/individualbirthdate},
    spRelatedPerson_Email {http://hl7.org/fhir/SearchParameter/individualemail},
    spRelatedPerson_Gender {http://hl7.org/fhir/SearchParameter/individualgender},
    spRelatedPerson_Identifier {http://hl7.org/fhir/SearchParameter/RelatedPersonidentifier},
    spRelatedPerson_Name {http://hl7.org/fhir/SearchParameter/RelatedPersonname},
    spRelatedPerson_Patient {http://hl7.org/fhir/SearchParameter/RelatedPersonpatient},
    spRelatedPerson_Phone {http://hl7.org/fhir/SearchParameter/individualphone},
    spRelatedPerson_Phonetic {http://hl7.org/fhir/SearchParameter/individualphonetic},
    spRelatedPerson_Relationship {http://hl7.org/fhir/SearchParameter/RelatedPersonrelationship},
    spRelatedPerson_Telecom {http://hl7.org/fhir/SearchParameter/individualtelecom});
{$ENDIF FHIR_RELATEDPERSON}

{$IFDEF FHIR_REQUESTGROUP}
  // Search Parameters for RequestGroup
  TSearchParamsRequestGroup = (
    spRequestGroup__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spRequestGroup__filter {http://hl7.org/fhir/SearchParameter/filter},
    spRequestGroup__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spRequestGroup__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spRequestGroup__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spRequestGroup__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spRequestGroup__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spRequestGroup__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spRequestGroup__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spRequestGroup__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spRequestGroup_Author {http://hl7.org/fhir/SearchParameter/RequestGroupauthor},
    spRequestGroup_Authored {http://hl7.org/fhir/SearchParameter/RequestGroupauthored},
    spRequestGroup_Code {http://hl7.org/fhir/SearchParameter/RequestGroupcode},
    spRequestGroup_Encounter {http://hl7.org/fhir/SearchParameter/RequestGroupencounter},
    spRequestGroup_Groupidentifier {http://hl7.org/fhir/SearchParameter/RequestGroupgroupidentifier},
    spRequestGroup_Identifier {http://hl7.org/fhir/SearchParameter/RequestGroupidentifier},
    spRequestGroup_Instantiatescanonical {http://hl7.org/fhir/SearchParameter/RequestGroupinstantiatescanonical},
    spRequestGroup_Instantiatesuri {http://hl7.org/fhir/SearchParameter/RequestGroupinstantiatesuri},
    spRequestGroup_Intent {http://hl7.org/fhir/SearchParameter/RequestGroupintent},
    spRequestGroup_Participant {http://hl7.org/fhir/SearchParameter/RequestGroupparticipant},
    spRequestGroup_Patient {http://hl7.org/fhir/SearchParameter/RequestGrouppatient},
    spRequestGroup_Priority {http://hl7.org/fhir/SearchParameter/RequestGrouppriority},
    spRequestGroup_Status {http://hl7.org/fhir/SearchParameter/RequestGroupstatus},
    spRequestGroup_Subject {http://hl7.org/fhir/SearchParameter/RequestGroupsubject});
{$ENDIF FHIR_REQUESTGROUP}

{$IFDEF FHIR_RESEARCHSTUDY}
  // Search Parameters for ResearchStudy
  TSearchParamsResearchStudy = (
    spResearchStudy__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spResearchStudy__filter {http://hl7.org/fhir/SearchParameter/filter},
    spResearchStudy__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spResearchStudy__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spResearchStudy__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spResearchStudy__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spResearchStudy__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spResearchStudy__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spResearchStudy__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spResearchStudy__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spResearchStudy_Category {http://hl7.org/fhir/SearchParameter/ResearchStudycategory},
    spResearchStudy_Date {http://hl7.org/fhir/SearchParameter/ResearchStudydate},
    spResearchStudy_Focus {http://hl7.org/fhir/SearchParameter/ResearchStudyfocus},
    spResearchStudy_Identifier {http://hl7.org/fhir/SearchParameter/ResearchStudyidentifier},
    spResearchStudy_Keyword {http://hl7.org/fhir/SearchParameter/ResearchStudykeyword},
    spResearchStudy_Location {http://hl7.org/fhir/SearchParameter/ResearchStudylocation},
    spResearchStudy_Partof {http://hl7.org/fhir/SearchParameter/ResearchStudypartof},
    spResearchStudy_Principalinvestigator {http://hl7.org/fhir/SearchParameter/ResearchStudyprincipalinvestigator},
    spResearchStudy_Protocol {http://hl7.org/fhir/SearchParameter/ResearchStudyprotocol},
    spResearchStudy_Site {http://hl7.org/fhir/SearchParameter/ResearchStudysite},
    spResearchStudy_Sponsor {http://hl7.org/fhir/SearchParameter/ResearchStudysponsor},
    spResearchStudy_Status {http://hl7.org/fhir/SearchParameter/ResearchStudystatus},
    spResearchStudy_Title {http://hl7.org/fhir/SearchParameter/ResearchStudytitle});
{$ENDIF FHIR_RESEARCHSTUDY}

{$IFDEF FHIR_RESEARCHSUBJECT}
  // Search Parameters for ResearchSubject
  TSearchParamsResearchSubject = (
    spResearchSubject__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spResearchSubject__filter {http://hl7.org/fhir/SearchParameter/filter},
    spResearchSubject__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spResearchSubject__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spResearchSubject__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spResearchSubject__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spResearchSubject__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spResearchSubject__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spResearchSubject__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spResearchSubject__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spResearchSubject_Date {http://hl7.org/fhir/SearchParameter/ResearchSubjectdate},
    spResearchSubject_Identifier {http://hl7.org/fhir/SearchParameter/ResearchSubjectidentifier},
    spResearchSubject_Individual {http://hl7.org/fhir/SearchParameter/ResearchSubjectindividual},
    spResearchSubject_Patient {http://hl7.org/fhir/SearchParameter/ResearchSubjectpatient},
    spResearchSubject_State {http://hl7.org/fhir/SearchParameter/ResearchSubjectstate},
    spResearchSubject_Status {http://hl7.org/fhir/SearchParameter/ResearchSubjectstatus},
    spResearchSubject_Study {http://hl7.org/fhir/SearchParameter/ResearchSubjectstudy});
{$ENDIF FHIR_RESEARCHSUBJECT}

{$IFDEF FHIR_RISKASSESSMENT}
  // Search Parameters for RiskAssessment
  TSearchParamsRiskAssessment = (
    spRiskAssessment__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spRiskAssessment__filter {http://hl7.org/fhir/SearchParameter/filter},
    spRiskAssessment__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spRiskAssessment__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spRiskAssessment__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spRiskAssessment__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spRiskAssessment__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spRiskAssessment__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spRiskAssessment__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spRiskAssessment__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spRiskAssessment_Condition {http://hl7.org/fhir/SearchParameter/RiskAssessmentcondition},
    spRiskAssessment_Date {http://hl7.org/fhir/SearchParameter/clinicaldate},
    spRiskAssessment_Encounter {http://hl7.org/fhir/SearchParameter/clinicalencounter},
    spRiskAssessment_Identifier {http://hl7.org/fhir/SearchParameter/clinicalidentifier},
    spRiskAssessment_Method {http://hl7.org/fhir/SearchParameter/RiskAssessmentmethod},
    spRiskAssessment_Patient {http://hl7.org/fhir/SearchParameter/clinicalpatient},
    spRiskAssessment_Performer {http://hl7.org/fhir/SearchParameter/RiskAssessmentperformer},
    spRiskAssessment_Probability {http://hl7.org/fhir/SearchParameter/RiskAssessmentprobability},
    spRiskAssessment_Risk {http://hl7.org/fhir/SearchParameter/RiskAssessmentrisk},
    spRiskAssessment_Subject {http://hl7.org/fhir/SearchParameter/RiskAssessmentsubject});
{$ENDIF FHIR_RISKASSESSMENT}

{$IFDEF FHIR_SCHEDULE}
  // Search Parameters for Schedule
  TSearchParamsSchedule = (
    spSchedule__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spSchedule__filter {http://hl7.org/fhir/SearchParameter/filter},
    spSchedule__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spSchedule__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spSchedule__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spSchedule__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spSchedule__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spSchedule__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spSchedule__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spSchedule__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spSchedule_Active {http://hl7.org/fhir/SearchParameter/Scheduleactive},
    spSchedule_Actor {http://hl7.org/fhir/SearchParameter/Scheduleactor},
    spSchedule_Date {http://hl7.org/fhir/SearchParameter/Scheduledate},
    spSchedule_Identifier {http://hl7.org/fhir/SearchParameter/Scheduleidentifier},
    spSchedule_Servicecategory {http://hl7.org/fhir/SearchParameter/Scheduleservicecategory},
    spSchedule_Servicetype {http://hl7.org/fhir/SearchParameter/Scheduleservicetype},
    spSchedule_Specialty {http://hl7.org/fhir/SearchParameter/Schedulespecialty});
{$ENDIF FHIR_SCHEDULE}

{$IFDEF FHIR_SEARCHPARAMETER}
  // Search Parameters for SearchParameter
  TSearchParamsSearchParameter = (
    spSearchParameter__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spSearchParameter__filter {http://hl7.org/fhir/SearchParameter/filter},
    spSearchParameter__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spSearchParameter__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spSearchParameter__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spSearchParameter__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spSearchParameter__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spSearchParameter__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spSearchParameter__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spSearchParameter__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spSearchParameter_Base {http://hl7.org/fhir/SearchParameter/SearchParameterbase},
    spSearchParameter_Code {http://hl7.org/fhir/SearchParameter/SearchParametercode},
    spSearchParameter_Component {http://hl7.org/fhir/SearchParameter/SearchParametercomponent},
    spSearchParameter_Context {http://hl7.org/fhir/SearchParameter/conformancecontext},
    spSearchParameter_Contextquantity {http://hl7.org/fhir/SearchParameter/conformancecontextquantity},
    spSearchParameter_Contexttype {http://hl7.org/fhir/SearchParameter/conformancecontexttype},
    spSearchParameter_Contexttypequantity {http://hl7.org/fhir/SearchParameter/conformancecontexttypequantity},
    spSearchParameter_Contexttypevalue {http://hl7.org/fhir/SearchParameter/conformancecontexttypevalue},
    spSearchParameter_Date {http://hl7.org/fhir/SearchParameter/conformancedate},
    spSearchParameter_Derivedfrom {http://hl7.org/fhir/SearchParameter/SearchParameterderivedfrom},
    spSearchParameter_Description {http://hl7.org/fhir/SearchParameter/conformancedescription},
    spSearchParameter_Jurisdiction {http://hl7.org/fhir/SearchParameter/conformancejurisdiction},
    spSearchParameter_Name {http://hl7.org/fhir/SearchParameter/conformancename},
    spSearchParameter_Publisher {http://hl7.org/fhir/SearchParameter/conformancepublisher},
    spSearchParameter_Status {http://hl7.org/fhir/SearchParameter/conformancestatus},
    spSearchParameter_Target {http://hl7.org/fhir/SearchParameter/SearchParametertarget},
    spSearchParameter_Type {http://hl7.org/fhir/SearchParameter/SearchParametertype},
    spSearchParameter_Url {http://hl7.org/fhir/SearchParameter/conformanceurl},
    spSearchParameter_Version {http://hl7.org/fhir/SearchParameter/conformanceversion});
{$ENDIF FHIR_SEARCHPARAMETER}

{$IFDEF FHIR_SERVICEREQUEST}
  // Search Parameters for ServiceRequest
  TSearchParamsServiceRequest = (
    spServiceRequest__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spServiceRequest__filter {http://hl7.org/fhir/SearchParameter/filter},
    spServiceRequest__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spServiceRequest__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spServiceRequest__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spServiceRequest__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spServiceRequest__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spServiceRequest__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spServiceRequest__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spServiceRequest__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spServiceRequest_Authored {http://hl7.org/fhir/SearchParameter/ServiceRequestauthored},
    spServiceRequest_Basedon {http://hl7.org/fhir/SearchParameter/ServiceRequestbasedon},
    spServiceRequest_Bodysite {http://hl7.org/fhir/SearchParameter/ServiceRequestbodysite},
    spServiceRequest_Category {http://hl7.org/fhir/SearchParameter/ServiceRequestcategory},
    spServiceRequest_Code {http://hl7.org/fhir/SearchParameter/clinicalcode},
    spServiceRequest_Encounter {http://hl7.org/fhir/SearchParameter/clinicalencounter},
    spServiceRequest_Identifier {http://hl7.org/fhir/SearchParameter/clinicalidentifier},
    spServiceRequest_Instantiatescanonical {http://hl7.org/fhir/SearchParameter/ServiceRequestinstantiatescanonical},
    spServiceRequest_Instantiatesuri {http://hl7.org/fhir/SearchParameter/ServiceRequestinstantiatesuri},
    spServiceRequest_Intent {http://hl7.org/fhir/SearchParameter/ServiceRequestintent},
    spServiceRequest_Occurrence {http://hl7.org/fhir/SearchParameter/ServiceRequestoccurrence},
    spServiceRequest_Patient {http://hl7.org/fhir/SearchParameter/clinicalpatient},
    spServiceRequest_Performer {http://hl7.org/fhir/SearchParameter/ServiceRequestperformer},
    spServiceRequest_Performertype {http://hl7.org/fhir/SearchParameter/ServiceRequestperformertype},
    spServiceRequest_Priority {http://hl7.org/fhir/SearchParameter/ServiceRequestpriority},
    spServiceRequest_Replaces {http://hl7.org/fhir/SearchParameter/ServiceRequestreplaces},
    spServiceRequest_Requester {http://hl7.org/fhir/SearchParameter/ServiceRequestrequester},
    spServiceRequest_Requisition {http://hl7.org/fhir/SearchParameter/ServiceRequestrequisition},
    spServiceRequest_Specimen {http://hl7.org/fhir/SearchParameter/ServiceRequestspecimen},
    spServiceRequest_Status {http://hl7.org/fhir/SearchParameter/ServiceRequeststatus},
    spServiceRequest_Subject {http://hl7.org/fhir/SearchParameter/ServiceRequestsubject});
{$ENDIF FHIR_SERVICEREQUEST}

{$IFDEF FHIR_SLOT}
  // Search Parameters for Slot
  TSearchParamsSlot = (
    spSlot__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spSlot__filter {http://hl7.org/fhir/SearchParameter/filter},
    spSlot__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spSlot__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spSlot__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spSlot__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spSlot__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spSlot__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spSlot__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spSlot__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spSlot_Appointmenttype {http://hl7.org/fhir/SearchParameter/Slotappointmenttype},
    spSlot_Identifier {http://hl7.org/fhir/SearchParameter/Slotidentifier},
    spSlot_Schedule {http://hl7.org/fhir/SearchParameter/Slotschedule},
    spSlot_Servicecategory {http://hl7.org/fhir/SearchParameter/Slotservicecategory},
    spSlot_Servicetype {http://hl7.org/fhir/SearchParameter/Slotservicetype},
    spSlot_Specialty {http://hl7.org/fhir/SearchParameter/Slotspecialty},
    spSlot_Start {http://hl7.org/fhir/SearchParameter/Slotstart},
    spSlot_Status {http://hl7.org/fhir/SearchParameter/Slotstatus});
{$ENDIF FHIR_SLOT}

{$IFDEF FHIR_SPECIMEN}
  // Search Parameters for Specimen
  TSearchParamsSpecimen = (
    spSpecimen__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spSpecimen__filter {http://hl7.org/fhir/SearchParameter/filter},
    spSpecimen__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spSpecimen__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spSpecimen__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spSpecimen__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spSpecimen__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spSpecimen__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spSpecimen__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spSpecimen__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spSpecimen_Accession {http://hl7.org/fhir/SearchParameter/Specimenaccession},
    spSpecimen_Bodysite {http://hl7.org/fhir/SearchParameter/Specimenbodysite},
    spSpecimen_Collected {http://hl7.org/fhir/SearchParameter/Specimencollected},
    spSpecimen_Collector {http://hl7.org/fhir/SearchParameter/Specimencollector},
    spSpecimen_Container {http://hl7.org/fhir/SearchParameter/Specimencontainer},
    spSpecimen_Containerid {http://hl7.org/fhir/SearchParameter/Specimencontainerid},
    spSpecimen_Identifier {http://hl7.org/fhir/SearchParameter/Specimenidentifier},
    spSpecimen_Parent {http://hl7.org/fhir/SearchParameter/Specimenparent},
    spSpecimen_Patient {http://hl7.org/fhir/SearchParameter/Specimenpatient},
    spSpecimen_Status {http://hl7.org/fhir/SearchParameter/Specimenstatus},
    spSpecimen_Subject {http://hl7.org/fhir/SearchParameter/Specimensubject},
    spSpecimen_Type {http://hl7.org/fhir/SearchParameter/Specimentype});
{$ENDIF FHIR_SPECIMEN}

{$IFDEF FHIR_SPECIMENDEFINITION}
  // Search Parameters for SpecimenDefinition
  TSearchParamsSpecimenDefinition = (
    spSpecimenDefinition__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spSpecimenDefinition__filter {http://hl7.org/fhir/SearchParameter/filter},
    spSpecimenDefinition__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spSpecimenDefinition__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spSpecimenDefinition__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spSpecimenDefinition__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spSpecimenDefinition__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spSpecimenDefinition__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spSpecimenDefinition__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spSpecimenDefinition__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spSpecimenDefinition_Container {http://hl7.org/fhir/SearchParameter/SpecimenDefinitioncontainer},
    spSpecimenDefinition_Experimental {http://hl7.org/fhir/SearchParameter/SpecimenDefinitionexperimental},
    spSpecimenDefinition_Identifier {http://hl7.org/fhir/SearchParameter/SpecimenDefinitionidentifier},
    spSpecimenDefinition_Isderived {http://hl7.org/fhir/SearchParameter/SpecimenDefinitionisderived},
    spSpecimenDefinition_Status {http://hl7.org/fhir/SearchParameter/SpecimenDefinitionstatus},
    spSpecimenDefinition_Title {http://hl7.org/fhir/SearchParameter/SpecimenDefinitiontitle},
    spSpecimenDefinition_Type {http://hl7.org/fhir/SearchParameter/SpecimenDefinitiontype},
    spSpecimenDefinition_Typetested {http://hl7.org/fhir/SearchParameter/SpecimenDefinitiontypetested},
    spSpecimenDefinition_Url {http://hl7.org/fhir/SearchParameter/SpecimenDefinitionurl});
{$ENDIF FHIR_SPECIMENDEFINITION}

{$IFDEF FHIR_STRUCTUREDEFINITION}
  // Search Parameters for StructureDefinition
  TSearchParamsStructureDefinition = (
    spStructureDefinition__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spStructureDefinition__filter {http://hl7.org/fhir/SearchParameter/filter},
    spStructureDefinition__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spStructureDefinition__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spStructureDefinition__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spStructureDefinition__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spStructureDefinition__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spStructureDefinition__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spStructureDefinition__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spStructureDefinition__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spStructureDefinition_Abstract {http://hl7.org/fhir/SearchParameter/StructureDefinitionabstract},
    spStructureDefinition_Base {http://hl7.org/fhir/SearchParameter/StructureDefinitionbase},
    spStructureDefinition_Basepath {http://hl7.org/fhir/SearchParameter/StructureDefinitionbasepath},
    spStructureDefinition_Context {http://hl7.org/fhir/SearchParameter/conformancecontext},
    spStructureDefinition_Contextquantity {http://hl7.org/fhir/SearchParameter/conformancecontextquantity},
    spStructureDefinition_Contexttype {http://hl7.org/fhir/SearchParameter/conformancecontexttype},
    spStructureDefinition_Contexttypequantity {http://hl7.org/fhir/SearchParameter/conformancecontexttypequantity},
    spStructureDefinition_Contexttypevalue {http://hl7.org/fhir/SearchParameter/conformancecontexttypevalue},
    spStructureDefinition_Date {http://hl7.org/fhir/SearchParameter/conformancedate},
    spStructureDefinition_Derivation {http://hl7.org/fhir/SearchParameter/StructureDefinitionderivation},
    spStructureDefinition_Description {http://hl7.org/fhir/SearchParameter/conformancedescription},
    spStructureDefinition_Experimental {http://hl7.org/fhir/SearchParameter/StructureDefinitionexperimental},
    spStructureDefinition_Extcontext {http://hl7.org/fhir/SearchParameter/StructureDefinitionextcontext},
    spStructureDefinition_Identifier {http://hl7.org/fhir/SearchParameter/conformanceidentifier},
    spStructureDefinition_Jurisdiction {http://hl7.org/fhir/SearchParameter/conformancejurisdiction},
    spStructureDefinition_Keyword {http://hl7.org/fhir/SearchParameter/StructureDefinitionkeyword},
    spStructureDefinition_Kind {http://hl7.org/fhir/SearchParameter/StructureDefinitionkind},
    spStructureDefinition_Name {http://hl7.org/fhir/SearchParameter/conformancename},
    spStructureDefinition_Path {http://hl7.org/fhir/SearchParameter/StructureDefinitionpath},
    spStructureDefinition_Publisher {http://hl7.org/fhir/SearchParameter/conformancepublisher},
    spStructureDefinition_Status {http://hl7.org/fhir/SearchParameter/conformancestatus},
    spStructureDefinition_Title {http://hl7.org/fhir/SearchParameter/conformancetitle},
    spStructureDefinition_Type {http://hl7.org/fhir/SearchParameter/StructureDefinitiontype},
    spStructureDefinition_Url {http://hl7.org/fhir/SearchParameter/conformanceurl},
    spStructureDefinition_Valueset {http://hl7.org/fhir/SearchParameter/StructureDefinitionvalueset},
    spStructureDefinition_Version {http://hl7.org/fhir/SearchParameter/conformanceversion});
{$ENDIF FHIR_STRUCTUREDEFINITION}

{$IFDEF FHIR_STRUCTUREMAP}
  // Search Parameters for StructureMap
  TSearchParamsStructureMap = (
    spStructureMap__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spStructureMap__filter {http://hl7.org/fhir/SearchParameter/filter},
    spStructureMap__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spStructureMap__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spStructureMap__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spStructureMap__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spStructureMap__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spStructureMap__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spStructureMap__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spStructureMap__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spStructureMap_Context {http://hl7.org/fhir/SearchParameter/conformancecontext},
    spStructureMap_Contextquantity {http://hl7.org/fhir/SearchParameter/conformancecontextquantity},
    spStructureMap_Contexttype {http://hl7.org/fhir/SearchParameter/conformancecontexttype},
    spStructureMap_Contexttypequantity {http://hl7.org/fhir/SearchParameter/conformancecontexttypequantity},
    spStructureMap_Contexttypevalue {http://hl7.org/fhir/SearchParameter/conformancecontexttypevalue},
    spStructureMap_Date {http://hl7.org/fhir/SearchParameter/conformancedate},
    spStructureMap_Description {http://hl7.org/fhir/SearchParameter/conformancedescription},
    spStructureMap_Identifier {http://hl7.org/fhir/SearchParameter/conformanceidentifier},
    spStructureMap_Jurisdiction {http://hl7.org/fhir/SearchParameter/conformancejurisdiction},
    spStructureMap_Name {http://hl7.org/fhir/SearchParameter/conformancename},
    spStructureMap_Publisher {http://hl7.org/fhir/SearchParameter/conformancepublisher},
    spStructureMap_Status {http://hl7.org/fhir/SearchParameter/conformancestatus},
    spStructureMap_Title {http://hl7.org/fhir/SearchParameter/conformancetitle},
    spStructureMap_Url {http://hl7.org/fhir/SearchParameter/conformanceurl},
    spStructureMap_Version {http://hl7.org/fhir/SearchParameter/conformanceversion});
{$ENDIF FHIR_STRUCTUREMAP}

{$IFDEF FHIR_SUBSCRIPTION}
  // Search Parameters for Subscription
  TSearchParamsSubscription = (
    spSubscription__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spSubscription__filter {http://hl7.org/fhir/SearchParameter/filter},
    spSubscription__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spSubscription__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spSubscription__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spSubscription__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spSubscription__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spSubscription__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spSubscription__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spSubscription__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spSubscription_Contact {http://hl7.org/fhir/SearchParameter/Subscriptioncontact},
    spSubscription_Payload {http://hl7.org/fhir/SearchParameter/Subscriptionpayload},
    spSubscription_Status {http://hl7.org/fhir/SearchParameter/Subscriptionstatus},
    spSubscription_Type {http://hl7.org/fhir/SearchParameter/Subscriptiontype},
    spSubscription_Url {http://hl7.org/fhir/SearchParameter/Subscriptionurl});
{$ENDIF FHIR_SUBSCRIPTION}

{$IFDEF FHIR_SUBSCRIPTIONSTATUS}
  // Search Parameters for SubscriptionStatus
  TSearchParamsSubscriptionStatus = (
    spSubscriptionStatus__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spSubscriptionStatus__filter {http://hl7.org/fhir/SearchParameter/filter},
    spSubscriptionStatus__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spSubscriptionStatus__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spSubscriptionStatus__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spSubscriptionStatus__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spSubscriptionStatus__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spSubscriptionStatus__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spSubscriptionStatus__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spSubscriptionStatus__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext});
{$ENDIF FHIR_SUBSCRIPTIONSTATUS}

{$IFDEF FHIR_SUBSCRIPTIONTOPIC}
  // Search Parameters for SubscriptionTopic
  TSearchParamsSubscriptionTopic = (
    spSubscriptionTopic__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spSubscriptionTopic__filter {http://hl7.org/fhir/SearchParameter/filter},
    spSubscriptionTopic__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spSubscriptionTopic__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spSubscriptionTopic__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spSubscriptionTopic__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spSubscriptionTopic__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spSubscriptionTopic__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spSubscriptionTopic__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spSubscriptionTopic__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spSubscriptionTopic_Date {http://hl7.org/fhir/SearchParameter/SubscriptionTopicdate},
    spSubscriptionTopic_Derivedorself {http://hl7.org/fhir/SearchParameter/SubscriptionTopicderivedorself},
    spSubscriptionTopic_Identifier {http://hl7.org/fhir/SearchParameter/SubscriptionTopicidentifier},
    spSubscriptionTopic_Publisher {http://hl7.org/fhir/SearchParameter/SubscriptionTopicpublisher},
    spSubscriptionTopic_Resourcetype {http://hl7.org/fhir/SearchParameter/SubscriptionTopicresourcetype},
    spSubscriptionTopic_Status {http://hl7.org/fhir/SearchParameter/SubscriptionTopicstatus},
    spSubscriptionTopic_Title {http://hl7.org/fhir/SearchParameter/SubscriptionTopictitle},
    spSubscriptionTopic_Triggerdescription {http://hl7.org/fhir/SearchParameter/SubscriptionTopictriggerdescription},
    spSubscriptionTopic_Url {http://hl7.org/fhir/SearchParameter/SubscriptionTopicurl},
    spSubscriptionTopic_Version {http://hl7.org/fhir/SearchParameter/SubscriptionTopicversion});
{$ENDIF FHIR_SUBSCRIPTIONTOPIC}

{$IFDEF FHIR_SUBSTANCE}
  // Search Parameters for Substance
  TSearchParamsSubstance = (
    spSubstance__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spSubstance__filter {http://hl7.org/fhir/SearchParameter/filter},
    spSubstance__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spSubstance__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spSubstance__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spSubstance__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spSubstance__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spSubstance__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spSubstance__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spSubstance__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spSubstance_Category {http://hl7.org/fhir/SearchParameter/Substancecategory},
    spSubstance_Code {http://hl7.org/fhir/SearchParameter/Substancecode},
    spSubstance_Containeridentifier {http://hl7.org/fhir/SearchParameter/Substancecontaineridentifier},
    spSubstance_Expiry {http://hl7.org/fhir/SearchParameter/Substanceexpiry},
    spSubstance_Identifier {http://hl7.org/fhir/SearchParameter/Substanceidentifier},
    spSubstance_Quantity {http://hl7.org/fhir/SearchParameter/Substancequantity},
    spSubstance_Status {http://hl7.org/fhir/SearchParameter/Substancestatus},
    spSubstance_Substancereference {http://hl7.org/fhir/SearchParameter/Substancesubstancereference});
{$ENDIF FHIR_SUBSTANCE}

{$IFDEF FHIR_SUBSTANCEDEFINITION}
  // Search Parameters for SubstanceDefinition
  TSearchParamsSubstanceDefinition = (
    spSubstanceDefinition__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spSubstanceDefinition__filter {http://hl7.org/fhir/SearchParameter/filter},
    spSubstanceDefinition__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spSubstanceDefinition__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spSubstanceDefinition__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spSubstanceDefinition__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spSubstanceDefinition__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spSubstanceDefinition__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spSubstanceDefinition__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spSubstanceDefinition__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spSubstanceDefinition_Category {http://hl7.org/fhir/SearchParameter/SubstanceDefinitioncategory},
    spSubstanceDefinition_Code {http://hl7.org/fhir/SearchParameter/SubstanceDefinitioncode},
    spSubstanceDefinition_Domain {http://hl7.org/fhir/SearchParameter/SubstanceDefinitiondomain},
    spSubstanceDefinition_Identifier {http://hl7.org/fhir/SearchParameter/SubstanceDefinitionidentifier},
    spSubstanceDefinition_Name {http://hl7.org/fhir/SearchParameter/SubstanceDefinitionname});
{$ENDIF FHIR_SUBSTANCEDEFINITION}

{$IFDEF FHIR_SUBSTANCENUCLEICACID}
  // Search Parameters for SubstanceNucleicAcid
  TSearchParamsSubstanceNucleicAcid = (
    spSubstanceNucleicAcid__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spSubstanceNucleicAcid__filter {http://hl7.org/fhir/SearchParameter/filter},
    spSubstanceNucleicAcid__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spSubstanceNucleicAcid__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spSubstanceNucleicAcid__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spSubstanceNucleicAcid__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spSubstanceNucleicAcid__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spSubstanceNucleicAcid__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spSubstanceNucleicAcid__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spSubstanceNucleicAcid__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext});
{$ENDIF FHIR_SUBSTANCENUCLEICACID}

{$IFDEF FHIR_SUBSTANCEPOLYMER}
  // Search Parameters for SubstancePolymer
  TSearchParamsSubstancePolymer = (
    spSubstancePolymer__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spSubstancePolymer__filter {http://hl7.org/fhir/SearchParameter/filter},
    spSubstancePolymer__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spSubstancePolymer__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spSubstancePolymer__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spSubstancePolymer__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spSubstancePolymer__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spSubstancePolymer__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spSubstancePolymer__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spSubstancePolymer__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext});
{$ENDIF FHIR_SUBSTANCEPOLYMER}

{$IFDEF FHIR_SUBSTANCEPROTEIN}
  // Search Parameters for SubstanceProtein
  TSearchParamsSubstanceProtein = (
    spSubstanceProtein__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spSubstanceProtein__filter {http://hl7.org/fhir/SearchParameter/filter},
    spSubstanceProtein__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spSubstanceProtein__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spSubstanceProtein__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spSubstanceProtein__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spSubstanceProtein__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spSubstanceProtein__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spSubstanceProtein__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spSubstanceProtein__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext});
{$ENDIF FHIR_SUBSTANCEPROTEIN}

{$IFDEF FHIR_SUBSTANCEREFERENCEINFORMATION}
  // Search Parameters for SubstanceReferenceInformation
  TSearchParamsSubstanceReferenceInformation = (
    spSubstanceReferenceInformation__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spSubstanceReferenceInformation__filter {http://hl7.org/fhir/SearchParameter/filter},
    spSubstanceReferenceInformation__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spSubstanceReferenceInformation__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spSubstanceReferenceInformation__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spSubstanceReferenceInformation__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spSubstanceReferenceInformation__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spSubstanceReferenceInformation__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spSubstanceReferenceInformation__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spSubstanceReferenceInformation__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext});
{$ENDIF FHIR_SUBSTANCEREFERENCEINFORMATION}

{$IFDEF FHIR_SUBSTANCESOURCEMATERIAL}
  // Search Parameters for SubstanceSourceMaterial
  TSearchParamsSubstanceSourceMaterial = (
    spSubstanceSourceMaterial__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spSubstanceSourceMaterial__filter {http://hl7.org/fhir/SearchParameter/filter},
    spSubstanceSourceMaterial__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spSubstanceSourceMaterial__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spSubstanceSourceMaterial__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spSubstanceSourceMaterial__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spSubstanceSourceMaterial__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spSubstanceSourceMaterial__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spSubstanceSourceMaterial__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spSubstanceSourceMaterial__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext});
{$ENDIF FHIR_SUBSTANCESOURCEMATERIAL}

{$IFDEF FHIR_SUPPLYDELIVERY}
  // Search Parameters for SupplyDelivery
  TSearchParamsSupplyDelivery = (
    spSupplyDelivery__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spSupplyDelivery__filter {http://hl7.org/fhir/SearchParameter/filter},
    spSupplyDelivery__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spSupplyDelivery__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spSupplyDelivery__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spSupplyDelivery__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spSupplyDelivery__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spSupplyDelivery__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spSupplyDelivery__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spSupplyDelivery__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spSupplyDelivery_Identifier {http://hl7.org/fhir/SearchParameter/clinicalidentifier},
    spSupplyDelivery_Patient {http://hl7.org/fhir/SearchParameter/clinicalpatient},
    spSupplyDelivery_Receiver {http://hl7.org/fhir/SearchParameter/SupplyDeliveryreceiver},
    spSupplyDelivery_Status {http://hl7.org/fhir/SearchParameter/SupplyDeliverystatus},
    spSupplyDelivery_Supplier {http://hl7.org/fhir/SearchParameter/SupplyDeliverysupplier});
{$ENDIF FHIR_SUPPLYDELIVERY}

{$IFDEF FHIR_SUPPLYREQUEST}
  // Search Parameters for SupplyRequest
  TSearchParamsSupplyRequest = (
    spSupplyRequest__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spSupplyRequest__filter {http://hl7.org/fhir/SearchParameter/filter},
    spSupplyRequest__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spSupplyRequest__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spSupplyRequest__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spSupplyRequest__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spSupplyRequest__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spSupplyRequest__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spSupplyRequest__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spSupplyRequest__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spSupplyRequest_Category {http://hl7.org/fhir/SearchParameter/SupplyRequestcategory},
    spSupplyRequest_Date {http://hl7.org/fhir/SearchParameter/clinicaldate},
    spSupplyRequest_Identifier {http://hl7.org/fhir/SearchParameter/clinicalidentifier},
    spSupplyRequest_Requester {http://hl7.org/fhir/SearchParameter/SupplyRequestrequester},
    spSupplyRequest_Status {http://hl7.org/fhir/SearchParameter/SupplyRequeststatus},
    spSupplyRequest_Subject {http://hl7.org/fhir/SearchParameter/SupplyRequestsubject},
    spSupplyRequest_Supplier {http://hl7.org/fhir/SearchParameter/SupplyRequestsupplier});
{$ENDIF FHIR_SUPPLYREQUEST}

{$IFDEF FHIR_TASK}
  // Search Parameters for Task
  TSearchParamsTask = (
    spTask__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spTask__filter {http://hl7.org/fhir/SearchParameter/filter},
    spTask__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spTask__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spTask__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spTask__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spTask__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spTask__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spTask__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spTask__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spTask_Authoredon {http://hl7.org/fhir/SearchParameter/Taskauthoredon},
    spTask_Basedon {http://hl7.org/fhir/SearchParameter/Taskbasedon},
    spTask_Businessstatus {http://hl7.org/fhir/SearchParameter/Taskbusinessstatus},
    spTask_Code {http://hl7.org/fhir/SearchParameter/Taskcode},
    spTask_Encounter {http://hl7.org/fhir/SearchParameter/Taskencounter},
    spTask_Focus {http://hl7.org/fhir/SearchParameter/Taskfocus},
    spTask_Groupidentifier {http://hl7.org/fhir/SearchParameter/Taskgroupidentifier},
    spTask_Identifier {http://hl7.org/fhir/SearchParameter/Taskidentifier},
    spTask_Intent {http://hl7.org/fhir/SearchParameter/Taskintent},
    spTask_Modified {http://hl7.org/fhir/SearchParameter/Taskmodified},
    spTask_Owner {http://hl7.org/fhir/SearchParameter/Taskowner},
    spTask_Partof {http://hl7.org/fhir/SearchParameter/Taskpartof},
    spTask_Patient {http://hl7.org/fhir/SearchParameter/Taskpatient},
    spTask_Performer {http://hl7.org/fhir/SearchParameter/Taskperformer},
    spTask_Period {http://hl7.org/fhir/SearchParameter/Taskperiod},
    spTask_Priority {http://hl7.org/fhir/SearchParameter/Taskpriority},
    spTask_Requester {http://hl7.org/fhir/SearchParameter/Taskrequester},
    spTask_Status {http://hl7.org/fhir/SearchParameter/Taskstatus},
    spTask_Subject {http://hl7.org/fhir/SearchParameter/Tasksubject});
{$ENDIF FHIR_TASK}

{$IFDEF FHIR_TERMINOLOGYCAPABILITIES}
  // Search Parameters for TerminologyCapabilities
  TSearchParamsTerminologyCapabilities = (
    spTerminologyCapabilities__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spTerminologyCapabilities__filter {http://hl7.org/fhir/SearchParameter/filter},
    spTerminologyCapabilities__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spTerminologyCapabilities__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spTerminologyCapabilities__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spTerminologyCapabilities__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spTerminologyCapabilities__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spTerminologyCapabilities__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spTerminologyCapabilities__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spTerminologyCapabilities__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spTerminologyCapabilities_Context {http://hl7.org/fhir/SearchParameter/conformancecontext},
    spTerminologyCapabilities_Contextquantity {http://hl7.org/fhir/SearchParameter/conformancecontextquantity},
    spTerminologyCapabilities_Contexttype {http://hl7.org/fhir/SearchParameter/conformancecontexttype},
    spTerminologyCapabilities_Contexttypequantity {http://hl7.org/fhir/SearchParameter/conformancecontexttypequantity},
    spTerminologyCapabilities_Contexttypevalue {http://hl7.org/fhir/SearchParameter/conformancecontexttypevalue},
    spTerminologyCapabilities_Date {http://hl7.org/fhir/SearchParameter/conformancedate},
    spTerminologyCapabilities_Description {http://hl7.org/fhir/SearchParameter/conformancedescription},
    spTerminologyCapabilities_Identifier {http://hl7.org/fhir/SearchParameter/conformanceidentifier},
    spTerminologyCapabilities_Jurisdiction {http://hl7.org/fhir/SearchParameter/conformancejurisdiction},
    spTerminologyCapabilities_Name {http://hl7.org/fhir/SearchParameter/conformancename},
    spTerminologyCapabilities_Publisher {http://hl7.org/fhir/SearchParameter/conformancepublisher},
    spTerminologyCapabilities_Status {http://hl7.org/fhir/SearchParameter/conformancestatus},
    spTerminologyCapabilities_Title {http://hl7.org/fhir/SearchParameter/conformancetitle},
    spTerminologyCapabilities_Url {http://hl7.org/fhir/SearchParameter/conformanceurl},
    spTerminologyCapabilities_Version {http://hl7.org/fhir/SearchParameter/conformanceversion});
{$ENDIF FHIR_TERMINOLOGYCAPABILITIES}

{$IFDEF FHIR_TESTREPORT}
  // Search Parameters for TestReport
  TSearchParamsTestReport = (
    spTestReport__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spTestReport__filter {http://hl7.org/fhir/SearchParameter/filter},
    spTestReport__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spTestReport__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spTestReport__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spTestReport__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spTestReport__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spTestReport__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spTestReport__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spTestReport__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spTestReport_Identifier {http://hl7.org/fhir/SearchParameter/TestReportidentifier},
    spTestReport_Issued {http://hl7.org/fhir/SearchParameter/TestReportissued},
    spTestReport_Participant {http://hl7.org/fhir/SearchParameter/TestReportparticipant},
    spTestReport_Result {http://hl7.org/fhir/SearchParameter/TestReportresult},
    spTestReport_Tester {http://hl7.org/fhir/SearchParameter/TestReporttester},
    spTestReport_Testscript {http://hl7.org/fhir/SearchParameter/TestReporttestscript});
{$ENDIF FHIR_TESTREPORT}

{$IFDEF FHIR_TESTSCRIPT}
  // Search Parameters for TestScript
  TSearchParamsTestScript = (
    spTestScript__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spTestScript__filter {http://hl7.org/fhir/SearchParameter/filter},
    spTestScript__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spTestScript__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spTestScript__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spTestScript__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spTestScript__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spTestScript__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spTestScript__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spTestScript__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spTestScript_Context {http://hl7.org/fhir/SearchParameter/TestScriptcontext},
    spTestScript_Contextquantity {http://hl7.org/fhir/SearchParameter/TestScriptcontextquantity},
    spTestScript_Contexttype {http://hl7.org/fhir/SearchParameter/TestScriptcontexttype},
    spTestScript_Contexttypequantity {http://hl7.org/fhir/SearchParameter/TestScriptcontexttypequantity},
    spTestScript_Contexttypevalue {http://hl7.org/fhir/SearchParameter/TestScriptcontexttypevalue},
    spTestScript_Date {http://hl7.org/fhir/SearchParameter/TestScriptdate},
    spTestScript_Description {http://hl7.org/fhir/SearchParameter/TestScriptdescription},
    spTestScript_Identifier {http://hl7.org/fhir/SearchParameter/TestScriptidentifier},
    spTestScript_Jurisdiction {http://hl7.org/fhir/SearchParameter/TestScriptjurisdiction},
    spTestScript_Name {http://hl7.org/fhir/SearchParameter/TestScriptname},
    spTestScript_Publisher {http://hl7.org/fhir/SearchParameter/TestScriptpublisher},
    spTestScript_Status {http://hl7.org/fhir/SearchParameter/TestScriptstatus},
    spTestScript_Testscriptcapability {http://hl7.org/fhir/SearchParameter/TestScripttestscriptcapability},
    spTestScript_Title {http://hl7.org/fhir/SearchParameter/TestScripttitle},
    spTestScript_Url {http://hl7.org/fhir/SearchParameter/TestScripturl},
    spTestScript_Version {http://hl7.org/fhir/SearchParameter/TestScriptversion});
{$ENDIF FHIR_TESTSCRIPT}

{$IFDEF FHIR_VALUESET}
  // Search Parameters for ValueSet
  TSearchParamsValueSet = (
    spValueSet__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spValueSet__filter {http://hl7.org/fhir/SearchParameter/filter},
    spValueSet__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spValueSet__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spValueSet__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spValueSet__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spValueSet__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spValueSet__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spValueSet__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spValueSet__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spValueSet_Code {http://hl7.org/fhir/SearchParameter/ValueSetcode},
    spValueSet_Context {http://hl7.org/fhir/SearchParameter/conformancecontext},
    spValueSet_Contextquantity {http://hl7.org/fhir/SearchParameter/conformancecontextquantity},
    spValueSet_Contexttype {http://hl7.org/fhir/SearchParameter/conformancecontexttype},
    spValueSet_Contexttypequantity {http://hl7.org/fhir/SearchParameter/conformancecontexttypequantity},
    spValueSet_Contexttypevalue {http://hl7.org/fhir/SearchParameter/conformancecontexttypevalue},
    spValueSet_Date {http://hl7.org/fhir/SearchParameter/conformancedate},
    spValueSet_Description {http://hl7.org/fhir/SearchParameter/conformancedescription},
    spValueSet_Expansion {http://hl7.org/fhir/SearchParameter/ValueSetexpansion},
    spValueSet_Identifier {http://hl7.org/fhir/SearchParameter/conformanceidentifier},
    spValueSet_Jurisdiction {http://hl7.org/fhir/SearchParameter/conformancejurisdiction},
    spValueSet_Name {http://hl7.org/fhir/SearchParameter/conformancename},
    spValueSet_Publisher {http://hl7.org/fhir/SearchParameter/conformancepublisher},
    spValueSet_Reference {http://hl7.org/fhir/SearchParameter/ValueSetreference},
    spValueSet_Status {http://hl7.org/fhir/SearchParameter/conformancestatus},
    spValueSet_Title {http://hl7.org/fhir/SearchParameter/conformancetitle},
    spValueSet_Url {http://hl7.org/fhir/SearchParameter/conformanceurl},
    spValueSet_Version {http://hl7.org/fhir/SearchParameter/conformanceversion});
{$ENDIF FHIR_VALUESET}

{$IFDEF FHIR_VERIFICATIONRESULT}
  // Search Parameters for VerificationResult
  TSearchParamsVerificationResult = (
    spVerificationResult__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spVerificationResult__filter {http://hl7.org/fhir/SearchParameter/filter},
    spVerificationResult__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spVerificationResult__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spVerificationResult__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spVerificationResult__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spVerificationResult__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spVerificationResult__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spVerificationResult__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spVerificationResult__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spVerificationResult_Target {http://hl7.org/fhir/SearchParameter/VerificationResulttarget});
{$ENDIF FHIR_VERIFICATIONRESULT}

{$IFDEF FHIR_VISIONPRESCRIPTION}
  // Search Parameters for VisionPrescription
  TSearchParamsVisionPrescription = (
    spVisionPrescription__content {http://hl7.org/fhir/SearchParameter/Resourcecontent},
    spVisionPrescription__filter {http://hl7.org/fhir/SearchParameter/filter},
    spVisionPrescription__id {http://hl7.org/fhir/SearchParameter/Resourceid},
    spVisionPrescription__lastUpdated {http://hl7.org/fhir/SearchParameter/ResourcelastUpdated},
    spVisionPrescription__profile {http://hl7.org/fhir/SearchParameter/Resourceprofile},
    spVisionPrescription__query {http://hl7.org/fhir/SearchParameter/Resourcequery},
    spVisionPrescription__security {http://hl7.org/fhir/SearchParameter/Resourcesecurity},
    spVisionPrescription__source {http://hl7.org/fhir/SearchParameter/Resourcesource},
    spVisionPrescription__tag {http://hl7.org/fhir/SearchParameter/Resourcetag},
    spVisionPrescription__text {http://hl7.org/fhir/SearchParameter/DomainResourcetext},
    spVisionPrescription_Datewritten {http://hl7.org/fhir/SearchParameter/VisionPrescriptiondatewritten},
    spVisionPrescription_Encounter {http://hl7.org/fhir/SearchParameter/clinicalencounter},
    spVisionPrescription_Identifier {http://hl7.org/fhir/SearchParameter/clinicalidentifier},
    spVisionPrescription_Patient {http://hl7.org/fhir/SearchParameter/clinicalpatient},
    spVisionPrescription_Prescriber {http://hl7.org/fhir/SearchParameter/VisionPrescriptionprescriber},
    spVisionPrescription_Status {http://hl7.org/fhir/SearchParameter/VisionPrescriptionstatus});
{$ENDIF FHIR_VISIONPRESCRIPTION}

const

{$IFDEF FHIR_ACCOUNT}
  CODES_TSearchParamsAccount : Array[TSearchParamsAccount] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'identifier {http://hl7.org/fhir/SearchParameter/Account-identifier}', 'name {http://hl7.org/fhir/SearchParameter/Account-name}', 'owner {http://hl7.org/fhir/SearchParameter/Account-owner}', 'patient {http://hl7.org/fhir/SearchParameter/Account-patient}',
       'period {http://hl7.org/fhir/SearchParameter/Account-period}', 'status {http://hl7.org/fhir/SearchParameter/Account-status}', 'subject {http://hl7.org/fhir/SearchParameter/Account-subject}', 'type {http://hl7.org/fhir/SearchParameter/Account-type}');
{$ENDIF}
{$IFDEF FHIR_ACTIVITYDEFINITION}
  CODES_TSearchParamsActivityDefinition : Array[TSearchParamsActivityDefinition] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'composed-of {http://hl7.org/fhir/SearchParameter/ActivityDefinition-composed-of}', 'context {http://hl7.org/fhir/SearchParameter/ActivityDefinition-context}', 'context-quantity {http://hl7.org/fhir/SearchParameter/ActivityDefinition-context-quantity}',
       'context-type {http://hl7.org/fhir/SearchParameter/ActivityDefinition-context-type}', 'context-type-quantity {http://hl7.org/fhir/SearchParameter/ActivityDefinition-context-type-quantity}', 'context-type-value {http://hl7.org/fhir/SearchParameter/ActivityDefinition-context-type-value}', 'date {http://hl7.org/fhir/SearchParameter/ActivityDefinition-date}', 'depends-on {http://hl7.org/fhir/SearchParameter/ActivityDefinition-depends-on}', 'derived-from {http://hl7.org/fhir/SearchParameter/ActivityDefinition-derived-from}', 'description {http://hl7.org/fhir/SearchParameter/ActivityDefinition-description}', 'effective {http://hl7.org/fhir/SearchParameter/ActivityDefinition-effective}', 'identifier {http://hl7.org/fhir/SearchParameter/ActivityDefinition-identifier}', 'jurisdiction {http://hl7.org/fhir/SearchParameter/ActivityDefinition-jurisdiction}', 'name {http://hl7.org/fhir/SearchParameter/ActivityDefinition-name}', 'predecessor {http://hl7.org/fhir/SearchParameter/ActivityDefinition-predecessor}',
       'publisher {http://hl7.org/fhir/SearchParameter/ActivityDefinition-publisher}', 'status {http://hl7.org/fhir/SearchParameter/ActivityDefinition-status}', 'successor {http://hl7.org/fhir/SearchParameter/ActivityDefinition-successor}', 'title {http://hl7.org/fhir/SearchParameter/ActivityDefinition-title}', 'topic {http://hl7.org/fhir/SearchParameter/ActivityDefinition-topic}', 'url {http://hl7.org/fhir/SearchParameter/ActivityDefinition-url}', 'version {http://hl7.org/fhir/SearchParameter/ActivityDefinition-version}');
{$ENDIF}
{$IFDEF FHIR_ADMINISTRABLEPRODUCTDEFINITION}
  CODES_TSearchParamsAdministrableProductDefinition : Array[TSearchParamsAdministrableProductDefinition] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'device {http://hl7.org/fhir/SearchParameter/AdministrableProductDefinition-device}', 'dose-form {http://hl7.org/fhir/SearchParameter/AdministrableProductDefinition-dose-form}',
       'identifier {http://hl7.org/fhir/SearchParameter/AdministrableProductDefinition-identifier}', 'ingredient {http://hl7.org/fhir/SearchParameter/AdministrableProductDefinition-ingredient}', 'manufactured-item {http://hl7.org/fhir/SearchParameter/AdministrableProductDefinition-manufactured-item}', 'route {http://hl7.org/fhir/SearchParameter/AdministrableProductDefinition-route}', 'subject {http://hl7.org/fhir/SearchParameter/AdministrableProductDefinition-subject}', 'target-species {http://hl7.org/fhir/SearchParameter/AdministrableProductDefinition-target-species}');
{$ENDIF}
{$IFDEF FHIR_ADVERSEEVENT}
  CODES_TSearchParamsAdverseEvent : Array[TSearchParamsAdverseEvent] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'actuality {http://hl7.org/fhir/SearchParameter/AdverseEvent-actuality}', 'category {http://hl7.org/fhir/SearchParameter/AdverseEvent-category}', 'code {http://hl7.org/fhir/SearchParameter/AdverseEvent-code}', 'date {http://hl7.org/fhir/SearchParameter/AdverseEvent-date}',
       'identifier {http://hl7.org/fhir/SearchParameter/AdverseEvent-identifier}', 'location {http://hl7.org/fhir/SearchParameter/AdverseEvent-location}', 'patient {http://hl7.org/fhir/SearchParameter/AdverseEvent-patient}', 'recorder {http://hl7.org/fhir/SearchParameter/AdverseEvent-recorder}', 'resultingcondition {http://hl7.org/fhir/SearchParameter/AdverseEvent-resultingcondition}', 'seriousness {http://hl7.org/fhir/SearchParameter/AdverseEvent-seriousness}', 'status {http://hl7.org/fhir/SearchParameter/AdverseEvent-status}', 'study {http://hl7.org/fhir/SearchParameter/AdverseEvent-study}', 'subject {http://hl7.org/fhir/SearchParameter/AdverseEvent-subject}', 'substance {http://hl7.org/fhir/SearchParameter/AdverseEvent-substance}');
{$ENDIF}
{$IFDEF FHIR_ALLERGYINTOLERANCE}
  CODES_TSearchParamsAllergyIntolerance : Array[TSearchParamsAllergyIntolerance] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'asserter {http://hl7.org/fhir/SearchParameter/AllergyIntolerance-asserter}', 'category {http://hl7.org/fhir/SearchParameter/AllergyIntolerance-category}', 'clinical-status {http://hl7.org/fhir/SearchParameter/AllergyIntolerance-clinical-status}',
       'code {http://hl7.org/fhir/SearchParameter/clinical-code}', 'criticality {http://hl7.org/fhir/SearchParameter/AllergyIntolerance-criticality}', 'date {http://hl7.org/fhir/SearchParameter/clinical-date}', 'identifier {http://hl7.org/fhir/SearchParameter/clinical-identifier}', 'last-date {http://hl7.org/fhir/SearchParameter/AllergyIntolerance-last-date}', 'manifestation {http://hl7.org/fhir/SearchParameter/AllergyIntolerance-manifestation}', 'patient {http://hl7.org/fhir/SearchParameter/clinical-patient}', 'recorder {http://hl7.org/fhir/SearchParameter/AllergyIntolerance-recorder}', 'route {http://hl7.org/fhir/SearchParameter/AllergyIntolerance-route}', 'severity {http://hl7.org/fhir/SearchParameter/AllergyIntolerance-severity}', 'type {http://hl7.org/fhir/SearchParameter/clinical-type}', 'verification-status {http://hl7.org/fhir/SearchParameter/AllergyIntolerance-verification-status}');
{$ENDIF}
{$IFDEF FHIR_APPOINTMENT}
  CODES_TSearchParamsAppointment : Array[TSearchParamsAppointment] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'actor {http://hl7.org/fhir/SearchParameter/Appointment-actor}', 'appointment-type {http://hl7.org/fhir/SearchParameter/Appointment-appointment-type}', 'based-on {http://hl7.org/fhir/SearchParameter/Appointment-based-on}', 'date {http://hl7.org/fhir/SearchParameter/Appointment-date}',
       'identifier {http://hl7.org/fhir/SearchParameter/Appointment-identifier}', 'location {http://hl7.org/fhir/SearchParameter/Appointment-location}', 'part-status {http://hl7.org/fhir/SearchParameter/Appointment-part-status}', 'patient {http://hl7.org/fhir/SearchParameter/Appointment-patient}', 'practitioner {http://hl7.org/fhir/SearchParameter/Appointment-practitioner}', 'reason-code {http://hl7.org/fhir/SearchParameter/Appointment-reason-code}', 'reason-reference {http://hl7.org/fhir/SearchParameter/Appointment-reason-reference}', 'service-category {http://hl7.org/fhir/SearchParameter/Appointment-service-category}', 'service-type {http://hl7.org/fhir/SearchParameter/Appointment-service-type}', 'slot {http://hl7.org/fhir/SearchParameter/Appointment-slot}', 'specialty {http://hl7.org/fhir/SearchParameter/Appointment-specialty}', 'status {http://hl7.org/fhir/SearchParameter/Appointment-status}', 'supporting-info {http://hl7.org/fhir/SearchParameter/Appointment-supporting-info}');
{$ENDIF}
{$IFDEF FHIR_APPOINTMENTRESPONSE}
  CODES_TSearchParamsAppointmentResponse : Array[TSearchParamsAppointmentResponse] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'actor {http://hl7.org/fhir/SearchParameter/AppointmentResponse-actor}', 'appointment {http://hl7.org/fhir/SearchParameter/AppointmentResponse-appointment}', 'identifier {http://hl7.org/fhir/SearchParameter/AppointmentResponse-identifier}',
       'location {http://hl7.org/fhir/SearchParameter/AppointmentResponse-location}', 'part-status {http://hl7.org/fhir/SearchParameter/AppointmentResponse-part-status}', 'patient {http://hl7.org/fhir/SearchParameter/AppointmentResponse-patient}', 'practitioner {http://hl7.org/fhir/SearchParameter/AppointmentResponse-practitioner}');
{$ENDIF}
{$IFDEF FHIR_AUDITEVENT}
  CODES_TSearchParamsAuditEvent : Array[TSearchParamsAuditEvent] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'action {http://hl7.org/fhir/SearchParameter/AuditEvent-action}', 'address {http://hl7.org/fhir/SearchParameter/AuditEvent-address}', 'agent {http://hl7.org/fhir/SearchParameter/AuditEvent-agent}', 'agent-name {http://hl7.org/fhir/SearchParameter/AuditEvent-agent-name}',
       'agent-role {http://hl7.org/fhir/SearchParameter/AuditEvent-agent-role}', 'altid {http://hl7.org/fhir/SearchParameter/AuditEvent-altid}', 'date {http://hl7.org/fhir/SearchParameter/AuditEvent-date}', 'entity {http://hl7.org/fhir/SearchParameter/AuditEvent-entity}', 'entity-name {http://hl7.org/fhir/SearchParameter/AuditEvent-entity-name}', 'entity-role {http://hl7.org/fhir/SearchParameter/AuditEvent-entity-role}', 'entity-type {http://hl7.org/fhir/SearchParameter/AuditEvent-entity-type}', 'outcome {http://hl7.org/fhir/SearchParameter/AuditEvent-outcome}', 'patient {http://hl7.org/fhir/SearchParameter/AuditEvent-patient}', 'policy {http://hl7.org/fhir/SearchParameter/AuditEvent-policy}', 'purpose {http://hl7.org/fhir/SearchParameter/AuditEvent-purpose}', 'site {http://hl7.org/fhir/SearchParameter/AuditEvent-site}', 'source {http://hl7.org/fhir/SearchParameter/AuditEvent-source}', 'subtype {http://hl7.org/fhir/SearchParameter/AuditEvent-subtype}',
       'type {http://hl7.org/fhir/SearchParameter/AuditEvent-type}');
{$ENDIF}
{$IFDEF FHIR_BASIC}
  CODES_TSearchParamsBasic : Array[TSearchParamsBasic] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'author {http://hl7.org/fhir/SearchParameter/Basic-author}', 'code {http://hl7.org/fhir/SearchParameter/Basic-code}', 'created {http://hl7.org/fhir/SearchParameter/Basic-created}', 'identifier {http://hl7.org/fhir/SearchParameter/Basic-identifier}',
       'patient {http://hl7.org/fhir/SearchParameter/Basic-patient}', 'subject {http://hl7.org/fhir/SearchParameter/Basic-subject}');
{$ENDIF}
{$IFDEF FHIR_BINARY}
  CODES_TSearchParamsBinary : Array[TSearchParamsBinary] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}');
{$ENDIF}
{$IFDEF FHIR_BIOLOGICALLYDERIVEDPRODUCT}
  CODES_TSearchParamsBiologicallyDerivedProduct : Array[TSearchParamsBiologicallyDerivedProduct] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}');
{$ENDIF}
{$IFDEF FHIR_BODYSTRUCTURE}
  CODES_TSearchParamsBodyStructure : Array[TSearchParamsBodyStructure] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'identifier {http://hl7.org/fhir/SearchParameter/BodyStructure-identifier}', 'location {http://hl7.org/fhir/SearchParameter/BodyStructure-location}', 'morphology {http://hl7.org/fhir/SearchParameter/BodyStructure-morphology}',
       'patient {http://hl7.org/fhir/SearchParameter/BodyStructure-patient}');
{$ENDIF}
{$IFDEF FHIR_BUNDLE}
  CODES_TSearchParamsBundle : Array[TSearchParamsBundle] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', 'composition {http://hl7.org/fhir/SearchParameter/Bundle-composition}', 'identifier {http://hl7.org/fhir/SearchParameter/Bundle-identifier}', 'message {http://hl7.org/fhir/SearchParameter/Bundle-message}', 'timestamp {http://hl7.org/fhir/SearchParameter/Bundle-timestamp}', 'type {http://hl7.org/fhir/SearchParameter/Bundle-type}');
{$ENDIF}
{$IFDEF FHIR_CAPABILITYSTATEMENT}
  CODES_TSearchParamsCapabilityStatement : Array[TSearchParamsCapabilityStatement] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'context {http://hl7.org/fhir/SearchParameter/conformance-context}', 'context-quantity {http://hl7.org/fhir/SearchParameter/conformance-context-quantity}', 'context-type {http://hl7.org/fhir/SearchParameter/conformance-context-type}',
       'context-type-quantity {http://hl7.org/fhir/SearchParameter/conformance-context-type-quantity}', 'context-type-value {http://hl7.org/fhir/SearchParameter/conformance-context-type-value}', 'date {http://hl7.org/fhir/SearchParameter/conformance-date}', 'description {http://hl7.org/fhir/SearchParameter/conformance-description}', 'fhirversion {http://hl7.org/fhir/SearchParameter/CapabilityStatement-fhirversion}', 'format {http://hl7.org/fhir/SearchParameter/CapabilityStatement-format}', 'guide {http://hl7.org/fhir/SearchParameter/CapabilityStatement-guide}', 'jurisdiction {http://hl7.org/fhir/SearchParameter/conformance-jurisdiction}', 'mode {http://hl7.org/fhir/SearchParameter/CapabilityStatement-mode}', 'name {http://hl7.org/fhir/SearchParameter/conformance-name}', 'publisher {http://hl7.org/fhir/SearchParameter/conformance-publisher}', 'resource {http://hl7.org/fhir/SearchParameter/CapabilityStatement-resource}',
       'resource-profile {http://hl7.org/fhir/SearchParameter/CapabilityStatement-resource-profile}', 'security-service {http://hl7.org/fhir/SearchParameter/CapabilityStatement-security-service}', 'software {http://hl7.org/fhir/SearchParameter/CapabilityStatement-software}', 'status {http://hl7.org/fhir/SearchParameter/conformance-status}', 'supported-profile {http://hl7.org/fhir/SearchParameter/CapabilityStatement-supported-profile}', 'title {http://hl7.org/fhir/SearchParameter/conformance-title}', 'url {http://hl7.org/fhir/SearchParameter/conformance-url}', 'version {http://hl7.org/fhir/SearchParameter/conformance-version}');
{$ENDIF}
{$IFDEF FHIR_CAPABILITYSTATEMENT2}
  CODES_TSearchParamsCapabilityStatement2 : Array[TSearchParamsCapabilityStatement2] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'context {http://hl7.org/fhir/SearchParameter/CapabilityStatement2-context}', 'context-quantity {http://hl7.org/fhir/SearchParameter/CapabilityStatement2-context-quantity}', 'context-type {http://hl7.org/fhir/SearchParameter/CapabilityStatement2-context-type}',
       'context-type-quantity {http://hl7.org/fhir/SearchParameter/CapabilityStatement2-context-type-quantity}', 'context-type-value {http://hl7.org/fhir/SearchParameter/CapabilityStatement2-context-type-value}', 'date {http://hl7.org/fhir/SearchParameter/CapabilityStatement2-date}', 'description {http://hl7.org/fhir/SearchParameter/CapabilityStatement2-description}', 'fhirversion {http://hl7.org/fhir/SearchParameter/CapabilityStatement2-fhirversion}', 'format {http://hl7.org/fhir/SearchParameter/CapabilityStatement2-format}', 'guide {http://hl7.org/fhir/SearchParameter/CapabilityStatement2-guide}', 'jurisdiction {http://hl7.org/fhir/SearchParameter/CapabilityStatement2-jurisdiction}', 'mode {http://hl7.org/fhir/SearchParameter/CapabilityStatement2-mode}', 'name {http://hl7.org/fhir/SearchParameter/CapabilityStatement2-name}', 'publisher {http://hl7.org/fhir/SearchParameter/CapabilityStatement2-publisher}', 'resource {http://hl7.org/fhir/SearchParameter/CapabilityStatement2-resource}',
       'resource-profile {http://hl7.org/fhir/SearchParameter/CapabilityStatement2-resource-profile}', 'software {http://hl7.org/fhir/SearchParameter/CapabilityStatement2-software}', 'status {http://hl7.org/fhir/SearchParameter/CapabilityStatement2-status}', 'supported-profile {http://hl7.org/fhir/SearchParameter/CapabilityStatement2-supported-profile}', 'title {http://hl7.org/fhir/SearchParameter/CapabilityStatement2-title}', 'url {http://hl7.org/fhir/SearchParameter/CapabilityStatement2-url}', 'version {http://hl7.org/fhir/SearchParameter/CapabilityStatement2-version}');
{$ENDIF}
{$IFDEF FHIR_CAREPLAN}
  CODES_TSearchParamsCarePlan : Array[TSearchParamsCarePlan] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'activity-code {http://hl7.org/fhir/SearchParameter/CarePlan-activity-code}', 'activity-reference {http://hl7.org/fhir/SearchParameter/CarePlan-activity-reference}', 'activity-scheduled-date {http://hl7.org/fhir/SearchParameter/CarePlan-activity-scheduled-date}',
       'activity-scheduled-string {http://hl7.org/fhir/SearchParameter/CarePlan-activity-scheduled-string}', 'based-on {http://hl7.org/fhir/SearchParameter/CarePlan-based-on}', 'care-team {http://hl7.org/fhir/SearchParameter/CarePlan-care-team}', 'category {http://hl7.org/fhir/SearchParameter/CarePlan-category}', 'condition {http://hl7.org/fhir/SearchParameter/CarePlan-condition}', 'date {http://hl7.org/fhir/SearchParameter/clinical-date}', 'encounter {http://hl7.org/fhir/SearchParameter/CarePlan-encounter}', 'goal {http://hl7.org/fhir/SearchParameter/CarePlan-goal}', 'identifier {http://hl7.org/fhir/SearchParameter/clinical-identifier}', 'instantiates-canonical {http://hl7.org/fhir/SearchParameter/CarePlan-instantiates-canonical}', 'instantiates-uri {http://hl7.org/fhir/SearchParameter/CarePlan-instantiates-uri}', 'intent {http://hl7.org/fhir/SearchParameter/CarePlan-intent}', 'part-of {http://hl7.org/fhir/SearchParameter/CarePlan-part-of}',
       'patient {http://hl7.org/fhir/SearchParameter/clinical-patient}', 'performer {http://hl7.org/fhir/SearchParameter/CarePlan-performer}', 'replaces {http://hl7.org/fhir/SearchParameter/CarePlan-replaces}', 'status {http://hl7.org/fhir/SearchParameter/CarePlan-status}', 'subject {http://hl7.org/fhir/SearchParameter/CarePlan-subject}');
{$ENDIF}
{$IFDEF FHIR_CARETEAM}
  CODES_TSearchParamsCareTeam : Array[TSearchParamsCareTeam] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'category {http://hl7.org/fhir/SearchParameter/CareTeam-category}', 'date {http://hl7.org/fhir/SearchParameter/clinical-date}', 'identifier {http://hl7.org/fhir/SearchParameter/clinical-identifier}', 'name {http://hl7.org/fhir/SearchParameter/CareTeam-name}',
       'participant {http://hl7.org/fhir/SearchParameter/CareTeam-participant}', 'patient {http://hl7.org/fhir/SearchParameter/clinical-patient}', 'status {http://hl7.org/fhir/SearchParameter/CareTeam-status}', 'subject {http://hl7.org/fhir/SearchParameter/CareTeam-subject}');
{$ENDIF}
{$IFDEF FHIR_CATALOGENTRY}
  CODES_TSearchParamsCatalogEntry : Array[TSearchParamsCatalogEntry] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'identifier {http://hl7.org/fhir/SearchParameter/CatalogEntry-identifier}', 'name {http://hl7.org/fhir/SearchParameter/CatalogEntry-name}', 'orderable {http://hl7.org/fhir/SearchParameter/CatalogEntry-orderable}',
       'referenced-item {http://hl7.org/fhir/SearchParameter/CatalogEntry-referenced-item}', 'related-entry {http://hl7.org/fhir/SearchParameter/CatalogEntry-related-entry}', 'status {http://hl7.org/fhir/SearchParameter/CatalogEntry-status}', 'type {http://hl7.org/fhir/SearchParameter/CatalogEntry-type}');
{$ENDIF}
{$IFDEF FHIR_CHARGEITEM}
  CODES_TSearchParamsChargeItem : Array[TSearchParamsChargeItem] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'account {http://hl7.org/fhir/SearchParameter/ChargeItem-account}', 'code {http://hl7.org/fhir/SearchParameter/ChargeItem-code}', 'context {http://hl7.org/fhir/SearchParameter/ChargeItem-context}', 'entered-date {http://hl7.org/fhir/SearchParameter/ChargeItem-entered-date}',
       'enterer {http://hl7.org/fhir/SearchParameter/ChargeItem-enterer}', 'factor-override {http://hl7.org/fhir/SearchParameter/ChargeItem-factor-override}', 'identifier {http://hl7.org/fhir/SearchParameter/ChargeItem-identifier}', 'occurrence {http://hl7.org/fhir/SearchParameter/ChargeItem-occurrence}', 'patient {http://hl7.org/fhir/SearchParameter/ChargeItem-patient}', 'performer-actor {http://hl7.org/fhir/SearchParameter/ChargeItem-performer-actor}', 'performer-function {http://hl7.org/fhir/SearchParameter/ChargeItem-performer-function}', 'performing-organization {http://hl7.org/fhir/SearchParameter/ChargeItem-performing-organization}', 'price-override {http://hl7.org/fhir/SearchParameter/ChargeItem-price-override}', 'quantity {http://hl7.org/fhir/SearchParameter/ChargeItem-quantity}', 'requesting-organization {http://hl7.org/fhir/SearchParameter/ChargeItem-requesting-organization}', 'service {http://hl7.org/fhir/SearchParameter/ChargeItem-service}',
       'subject {http://hl7.org/fhir/SearchParameter/ChargeItem-subject}');
{$ENDIF}
{$IFDEF FHIR_CHARGEITEMDEFINITION}
  CODES_TSearchParamsChargeItemDefinition : Array[TSearchParamsChargeItemDefinition] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'context {http://hl7.org/fhir/SearchParameter/ChargeItemDefinition-context}', 'context-quantity {http://hl7.org/fhir/SearchParameter/ChargeItemDefinition-context-quantity}', 'context-type {http://hl7.org/fhir/SearchParameter/ChargeItemDefinition-context-type}',
       'context-type-quantity {http://hl7.org/fhir/SearchParameter/ChargeItemDefinition-context-type-quantity}', 'context-type-value {http://hl7.org/fhir/SearchParameter/ChargeItemDefinition-context-type-value}', 'date {http://hl7.org/fhir/SearchParameter/ChargeItemDefinition-date}', 'description {http://hl7.org/fhir/SearchParameter/ChargeItemDefinition-description}', 'effective {http://hl7.org/fhir/SearchParameter/ChargeItemDefinition-effective}', 'identifier {http://hl7.org/fhir/SearchParameter/ChargeItemDefinition-identifier}', 'jurisdiction {http://hl7.org/fhir/SearchParameter/ChargeItemDefinition-jurisdiction}', 'publisher {http://hl7.org/fhir/SearchParameter/ChargeItemDefinition-publisher}', 'status {http://hl7.org/fhir/SearchParameter/ChargeItemDefinition-status}', 'title {http://hl7.org/fhir/SearchParameter/ChargeItemDefinition-title}', 'url {http://hl7.org/fhir/SearchParameter/ChargeItemDefinition-url}', 'version {http://hl7.org/fhir/SearchParameter/ChargeItemDefinition-version}');
{$ENDIF}
{$IFDEF FHIR_CITATION}
  CODES_TSearchParamsCitation : Array[TSearchParamsCitation] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'context {http://hl7.org/fhir/SearchParameter/Citation-context}', 'context-quantity {http://hl7.org/fhir/SearchParameter/Citation-context-quantity}', 'context-type {http://hl7.org/fhir/SearchParameter/Citation-context-type}',
       'context-type-quantity {http://hl7.org/fhir/SearchParameter/Citation-context-type-quantity}', 'context-type-value {http://hl7.org/fhir/SearchParameter/Citation-context-type-value}', 'identifier {http://hl7.org/fhir/SearchParameter/Citation-identifier}', 'status {http://hl7.org/fhir/SearchParameter/Citation-status}', 'url {http://hl7.org/fhir/SearchParameter/Citation-url}');
{$ENDIF}
{$IFDEF FHIR_CLAIM}
  CODES_TSearchParamsClaim : Array[TSearchParamsClaim] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'care-team {http://hl7.org/fhir/SearchParameter/Claim-care-team}', 'created {http://hl7.org/fhir/SearchParameter/Claim-created}', 'detail-udi {http://hl7.org/fhir/SearchParameter/Claim-detail-udi}', 'encounter {http://hl7.org/fhir/SearchParameter/Claim-encounter}',
       'enterer {http://hl7.org/fhir/SearchParameter/Claim-enterer}', 'facility {http://hl7.org/fhir/SearchParameter/Claim-facility}', 'identifier {http://hl7.org/fhir/SearchParameter/Claim-identifier}', 'insurer {http://hl7.org/fhir/SearchParameter/Claim-insurer}', 'item-udi {http://hl7.org/fhir/SearchParameter/Claim-item-udi}', 'patient {http://hl7.org/fhir/SearchParameter/Claim-patient}', 'payee {http://hl7.org/fhir/SearchParameter/Claim-payee}', 'priority {http://hl7.org/fhir/SearchParameter/Claim-priority}', 'procedure-udi {http://hl7.org/fhir/SearchParameter/Claim-procedure-udi}', 'provider {http://hl7.org/fhir/SearchParameter/Claim-provider}', 'status {http://hl7.org/fhir/SearchParameter/Claim-status}', 'subdetail-udi {http://hl7.org/fhir/SearchParameter/Claim-subdetail-udi}', 'use {http://hl7.org/fhir/SearchParameter/Claim-use}');
{$ENDIF}
{$IFDEF FHIR_CLAIMRESPONSE}
  CODES_TSearchParamsClaimResponse : Array[TSearchParamsClaimResponse] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'created {http://hl7.org/fhir/SearchParameter/ClaimResponse-created}', 'disposition {http://hl7.org/fhir/SearchParameter/ClaimResponse-disposition}', 'identifier {http://hl7.org/fhir/SearchParameter/ClaimResponse-identifier}',
       'insurer {http://hl7.org/fhir/SearchParameter/ClaimResponse-insurer}', 'outcome {http://hl7.org/fhir/SearchParameter/ClaimResponse-outcome}', 'patient {http://hl7.org/fhir/SearchParameter/ClaimResponse-patient}', 'payment-date {http://hl7.org/fhir/SearchParameter/ClaimResponse-payment-date}', 'request {http://hl7.org/fhir/SearchParameter/ClaimResponse-request}', 'requestor {http://hl7.org/fhir/SearchParameter/ClaimResponse-requestor}', 'status {http://hl7.org/fhir/SearchParameter/ClaimResponse-status}', 'use {http://hl7.org/fhir/SearchParameter/ClaimResponse-use}');
{$ENDIF}
{$IFDEF FHIR_CLINICALIMPRESSION}
  CODES_TSearchParamsClinicalImpression : Array[TSearchParamsClinicalImpression] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'date {http://hl7.org/fhir/SearchParameter/clinical-date}', 'encounter {http://hl7.org/fhir/SearchParameter/ClinicalImpression-encounter}', 'finding-code {http://hl7.org/fhir/SearchParameter/ClinicalImpression-finding-code}',
       'finding-ref {http://hl7.org/fhir/SearchParameter/ClinicalImpression-finding-ref}', 'identifier {http://hl7.org/fhir/SearchParameter/ClinicalImpression-identifier}', 'patient {http://hl7.org/fhir/SearchParameter/clinical-patient}', 'performer {http://hl7.org/fhir/SearchParameter/ClinicalImpression-performer}', 'previous {http://hl7.org/fhir/SearchParameter/ClinicalImpression-previous}', 'problem {http://hl7.org/fhir/SearchParameter/ClinicalImpression-problem}', 'status {http://hl7.org/fhir/SearchParameter/ClinicalImpression-status}', 'subject {http://hl7.org/fhir/SearchParameter/ClinicalImpression-subject}', 'supporting-info {http://hl7.org/fhir/SearchParameter/ClinicalImpression-supporting-info}');
{$ENDIF}
{$IFDEF FHIR_CLINICALUSEISSUE}
  CODES_TSearchParamsClinicalUseIssue : Array[TSearchParamsClinicalUseIssue] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'contraindication {http://hl7.org/fhir/SearchParameter/ClinicalUseIssue-contraindication}', 'effect {http://hl7.org/fhir/SearchParameter/ClinicalUseIssue-effect}', 'identifier {http://hl7.org/fhir/SearchParameter/ClinicalUseIssue-identifier}',
       'indication {http://hl7.org/fhir/SearchParameter/ClinicalUseIssue-indication}', 'interaction {http://hl7.org/fhir/SearchParameter/ClinicalUseIssue-interaction}', 'product {http://hl7.org/fhir/SearchParameter/ClinicalUseIssue-product}', 'subject {http://hl7.org/fhir/SearchParameter/ClinicalUseIssue-subject}', 'type {http://hl7.org/fhir/SearchParameter/ClinicalUseIssue-type}');
{$ENDIF}
{$IFDEF FHIR_CODESYSTEM}
  CODES_TSearchParamsCodeSystem : Array[TSearchParamsCodeSystem] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'code {http://hl7.org/fhir/SearchParameter/CodeSystem-code}', 'content-mode {http://hl7.org/fhir/SearchParameter/CodeSystem-content-mode}', 'context {http://hl7.org/fhir/SearchParameter/conformance-context}', 'context-quantity {http://hl7.org/fhir/SearchParameter/conformance-context-quantity}',
       'context-type {http://hl7.org/fhir/SearchParameter/conformance-context-type}', 'context-type-quantity {http://hl7.org/fhir/SearchParameter/conformance-context-type-quantity}', 'context-type-value {http://hl7.org/fhir/SearchParameter/conformance-context-type-value}', 'date {http://hl7.org/fhir/SearchParameter/conformance-date}', 'description {http://hl7.org/fhir/SearchParameter/conformance-description}', 'identifier {http://hl7.org/fhir/SearchParameter/conformance-identifier}', 'jurisdiction {http://hl7.org/fhir/SearchParameter/conformance-jurisdiction}', 'language {http://hl7.org/fhir/SearchParameter/CodeSystem-language}', 'name {http://hl7.org/fhir/SearchParameter/conformance-name}', 'publisher {http://hl7.org/fhir/SearchParameter/conformance-publisher}', 'status {http://hl7.org/fhir/SearchParameter/conformance-status}', 'supplements {http://hl7.org/fhir/SearchParameter/CodeSystem-supplements}', 'system {http://hl7.org/fhir/SearchParameter/CodeSystem-system}',
       'title {http://hl7.org/fhir/SearchParameter/conformance-title}', 'url {http://hl7.org/fhir/SearchParameter/conformance-url}', 'version {http://hl7.org/fhir/SearchParameter/conformance-version}');
{$ENDIF}
{$IFDEF FHIR_COMMUNICATION}
  CODES_TSearchParamsCommunication : Array[TSearchParamsCommunication] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'based-on {http://hl7.org/fhir/SearchParameter/Communication-based-on}', 'category {http://hl7.org/fhir/SearchParameter/Communication-category}', 'encounter {http://hl7.org/fhir/SearchParameter/Communication-encounter}',
       'identifier {http://hl7.org/fhir/SearchParameter/Communication-identifier}', 'instantiates-canonical {http://hl7.org/fhir/SearchParameter/Communication-instantiates-canonical}', 'instantiates-uri {http://hl7.org/fhir/SearchParameter/Communication-instantiates-uri}', 'medium {http://hl7.org/fhir/SearchParameter/Communication-medium}', 'part-of {http://hl7.org/fhir/SearchParameter/Communication-part-of}', 'patient {http://hl7.org/fhir/SearchParameter/Communication-patient}', 'received {http://hl7.org/fhir/SearchParameter/Communication-received}', 'recipient {http://hl7.org/fhir/SearchParameter/Communication-recipient}', 'sender {http://hl7.org/fhir/SearchParameter/Communication-sender}', 'sent {http://hl7.org/fhir/SearchParameter/Communication-sent}', 'status {http://hl7.org/fhir/SearchParameter/Communication-status}', 'subject {http://hl7.org/fhir/SearchParameter/Communication-subject}', 'topic {http://hl7.org/fhir/SearchParameter/Communication-topic}');
{$ENDIF}
{$IFDEF FHIR_COMMUNICATIONREQUEST}
  CODES_TSearchParamsCommunicationRequest : Array[TSearchParamsCommunicationRequest] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'authored {http://hl7.org/fhir/SearchParameter/CommunicationRequest-authored}', 'based-on {http://hl7.org/fhir/SearchParameter/CommunicationRequest-based-on}', 'category {http://hl7.org/fhir/SearchParameter/CommunicationRequest-category}',
       'encounter {http://hl7.org/fhir/SearchParameter/CommunicationRequest-encounter}', 'group-identifier {http://hl7.org/fhir/SearchParameter/CommunicationRequest-group-identifier}', 'identifier {http://hl7.org/fhir/SearchParameter/CommunicationRequest-identifier}', 'information-provider {http://hl7.org/fhir/SearchParameter/CommunicationRequest-information-provider}', 'medium {http://hl7.org/fhir/SearchParameter/CommunicationRequest-medium}', 'occurrence {http://hl7.org/fhir/SearchParameter/CommunicationRequest-occurrence}', 'patient {http://hl7.org/fhir/SearchParameter/CommunicationRequest-patient}', 'priority {http://hl7.org/fhir/SearchParameter/CommunicationRequest-priority}', 'recipient {http://hl7.org/fhir/SearchParameter/CommunicationRequest-recipient}', 'replaces {http://hl7.org/fhir/SearchParameter/CommunicationRequest-replaces}', 'requester {http://hl7.org/fhir/SearchParameter/CommunicationRequest-requester}', 'status {http://hl7.org/fhir/SearchParameter/CommunicationRequest-status}',
       'subject {http://hl7.org/fhir/SearchParameter/CommunicationRequest-subject}');
{$ENDIF}
{$IFDEF FHIR_COMPARTMENTDEFINITION}
  CODES_TSearchParamsCompartmentDefinition : Array[TSearchParamsCompartmentDefinition] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'code {http://hl7.org/fhir/SearchParameter/CompartmentDefinition-code}', 'context {http://hl7.org/fhir/SearchParameter/conformance-context}', 'context-quantity {http://hl7.org/fhir/SearchParameter/conformance-context-quantity}',
       'context-type {http://hl7.org/fhir/SearchParameter/conformance-context-type}', 'context-type-quantity {http://hl7.org/fhir/SearchParameter/conformance-context-type-quantity}', 'context-type-value {http://hl7.org/fhir/SearchParameter/conformance-context-type-value}', 'date {http://hl7.org/fhir/SearchParameter/conformance-date}', 'description {http://hl7.org/fhir/SearchParameter/conformance-description}', 'name {http://hl7.org/fhir/SearchParameter/conformance-name}', 'publisher {http://hl7.org/fhir/SearchParameter/conformance-publisher}', 'resource {http://hl7.org/fhir/SearchParameter/CompartmentDefinition-resource}', 'status {http://hl7.org/fhir/SearchParameter/conformance-status}', 'url {http://hl7.org/fhir/SearchParameter/conformance-url}', 'version {http://hl7.org/fhir/SearchParameter/conformance-version}');
{$ENDIF}
{$IFDEF FHIR_COMPOSITION}
  CODES_TSearchParamsComposition : Array[TSearchParamsComposition] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'attester {http://hl7.org/fhir/SearchParameter/Composition-attester}', 'author {http://hl7.org/fhir/SearchParameter/Composition-author}', 'category {http://hl7.org/fhir/SearchParameter/Composition-category}', 'confidentiality {http://hl7.org/fhir/SearchParameter/Composition-confidentiality}',
       'context {http://hl7.org/fhir/SearchParameter/Composition-context}', 'date {http://hl7.org/fhir/SearchParameter/clinical-date}', 'encounter {http://hl7.org/fhir/SearchParameter/clinical-encounter}', 'entry {http://hl7.org/fhir/SearchParameter/Composition-entry}', 'identifier {http://hl7.org/fhir/SearchParameter/clinical-identifier}', 'patient {http://hl7.org/fhir/SearchParameter/clinical-patient}', 'period {http://hl7.org/fhir/SearchParameter/Composition-period}', 'related-id {http://hl7.org/fhir/SearchParameter/Composition-related-id}', 'related-ref {http://hl7.org/fhir/SearchParameter/Composition-related-ref}', 'section {http://hl7.org/fhir/SearchParameter/Composition-section}', 'status {http://hl7.org/fhir/SearchParameter/Composition-status}', 'subject {http://hl7.org/fhir/SearchParameter/Composition-subject}', 'title {http://hl7.org/fhir/SearchParameter/Composition-title}', 'type {http://hl7.org/fhir/SearchParameter/clinical-type}');
{$ENDIF}
{$IFDEF FHIR_CONCEPTMAP}
  CODES_TSearchParamsConceptMap : Array[TSearchParamsConceptMap] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'context {http://hl7.org/fhir/SearchParameter/conformance-context}', 'context-quantity {http://hl7.org/fhir/SearchParameter/conformance-context-quantity}', 'context-type {http://hl7.org/fhir/SearchParameter/conformance-context-type}',
       'context-type-quantity {http://hl7.org/fhir/SearchParameter/conformance-context-type-quantity}', 'context-type-value {http://hl7.org/fhir/SearchParameter/conformance-context-type-value}', 'date {http://hl7.org/fhir/SearchParameter/conformance-date}', 'dependson {http://hl7.org/fhir/SearchParameter/ConceptMap-dependson}', 'description {http://hl7.org/fhir/SearchParameter/conformance-description}', 'identifier {http://hl7.org/fhir/SearchParameter/conformance-identifier}', 'jurisdiction {http://hl7.org/fhir/SearchParameter/conformance-jurisdiction}', 'name {http://hl7.org/fhir/SearchParameter/conformance-name}', 'other {http://hl7.org/fhir/SearchParameter/ConceptMap-other}', 'product {http://hl7.org/fhir/SearchParameter/ConceptMap-product}', 'publisher {http://hl7.org/fhir/SearchParameter/conformance-publisher}', 'source {http://hl7.org/fhir/SearchParameter/ConceptMap-source}', 'source-code {http://hl7.org/fhir/SearchParameter/ConceptMap-source-code}',
       'source-system {http://hl7.org/fhir/SearchParameter/ConceptMap-source-system}', 'source-uri {http://hl7.org/fhir/SearchParameter/ConceptMap-source-uri}', 'status {http://hl7.org/fhir/SearchParameter/conformance-status}', 'target {http://hl7.org/fhir/SearchParameter/ConceptMap-target}', 'target-code {http://hl7.org/fhir/SearchParameter/ConceptMap-target-code}', 'target-system {http://hl7.org/fhir/SearchParameter/ConceptMap-target-system}', 'target-uri {http://hl7.org/fhir/SearchParameter/ConceptMap-target-uri}', 'title {http://hl7.org/fhir/SearchParameter/conformance-title}', 'url {http://hl7.org/fhir/SearchParameter/conformance-url}', 'version {http://hl7.org/fhir/SearchParameter/conformance-version}');
{$ENDIF}
{$IFDEF FHIR_CONDITION}
  CODES_TSearchParamsCondition : Array[TSearchParamsCondition] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'abatement-age {http://hl7.org/fhir/SearchParameter/Condition-abatement-age}', 'abatement-date {http://hl7.org/fhir/SearchParameter/Condition-abatement-date}', 'abatement-string {http://hl7.org/fhir/SearchParameter/Condition-abatement-string}',
       'asserter {http://hl7.org/fhir/SearchParameter/Condition-asserter}', 'body-site {http://hl7.org/fhir/SearchParameter/Condition-body-site}', 'category {http://hl7.org/fhir/SearchParameter/Condition-category}', 'clinical-status {http://hl7.org/fhir/SearchParameter/Condition-clinical-status}', 'code {http://hl7.org/fhir/SearchParameter/clinical-code}', 'encounter {http://hl7.org/fhir/SearchParameter/Condition-encounter}', 'evidence {http://hl7.org/fhir/SearchParameter/Condition-evidence}', 'evidence-detail {http://hl7.org/fhir/SearchParameter/Condition-evidence-detail}', 'identifier {http://hl7.org/fhir/SearchParameter/clinical-identifier}', 'onset-age {http://hl7.org/fhir/SearchParameter/Condition-onset-age}', 'onset-date {http://hl7.org/fhir/SearchParameter/Condition-onset-date}', 'onset-info {http://hl7.org/fhir/SearchParameter/Condition-onset-info}', 'patient {http://hl7.org/fhir/SearchParameter/clinical-patient}', 'recorded-date {http://hl7.org/fhir/SearchParameter/Condition-recorded-date}',
       'severity {http://hl7.org/fhir/SearchParameter/Condition-severity}', 'stage {http://hl7.org/fhir/SearchParameter/Condition-stage}', 'subject {http://hl7.org/fhir/SearchParameter/Condition-subject}', 'verification-status {http://hl7.org/fhir/SearchParameter/Condition-verification-status}');
{$ENDIF}
{$IFDEF FHIR_CONDITIONDEFINITION}
  CODES_TSearchParamsConditionDefinition : Array[TSearchParamsConditionDefinition] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'context {http://hl7.org/fhir/SearchParameter/ConditionDefinition-context}', 'context-quantity {http://hl7.org/fhir/SearchParameter/ConditionDefinition-context-quantity}', 'context-type {http://hl7.org/fhir/SearchParameter/ConditionDefinition-context-type}',
       'context-type-quantity {http://hl7.org/fhir/SearchParameter/ConditionDefinition-context-type-quantity}', 'context-type-value {http://hl7.org/fhir/SearchParameter/ConditionDefinition-context-type-value}', 'date {http://hl7.org/fhir/SearchParameter/ConditionDefinition-date}', 'description {http://hl7.org/fhir/SearchParameter/ConditionDefinition-description}', 'identifier {http://hl7.org/fhir/SearchParameter/ConditionDefinition-identifier}', 'jurisdiction {http://hl7.org/fhir/SearchParameter/ConditionDefinition-jurisdiction}', 'name {http://hl7.org/fhir/SearchParameter/ConditionDefinition-name}', 'publisher {http://hl7.org/fhir/SearchParameter/ConditionDefinition-publisher}', 'status {http://hl7.org/fhir/SearchParameter/ConditionDefinition-status}', 'title {http://hl7.org/fhir/SearchParameter/ConditionDefinition-title}', 'url {http://hl7.org/fhir/SearchParameter/ConditionDefinition-url}', 'version {http://hl7.org/fhir/SearchParameter/ConditionDefinition-version}');
{$ENDIF}
{$IFDEF FHIR_CONSENT}
  CODES_TSearchParamsConsent : Array[TSearchParamsConsent] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'action {http://hl7.org/fhir/SearchParameter/Consent-action}', 'actor {http://hl7.org/fhir/SearchParameter/Consent-actor}', 'category {http://hl7.org/fhir/SearchParameter/Consent-category}', 'consentor {http://hl7.org/fhir/SearchParameter/Consent-consentor}',
       'controller {http://hl7.org/fhir/SearchParameter/Consent-controller}', 'data {http://hl7.org/fhir/SearchParameter/Consent-data}', 'date {http://hl7.org/fhir/SearchParameter/clinical-date}', 'identifier {http://hl7.org/fhir/SearchParameter/clinical-identifier}', 'manager {http://hl7.org/fhir/SearchParameter/Consent-manager}', 'patient {http://hl7.org/fhir/SearchParameter/clinical-patient}', 'period {http://hl7.org/fhir/SearchParameter/Consent-period}', 'policy-uri {http://hl7.org/fhir/SearchParameter/Consent-policy-uri}', 'purpose {http://hl7.org/fhir/SearchParameter/Consent-purpose}', 'scope {http://hl7.org/fhir/SearchParameter/Consent-scope}', 'security-label {http://hl7.org/fhir/SearchParameter/Consent-security-label}', 'source-reference {http://hl7.org/fhir/SearchParameter/Consent-source-reference}', 'status {http://hl7.org/fhir/SearchParameter/Consent-status}', 'subject {http://hl7.org/fhir/SearchParameter/Consent-subject}', 'verified {http://hl7.org/fhir/SearchParameter/Consent-verified}',
       'verified-date {http://hl7.org/fhir/SearchParameter/Consent-verified-date}');
{$ENDIF}
{$IFDEF FHIR_CONTRACT}
  CODES_TSearchParamsContract : Array[TSearchParamsContract] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'authority {http://hl7.org/fhir/SearchParameter/Contract-authority}', 'domain {http://hl7.org/fhir/SearchParameter/Contract-domain}', 'identifier {http://hl7.org/fhir/SearchParameter/Contract-identifier}', 'instantiates {http://hl7.org/fhir/SearchParameter/Contract-instantiates}',
       'issued {http://hl7.org/fhir/SearchParameter/Contract-issued}', 'patient {http://hl7.org/fhir/SearchParameter/Contract-patient}', 'signer {http://hl7.org/fhir/SearchParameter/Contract-signer}', 'status {http://hl7.org/fhir/SearchParameter/Contract-status}', 'subject {http://hl7.org/fhir/SearchParameter/Contract-subject}', 'url {http://hl7.org/fhir/SearchParameter/Contract-url}');
{$ENDIF}
{$IFDEF FHIR_COVERAGE}
  CODES_TSearchParamsCoverage : Array[TSearchParamsCoverage] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'beneficiary {http://hl7.org/fhir/SearchParameter/Coverage-beneficiary}', 'class-type {http://hl7.org/fhir/SearchParameter/Coverage-class-type}', 'class-value {http://hl7.org/fhir/SearchParameter/Coverage-class-value}', 'dependent {http://hl7.org/fhir/SearchParameter/Coverage-dependent}',
       'identifier {http://hl7.org/fhir/SearchParameter/Coverage-identifier}', 'patient {http://hl7.org/fhir/SearchParameter/Coverage-patient}', 'payor {http://hl7.org/fhir/SearchParameter/Coverage-payor}', 'policy-holder {http://hl7.org/fhir/SearchParameter/Coverage-policy-holder}', 'status {http://hl7.org/fhir/SearchParameter/Coverage-status}', 'subscriber {http://hl7.org/fhir/SearchParameter/Coverage-subscriber}', 'type {http://hl7.org/fhir/SearchParameter/Coverage-type}');
{$ENDIF}
{$IFDEF FHIR_COVERAGEELIGIBILITYREQUEST}
  CODES_TSearchParamsCoverageEligibilityRequest : Array[TSearchParamsCoverageEligibilityRequest] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'created {http://hl7.org/fhir/SearchParameter/CoverageEligibilityRequest-created}', 'enterer {http://hl7.org/fhir/SearchParameter/CoverageEligibilityRequest-enterer}', 'facility {http://hl7.org/fhir/SearchParameter/CoverageEligibilityRequest-facility}',
       'identifier {http://hl7.org/fhir/SearchParameter/CoverageEligibilityRequest-identifier}', 'patient {http://hl7.org/fhir/SearchParameter/CoverageEligibilityRequest-patient}', 'provider {http://hl7.org/fhir/SearchParameter/CoverageEligibilityRequest-provider}', 'status {http://hl7.org/fhir/SearchParameter/CoverageEligibilityRequest-status}');
{$ENDIF}
{$IFDEF FHIR_COVERAGEELIGIBILITYRESPONSE}
  CODES_TSearchParamsCoverageEligibilityResponse : Array[TSearchParamsCoverageEligibilityResponse] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'created {http://hl7.org/fhir/SearchParameter/CoverageEligibilityResponse-created}', 'disposition {http://hl7.org/fhir/SearchParameter/CoverageEligibilityResponse-disposition}',
       'identifier {http://hl7.org/fhir/SearchParameter/CoverageEligibilityResponse-identifier}', 'insurer {http://hl7.org/fhir/SearchParameter/CoverageEligibilityResponse-insurer}', 'outcome {http://hl7.org/fhir/SearchParameter/CoverageEligibilityResponse-outcome}', 'patient {http://hl7.org/fhir/SearchParameter/CoverageEligibilityResponse-patient}', 'request {http://hl7.org/fhir/SearchParameter/CoverageEligibilityResponse-request}', 'requestor {http://hl7.org/fhir/SearchParameter/CoverageEligibilityResponse-requestor}', 'status {http://hl7.org/fhir/SearchParameter/CoverageEligibilityResponse-status}');
{$ENDIF}
{$IFDEF FHIR_DETECTEDISSUE}
  CODES_TSearchParamsDetectedIssue : Array[TSearchParamsDetectedIssue] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'author {http://hl7.org/fhir/SearchParameter/DetectedIssue-author}', 'code {http://hl7.org/fhir/SearchParameter/DetectedIssue-code}', 'identified {http://hl7.org/fhir/SearchParameter/DetectedIssue-identified}', 'identifier {http://hl7.org/fhir/SearchParameter/clinical-identifier}',
       'implicated {http://hl7.org/fhir/SearchParameter/DetectedIssue-implicated}', 'patient {http://hl7.org/fhir/SearchParameter/clinical-patient}');
{$ENDIF}
{$IFDEF FHIR_DEVICE}
  CODES_TSearchParamsDevice : Array[TSearchParamsDevice] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'definition {http://hl7.org/fhir/SearchParameter/Device-definition}', 'device-name {http://hl7.org/fhir/SearchParameter/Device-device-name}', 'din {http://hl7.org/fhir/SearchParameter/device-extensions-Device-din}', 'expiration-date {http://hl7.org/fhir/SearchParameter/Device-expiration-date}',
       'identifier {http://hl7.org/fhir/SearchParameter/Device-identifier}', 'location {http://hl7.org/fhir/SearchParameter/Device-location}', 'lot-number {http://hl7.org/fhir/SearchParameter/Device-lot-number}', 'manufacture-date {http://hl7.org/fhir/SearchParameter/Device-manufacture-date}', 'manufacturer {http://hl7.org/fhir/SearchParameter/Device-manufacturer}', 'model {http://hl7.org/fhir/SearchParameter/Device-model}', 'organization {http://hl7.org/fhir/SearchParameter/Device-organization}', 'parent {http://hl7.org/fhir/SearchParameter/Device-parent}', 'patient {http://hl7.org/fhir/SearchParameter/Device-patient}', 'serial-number {http://hl7.org/fhir/SearchParameter/Device-serial-number}', 'status {http://hl7.org/fhir/SearchParameter/Device-status}', 'type {http://hl7.org/fhir/SearchParameter/Device-type}', 'udi-carrier {http://hl7.org/fhir/SearchParameter/Device-udi-carrier}', 'udi-di {http://hl7.org/fhir/SearchParameter/Device-udi-di}', 'url {http://hl7.org/fhir/SearchParameter/Device-url}',
       'version {http://hl7.org/fhir/SearchParameter/Device-version}');
{$ENDIF}
{$IFDEF FHIR_DEVICEDEFINITION}
  CODES_TSearchParamsDeviceDefinition : Array[TSearchParamsDeviceDefinition] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'identifier {http://hl7.org/fhir/SearchParameter/DeviceDefinition-identifier}', 'parent {http://hl7.org/fhir/SearchParameter/DeviceDefinition-parent}', 'type {http://hl7.org/fhir/SearchParameter/DeviceDefinition-type}');
{$ENDIF}
{$IFDEF FHIR_DEVICEMETRIC}
  CODES_TSearchParamsDeviceMetric : Array[TSearchParamsDeviceMetric] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'category {http://hl7.org/fhir/SearchParameter/DeviceMetric-category}', 'identifier {http://hl7.org/fhir/SearchParameter/DeviceMetric-identifier}', 'parent {http://hl7.org/fhir/SearchParameter/DeviceMetric-parent}', 'source {http://hl7.org/fhir/SearchParameter/DeviceMetric-source}',
       'type {http://hl7.org/fhir/SearchParameter/DeviceMetric-type}');
{$ENDIF}
{$IFDEF FHIR_DEVICEREQUEST}
  CODES_TSearchParamsDeviceRequest : Array[TSearchParamsDeviceRequest] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'authored-on {http://hl7.org/fhir/SearchParameter/DeviceRequest-authored-on}', 'based-on {http://hl7.org/fhir/SearchParameter/DeviceRequest-based-on}', 'code {http://hl7.org/fhir/SearchParameter/clinical-code}', 'device {http://hl7.org/fhir/SearchParameter/DeviceRequest-device}',
       'encounter {http://hl7.org/fhir/SearchParameter/clinical-encounter}', 'event-date {http://hl7.org/fhir/SearchParameter/DeviceRequest-event-date}', 'group-identifier {http://hl7.org/fhir/SearchParameter/DeviceRequest-group-identifier}', 'identifier {http://hl7.org/fhir/SearchParameter/clinical-identifier}', 'instantiates-canonical {http://hl7.org/fhir/SearchParameter/DeviceRequest-instantiates-canonical}', 'instantiates-uri {http://hl7.org/fhir/SearchParameter/DeviceRequest-instantiates-uri}', 'insurance {http://hl7.org/fhir/SearchParameter/DeviceRequest-insurance}', 'intent {http://hl7.org/fhir/SearchParameter/DeviceRequest-intent}', 'patient {http://hl7.org/fhir/SearchParameter/clinical-patient}', 'performer {http://hl7.org/fhir/SearchParameter/DeviceRequest-performer}', 'prior-request {http://hl7.org/fhir/SearchParameter/DeviceRequest-prior-request}', 'requester {http://hl7.org/fhir/SearchParameter/DeviceRequest-requester}', 'status {http://hl7.org/fhir/SearchParameter/DeviceRequest-status}',
       'subject {http://hl7.org/fhir/SearchParameter/DeviceRequest-subject}');
{$ENDIF}
{$IFDEF FHIR_DEVICEUSESTATEMENT}
  CODES_TSearchParamsDeviceUseStatement : Array[TSearchParamsDeviceUseStatement] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'device {http://hl7.org/fhir/SearchParameter/DeviceUseStatement-device}', 'identifier {http://hl7.org/fhir/SearchParameter/DeviceUseStatement-identifier}', 'patient {http://hl7.org/fhir/SearchParameter/clinical-patient}',
       'subject {http://hl7.org/fhir/SearchParameter/DeviceUseStatement-subject}');
{$ENDIF}
{$IFDEF FHIR_DIAGNOSTICREPORT}
  CODES_TSearchParamsDiagnosticReport : Array[TSearchParamsDiagnosticReport] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'based-on {http://hl7.org/fhir/SearchParameter/DiagnosticReport-based-on}', 'category {http://hl7.org/fhir/SearchParameter/DiagnosticReport-category}', 'code {http://hl7.org/fhir/SearchParameter/clinical-code}',
       'conclusion {http://hl7.org/fhir/SearchParameter/DiagnosticReport-conclusion}', 'date {http://hl7.org/fhir/SearchParameter/clinical-date}', 'encounter {http://hl7.org/fhir/SearchParameter/clinical-encounter}', 'identifier {http://hl7.org/fhir/SearchParameter/clinical-identifier}', 'issued {http://hl7.org/fhir/SearchParameter/DiagnosticReport-issued}', 'media {http://hl7.org/fhir/SearchParameter/DiagnosticReport-media}', 'patient {http://hl7.org/fhir/SearchParameter/clinical-patient}', 'performer {http://hl7.org/fhir/SearchParameter/DiagnosticReport-performer}', 'result {http://hl7.org/fhir/SearchParameter/DiagnosticReport-result}', 'results-interpreter {http://hl7.org/fhir/SearchParameter/DiagnosticReport-results-interpreter}', 'specimen {http://hl7.org/fhir/SearchParameter/DiagnosticReport-specimen}', 'status {http://hl7.org/fhir/SearchParameter/DiagnosticReport-status}', 'subject {http://hl7.org/fhir/SearchParameter/DiagnosticReport-subject}');
{$ENDIF}
{$IFDEF FHIR_DOCUMENTMANIFEST}
  CODES_TSearchParamsDocumentManifest : Array[TSearchParamsDocumentManifest] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'author {http://hl7.org/fhir/SearchParameter/DocumentManifest-author}', 'created {http://hl7.org/fhir/SearchParameter/DocumentManifest-created}', 'description {http://hl7.org/fhir/SearchParameter/DocumentManifest-description}',
       'identifier {http://hl7.org/fhir/SearchParameter/clinical-identifier}', 'item {http://hl7.org/fhir/SearchParameter/DocumentManifest-item}', 'patient {http://hl7.org/fhir/SearchParameter/clinical-patient}', 'recipient {http://hl7.org/fhir/SearchParameter/DocumentManifest-recipient}', 'related-id {http://hl7.org/fhir/SearchParameter/DocumentManifest-related-id}', 'related-ref {http://hl7.org/fhir/SearchParameter/DocumentManifest-related-ref}', 'source {http://hl7.org/fhir/SearchParameter/DocumentManifest-source}', 'status {http://hl7.org/fhir/SearchParameter/DocumentManifest-status}', 'subject {http://hl7.org/fhir/SearchParameter/DocumentManifest-subject}', 'type {http://hl7.org/fhir/SearchParameter/clinical-type}');
{$ENDIF}
{$IFDEF FHIR_DOCUMENTREFERENCE}
  CODES_TSearchParamsDocumentReference : Array[TSearchParamsDocumentReference] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'attester {http://hl7.org/fhir/SearchParameter/DocumentReference-attester}', 'author {http://hl7.org/fhir/SearchParameter/DocumentReference-author}', 'based-on {http://hl7.org/fhir/SearchParameter/DocumentReference-based-on}',
       'category {http://hl7.org/fhir/SearchParameter/DocumentReference-category}', 'contenttype {http://hl7.org/fhir/SearchParameter/DocumentReference-contenttype}', 'creation {http://hl7.org/fhir/SearchParameter/DocumentReference-creation}', 'custodian {http://hl7.org/fhir/SearchParameter/DocumentReference-custodian}', 'date {http://hl7.org/fhir/SearchParameter/DocumentReference-date}', 'description {http://hl7.org/fhir/SearchParameter/DocumentReference-description}', 'doc-status {http://hl7.org/fhir/SearchParameter/DocumentReference-doc-status}', 'encounter {http://hl7.org/fhir/SearchParameter/clinical-encounter}', 'event {http://hl7.org/fhir/SearchParameter/DocumentReference-event}', 'facility {http://hl7.org/fhir/SearchParameter/DocumentReference-facility}', 'format {http://hl7.org/fhir/SearchParameter/DocumentReference-format}', 'identifier {http://hl7.org/fhir/SearchParameter/clinical-identifier}', 'language {http://hl7.org/fhir/SearchParameter/DocumentReference-language}',
       'location {http://hl7.org/fhir/SearchParameter/DocumentReference-location}', 'patient {http://hl7.org/fhir/SearchParameter/clinical-patient}', 'period {http://hl7.org/fhir/SearchParameter/DocumentReference-period}', 'related {http://hl7.org/fhir/SearchParameter/DocumentReference-related}', 'relatesto {http://hl7.org/fhir/SearchParameter/DocumentReference-relatesto}', 'relation {http://hl7.org/fhir/SearchParameter/DocumentReference-relation}', 'relationship {http://hl7.org/fhir/SearchParameter/DocumentReference-relationship}', 'security-label {http://hl7.org/fhir/SearchParameter/DocumentReference-security-label}', 'setting {http://hl7.org/fhir/SearchParameter/DocumentReference-setting}', 'status {http://hl7.org/fhir/SearchParameter/DocumentReference-status}', 'subject {http://hl7.org/fhir/SearchParameter/DocumentReference-subject}', 'type {http://hl7.org/fhir/SearchParameter/clinical-type}');
{$ENDIF}
{$IFDEF FHIR_ENCOUNTER}
  CODES_TSearchParamsEncounter : Array[TSearchParamsEncounter] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'account {http://hl7.org/fhir/SearchParameter/Encounter-account}', 'appointment {http://hl7.org/fhir/SearchParameter/Encounter-appointment}', 'based-on {http://hl7.org/fhir/SearchParameter/Encounter-based-on}', 'class {http://hl7.org/fhir/SearchParameter/Encounter-class}',
       'date {http://hl7.org/fhir/SearchParameter/clinical-date}', 'diagnosis {http://hl7.org/fhir/SearchParameter/Encounter-diagnosis}', 'episode-of-care {http://hl7.org/fhir/SearchParameter/Encounter-episode-of-care}', 'identifier {http://hl7.org/fhir/SearchParameter/clinical-identifier}', 'length {http://hl7.org/fhir/SearchParameter/Encounter-length}', 'location {http://hl7.org/fhir/SearchParameter/Encounter-location}', 'location-period {http://hl7.org/fhir/SearchParameter/Encounter-location-period}', 'part-of {http://hl7.org/fhir/SearchParameter/Encounter-part-of}', 'participant {http://hl7.org/fhir/SearchParameter/Encounter-participant}', 'participant-type {http://hl7.org/fhir/SearchParameter/Encounter-participant-type}', 'patient {http://hl7.org/fhir/SearchParameter/clinical-patient}', 'practitioner {http://hl7.org/fhir/SearchParameter/Encounter-practitioner}', 'reason-code {http://hl7.org/fhir/SearchParameter/Encounter-reason-code}',
       'reason-reference {http://hl7.org/fhir/SearchParameter/Encounter-reason-reference}', 'service-provider {http://hl7.org/fhir/SearchParameter/Encounter-service-provider}', 'special-arrangement {http://hl7.org/fhir/SearchParameter/Encounter-special-arrangement}', 'status {http://hl7.org/fhir/SearchParameter/Encounter-status}', 'subject {http://hl7.org/fhir/SearchParameter/Encounter-subject}', 'subject-status {http://hl7.org/fhir/SearchParameter/Encounter-subject-status}', 'type {http://hl7.org/fhir/SearchParameter/clinical-type}');
{$ENDIF}
{$IFDEF FHIR_ENDPOINT}
  CODES_TSearchParamsEndpoint : Array[TSearchParamsEndpoint] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'connection-type {http://hl7.org/fhir/SearchParameter/Endpoint-connection-type}', 'identifier {http://hl7.org/fhir/SearchParameter/Endpoint-identifier}', 'name {http://hl7.org/fhir/SearchParameter/Endpoint-name}', 'organization {http://hl7.org/fhir/SearchParameter/Endpoint-organization}',
       'payload-type {http://hl7.org/fhir/SearchParameter/Endpoint-payload-type}', 'status {http://hl7.org/fhir/SearchParameter/Endpoint-status}');
{$ENDIF}
{$IFDEF FHIR_ENROLLMENTREQUEST}
  CODES_TSearchParamsEnrollmentRequest : Array[TSearchParamsEnrollmentRequest] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'identifier {http://hl7.org/fhir/SearchParameter/EnrollmentRequest-identifier}', 'patient {http://hl7.org/fhir/SearchParameter/EnrollmentRequest-patient}', 'status {http://hl7.org/fhir/SearchParameter/EnrollmentRequest-status}',
       'subject {http://hl7.org/fhir/SearchParameter/EnrollmentRequest-subject}');
{$ENDIF}
{$IFDEF FHIR_ENROLLMENTRESPONSE}
  CODES_TSearchParamsEnrollmentResponse : Array[TSearchParamsEnrollmentResponse] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'identifier {http://hl7.org/fhir/SearchParameter/EnrollmentResponse-identifier}', 'request {http://hl7.org/fhir/SearchParameter/EnrollmentResponse-request}', 'status {http://hl7.org/fhir/SearchParameter/EnrollmentResponse-status}');
{$ENDIF}
{$IFDEF FHIR_EPISODEOFCARE}
  CODES_TSearchParamsEpisodeOfCare : Array[TSearchParamsEpisodeOfCare] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'care-manager {http://hl7.org/fhir/SearchParameter/EpisodeOfCare-care-manager}', 'condition {http://hl7.org/fhir/SearchParameter/EpisodeOfCare-condition}', 'date {http://hl7.org/fhir/SearchParameter/clinical-date}', 'identifier {http://hl7.org/fhir/SearchParameter/clinical-identifier}',
       'incoming-referral {http://hl7.org/fhir/SearchParameter/EpisodeOfCare-incoming-referral}', 'organization {http://hl7.org/fhir/SearchParameter/EpisodeOfCare-organization}', 'patient {http://hl7.org/fhir/SearchParameter/clinical-patient}', 'status {http://hl7.org/fhir/SearchParameter/EpisodeOfCare-status}', 'type {http://hl7.org/fhir/SearchParameter/clinical-type}');
{$ENDIF}
{$IFDEF FHIR_EVENTDEFINITION}
  CODES_TSearchParamsEventDefinition : Array[TSearchParamsEventDefinition] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'composed-of {http://hl7.org/fhir/SearchParameter/EventDefinition-composed-of}', 'context {http://hl7.org/fhir/SearchParameter/EventDefinition-context}', 'context-quantity {http://hl7.org/fhir/SearchParameter/EventDefinition-context-quantity}',
       'context-type {http://hl7.org/fhir/SearchParameter/EventDefinition-context-type}', 'context-type-quantity {http://hl7.org/fhir/SearchParameter/EventDefinition-context-type-quantity}', 'context-type-value {http://hl7.org/fhir/SearchParameter/EventDefinition-context-type-value}', 'date {http://hl7.org/fhir/SearchParameter/EventDefinition-date}', 'depends-on {http://hl7.org/fhir/SearchParameter/EventDefinition-depends-on}', 'derived-from {http://hl7.org/fhir/SearchParameter/EventDefinition-derived-from}', 'description {http://hl7.org/fhir/SearchParameter/EventDefinition-description}', 'effective {http://hl7.org/fhir/SearchParameter/EventDefinition-effective}', 'identifier {http://hl7.org/fhir/SearchParameter/EventDefinition-identifier}', 'jurisdiction {http://hl7.org/fhir/SearchParameter/EventDefinition-jurisdiction}', 'name {http://hl7.org/fhir/SearchParameter/EventDefinition-name}', 'predecessor {http://hl7.org/fhir/SearchParameter/EventDefinition-predecessor}',
       'publisher {http://hl7.org/fhir/SearchParameter/EventDefinition-publisher}', 'status {http://hl7.org/fhir/SearchParameter/EventDefinition-status}', 'successor {http://hl7.org/fhir/SearchParameter/EventDefinition-successor}', 'title {http://hl7.org/fhir/SearchParameter/EventDefinition-title}', 'topic {http://hl7.org/fhir/SearchParameter/EventDefinition-topic}', 'url {http://hl7.org/fhir/SearchParameter/EventDefinition-url}', 'version {http://hl7.org/fhir/SearchParameter/EventDefinition-version}');
{$ENDIF}
{$IFDEF FHIR_EVIDENCE}
  CODES_TSearchParamsEvidence : Array[TSearchParamsEvidence] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'context {http://hl7.org/fhir/SearchParameter/Evidence-context}', 'context-quantity {http://hl7.org/fhir/SearchParameter/Evidence-context-quantity}', 'context-type {http://hl7.org/fhir/SearchParameter/Evidence-context-type}',
       'context-type-quantity {http://hl7.org/fhir/SearchParameter/Evidence-context-type-quantity}', 'context-type-value {http://hl7.org/fhir/SearchParameter/Evidence-context-type-value}', 'date {http://hl7.org/fhir/SearchParameter/Evidence-date}', 'description {http://hl7.org/fhir/SearchParameter/Evidence-description}', 'identifier {http://hl7.org/fhir/SearchParameter/Evidence-identifier}', 'publisher {http://hl7.org/fhir/SearchParameter/Evidence-publisher}', 'status {http://hl7.org/fhir/SearchParameter/Evidence-status}', 'title {http://hl7.org/fhir/SearchParameter/Evidence-title}', 'url {http://hl7.org/fhir/SearchParameter/Evidence-url}', 'version {http://hl7.org/fhir/SearchParameter/Evidence-version}');
{$ENDIF}
{$IFDEF FHIR_EVIDENCEREPORT}
  CODES_TSearchParamsEvidenceReport : Array[TSearchParamsEvidenceReport] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'context {http://hl7.org/fhir/SearchParameter/EvidenceReport-context}', 'context-quantity {http://hl7.org/fhir/SearchParameter/EvidenceReport-context-quantity}', 'context-type {http://hl7.org/fhir/SearchParameter/EvidenceReport-context-type}',
       'context-type-quantity {http://hl7.org/fhir/SearchParameter/EvidenceReport-context-type-quantity}', 'context-type-value {http://hl7.org/fhir/SearchParameter/EvidenceReport-context-type-value}', 'identifier {http://hl7.org/fhir/SearchParameter/EvidenceReport-identifier}', 'publisher {http://hl7.org/fhir/SearchParameter/EvidenceReport-publisher}', 'status {http://hl7.org/fhir/SearchParameter/EvidenceReport-status}', 'url {http://hl7.org/fhir/SearchParameter/EvidenceReport-url}');
{$ENDIF}
{$IFDEF FHIR_EVIDENCEVARIABLE}
  CODES_TSearchParamsEvidenceVariable : Array[TSearchParamsEvidenceVariable] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'composed-of {http://hl7.org/fhir/SearchParameter/EvidenceVariable-composed-of}', 'context {http://hl7.org/fhir/SearchParameter/EvidenceVariable-context}', 'context-quantity {http://hl7.org/fhir/SearchParameter/EvidenceVariable-context-quantity}',
       'context-type {http://hl7.org/fhir/SearchParameter/EvidenceVariable-context-type}', 'context-type-quantity {http://hl7.org/fhir/SearchParameter/EvidenceVariable-context-type-quantity}', 'context-type-value {http://hl7.org/fhir/SearchParameter/EvidenceVariable-context-type-value}', 'date {http://hl7.org/fhir/SearchParameter/EvidenceVariable-date}', 'depends-on {http://hl7.org/fhir/SearchParameter/EvidenceVariable-depends-on}', 'derived-from {http://hl7.org/fhir/SearchParameter/EvidenceVariable-derived-from}', 'description {http://hl7.org/fhir/SearchParameter/EvidenceVariable-description}', 'identifier {http://hl7.org/fhir/SearchParameter/EvidenceVariable-identifier}', 'name {http://hl7.org/fhir/SearchParameter/EvidenceVariable-name}', 'predecessor {http://hl7.org/fhir/SearchParameter/EvidenceVariable-predecessor}', 'publisher {http://hl7.org/fhir/SearchParameter/EvidenceVariable-publisher}', 'status {http://hl7.org/fhir/SearchParameter/EvidenceVariable-status}',
       'successor {http://hl7.org/fhir/SearchParameter/EvidenceVariable-successor}', 'title {http://hl7.org/fhir/SearchParameter/EvidenceVariable-title}', 'topic {http://hl7.org/fhir/SearchParameter/EvidenceVariable-topic}', 'url {http://hl7.org/fhir/SearchParameter/EvidenceVariable-url}', 'version {http://hl7.org/fhir/SearchParameter/EvidenceVariable-version}');
{$ENDIF}
{$IFDEF FHIR_EXAMPLESCENARIO}
  CODES_TSearchParamsExampleScenario : Array[TSearchParamsExampleScenario] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'context {http://hl7.org/fhir/SearchParameter/ExampleScenario-context}', 'context-quantity {http://hl7.org/fhir/SearchParameter/ExampleScenario-context-quantity}', 'context-type {http://hl7.org/fhir/SearchParameter/ExampleScenario-context-type}',
       'context-type-quantity {http://hl7.org/fhir/SearchParameter/ExampleScenario-context-type-quantity}', 'context-type-value {http://hl7.org/fhir/SearchParameter/ExampleScenario-context-type-value}', 'date {http://hl7.org/fhir/SearchParameter/ExampleScenario-date}', 'identifier {http://hl7.org/fhir/SearchParameter/ExampleScenario-identifier}', 'jurisdiction {http://hl7.org/fhir/SearchParameter/ExampleScenario-jurisdiction}', 'name {http://hl7.org/fhir/SearchParameter/ExampleScenario-name}', 'publisher {http://hl7.org/fhir/SearchParameter/ExampleScenario-publisher}', 'status {http://hl7.org/fhir/SearchParameter/ExampleScenario-status}', 'url {http://hl7.org/fhir/SearchParameter/ExampleScenario-url}', 'version {http://hl7.org/fhir/SearchParameter/ExampleScenario-version}');
{$ENDIF}
{$IFDEF FHIR_EXPLANATIONOFBENEFIT}
  CODES_TSearchParamsExplanationOfBenefit : Array[TSearchParamsExplanationOfBenefit] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'care-team {http://hl7.org/fhir/SearchParameter/ExplanationOfBenefit-care-team}', 'claim {http://hl7.org/fhir/SearchParameter/ExplanationOfBenefit-claim}', 'coverage {http://hl7.org/fhir/SearchParameter/ExplanationOfBenefit-coverage}',
       'created {http://hl7.org/fhir/SearchParameter/ExplanationOfBenefit-created}', 'detail-udi {http://hl7.org/fhir/SearchParameter/ExplanationOfBenefit-detail-udi}', 'disposition {http://hl7.org/fhir/SearchParameter/ExplanationOfBenefit-disposition}', 'encounter {http://hl7.org/fhir/SearchParameter/ExplanationOfBenefit-encounter}', 'enterer {http://hl7.org/fhir/SearchParameter/ExplanationOfBenefit-enterer}', 'facility {http://hl7.org/fhir/SearchParameter/ExplanationOfBenefit-facility}', 'identifier {http://hl7.org/fhir/SearchParameter/ExplanationOfBenefit-identifier}', 'item-udi {http://hl7.org/fhir/SearchParameter/ExplanationOfBenefit-item-udi}', 'patient {http://hl7.org/fhir/SearchParameter/ExplanationOfBenefit-patient}', 'payee {http://hl7.org/fhir/SearchParameter/ExplanationOfBenefit-payee}', 'procedure-udi {http://hl7.org/fhir/SearchParameter/ExplanationOfBenefit-procedure-udi}', 'provider {http://hl7.org/fhir/SearchParameter/ExplanationOfBenefit-provider}',
       'status {http://hl7.org/fhir/SearchParameter/ExplanationOfBenefit-status}', 'subdetail-udi {http://hl7.org/fhir/SearchParameter/ExplanationOfBenefit-subdetail-udi}');
{$ENDIF}
{$IFDEF FHIR_FAMILYMEMBERHISTORY}
  CODES_TSearchParamsFamilyMemberHistory : Array[TSearchParamsFamilyMemberHistory] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'code {http://hl7.org/fhir/SearchParameter/clinical-code}', 'date {http://hl7.org/fhir/SearchParameter/clinical-date}', 'identifier {http://hl7.org/fhir/SearchParameter/clinical-identifier}',
       'instantiates-canonical {http://hl7.org/fhir/SearchParameter/FamilyMemberHistory-instantiates-canonical}', 'instantiates-uri {http://hl7.org/fhir/SearchParameter/FamilyMemberHistory-instantiates-uri}', 'patient {http://hl7.org/fhir/SearchParameter/clinical-patient}', 'relationship {http://hl7.org/fhir/SearchParameter/FamilyMemberHistory-relationship}', 'sex {http://hl7.org/fhir/SearchParameter/FamilyMemberHistory-sex}', 'status {http://hl7.org/fhir/SearchParameter/FamilyMemberHistory-status}');
{$ENDIF}
{$IFDEF FHIR_FLAG}
  CODES_TSearchParamsFlag : Array[TSearchParamsFlag] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'author {http://hl7.org/fhir/SearchParameter/Flag-author}', 'date {http://hl7.org/fhir/SearchParameter/clinical-date}', 'encounter {http://hl7.org/fhir/SearchParameter/clinical-encounter}', 'identifier {http://hl7.org/fhir/SearchParameter/Flag-identifier}',
       'patient {http://hl7.org/fhir/SearchParameter/clinical-patient}', 'status {http://hl7.org/fhir/SearchParameter/Flag-status}', 'subject {http://hl7.org/fhir/SearchParameter/Flag-subject}');
{$ENDIF}
{$IFDEF FHIR_GOAL}
  CODES_TSearchParamsGoal : Array[TSearchParamsGoal] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'achievement-status {http://hl7.org/fhir/SearchParameter/Goal-achievement-status}', 'category {http://hl7.org/fhir/SearchParameter/Goal-category}', 'identifier {http://hl7.org/fhir/SearchParameter/clinical-identifier}', 'lifecycle-status {http://hl7.org/fhir/SearchParameter/Goal-lifecycle-status}',
       'patient {http://hl7.org/fhir/SearchParameter/clinical-patient}', 'start-date {http://hl7.org/fhir/SearchParameter/Goal-start-date}', 'subject {http://hl7.org/fhir/SearchParameter/Goal-subject}', 'target-date {http://hl7.org/fhir/SearchParameter/Goal-target-date}');
{$ENDIF}
{$IFDEF FHIR_GRAPHDEFINITION}
  CODES_TSearchParamsGraphDefinition : Array[TSearchParamsGraphDefinition] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'context {http://hl7.org/fhir/SearchParameter/conformance-context}', 'context-quantity {http://hl7.org/fhir/SearchParameter/conformance-context-quantity}', 'context-type {http://hl7.org/fhir/SearchParameter/conformance-context-type}',
       'context-type-quantity {http://hl7.org/fhir/SearchParameter/conformance-context-type-quantity}', 'context-type-value {http://hl7.org/fhir/SearchParameter/conformance-context-type-value}', 'date {http://hl7.org/fhir/SearchParameter/conformance-date}', 'description {http://hl7.org/fhir/SearchParameter/conformance-description}', 'jurisdiction {http://hl7.org/fhir/SearchParameter/conformance-jurisdiction}', 'name {http://hl7.org/fhir/SearchParameter/conformance-name}', 'publisher {http://hl7.org/fhir/SearchParameter/conformance-publisher}', 'start {http://hl7.org/fhir/SearchParameter/GraphDefinition-start}', 'status {http://hl7.org/fhir/SearchParameter/conformance-status}', 'url {http://hl7.org/fhir/SearchParameter/conformance-url}', 'version {http://hl7.org/fhir/SearchParameter/conformance-version}');
{$ENDIF}
{$IFDEF FHIR_GROUP}
  CODES_TSearchParamsGroup : Array[TSearchParamsGroup] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'actual {http://hl7.org/fhir/SearchParameter/Group-actual}', 'characteristic {http://hl7.org/fhir/SearchParameter/Group-characteristic}', 'characteristic-value {http://hl7.org/fhir/SearchParameter/Group-characteristic-value}', 'code {http://hl7.org/fhir/SearchParameter/Group-code}',
       'exclude {http://hl7.org/fhir/SearchParameter/Group-exclude}', 'identifier {http://hl7.org/fhir/SearchParameter/Group-identifier}', 'managing-entity {http://hl7.org/fhir/SearchParameter/Group-managing-entity}', 'member {http://hl7.org/fhir/SearchParameter/Group-member}', 'type {http://hl7.org/fhir/SearchParameter/Group-type}', 'value {http://hl7.org/fhir/SearchParameter/Group-value}');
{$ENDIF}
{$IFDEF FHIR_GUIDANCERESPONSE}
  CODES_TSearchParamsGuidanceResponse : Array[TSearchParamsGuidanceResponse] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'identifier {http://hl7.org/fhir/SearchParameter/GuidanceResponse-identifier}', 'patient {http://hl7.org/fhir/SearchParameter/GuidanceResponse-patient}', 'request {http://hl7.org/fhir/SearchParameter/GuidanceResponse-request}',
       'subject {http://hl7.org/fhir/SearchParameter/GuidanceResponse-subject}');
{$ENDIF}
{$IFDEF FHIR_HEALTHCARESERVICE}
  CODES_TSearchParamsHealthcareService : Array[TSearchParamsHealthcareService] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'active {http://hl7.org/fhir/SearchParameter/HealthcareService-active}', 'characteristic {http://hl7.org/fhir/SearchParameter/HealthcareService-characteristic}', 'coverage-area {http://hl7.org/fhir/SearchParameter/HealthcareService-coverage-area}',
       'endpoint {http://hl7.org/fhir/SearchParameter/HealthcareService-endpoint}', 'identifier {http://hl7.org/fhir/SearchParameter/HealthcareService-identifier}', 'location {http://hl7.org/fhir/SearchParameter/HealthcareService-location}', 'name {http://hl7.org/fhir/SearchParameter/HealthcareService-name}', 'organization {http://hl7.org/fhir/SearchParameter/HealthcareService-organization}', 'program {http://hl7.org/fhir/SearchParameter/HealthcareService-program}', 'service-category {http://hl7.org/fhir/SearchParameter/HealthcareService-service-category}', 'service-type {http://hl7.org/fhir/SearchParameter/HealthcareService-service-type}', 'specialty {http://hl7.org/fhir/SearchParameter/HealthcareService-specialty}');
{$ENDIF}
{$IFDEF FHIR_IMAGINGSTUDY}
  CODES_TSearchParamsImagingStudy : Array[TSearchParamsImagingStudy] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'basedon {http://hl7.org/fhir/SearchParameter/ImagingStudy-basedon}', 'bodysite {http://hl7.org/fhir/SearchParameter/ImagingStudy-bodysite}', 'dicom-class {http://hl7.org/fhir/SearchParameter/ImagingStudy-dicom-class}',
       'encounter {http://hl7.org/fhir/SearchParameter/ImagingStudy-encounter}', 'endpoint {http://hl7.org/fhir/SearchParameter/ImagingStudy-endpoint}', 'identifier {http://hl7.org/fhir/SearchParameter/clinical-identifier}', 'instance {http://hl7.org/fhir/SearchParameter/ImagingStudy-instance}', 'interpreter {http://hl7.org/fhir/SearchParameter/ImagingStudy-interpreter}', 'modality {http://hl7.org/fhir/SearchParameter/ImagingStudy-modality}', 'patient {http://hl7.org/fhir/SearchParameter/clinical-patient}', 'performer {http://hl7.org/fhir/SearchParameter/ImagingStudy-performer}', 'reason {http://hl7.org/fhir/SearchParameter/ImagingStudy-reason}', 'referrer {http://hl7.org/fhir/SearchParameter/ImagingStudy-referrer}', 'series {http://hl7.org/fhir/SearchParameter/ImagingStudy-series}', 'started {http://hl7.org/fhir/SearchParameter/ImagingStudy-started}', 'status {http://hl7.org/fhir/SearchParameter/ImagingStudy-status}', 'subject {http://hl7.org/fhir/SearchParameter/ImagingStudy-subject}');
{$ENDIF}
{$IFDEF FHIR_IMMUNIZATION}
  CODES_TSearchParamsImmunization : Array[TSearchParamsImmunization] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'date {http://hl7.org/fhir/SearchParameter/clinical-date}', 'identifier {http://hl7.org/fhir/SearchParameter/clinical-identifier}', 'location {http://hl7.org/fhir/SearchParameter/Immunization-location}', 'lot-number {http://hl7.org/fhir/SearchParameter/Immunization-lot-number}',
       'manufacturer {http://hl7.org/fhir/SearchParameter/Immunization-manufacturer}', 'patient {http://hl7.org/fhir/SearchParameter/clinical-patient}', 'performer {http://hl7.org/fhir/SearchParameter/Immunization-performer}', 'reaction {http://hl7.org/fhir/SearchParameter/Immunization-reaction}', 'reaction-date {http://hl7.org/fhir/SearchParameter/Immunization-reaction-date}', 'reason-code {http://hl7.org/fhir/SearchParameter/Immunization-reason-code}', 'reason-reference {http://hl7.org/fhir/SearchParameter/Immunization-reason-reference}', 'series {http://hl7.org/fhir/SearchParameter/Immunization-series}', 'status {http://hl7.org/fhir/SearchParameter/Immunization-status}', 'status-reason {http://hl7.org/fhir/SearchParameter/Immunization-status-reason}', 'target-disease {http://hl7.org/fhir/SearchParameter/Immunization-target-disease}', 'vaccine-code {http://hl7.org/fhir/SearchParameter/Immunization-vaccine-code}');
{$ENDIF}
{$IFDEF FHIR_IMMUNIZATIONEVALUATION}
  CODES_TSearchParamsImmunizationEvaluation : Array[TSearchParamsImmunizationEvaluation] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'date {http://hl7.org/fhir/SearchParameter/ImmunizationEvaluation-date}', 'dose-status {http://hl7.org/fhir/SearchParameter/ImmunizationEvaluation-dose-status}', 'identifier {http://hl7.org/fhir/SearchParameter/ImmunizationEvaluation-identifier}',
       'immunization-event {http://hl7.org/fhir/SearchParameter/ImmunizationEvaluation-immunization-event}', 'patient {http://hl7.org/fhir/SearchParameter/ImmunizationEvaluation-patient}', 'status {http://hl7.org/fhir/SearchParameter/ImmunizationEvaluation-status}', 'target-disease {http://hl7.org/fhir/SearchParameter/ImmunizationEvaluation-target-disease}');
{$ENDIF}
{$IFDEF FHIR_IMMUNIZATIONRECOMMENDATION}
  CODES_TSearchParamsImmunizationRecommendation : Array[TSearchParamsImmunizationRecommendation] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'date {http://hl7.org/fhir/SearchParameter/ImmunizationRecommendation-date}', 'identifier {http://hl7.org/fhir/SearchParameter/ImmunizationRecommendation-identifier}', 'information {http://hl7.org/fhir/SearchParameter/ImmunizationRecommendation-information}',
       'patient {http://hl7.org/fhir/SearchParameter/ImmunizationRecommendation-patient}', 'status {http://hl7.org/fhir/SearchParameter/ImmunizationRecommendation-status}', 'support {http://hl7.org/fhir/SearchParameter/ImmunizationRecommendation-support}', 'target-disease {http://hl7.org/fhir/SearchParameter/ImmunizationRecommendation-target-disease}', 'vaccine-type {http://hl7.org/fhir/SearchParameter/ImmunizationRecommendation-vaccine-type}');
{$ENDIF}
{$IFDEF FHIR_IMPLEMENTATIONGUIDE}
  CODES_TSearchParamsImplementationGuide : Array[TSearchParamsImplementationGuide] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'context {http://hl7.org/fhir/SearchParameter/conformance-context}', 'context-quantity {http://hl7.org/fhir/SearchParameter/conformance-context-quantity}', 'context-type {http://hl7.org/fhir/SearchParameter/conformance-context-type}',
       'context-type-quantity {http://hl7.org/fhir/SearchParameter/conformance-context-type-quantity}', 'context-type-value {http://hl7.org/fhir/SearchParameter/conformance-context-type-value}', 'date {http://hl7.org/fhir/SearchParameter/conformance-date}', 'depends-on {http://hl7.org/fhir/SearchParameter/ImplementationGuide-depends-on}', 'description {http://hl7.org/fhir/SearchParameter/conformance-description}', 'experimental {http://hl7.org/fhir/SearchParameter/ImplementationGuide-experimental}', 'global {http://hl7.org/fhir/SearchParameter/ImplementationGuide-global}', 'jurisdiction {http://hl7.org/fhir/SearchParameter/conformance-jurisdiction}', 'name {http://hl7.org/fhir/SearchParameter/conformance-name}', 'publisher {http://hl7.org/fhir/SearchParameter/conformance-publisher}', 'resource {http://hl7.org/fhir/SearchParameter/ImplementationGuide-resource}', 'status {http://hl7.org/fhir/SearchParameter/conformance-status}', 'title {http://hl7.org/fhir/SearchParameter/conformance-title}',
       'url {http://hl7.org/fhir/SearchParameter/conformance-url}', 'version {http://hl7.org/fhir/SearchParameter/conformance-version}');
{$ENDIF}
{$IFDEF FHIR_INGREDIENT}
  CODES_TSearchParamsIngredient : Array[TSearchParamsIngredient] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'function {http://hl7.org/fhir/SearchParameter/Ingredient-function}', 'identifier {http://hl7.org/fhir/SearchParameter/Ingredient-identifier}', 'manufacturer {http://hl7.org/fhir/SearchParameter/Ingredient-manufacturer}', 'role {http://hl7.org/fhir/SearchParameter/Ingredient-role}',
       'specified-substance {http://hl7.org/fhir/SearchParameter/Ingredient-specified-substance}', 'specified-substance-code {http://hl7.org/fhir/SearchParameter/Ingredient-specified-substance-code}', 'specified-substance-definition {http://hl7.org/fhir/SearchParameter/Ingredient-specified-substance-definition}', 'substance {http://hl7.org/fhir/SearchParameter/Ingredient-substance}', 'substance-code {http://hl7.org/fhir/SearchParameter/Ingredient-substance-code}', 'substance-definition {http://hl7.org/fhir/SearchParameter/Ingredient-substance-definition}');
{$ENDIF}
{$IFDEF FHIR_INSURANCEPLAN}
  CODES_TSearchParamsInsurancePlan : Array[TSearchParamsInsurancePlan] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'address {http://hl7.org/fhir/SearchParameter/InsurancePlan-address}', 'address-city {http://hl7.org/fhir/SearchParameter/InsurancePlan-address-city}', 'address-country {http://hl7.org/fhir/SearchParameter/InsurancePlan-address-country}',
       'address-postalcode {http://hl7.org/fhir/SearchParameter/InsurancePlan-address-postalcode}', 'address-state {http://hl7.org/fhir/SearchParameter/InsurancePlan-address-state}', 'address-use {http://hl7.org/fhir/SearchParameter/InsurancePlan-address-use}', 'administered-by {http://hl7.org/fhir/SearchParameter/InsurancePlan-administered-by}', 'endpoint {http://hl7.org/fhir/SearchParameter/InsurancePlan-endpoint}', 'identifier {http://hl7.org/fhir/SearchParameter/InsurancePlan-identifier}', 'name {http://hl7.org/fhir/SearchParameter/InsurancePlan-name}', 'owned-by {http://hl7.org/fhir/SearchParameter/InsurancePlan-owned-by}', 'phonetic {http://hl7.org/fhir/SearchParameter/InsurancePlan-phonetic}', 'status {http://hl7.org/fhir/SearchParameter/InsurancePlan-status}', 'type {http://hl7.org/fhir/SearchParameter/InsurancePlan-type}');
{$ENDIF}
{$IFDEF FHIR_INVOICE}
  CODES_TSearchParamsInvoice : Array[TSearchParamsInvoice] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'account {http://hl7.org/fhir/SearchParameter/Invoice-account}', 'date {http://hl7.org/fhir/SearchParameter/Invoice-date}', 'identifier {http://hl7.org/fhir/SearchParameter/Invoice-identifier}', 'issuer {http://hl7.org/fhir/SearchParameter/Invoice-issuer}',
       'participant {http://hl7.org/fhir/SearchParameter/Invoice-participant}', 'participant-role {http://hl7.org/fhir/SearchParameter/Invoice-participant-role}', 'patient {http://hl7.org/fhir/SearchParameter/Invoice-patient}', 'recipient {http://hl7.org/fhir/SearchParameter/Invoice-recipient}', 'status {http://hl7.org/fhir/SearchParameter/Invoice-status}', 'subject {http://hl7.org/fhir/SearchParameter/Invoice-subject}', 'totalgross {http://hl7.org/fhir/SearchParameter/Invoice-totalgross}', 'totalnet {http://hl7.org/fhir/SearchParameter/Invoice-totalnet}', 'type {http://hl7.org/fhir/SearchParameter/Invoice-type}');
{$ENDIF}
{$IFDEF FHIR_LIBRARY}
  CODES_TSearchParamsLibrary : Array[TSearchParamsLibrary] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'composed-of {http://hl7.org/fhir/SearchParameter/Library-composed-of}', 'content-type {http://hl7.org/fhir/SearchParameter/Library-content-type}', 'context {http://hl7.org/fhir/SearchParameter/Library-context}', 'context-quantity {http://hl7.org/fhir/SearchParameter/Library-context-quantity}',
       'context-type {http://hl7.org/fhir/SearchParameter/Library-context-type}', 'context-type-quantity {http://hl7.org/fhir/SearchParameter/Library-context-type-quantity}', 'context-type-value {http://hl7.org/fhir/SearchParameter/Library-context-type-value}', 'date {http://hl7.org/fhir/SearchParameter/Library-date}', 'depends-on {http://hl7.org/fhir/SearchParameter/Library-depends-on}', 'derived-from {http://hl7.org/fhir/SearchParameter/Library-derived-from}', 'description {http://hl7.org/fhir/SearchParameter/Library-description}', 'effective {http://hl7.org/fhir/SearchParameter/Library-effective}', 'identifier {http://hl7.org/fhir/SearchParameter/Library-identifier}', 'jurisdiction {http://hl7.org/fhir/SearchParameter/Library-jurisdiction}', 'name {http://hl7.org/fhir/SearchParameter/Library-name}', 'predecessor {http://hl7.org/fhir/SearchParameter/Library-predecessor}', 'publisher {http://hl7.org/fhir/SearchParameter/Library-publisher}', 'status {http://hl7.org/fhir/SearchParameter/Library-status}',
       'successor {http://hl7.org/fhir/SearchParameter/Library-successor}', 'title {http://hl7.org/fhir/SearchParameter/Library-title}', 'topic {http://hl7.org/fhir/SearchParameter/Library-topic}', 'type {http://hl7.org/fhir/SearchParameter/Library-type}', 'url {http://hl7.org/fhir/SearchParameter/Library-url}', 'version {http://hl7.org/fhir/SearchParameter/Library-version}');
{$ENDIF}
{$IFDEF FHIR_LINKAGE}
  CODES_TSearchParamsLinkage : Array[TSearchParamsLinkage] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'author {http://hl7.org/fhir/SearchParameter/Linkage-author}', 'item {http://hl7.org/fhir/SearchParameter/Linkage-item}', 'source {http://hl7.org/fhir/SearchParameter/Linkage-source}');
{$ENDIF}
{$IFDEF FHIR_LIST}
  CODES_TSearchParamsList : Array[TSearchParamsList] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'code {http://hl7.org/fhir/SearchParameter/clinical-code}', 'date {http://hl7.org/fhir/SearchParameter/clinical-date}', 'empty-reason {http://hl7.org/fhir/SearchParameter/List-empty-reason}', 'encounter {http://hl7.org/fhir/SearchParameter/clinical-encounter}',
       'identifier {http://hl7.org/fhir/SearchParameter/clinical-identifier}', 'item {http://hl7.org/fhir/SearchParameter/List-item}', 'notes {http://hl7.org/fhir/SearchParameter/List-notes}', 'patient {http://hl7.org/fhir/SearchParameter/clinical-patient}', 'source {http://hl7.org/fhir/SearchParameter/List-source}', 'status {http://hl7.org/fhir/SearchParameter/List-status}', 'subject {http://hl7.org/fhir/SearchParameter/List-subject}', 'title {http://hl7.org/fhir/SearchParameter/List-title}');
{$ENDIF}
{$IFDEF FHIR_LOCATION}
  CODES_TSearchParamsLocation : Array[TSearchParamsLocation] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'address {http://hl7.org/fhir/SearchParameter/Location-address}', 'address-city {http://hl7.org/fhir/SearchParameter/Location-address-city}', 'address-country {http://hl7.org/fhir/SearchParameter/Location-address-country}',
       'address-postalcode {http://hl7.org/fhir/SearchParameter/Location-address-postalcode}', 'address-state {http://hl7.org/fhir/SearchParameter/Location-address-state}', 'address-use {http://hl7.org/fhir/SearchParameter/Location-address-use}', 'endpoint {http://hl7.org/fhir/SearchParameter/Location-endpoint}', 'identifier {http://hl7.org/fhir/SearchParameter/Location-identifier}', 'name {http://hl7.org/fhir/SearchParameter/Location-name}', 'near {http://hl7.org/fhir/SearchParameter/Location-near}', 'operational-status {http://hl7.org/fhir/SearchParameter/Location-operational-status}', 'organization {http://hl7.org/fhir/SearchParameter/Location-organization}', 'partof {http://hl7.org/fhir/SearchParameter/Location-partof}', 'status {http://hl7.org/fhir/SearchParameter/Location-status}', 'type {http://hl7.org/fhir/SearchParameter/Location-type}');
{$ENDIF}
{$IFDEF FHIR_MANUFACTUREDITEMDEFINITION}
  CODES_TSearchParamsManufacturedItemDefinition : Array[TSearchParamsManufacturedItemDefinition] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'dose-form {http://hl7.org/fhir/SearchParameter/ManufacturedItemDefinition-dose-form}', 'identifier {http://hl7.org/fhir/SearchParameter/ManufacturedItemDefinition-identifier}',
       'ingredient {http://hl7.org/fhir/SearchParameter/ManufacturedItemDefinition-ingredient}');
{$ENDIF}
{$IFDEF FHIR_MEASURE}
  CODES_TSearchParamsMeasure : Array[TSearchParamsMeasure] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'composed-of {http://hl7.org/fhir/SearchParameter/Measure-composed-of}', 'context {http://hl7.org/fhir/SearchParameter/Measure-context}', 'context-quantity {http://hl7.org/fhir/SearchParameter/Measure-context-quantity}', 'context-type {http://hl7.org/fhir/SearchParameter/Measure-context-type}',
       'context-type-quantity {http://hl7.org/fhir/SearchParameter/Measure-context-type-quantity}', 'context-type-value {http://hl7.org/fhir/SearchParameter/Measure-context-type-value}', 'date {http://hl7.org/fhir/SearchParameter/Measure-date}', 'depends-on {http://hl7.org/fhir/SearchParameter/Measure-depends-on}', 'derived-from {http://hl7.org/fhir/SearchParameter/Measure-derived-from}', 'description {http://hl7.org/fhir/SearchParameter/Measure-description}', 'effective {http://hl7.org/fhir/SearchParameter/Measure-effective}', 'identifier {http://hl7.org/fhir/SearchParameter/Measure-identifier}', 'jurisdiction {http://hl7.org/fhir/SearchParameter/Measure-jurisdiction}', 'name {http://hl7.org/fhir/SearchParameter/Measure-name}', 'predecessor {http://hl7.org/fhir/SearchParameter/Measure-predecessor}', 'publisher {http://hl7.org/fhir/SearchParameter/Measure-publisher}', 'status {http://hl7.org/fhir/SearchParameter/Measure-status}', 'successor {http://hl7.org/fhir/SearchParameter/Measure-successor}',
       'title {http://hl7.org/fhir/SearchParameter/Measure-title}', 'topic {http://hl7.org/fhir/SearchParameter/Measure-topic}', 'url {http://hl7.org/fhir/SearchParameter/Measure-url}', 'version {http://hl7.org/fhir/SearchParameter/Measure-version}');
{$ENDIF}
{$IFDEF FHIR_MEASUREREPORT}
  CODES_TSearchParamsMeasureReport : Array[TSearchParamsMeasureReport] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'date {http://hl7.org/fhir/SearchParameter/MeasureReport-date}', 'evaluated-resource {http://hl7.org/fhir/SearchParameter/MeasureReport-evaluated-resource}', 'identifier {http://hl7.org/fhir/SearchParameter/MeasureReport-identifier}',
       'measure {http://hl7.org/fhir/SearchParameter/MeasureReport-measure}', 'patient {http://hl7.org/fhir/SearchParameter/MeasureReport-patient}', 'period {http://hl7.org/fhir/SearchParameter/MeasureReport-period}', 'reporter {http://hl7.org/fhir/SearchParameter/MeasureReport-reporter}', 'status {http://hl7.org/fhir/SearchParameter/MeasureReport-status}', 'subject {http://hl7.org/fhir/SearchParameter/MeasureReport-subject}');
{$ENDIF}
{$IFDEF FHIR_MEDICATION}
  CODES_TSearchParamsMedication : Array[TSearchParamsMedication] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'code {http://hl7.org/fhir/SearchParameter/clinical-code}', 'expiration-date {http://hl7.org/fhir/SearchParameter/Medication-expiration-date}', 'form {http://hl7.org/fhir/SearchParameter/Medication-form}', 'identifier {http://hl7.org/fhir/SearchParameter/Medication-identifier}',
       'ingredient {http://hl7.org/fhir/SearchParameter/Medication-ingredient}', 'ingredient-code {http://hl7.org/fhir/SearchParameter/Medication-ingredient-code}', 'lot-number {http://hl7.org/fhir/SearchParameter/Medication-lot-number}', 'manufacturer {http://hl7.org/fhir/SearchParameter/Medication-manufacturer}', 'status {http://hl7.org/fhir/SearchParameter/Medication-status}');
{$ENDIF}
{$IFDEF FHIR_MEDICATIONADMINISTRATION}
  CODES_TSearchParamsMedicationAdministration : Array[TSearchParamsMedicationAdministration] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'code {http://hl7.org/fhir/SearchParameter/clinical-code}', 'date {http://hl7.org/fhir/SearchParameter/medications-date}', 'device {http://hl7.org/fhir/SearchParameter/MedicationAdministration-device}',
       'encounter {http://hl7.org/fhir/SearchParameter/medications-encounter}', 'identifier {http://hl7.org/fhir/SearchParameter/clinical-identifier}', 'medication {http://hl7.org/fhir/SearchParameter/medications-medication}', 'patient {http://hl7.org/fhir/SearchParameter/clinical-patient}', 'performer {http://hl7.org/fhir/SearchParameter/MedicationAdministration-performer}', 'reason-given {http://hl7.org/fhir/SearchParameter/MedicationAdministration-reason-given}', 'reason-given-code {http://hl7.org/fhir/SearchParameter/MedicationAdministration-reason-given-code}', 'reason-not-given {http://hl7.org/fhir/SearchParameter/MedicationAdministration-reason-not-given}', 'request {http://hl7.org/fhir/SearchParameter/MedicationAdministration-request}', 'status {http://hl7.org/fhir/SearchParameter/medications-status}', 'subject {http://hl7.org/fhir/SearchParameter/MedicationAdministration-subject}');
{$ENDIF}
{$IFDEF FHIR_MEDICATIONDISPENSE}
  CODES_TSearchParamsMedicationDispense : Array[TSearchParamsMedicationDispense] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'code {http://hl7.org/fhir/SearchParameter/clinical-code}', 'destination {http://hl7.org/fhir/SearchParameter/MedicationDispense-destination}', 'encounter {http://hl7.org/fhir/SearchParameter/MedicationDispense-encounter}',
       'identifier {http://hl7.org/fhir/SearchParameter/clinical-identifier}', 'medication {http://hl7.org/fhir/SearchParameter/medications-medication}', 'patient {http://hl7.org/fhir/SearchParameter/clinical-patient}', 'performer {http://hl7.org/fhir/SearchParameter/MedicationDispense-performer}', 'prescription {http://hl7.org/fhir/SearchParameter/medications-prescription}', 'receiver {http://hl7.org/fhir/SearchParameter/MedicationDispense-receiver}', 'responsibleparty {http://hl7.org/fhir/SearchParameter/MedicationDispense-responsibleparty}', 'status {http://hl7.org/fhir/SearchParameter/medications-status}', 'subject {http://hl7.org/fhir/SearchParameter/MedicationDispense-subject}', 'type {http://hl7.org/fhir/SearchParameter/MedicationDispense-type}', 'whenhandedover {http://hl7.org/fhir/SearchParameter/MedicationDispense-whenhandedover}', 'whenprepared {http://hl7.org/fhir/SearchParameter/MedicationDispense-whenprepared}');
{$ENDIF}
{$IFDEF FHIR_MEDICATIONKNOWLEDGE}
  CODES_TSearchParamsMedicationKnowledge : Array[TSearchParamsMedicationKnowledge] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'classification {http://hl7.org/fhir/SearchParameter/MedicationKnowledge-classification}', 'classification-type {http://hl7.org/fhir/SearchParameter/MedicationKnowledge-classification-type}', 'code {http://hl7.org/fhir/SearchParameter/MedicationKnowledge-code}',
       'doseform {http://hl7.org/fhir/SearchParameter/MedicationKnowledge-doseform}', 'identifier {http://hl7.org/fhir/SearchParameter/MedicationKnowledge-identifier}', 'ingredient {http://hl7.org/fhir/SearchParameter/MedicationKnowledge-ingredient}', 'ingredient-code {http://hl7.org/fhir/SearchParameter/MedicationKnowledge-ingredient-code}', 'manufacturer {http://hl7.org/fhir/SearchParameter/MedicationKnowledge-manufacturer}', 'monitoring-program-name {http://hl7.org/fhir/SearchParameter/MedicationKnowledge-monitoring-program-name}', 'monitoring-program-type {http://hl7.org/fhir/SearchParameter/MedicationKnowledge-monitoring-program-type}', 'monograph {http://hl7.org/fhir/SearchParameter/MedicationKnowledge-monograph}', 'monograph-type {http://hl7.org/fhir/SearchParameter/MedicationKnowledge-monograph-type}', 'packaging-cost {http://hl7.org/fhir/SearchParameter/MedicationKnowledge-packaging-cost}',
       'packaging-cost-concept {http://hl7.org/fhir/SearchParameter/MedicationKnowledge-packaging-cost-concept}', 'product-type {http://hl7.org/fhir/SearchParameter/MedicationKnowledge-product-type}', 'source-cost {http://hl7.org/fhir/SearchParameter/MedicationKnowledge-source-cost}', 'status {http://hl7.org/fhir/SearchParameter/MedicationKnowledge-status}');
{$ENDIF}
{$IFDEF FHIR_MEDICATIONREQUEST}
  CODES_TSearchParamsMedicationRequest : Array[TSearchParamsMedicationRequest] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'authoredon {http://hl7.org/fhir/SearchParameter/MedicationRequest-authoredon}', 'category {http://hl7.org/fhir/SearchParameter/MedicationRequest-category}', 'code {http://hl7.org/fhir/SearchParameter/clinical-code}',
       'combo-date {http://hl7.org/fhir/SearchParameter/MedicationRequest-combo-date}', 'encounter {http://hl7.org/fhir/SearchParameter/medications-encounter}', 'identifier {http://hl7.org/fhir/SearchParameter/clinical-identifier}', 'intended-dispenser {http://hl7.org/fhir/SearchParameter/MedicationRequest-intended-dispenser}', 'intended-performer {http://hl7.org/fhir/SearchParameter/MedicationRequest-intended-performer}', 'intended-performertype {http://hl7.org/fhir/SearchParameter/MedicationRequest-intended-performertype}', 'intent {http://hl7.org/fhir/SearchParameter/MedicationRequest-intent}', 'medication {http://hl7.org/fhir/SearchParameter/medications-medication}', 'patient {http://hl7.org/fhir/SearchParameter/clinical-patient}', 'priority {http://hl7.org/fhir/SearchParameter/MedicationRequest-priority}', 'requester {http://hl7.org/fhir/SearchParameter/MedicationRequest-requester}', 'status {http://hl7.org/fhir/SearchParameter/medications-status}',
       'subject {http://hl7.org/fhir/SearchParameter/MedicationRequest-subject}');
{$ENDIF}
{$IFDEF FHIR_MEDICATIONUSAGE}
  CODES_TSearchParamsMedicationUsage : Array[TSearchParamsMedicationUsage] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'adherence {http://hl7.org/fhir/SearchParameter/MedicationUsage-adherence}', 'category {http://hl7.org/fhir/SearchParameter/MedicationUsage-category}', 'code {http://hl7.org/fhir/SearchParameter/clinical-code}',
       'effective {http://hl7.org/fhir/SearchParameter/MedicationUsage-effective}', 'encounter {http://hl7.org/fhir/SearchParameter/MedicationUsage-encounter}', 'identifier {http://hl7.org/fhir/SearchParameter/clinical-identifier}', 'medication {http://hl7.org/fhir/SearchParameter/medications-medication}', 'part-of {http://hl7.org/fhir/SearchParameter/MedicationUsage-part-of}', 'patient {http://hl7.org/fhir/SearchParameter/clinical-patient}', 'source {http://hl7.org/fhir/SearchParameter/MedicationUsage-source}', 'status {http://hl7.org/fhir/SearchParameter/medications-status}', 'subject {http://hl7.org/fhir/SearchParameter/MedicationUsage-subject}');
{$ENDIF}
{$IFDEF FHIR_MEDICINALPRODUCTDEFINITION}
  CODES_TSearchParamsMedicinalProductDefinition : Array[TSearchParamsMedicinalProductDefinition] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'characteristic {http://hl7.org/fhir/SearchParameter/MedicinalProductDefinition-characteristic}', 'contact {http://hl7.org/fhir/SearchParameter/MedicinalProductDefinition-contact}', 'domain {http://hl7.org/fhir/SearchParameter/MedicinalProductDefinition-domain}',
       'identifier {http://hl7.org/fhir/SearchParameter/MedicinalProductDefinition-identifier}', 'ingredient {http://hl7.org/fhir/SearchParameter/MedicinalProductDefinition-ingredient}', 'master-file {http://hl7.org/fhir/SearchParameter/MedicinalProductDefinition-master-file}', 'name {http://hl7.org/fhir/SearchParameter/MedicinalProductDefinition-name}', 'name-language {http://hl7.org/fhir/SearchParameter/MedicinalProductDefinition-name-language}', 'product-classification {http://hl7.org/fhir/SearchParameter/MedicinalProductDefinition-product-classification}', 'status {http://hl7.org/fhir/SearchParameter/MedicinalProductDefinition-status}', 'type {http://hl7.org/fhir/SearchParameter/MedicinalProductDefinition-type}');
{$ENDIF}
{$IFDEF FHIR_MESSAGEDEFINITION}
  CODES_TSearchParamsMessageDefinition : Array[TSearchParamsMessageDefinition] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'category {http://hl7.org/fhir/SearchParameter/MessageDefinition-category}', 'context {http://hl7.org/fhir/SearchParameter/conformance-context}', 'context-quantity {http://hl7.org/fhir/SearchParameter/conformance-context-quantity}',
       'context-type {http://hl7.org/fhir/SearchParameter/conformance-context-type}', 'context-type-quantity {http://hl7.org/fhir/SearchParameter/conformance-context-type-quantity}', 'context-type-value {http://hl7.org/fhir/SearchParameter/conformance-context-type-value}', 'date {http://hl7.org/fhir/SearchParameter/conformance-date}', 'description {http://hl7.org/fhir/SearchParameter/conformance-description}', 'event {http://hl7.org/fhir/SearchParameter/MessageDefinition-event}', 'focus {http://hl7.org/fhir/SearchParameter/MessageDefinition-focus}', 'identifier {http://hl7.org/fhir/SearchParameter/conformance-identifier}', 'jurisdiction {http://hl7.org/fhir/SearchParameter/conformance-jurisdiction}', 'name {http://hl7.org/fhir/SearchParameter/conformance-name}', 'parent {http://hl7.org/fhir/SearchParameter/MessageDefinition-parent}', 'publisher {http://hl7.org/fhir/SearchParameter/conformance-publisher}', 'status {http://hl7.org/fhir/SearchParameter/conformance-status}',
       'title {http://hl7.org/fhir/SearchParameter/conformance-title}', 'url {http://hl7.org/fhir/SearchParameter/conformance-url}', 'version {http://hl7.org/fhir/SearchParameter/conformance-version}');
{$ENDIF}
{$IFDEF FHIR_MESSAGEHEADER}
  CODES_TSearchParamsMessageHeader : Array[TSearchParamsMessageHeader] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'author {http://hl7.org/fhir/SearchParameter/MessageHeader-author}', 'code {http://hl7.org/fhir/SearchParameter/MessageHeader-code}', 'destination {http://hl7.org/fhir/SearchParameter/MessageHeader-destination}',
       'destination-uri {http://hl7.org/fhir/SearchParameter/MessageHeader-destination-uri}', 'enterer {http://hl7.org/fhir/SearchParameter/MessageHeader-enterer}', 'event {http://hl7.org/fhir/SearchParameter/MessageHeader-event}', 'focus {http://hl7.org/fhir/SearchParameter/MessageHeader-focus}', 'receiver {http://hl7.org/fhir/SearchParameter/MessageHeader-receiver}', 'response-id {http://hl7.org/fhir/SearchParameter/MessageHeader-response-id}', 'responsible {http://hl7.org/fhir/SearchParameter/MessageHeader-responsible}', 'sender {http://hl7.org/fhir/SearchParameter/MessageHeader-sender}', 'source {http://hl7.org/fhir/SearchParameter/MessageHeader-source}', 'source-uri {http://hl7.org/fhir/SearchParameter/MessageHeader-source-uri}', 'target {http://hl7.org/fhir/SearchParameter/MessageHeader-target}');
{$ENDIF}
{$IFDEF FHIR_MOLECULARSEQUENCE}
  CODES_TSearchParamsMolecularSequence : Array[TSearchParamsMolecularSequence] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'chromosome {http://hl7.org/fhir/SearchParameter/MolecularSequence-chromosome}', 'chromosome-variant-coordinate {http://hl7.org/fhir/SearchParameter/MolecularSequence-chromosome-variant-coordinate}',
       'chromosome-window-coordinate {http://hl7.org/fhir/SearchParameter/MolecularSequence-chromosome-window-coordinate}', 'identifier {http://hl7.org/fhir/SearchParameter/MolecularSequence-identifier}', 'patient {http://hl7.org/fhir/SearchParameter/MolecularSequence-patient}', 'referenceseqid {http://hl7.org/fhir/SearchParameter/MolecularSequence-referenceseqid}', 'referenceseqid-variant-coordinate {http://hl7.org/fhir/SearchParameter/MolecularSequence-referenceseqid-variant-coordinate}', 'referenceseqid-window-coordinate {http://hl7.org/fhir/SearchParameter/MolecularSequence-referenceseqid-window-coordinate}', 'type {http://hl7.org/fhir/SearchParameter/MolecularSequence-type}', 'variant-end {http://hl7.org/fhir/SearchParameter/MolecularSequence-variant-end}', 'variant-start {http://hl7.org/fhir/SearchParameter/MolecularSequence-variant-start}', 'window-end {http://hl7.org/fhir/SearchParameter/MolecularSequence-window-end}',
       'window-start {http://hl7.org/fhir/SearchParameter/MolecularSequence-window-start}');
{$ENDIF}
{$IFDEF FHIR_NAMINGSYSTEM}
  CODES_TSearchParamsNamingSystem : Array[TSearchParamsNamingSystem] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'contact {http://hl7.org/fhir/SearchParameter/NamingSystem-contact}', 'context {http://hl7.org/fhir/SearchParameter/conformance-context}', 'context-quantity {http://hl7.org/fhir/SearchParameter/conformance-context-quantity}',
       'context-type {http://hl7.org/fhir/SearchParameter/conformance-context-type}', 'context-type-quantity {http://hl7.org/fhir/SearchParameter/conformance-context-type-quantity}', 'context-type-value {http://hl7.org/fhir/SearchParameter/conformance-context-type-value}', 'date {http://hl7.org/fhir/SearchParameter/conformance-date}', 'description {http://hl7.org/fhir/SearchParameter/conformance-description}', 'id-type {http://hl7.org/fhir/SearchParameter/NamingSystem-id-type}', 'jurisdiction {http://hl7.org/fhir/SearchParameter/conformance-jurisdiction}', 'kind {http://hl7.org/fhir/SearchParameter/NamingSystem-kind}', 'name {http://hl7.org/fhir/SearchParameter/conformance-name}', 'period {http://hl7.org/fhir/SearchParameter/NamingSystem-period}', 'publisher {http://hl7.org/fhir/SearchParameter/conformance-publisher}', 'responsible {http://hl7.org/fhir/SearchParameter/NamingSystem-responsible}', 'status {http://hl7.org/fhir/SearchParameter/conformance-status}',
       'telecom {http://hl7.org/fhir/SearchParameter/NamingSystem-telecom}', 'type {http://hl7.org/fhir/SearchParameter/NamingSystem-type}', 'url {http://hl7.org/fhir/SearchParameter/conformance-url}', 'value {http://hl7.org/fhir/SearchParameter/NamingSystem-value}', 'version {http://hl7.org/fhir/SearchParameter/conformance-version}');
{$ENDIF}
{$IFDEF FHIR_NUTRITIONINTAKE}
  CODES_TSearchParamsNutritionIntake : Array[TSearchParamsNutritionIntake] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'code {http://hl7.org/fhir/SearchParameter/NutritionIntake-code}', 'date {http://hl7.org/fhir/SearchParameter/NutritionIntake-date}', 'encounter {http://hl7.org/fhir/SearchParameter/NutritionIntake-encounter}',
       'identifier {http://hl7.org/fhir/SearchParameter/NutritionIntake-identifier}', 'nutrition {http://hl7.org/fhir/SearchParameter/NutritionIntake-nutrition}', 'part-of {http://hl7.org/fhir/SearchParameter/NutritionIntake-part-of}', 'patient {http://hl7.org/fhir/SearchParameter/NutritionIntake-patient}', 'source {http://hl7.org/fhir/SearchParameter/NutritionIntake-source}', 'status {http://hl7.org/fhir/SearchParameter/NutritionIntake-status}', 'subject {http://hl7.org/fhir/SearchParameter/NutritionIntake-subject}');
{$ENDIF}
{$IFDEF FHIR_NUTRITIONORDER}
  CODES_TSearchParamsNutritionOrder : Array[TSearchParamsNutritionOrder] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'additive {http://hl7.org/fhir/SearchParameter/NutritionOrder-additive}', 'datetime {http://hl7.org/fhir/SearchParameter/NutritionOrder-datetime}', 'encounter {http://hl7.org/fhir/SearchParameter/clinical-encounter}',
       'formula {http://hl7.org/fhir/SearchParameter/NutritionOrder-formula}', 'identifier {http://hl7.org/fhir/SearchParameter/clinical-identifier}', 'instantiates-canonical {http://hl7.org/fhir/SearchParameter/NutritionOrder-instantiates-canonical}', 'instantiates-uri {http://hl7.org/fhir/SearchParameter/NutritionOrder-instantiates-uri}', 'oraldiet {http://hl7.org/fhir/SearchParameter/NutritionOrder-oraldiet}', 'patient {http://hl7.org/fhir/SearchParameter/clinical-patient}', 'provider {http://hl7.org/fhir/SearchParameter/NutritionOrder-provider}', 'status {http://hl7.org/fhir/SearchParameter/NutritionOrder-status}', 'supplement {http://hl7.org/fhir/SearchParameter/NutritionOrder-supplement}');
{$ENDIF}
{$IFDEF FHIR_NUTRITIONPRODUCT}
  CODES_TSearchParamsNutritionProduct : Array[TSearchParamsNutritionProduct] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'identifier {http://hl7.org/fhir/SearchParameter/NutritionProduct-identifier}', 'status {http://hl7.org/fhir/SearchParameter/NutritionProduct-status}');
{$ENDIF}
{$IFDEF FHIR_OBSERVATION}
  CODES_TSearchParamsObservation : Array[TSearchParamsObservation] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'based-on {http://hl7.org/fhir/SearchParameter/Observation-based-on}', 'category {http://hl7.org/fhir/SearchParameter/Observation-category}', 'code {http://hl7.org/fhir/SearchParameter/clinical-code}', 'code-value-concept {http://hl7.org/fhir/SearchParameter/Observation-code-value-concept}',
       'code-value-date {http://hl7.org/fhir/SearchParameter/Observation-code-value-date}', 'code-value-quantity {http://hl7.org/fhir/SearchParameter/Observation-code-value-quantity}', 'code-value-string {http://hl7.org/fhir/SearchParameter/Observation-code-value-string}', 'combo-code {http://hl7.org/fhir/SearchParameter/Observation-combo-code}', 'combo-code-value-concept {http://hl7.org/fhir/SearchParameter/Observation-combo-code-value-concept}', 'combo-code-value-quantity {http://hl7.org/fhir/SearchParameter/Observation-combo-code-value-quantity}', 'combo-data-absent-reason {http://hl7.org/fhir/SearchParameter/Observation-combo-data-absent-reason}', 'combo-value-concept {http://hl7.org/fhir/SearchParameter/Observation-combo-value-concept}', 'combo-value-quantity {http://hl7.org/fhir/SearchParameter/Observation-combo-value-quantity}', 'component-code {http://hl7.org/fhir/SearchParameter/Observation-component-code}',
       'component-code-value-concept {http://hl7.org/fhir/SearchParameter/Observation-component-code-value-concept}', 'component-code-value-quantity {http://hl7.org/fhir/SearchParameter/Observation-component-code-value-quantity}', 'component-data-absent-reason {http://hl7.org/fhir/SearchParameter/Observation-component-data-absent-reason}', 'component-value-concept {http://hl7.org/fhir/SearchParameter/Observation-component-value-concept}', 'component-value-quantity {http://hl7.org/fhir/SearchParameter/Observation-component-value-quantity}', 'data-absent-reason {http://hl7.org/fhir/SearchParameter/Observation-data-absent-reason}', 'date {http://hl7.org/fhir/SearchParameter/clinical-date}', 'derived-from {http://hl7.org/fhir/SearchParameter/Observation-derived-from}', 'device {http://hl7.org/fhir/SearchParameter/Observation-device}', 'encounter {http://hl7.org/fhir/SearchParameter/clinical-encounter}', 'focus {http://hl7.org/fhir/SearchParameter/Observation-focus}',
       'has-member {http://hl7.org/fhir/SearchParameter/Observation-has-member}', 'identifier {http://hl7.org/fhir/SearchParameter/clinical-identifier}', 'method {http://hl7.org/fhir/SearchParameter/Observation-method}', 'part-of {http://hl7.org/fhir/SearchParameter/Observation-part-of}', 'patient {http://hl7.org/fhir/SearchParameter/clinical-patient}', 'performer {http://hl7.org/fhir/SearchParameter/Observation-performer}', 'specimen {http://hl7.org/fhir/SearchParameter/Observation-specimen}', 'status {http://hl7.org/fhir/SearchParameter/Observation-status}', 'subject {http://hl7.org/fhir/SearchParameter/Observation-subject}', 'value-concept {http://hl7.org/fhir/SearchParameter/Observation-value-concept}', 'value-date {http://hl7.org/fhir/SearchParameter/Observation-value-date}', 'value-quantity {http://hl7.org/fhir/SearchParameter/Observation-value-quantity}', 'value-string {http://hl7.org/fhir/SearchParameter/Observation-value-string}');
{$ENDIF}
{$IFDEF FHIR_OBSERVATIONDEFINITION}
  CODES_TSearchParamsObservationDefinition : Array[TSearchParamsObservationDefinition] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'category {http://hl7.org/fhir/SearchParameter/ObservationDefinition-category}', 'code {http://hl7.org/fhir/SearchParameter/ObservationDefinition-code}', 'experimental {http://hl7.org/fhir/SearchParameter/ObservationDefinition-experimental}',
       'identifier {http://hl7.org/fhir/SearchParameter/ObservationDefinition-identifier}', 'method {http://hl7.org/fhir/SearchParameter/ObservationDefinition-method}', 'status {http://hl7.org/fhir/SearchParameter/ObservationDefinition-status}', 'title {http://hl7.org/fhir/SearchParameter/ObservationDefinition-title}', 'url {http://hl7.org/fhir/SearchParameter/ObservationDefinition-url}');
{$ENDIF}
{$IFDEF FHIR_OPERATIONDEFINITION}
  CODES_TSearchParamsOperationDefinition : Array[TSearchParamsOperationDefinition] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'base {http://hl7.org/fhir/SearchParameter/OperationDefinition-base}', 'code {http://hl7.org/fhir/SearchParameter/OperationDefinition-code}', 'context {http://hl7.org/fhir/SearchParameter/conformance-context}',
       'context-quantity {http://hl7.org/fhir/SearchParameter/conformance-context-quantity}', 'context-type {http://hl7.org/fhir/SearchParameter/conformance-context-type}', 'context-type-quantity {http://hl7.org/fhir/SearchParameter/conformance-context-type-quantity}', 'context-type-value {http://hl7.org/fhir/SearchParameter/conformance-context-type-value}', 'date {http://hl7.org/fhir/SearchParameter/conformance-date}', 'description {http://hl7.org/fhir/SearchParameter/conformance-description}', 'input-profile {http://hl7.org/fhir/SearchParameter/OperationDefinition-input-profile}', 'instance {http://hl7.org/fhir/SearchParameter/OperationDefinition-instance}', 'jurisdiction {http://hl7.org/fhir/SearchParameter/conformance-jurisdiction}', 'kind {http://hl7.org/fhir/SearchParameter/OperationDefinition-kind}', 'name {http://hl7.org/fhir/SearchParameter/conformance-name}', 'output-profile {http://hl7.org/fhir/SearchParameter/OperationDefinition-output-profile}',
       'publisher {http://hl7.org/fhir/SearchParameter/conformance-publisher}', 'status {http://hl7.org/fhir/SearchParameter/conformance-status}', 'system {http://hl7.org/fhir/SearchParameter/OperationDefinition-system}', 'title {http://hl7.org/fhir/SearchParameter/conformance-title}', 'type {http://hl7.org/fhir/SearchParameter/OperationDefinition-type}', 'url {http://hl7.org/fhir/SearchParameter/conformance-url}', 'version {http://hl7.org/fhir/SearchParameter/conformance-version}');
{$ENDIF}
{$IFDEF FHIR_OPERATIONOUTCOME}
  CODES_TSearchParamsOperationOutcome : Array[TSearchParamsOperationOutcome] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}');
{$ENDIF}
{$IFDEF FHIR_ORGANIZATION}
  CODES_TSearchParamsOrganization : Array[TSearchParamsOrganization] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'active {http://hl7.org/fhir/SearchParameter/Organization-active}', 'address {http://hl7.org/fhir/SearchParameter/Organization-address}', 'address-city {http://hl7.org/fhir/SearchParameter/Organization-address-city}',
       'address-country {http://hl7.org/fhir/SearchParameter/Organization-address-country}', 'address-postalcode {http://hl7.org/fhir/SearchParameter/Organization-address-postalcode}', 'address-state {http://hl7.org/fhir/SearchParameter/Organization-address-state}', 'address-use {http://hl7.org/fhir/SearchParameter/Organization-address-use}', 'endpoint {http://hl7.org/fhir/SearchParameter/Organization-endpoint}', 'identifier {http://hl7.org/fhir/SearchParameter/Organization-identifier}', 'name {http://hl7.org/fhir/SearchParameter/Organization-name}', 'partof {http://hl7.org/fhir/SearchParameter/Organization-partof}', 'phonetic {http://hl7.org/fhir/SearchParameter/Organization-phonetic}', 'type {http://hl7.org/fhir/SearchParameter/Organization-type}');
{$ENDIF}
{$IFDEF FHIR_ORGANIZATIONAFFILIATION}
  CODES_TSearchParamsOrganizationAffiliation : Array[TSearchParamsOrganizationAffiliation] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'active {http://hl7.org/fhir/SearchParameter/OrganizationAffiliation-active}', 'date {http://hl7.org/fhir/SearchParameter/OrganizationAffiliation-date}', 'email {http://hl7.org/fhir/SearchParameter/OrganizationAffiliation-email}',
       'endpoint {http://hl7.org/fhir/SearchParameter/OrganizationAffiliation-endpoint}', 'identifier {http://hl7.org/fhir/SearchParameter/OrganizationAffiliation-identifier}', 'location {http://hl7.org/fhir/SearchParameter/OrganizationAffiliation-location}', 'network {http://hl7.org/fhir/SearchParameter/OrganizationAffiliation-network}', 'participating-organization {http://hl7.org/fhir/SearchParameter/OrganizationAffiliation-participating-organization}', 'phone {http://hl7.org/fhir/SearchParameter/OrganizationAffiliation-phone}', 'primary-organization {http://hl7.org/fhir/SearchParameter/OrganizationAffiliation-primary-organization}', 'role {http://hl7.org/fhir/SearchParameter/OrganizationAffiliation-role}', 'service {http://hl7.org/fhir/SearchParameter/OrganizationAffiliation-service}', 'specialty {http://hl7.org/fhir/SearchParameter/OrganizationAffiliation-specialty}', 'telecom {http://hl7.org/fhir/SearchParameter/OrganizationAffiliation-telecom}');
{$ENDIF}
{$IFDEF FHIR_PACKAGEDPRODUCTDEFINITION}
  CODES_TSearchParamsPackagedProductDefinition : Array[TSearchParamsPackagedProductDefinition] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'biological {http://hl7.org/fhir/SearchParameter/PackagedProductDefinition-biological}', 'contained-item {http://hl7.org/fhir/SearchParameter/PackagedProductDefinition-contained-item}', 'device {http://hl7.org/fhir/SearchParameter/PackagedProductDefinition-device}',
       'identifier {http://hl7.org/fhir/SearchParameter/PackagedProductDefinition-identifier}', 'manufactured-item {http://hl7.org/fhir/SearchParameter/PackagedProductDefinition-manufactured-item}', 'medication {http://hl7.org/fhir/SearchParameter/PackagedProductDefinition-medication}', 'name {http://hl7.org/fhir/SearchParameter/PackagedProductDefinition-name}', 'nutrition {http://hl7.org/fhir/SearchParameter/PackagedProductDefinition-nutrition}', 'status {http://hl7.org/fhir/SearchParameter/PackagedProductDefinition-status}', 'subject {http://hl7.org/fhir/SearchParameter/PackagedProductDefinition-subject}');
{$ENDIF}
{$IFDEF FHIR_PARAMETERS}
  CODES_TSearchParamsParameters : Array[TSearchParamsParameters] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}');
{$ENDIF}
{$IFDEF FHIR_PATIENT}
  CODES_TSearchParamsPatient : Array[TSearchParamsPatient] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'active {http://hl7.org/fhir/SearchParameter/Patient-active}', 'address {http://hl7.org/fhir/SearchParameter/individual-address}', 'address-city {http://hl7.org/fhir/SearchParameter/individual-address-city}', 'address-country {http://hl7.org/fhir/SearchParameter/individual-address-country}',
       'address-postalcode {http://hl7.org/fhir/SearchParameter/individual-address-postalcode}', 'address-state {http://hl7.org/fhir/SearchParameter/individual-address-state}', 'address-use {http://hl7.org/fhir/SearchParameter/individual-address-use}', 'age {http://hl7.org/fhir/SearchParameter/patient-extensions-Patient-age}', 'birthOrderBoolean {http://hl7.org/fhir/SearchParameter/patient-extensions-Patient-birthOrderBoolean}', 'birthdate {http://hl7.org/fhir/SearchParameter/individual-birthdate}', 'death-date {http://hl7.org/fhir/SearchParameter/Patient-death-date}', 'deceased {http://hl7.org/fhir/SearchParameter/Patient-deceased}', 'email {http://hl7.org/fhir/SearchParameter/individual-email}', 'family {http://hl7.org/fhir/SearchParameter/individual-family}', 'gender {http://hl7.org/fhir/SearchParameter/individual-gender}', 'general-practitioner {http://hl7.org/fhir/SearchParameter/Patient-general-practitioner}', 'given {http://hl7.org/fhir/SearchParameter/individual-given}',
       'identifier {http://hl7.org/fhir/SearchParameter/Patient-identifier}', 'language {http://hl7.org/fhir/SearchParameter/Patient-language}', 'link {http://hl7.org/fhir/SearchParameter/Patient-link}', 'mothersMaidenName {http://hl7.org/fhir/SearchParameter/patient-extensions-Patient-mothersMaidenName}', 'name {http://hl7.org/fhir/SearchParameter/Patient-name}', 'organization {http://hl7.org/fhir/SearchParameter/Patient-organization}', 'phone {http://hl7.org/fhir/SearchParameter/individual-phone}', 'phonetic {http://hl7.org/fhir/SearchParameter/individual-phonetic}', 'telecom {http://hl7.org/fhir/SearchParameter/individual-telecom}');
{$ENDIF}
{$IFDEF FHIR_PAYMENTNOTICE}
  CODES_TSearchParamsPaymentNotice : Array[TSearchParamsPaymentNotice] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'created {http://hl7.org/fhir/SearchParameter/PaymentNotice-created}', 'identifier {http://hl7.org/fhir/SearchParameter/PaymentNotice-identifier}', 'payment-status {http://hl7.org/fhir/SearchParameter/PaymentNotice-payment-status}',
       'provider {http://hl7.org/fhir/SearchParameter/PaymentNotice-provider}', 'request {http://hl7.org/fhir/SearchParameter/PaymentNotice-request}', 'response {http://hl7.org/fhir/SearchParameter/PaymentNotice-response}', 'status {http://hl7.org/fhir/SearchParameter/PaymentNotice-status}');
{$ENDIF}
{$IFDEF FHIR_PAYMENTRECONCILIATION}
  CODES_TSearchParamsPaymentReconciliation : Array[TSearchParamsPaymentReconciliation] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'created {http://hl7.org/fhir/SearchParameter/PaymentReconciliation-created}', 'disposition {http://hl7.org/fhir/SearchParameter/PaymentReconciliation-disposition}', 'identifier {http://hl7.org/fhir/SearchParameter/PaymentReconciliation-identifier}',
       'outcome {http://hl7.org/fhir/SearchParameter/PaymentReconciliation-outcome}', 'payment-issuer {http://hl7.org/fhir/SearchParameter/PaymentReconciliation-payment-issuer}', 'request {http://hl7.org/fhir/SearchParameter/PaymentReconciliation-request}', 'requestor {http://hl7.org/fhir/SearchParameter/PaymentReconciliation-requestor}', 'status {http://hl7.org/fhir/SearchParameter/PaymentReconciliation-status}');
{$ENDIF}
{$IFDEF FHIR_PERMISSION}
  CODES_TSearchParamsPermission : Array[TSearchParamsPermission] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'status {http://hl7.org/fhir/SearchParameter/Permission-status}');
{$ENDIF}
{$IFDEF FHIR_PERSON}
  CODES_TSearchParamsPerson : Array[TSearchParamsPerson] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'address {http://hl7.org/fhir/SearchParameter/individual-address}', 'address-city {http://hl7.org/fhir/SearchParameter/individual-address-city}', 'address-country {http://hl7.org/fhir/SearchParameter/individual-address-country}',
       'address-postalcode {http://hl7.org/fhir/SearchParameter/individual-address-postalcode}', 'address-state {http://hl7.org/fhir/SearchParameter/individual-address-state}', 'address-use {http://hl7.org/fhir/SearchParameter/individual-address-use}', 'birthdate {http://hl7.org/fhir/SearchParameter/individual-birthdate}', 'email {http://hl7.org/fhir/SearchParameter/individual-email}', 'gender {http://hl7.org/fhir/SearchParameter/individual-gender}', 'identifier {http://hl7.org/fhir/SearchParameter/Person-identifier}', 'link {http://hl7.org/fhir/SearchParameter/Person-link}', 'name {http://hl7.org/fhir/SearchParameter/Person-name}', 'organization {http://hl7.org/fhir/SearchParameter/Person-organization}', 'patient {http://hl7.org/fhir/SearchParameter/Person-patient}', 'phone {http://hl7.org/fhir/SearchParameter/individual-phone}', 'phonetic {http://hl7.org/fhir/SearchParameter/individual-phonetic}', 'practitioner {http://hl7.org/fhir/SearchParameter/Person-practitioner}',
       'relatedperson {http://hl7.org/fhir/SearchParameter/Person-relatedperson}', 'telecom {http://hl7.org/fhir/SearchParameter/individual-telecom}');
{$ENDIF}
{$IFDEF FHIR_PLANDEFINITION}
  CODES_TSearchParamsPlanDefinition : Array[TSearchParamsPlanDefinition] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'composed-of {http://hl7.org/fhir/SearchParameter/PlanDefinition-composed-of}', 'context {http://hl7.org/fhir/SearchParameter/PlanDefinition-context}', 'context-quantity {http://hl7.org/fhir/SearchParameter/PlanDefinition-context-quantity}',
       'context-type {http://hl7.org/fhir/SearchParameter/PlanDefinition-context-type}', 'context-type-quantity {http://hl7.org/fhir/SearchParameter/PlanDefinition-context-type-quantity}', 'context-type-value {http://hl7.org/fhir/SearchParameter/PlanDefinition-context-type-value}', 'date {http://hl7.org/fhir/SearchParameter/PlanDefinition-date}', 'definition {http://hl7.org/fhir/SearchParameter/PlanDefinition-definition}', 'depends-on {http://hl7.org/fhir/SearchParameter/PlanDefinition-depends-on}', 'derived-from {http://hl7.org/fhir/SearchParameter/PlanDefinition-derived-from}', 'description {http://hl7.org/fhir/SearchParameter/PlanDefinition-description}', 'effective {http://hl7.org/fhir/SearchParameter/PlanDefinition-effective}', 'identifier {http://hl7.org/fhir/SearchParameter/PlanDefinition-identifier}', 'jurisdiction {http://hl7.org/fhir/SearchParameter/PlanDefinition-jurisdiction}', 'name {http://hl7.org/fhir/SearchParameter/PlanDefinition-name}',
       'predecessor {http://hl7.org/fhir/SearchParameter/PlanDefinition-predecessor}', 'publisher {http://hl7.org/fhir/SearchParameter/PlanDefinition-publisher}', 'status {http://hl7.org/fhir/SearchParameter/PlanDefinition-status}', 'successor {http://hl7.org/fhir/SearchParameter/PlanDefinition-successor}', 'title {http://hl7.org/fhir/SearchParameter/PlanDefinition-title}', 'topic {http://hl7.org/fhir/SearchParameter/PlanDefinition-topic}', 'type {http://hl7.org/fhir/SearchParameter/PlanDefinition-type}', 'url {http://hl7.org/fhir/SearchParameter/PlanDefinition-url}', 'version {http://hl7.org/fhir/SearchParameter/PlanDefinition-version}');
{$ENDIF}
{$IFDEF FHIR_PRACTITIONER}
  CODES_TSearchParamsPractitioner : Array[TSearchParamsPractitioner] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'active {http://hl7.org/fhir/SearchParameter/Practitioner-active}', 'address {http://hl7.org/fhir/SearchParameter/individual-address}', 'address-city {http://hl7.org/fhir/SearchParameter/individual-address-city}',
       'address-country {http://hl7.org/fhir/SearchParameter/individual-address-country}', 'address-postalcode {http://hl7.org/fhir/SearchParameter/individual-address-postalcode}', 'address-state {http://hl7.org/fhir/SearchParameter/individual-address-state}', 'address-use {http://hl7.org/fhir/SearchParameter/individual-address-use}', 'communication {http://hl7.org/fhir/SearchParameter/Practitioner-communication}', 'email {http://hl7.org/fhir/SearchParameter/individual-email}', 'family {http://hl7.org/fhir/SearchParameter/individual-family}', 'gender {http://hl7.org/fhir/SearchParameter/individual-gender}', 'given {http://hl7.org/fhir/SearchParameter/individual-given}', 'identifier {http://hl7.org/fhir/SearchParameter/Practitioner-identifier}', 'name {http://hl7.org/fhir/SearchParameter/Practitioner-name}', 'phone {http://hl7.org/fhir/SearchParameter/individual-phone}', 'phonetic {http://hl7.org/fhir/SearchParameter/individual-phonetic}',
       'telecom {http://hl7.org/fhir/SearchParameter/individual-telecom}');
{$ENDIF}
{$IFDEF FHIR_PRACTITIONERROLE}
  CODES_TSearchParamsPractitionerRole : Array[TSearchParamsPractitionerRole] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'active {http://hl7.org/fhir/SearchParameter/PractitionerRole-active}', 'date {http://hl7.org/fhir/SearchParameter/PractitionerRole-date}', 'email {http://hl7.org/fhir/SearchParameter/individual-email}', 'endpoint {http://hl7.org/fhir/SearchParameter/PractitionerRole-endpoint}',
       'identifier {http://hl7.org/fhir/SearchParameter/PractitionerRole-identifier}', 'location {http://hl7.org/fhir/SearchParameter/PractitionerRole-location}', 'organization {http://hl7.org/fhir/SearchParameter/PractitionerRole-organization}', 'phone {http://hl7.org/fhir/SearchParameter/individual-phone}', 'practitioner {http://hl7.org/fhir/SearchParameter/PractitionerRole-practitioner}', 'role {http://hl7.org/fhir/SearchParameter/PractitionerRole-role}', 'service {http://hl7.org/fhir/SearchParameter/PractitionerRole-service}', 'specialty {http://hl7.org/fhir/SearchParameter/PractitionerRole-specialty}', 'telecom {http://hl7.org/fhir/SearchParameter/individual-telecom}');
{$ENDIF}
{$IFDEF FHIR_PROCEDURE}
  CODES_TSearchParamsProcedure : Array[TSearchParamsProcedure] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'based-on {http://hl7.org/fhir/SearchParameter/Procedure-based-on}', 'category {http://hl7.org/fhir/SearchParameter/Procedure-category}', 'code {http://hl7.org/fhir/SearchParameter/clinical-code}', 'date {http://hl7.org/fhir/SearchParameter/clinical-date}',
       'encounter {http://hl7.org/fhir/SearchParameter/clinical-encounter}', 'identifier {http://hl7.org/fhir/SearchParameter/clinical-identifier}', 'instantiates-canonical {http://hl7.org/fhir/SearchParameter/Procedure-instantiates-canonical}', 'instantiates-uri {http://hl7.org/fhir/SearchParameter/Procedure-instantiates-uri}', 'location {http://hl7.org/fhir/SearchParameter/Procedure-location}', 'part-of {http://hl7.org/fhir/SearchParameter/Procedure-part-of}', 'patient {http://hl7.org/fhir/SearchParameter/clinical-patient}', 'performer {http://hl7.org/fhir/SearchParameter/Procedure-performer}', 'reason-code {http://hl7.org/fhir/SearchParameter/Procedure-reason-code}', 'reason-reference {http://hl7.org/fhir/SearchParameter/Procedure-reason-reference}', 'status {http://hl7.org/fhir/SearchParameter/Procedure-status}', 'subject {http://hl7.org/fhir/SearchParameter/Procedure-subject}');
{$ENDIF}
{$IFDEF FHIR_PROVENANCE}
  CODES_TSearchParamsProvenance : Array[TSearchParamsProvenance] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'activity {http://hl7.org/fhir/SearchParameter/Provenance-activity}', 'agent {http://hl7.org/fhir/SearchParameter/Provenance-agent}', 'agent-role {http://hl7.org/fhir/SearchParameter/Provenance-agent-role}', 'agent-type {http://hl7.org/fhir/SearchParameter/Provenance-agent-type}',
       'entity {http://hl7.org/fhir/SearchParameter/Provenance-entity}', 'location {http://hl7.org/fhir/SearchParameter/Provenance-location}', 'patient {http://hl7.org/fhir/SearchParameter/Provenance-patient}', 'recorded {http://hl7.org/fhir/SearchParameter/Provenance-recorded}', 'signature-type {http://hl7.org/fhir/SearchParameter/Provenance-signature-type}', 'target {http://hl7.org/fhir/SearchParameter/Provenance-target}', 'when {http://hl7.org/fhir/SearchParameter/Provenance-when}');
{$ENDIF}
{$IFDEF FHIR_QUESTIONNAIRE}
  CODES_TSearchParamsQuestionnaire : Array[TSearchParamsQuestionnaire] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'code {http://hl7.org/fhir/SearchParameter/Questionnaire-code}', 'context {http://hl7.org/fhir/SearchParameter/Questionnaire-context}', 'context-quantity {http://hl7.org/fhir/SearchParameter/Questionnaire-context-quantity}',
       'context-type {http://hl7.org/fhir/SearchParameter/Questionnaire-context-type}', 'context-type-quantity {http://hl7.org/fhir/SearchParameter/Questionnaire-context-type-quantity}', 'context-type-value {http://hl7.org/fhir/SearchParameter/Questionnaire-context-type-value}', 'date {http://hl7.org/fhir/SearchParameter/Questionnaire-date}', 'definition {http://hl7.org/fhir/SearchParameter/Questionnaire-definition}', 'description {http://hl7.org/fhir/SearchParameter/Questionnaire-description}', 'effective {http://hl7.org/fhir/SearchParameter/Questionnaire-effective}', 'identifier {http://hl7.org/fhir/SearchParameter/Questionnaire-identifier}', 'jurisdiction {http://hl7.org/fhir/SearchParameter/Questionnaire-jurisdiction}', 'name {http://hl7.org/fhir/SearchParameter/Questionnaire-name}', 'publisher {http://hl7.org/fhir/SearchParameter/Questionnaire-publisher}', 'status {http://hl7.org/fhir/SearchParameter/Questionnaire-status}',
       'subject-type {http://hl7.org/fhir/SearchParameter/Questionnaire-subject-type}', 'title {http://hl7.org/fhir/SearchParameter/Questionnaire-title}', 'url {http://hl7.org/fhir/SearchParameter/Questionnaire-url}', 'version {http://hl7.org/fhir/SearchParameter/Questionnaire-version}');
{$ENDIF}
{$IFDEF FHIR_QUESTIONNAIRERESPONSE}
  CODES_TSearchParamsQuestionnaireResponse : Array[TSearchParamsQuestionnaireResponse] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'author {http://hl7.org/fhir/SearchParameter/QuestionnaireResponse-author}', 'authored {http://hl7.org/fhir/SearchParameter/QuestionnaireResponse-authored}', 'based-on {http://hl7.org/fhir/SearchParameter/QuestionnaireResponse-based-on}',
       'encounter {http://hl7.org/fhir/SearchParameter/QuestionnaireResponse-encounter}', 'identifier {http://hl7.org/fhir/SearchParameter/QuestionnaireResponse-identifier}', 'item-subject {http://hl7.org/fhir/SearchParameter/questionnaireresponse-extensions-QuestionnaireResponse-item-subject}', 'part-of {http://hl7.org/fhir/SearchParameter/QuestionnaireResponse-part-of}', 'patient {http://hl7.org/fhir/SearchParameter/QuestionnaireResponse-patient}', 'questionnaire {http://hl7.org/fhir/SearchParameter/QuestionnaireResponse-questionnaire}', 'source {http://hl7.org/fhir/SearchParameter/QuestionnaireResponse-source}', 'status {http://hl7.org/fhir/SearchParameter/QuestionnaireResponse-status}', 'subject {http://hl7.org/fhir/SearchParameter/QuestionnaireResponse-subject}');
{$ENDIF}
{$IFDEF FHIR_REGULATEDAUTHORIZATION}
  CODES_TSearchParamsRegulatedAuthorization : Array[TSearchParamsRegulatedAuthorization] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'case {http://hl7.org/fhir/SearchParameter/RegulatedAuthorization-case}', 'case-type {http://hl7.org/fhir/SearchParameter/RegulatedAuthorization-case-type}', 'holder {http://hl7.org/fhir/SearchParameter/RegulatedAuthorization-holder}',
       'identifier {http://hl7.org/fhir/SearchParameter/RegulatedAuthorization-identifier}', 'region {http://hl7.org/fhir/SearchParameter/RegulatedAuthorization-region}', 'status {http://hl7.org/fhir/SearchParameter/RegulatedAuthorization-status}', 'subject {http://hl7.org/fhir/SearchParameter/RegulatedAuthorization-subject}');
{$ENDIF}
{$IFDEF FHIR_RELATEDPERSON}
  CODES_TSearchParamsRelatedPerson : Array[TSearchParamsRelatedPerson] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'active {http://hl7.org/fhir/SearchParameter/RelatedPerson-active}', 'address {http://hl7.org/fhir/SearchParameter/individual-address}', 'address-city {http://hl7.org/fhir/SearchParameter/individual-address-city}',
       'address-country {http://hl7.org/fhir/SearchParameter/individual-address-country}', 'address-postalcode {http://hl7.org/fhir/SearchParameter/individual-address-postalcode}', 'address-state {http://hl7.org/fhir/SearchParameter/individual-address-state}', 'address-use {http://hl7.org/fhir/SearchParameter/individual-address-use}', 'birthdate {http://hl7.org/fhir/SearchParameter/individual-birthdate}', 'email {http://hl7.org/fhir/SearchParameter/individual-email}', 'gender {http://hl7.org/fhir/SearchParameter/individual-gender}', 'identifier {http://hl7.org/fhir/SearchParameter/RelatedPerson-identifier}', 'name {http://hl7.org/fhir/SearchParameter/RelatedPerson-name}', 'patient {http://hl7.org/fhir/SearchParameter/RelatedPerson-patient}', 'phone {http://hl7.org/fhir/SearchParameter/individual-phone}', 'phonetic {http://hl7.org/fhir/SearchParameter/individual-phonetic}', 'relationship {http://hl7.org/fhir/SearchParameter/RelatedPerson-relationship}',
       'telecom {http://hl7.org/fhir/SearchParameter/individual-telecom}');
{$ENDIF}
{$IFDEF FHIR_REQUESTGROUP}
  CODES_TSearchParamsRequestGroup : Array[TSearchParamsRequestGroup] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'author {http://hl7.org/fhir/SearchParameter/RequestGroup-author}', 'authored {http://hl7.org/fhir/SearchParameter/RequestGroup-authored}', 'code {http://hl7.org/fhir/SearchParameter/RequestGroup-code}', 'encounter {http://hl7.org/fhir/SearchParameter/RequestGroup-encounter}',
       'group-identifier {http://hl7.org/fhir/SearchParameter/RequestGroup-group-identifier}', 'identifier {http://hl7.org/fhir/SearchParameter/RequestGroup-identifier}', 'instantiates-canonical {http://hl7.org/fhir/SearchParameter/RequestGroup-instantiates-canonical}', 'instantiates-uri {http://hl7.org/fhir/SearchParameter/RequestGroup-instantiates-uri}', 'intent {http://hl7.org/fhir/SearchParameter/RequestGroup-intent}', 'participant {http://hl7.org/fhir/SearchParameter/RequestGroup-participant}', 'patient {http://hl7.org/fhir/SearchParameter/RequestGroup-patient}', 'priority {http://hl7.org/fhir/SearchParameter/RequestGroup-priority}', 'status {http://hl7.org/fhir/SearchParameter/RequestGroup-status}', 'subject {http://hl7.org/fhir/SearchParameter/RequestGroup-subject}');
{$ENDIF}
{$IFDEF FHIR_RESEARCHSTUDY}
  CODES_TSearchParamsResearchStudy : Array[TSearchParamsResearchStudy] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'category {http://hl7.org/fhir/SearchParameter/ResearchStudy-category}', 'date {http://hl7.org/fhir/SearchParameter/ResearchStudy-date}', 'focus {http://hl7.org/fhir/SearchParameter/ResearchStudy-focus}', 'identifier {http://hl7.org/fhir/SearchParameter/ResearchStudy-identifier}',
       'keyword {http://hl7.org/fhir/SearchParameter/ResearchStudy-keyword}', 'location {http://hl7.org/fhir/SearchParameter/ResearchStudy-location}', 'partof {http://hl7.org/fhir/SearchParameter/ResearchStudy-partof}', 'principalinvestigator {http://hl7.org/fhir/SearchParameter/ResearchStudy-principalinvestigator}', 'protocol {http://hl7.org/fhir/SearchParameter/ResearchStudy-protocol}', 'site {http://hl7.org/fhir/SearchParameter/ResearchStudy-site}', 'sponsor {http://hl7.org/fhir/SearchParameter/ResearchStudy-sponsor}', 'status {http://hl7.org/fhir/SearchParameter/ResearchStudy-status}', 'title {http://hl7.org/fhir/SearchParameter/ResearchStudy-title}');
{$ENDIF}
{$IFDEF FHIR_RESEARCHSUBJECT}
  CODES_TSearchParamsResearchSubject : Array[TSearchParamsResearchSubject] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'date {http://hl7.org/fhir/SearchParameter/ResearchSubject-date}', 'identifier {http://hl7.org/fhir/SearchParameter/ResearchSubject-identifier}', 'individual {http://hl7.org/fhir/SearchParameter/ResearchSubject-individual}',
       'patient {http://hl7.org/fhir/SearchParameter/ResearchSubject-patient}', 'state {http://hl7.org/fhir/SearchParameter/ResearchSubject-state}', 'status {http://hl7.org/fhir/SearchParameter/ResearchSubject-status}', 'study {http://hl7.org/fhir/SearchParameter/ResearchSubject-study}');
{$ENDIF}
{$IFDEF FHIR_RISKASSESSMENT}
  CODES_TSearchParamsRiskAssessment : Array[TSearchParamsRiskAssessment] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'condition {http://hl7.org/fhir/SearchParameter/RiskAssessment-condition}', 'date {http://hl7.org/fhir/SearchParameter/clinical-date}', 'encounter {http://hl7.org/fhir/SearchParameter/clinical-encounter}', 'identifier {http://hl7.org/fhir/SearchParameter/clinical-identifier}',
       'method {http://hl7.org/fhir/SearchParameter/RiskAssessment-method}', 'patient {http://hl7.org/fhir/SearchParameter/clinical-patient}', 'performer {http://hl7.org/fhir/SearchParameter/RiskAssessment-performer}', 'probability {http://hl7.org/fhir/SearchParameter/RiskAssessment-probability}', 'risk {http://hl7.org/fhir/SearchParameter/RiskAssessment-risk}', 'subject {http://hl7.org/fhir/SearchParameter/RiskAssessment-subject}');
{$ENDIF}
{$IFDEF FHIR_SCHEDULE}
  CODES_TSearchParamsSchedule : Array[TSearchParamsSchedule] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'active {http://hl7.org/fhir/SearchParameter/Schedule-active}', 'actor {http://hl7.org/fhir/SearchParameter/Schedule-actor}', 'date {http://hl7.org/fhir/SearchParameter/Schedule-date}', 'identifier {http://hl7.org/fhir/SearchParameter/Schedule-identifier}',
       'service-category {http://hl7.org/fhir/SearchParameter/Schedule-service-category}', 'service-type {http://hl7.org/fhir/SearchParameter/Schedule-service-type}', 'specialty {http://hl7.org/fhir/SearchParameter/Schedule-specialty}');
{$ENDIF}
{$IFDEF FHIR_SEARCHPARAMETER}
  CODES_TSearchParamsSearchParameter : Array[TSearchParamsSearchParameter] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'base {http://hl7.org/fhir/SearchParameter/SearchParameter-base}', 'code {http://hl7.org/fhir/SearchParameter/SearchParameter-code}', 'component {http://hl7.org/fhir/SearchParameter/SearchParameter-component}', 'context {http://hl7.org/fhir/SearchParameter/conformance-context}',
       'context-quantity {http://hl7.org/fhir/SearchParameter/conformance-context-quantity}', 'context-type {http://hl7.org/fhir/SearchParameter/conformance-context-type}', 'context-type-quantity {http://hl7.org/fhir/SearchParameter/conformance-context-type-quantity}', 'context-type-value {http://hl7.org/fhir/SearchParameter/conformance-context-type-value}', 'date {http://hl7.org/fhir/SearchParameter/conformance-date}', 'derived-from {http://hl7.org/fhir/SearchParameter/SearchParameter-derived-from}', 'description {http://hl7.org/fhir/SearchParameter/conformance-description}', 'jurisdiction {http://hl7.org/fhir/SearchParameter/conformance-jurisdiction}', 'name {http://hl7.org/fhir/SearchParameter/conformance-name}', 'publisher {http://hl7.org/fhir/SearchParameter/conformance-publisher}', 'status {http://hl7.org/fhir/SearchParameter/conformance-status}', 'target {http://hl7.org/fhir/SearchParameter/SearchParameter-target}', 'type {http://hl7.org/fhir/SearchParameter/SearchParameter-type}',
       'url {http://hl7.org/fhir/SearchParameter/conformance-url}', 'version {http://hl7.org/fhir/SearchParameter/conformance-version}');
{$ENDIF}
{$IFDEF FHIR_SERVICEREQUEST}
  CODES_TSearchParamsServiceRequest : Array[TSearchParamsServiceRequest] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'authored {http://hl7.org/fhir/SearchParameter/ServiceRequest-authored}', 'based-on {http://hl7.org/fhir/SearchParameter/ServiceRequest-based-on}', 'body-site {http://hl7.org/fhir/SearchParameter/ServiceRequest-body-site}',
       'category {http://hl7.org/fhir/SearchParameter/ServiceRequest-category}', 'code {http://hl7.org/fhir/SearchParameter/clinical-code}', 'encounter {http://hl7.org/fhir/SearchParameter/clinical-encounter}', 'identifier {http://hl7.org/fhir/SearchParameter/clinical-identifier}', 'instantiates-canonical {http://hl7.org/fhir/SearchParameter/ServiceRequest-instantiates-canonical}', 'instantiates-uri {http://hl7.org/fhir/SearchParameter/ServiceRequest-instantiates-uri}', 'intent {http://hl7.org/fhir/SearchParameter/ServiceRequest-intent}', 'occurrence {http://hl7.org/fhir/SearchParameter/ServiceRequest-occurrence}', 'patient {http://hl7.org/fhir/SearchParameter/clinical-patient}', 'performer {http://hl7.org/fhir/SearchParameter/ServiceRequest-performer}', 'performer-type {http://hl7.org/fhir/SearchParameter/ServiceRequest-performer-type}', 'priority {http://hl7.org/fhir/SearchParameter/ServiceRequest-priority}', 'replaces {http://hl7.org/fhir/SearchParameter/ServiceRequest-replaces}',
       'requester {http://hl7.org/fhir/SearchParameter/ServiceRequest-requester}', 'requisition {http://hl7.org/fhir/SearchParameter/ServiceRequest-requisition}', 'specimen {http://hl7.org/fhir/SearchParameter/ServiceRequest-specimen}', 'status {http://hl7.org/fhir/SearchParameter/ServiceRequest-status}', 'subject {http://hl7.org/fhir/SearchParameter/ServiceRequest-subject}');
{$ENDIF}
{$IFDEF FHIR_SLOT}
  CODES_TSearchParamsSlot : Array[TSearchParamsSlot] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'appointment-type {http://hl7.org/fhir/SearchParameter/Slot-appointment-type}', 'identifier {http://hl7.org/fhir/SearchParameter/Slot-identifier}', 'schedule {http://hl7.org/fhir/SearchParameter/Slot-schedule}', 'service-category {http://hl7.org/fhir/SearchParameter/Slot-service-category}',
       'service-type {http://hl7.org/fhir/SearchParameter/Slot-service-type}', 'specialty {http://hl7.org/fhir/SearchParameter/Slot-specialty}', 'start {http://hl7.org/fhir/SearchParameter/Slot-start}', 'status {http://hl7.org/fhir/SearchParameter/Slot-status}');
{$ENDIF}
{$IFDEF FHIR_SPECIMEN}
  CODES_TSearchParamsSpecimen : Array[TSearchParamsSpecimen] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'accession {http://hl7.org/fhir/SearchParameter/Specimen-accession}', 'bodysite {http://hl7.org/fhir/SearchParameter/Specimen-bodysite}', 'collected {http://hl7.org/fhir/SearchParameter/Specimen-collected}', 'collector {http://hl7.org/fhir/SearchParameter/Specimen-collector}',
       'container {http://hl7.org/fhir/SearchParameter/Specimen-container}', 'container-id {http://hl7.org/fhir/SearchParameter/Specimen-container-id}', 'identifier {http://hl7.org/fhir/SearchParameter/Specimen-identifier}', 'parent {http://hl7.org/fhir/SearchParameter/Specimen-parent}', 'patient {http://hl7.org/fhir/SearchParameter/Specimen-patient}', 'status {http://hl7.org/fhir/SearchParameter/Specimen-status}', 'subject {http://hl7.org/fhir/SearchParameter/Specimen-subject}', 'type {http://hl7.org/fhir/SearchParameter/Specimen-type}');
{$ENDIF}
{$IFDEF FHIR_SPECIMENDEFINITION}
  CODES_TSearchParamsSpecimenDefinition : Array[TSearchParamsSpecimenDefinition] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'container {http://hl7.org/fhir/SearchParameter/SpecimenDefinition-container}', 'experimental {http://hl7.org/fhir/SearchParameter/SpecimenDefinition-experimental}', 'identifier {http://hl7.org/fhir/SearchParameter/SpecimenDefinition-identifier}',
       'is-derived {http://hl7.org/fhir/SearchParameter/SpecimenDefinition-is-derived}', 'status {http://hl7.org/fhir/SearchParameter/SpecimenDefinition-status}', 'title {http://hl7.org/fhir/SearchParameter/SpecimenDefinition-title}', 'type {http://hl7.org/fhir/SearchParameter/SpecimenDefinition-type}', 'type-tested {http://hl7.org/fhir/SearchParameter/SpecimenDefinition-type-tested}', 'url {http://hl7.org/fhir/SearchParameter/SpecimenDefinition-url}');
{$ENDIF}
{$IFDEF FHIR_STRUCTUREDEFINITION}
  CODES_TSearchParamsStructureDefinition : Array[TSearchParamsStructureDefinition] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'abstract {http://hl7.org/fhir/SearchParameter/StructureDefinition-abstract}', 'base {http://hl7.org/fhir/SearchParameter/StructureDefinition-base}', 'base-path {http://hl7.org/fhir/SearchParameter/StructureDefinition-base-path}',
       'context {http://hl7.org/fhir/SearchParameter/conformance-context}', 'context-quantity {http://hl7.org/fhir/SearchParameter/conformance-context-quantity}', 'context-type {http://hl7.org/fhir/SearchParameter/conformance-context-type}', 'context-type-quantity {http://hl7.org/fhir/SearchParameter/conformance-context-type-quantity}', 'context-type-value {http://hl7.org/fhir/SearchParameter/conformance-context-type-value}', 'date {http://hl7.org/fhir/SearchParameter/conformance-date}', 'derivation {http://hl7.org/fhir/SearchParameter/StructureDefinition-derivation}', 'description {http://hl7.org/fhir/SearchParameter/conformance-description}', 'experimental {http://hl7.org/fhir/SearchParameter/StructureDefinition-experimental}', 'ext-context {http://hl7.org/fhir/SearchParameter/StructureDefinition-ext-context}', 'identifier {http://hl7.org/fhir/SearchParameter/conformance-identifier}', 'jurisdiction {http://hl7.org/fhir/SearchParameter/conformance-jurisdiction}',
       'keyword {http://hl7.org/fhir/SearchParameter/StructureDefinition-keyword}', 'kind {http://hl7.org/fhir/SearchParameter/StructureDefinition-kind}', 'name {http://hl7.org/fhir/SearchParameter/conformance-name}', 'path {http://hl7.org/fhir/SearchParameter/StructureDefinition-path}', 'publisher {http://hl7.org/fhir/SearchParameter/conformance-publisher}', 'status {http://hl7.org/fhir/SearchParameter/conformance-status}', 'title {http://hl7.org/fhir/SearchParameter/conformance-title}', 'type {http://hl7.org/fhir/SearchParameter/StructureDefinition-type}', 'url {http://hl7.org/fhir/SearchParameter/conformance-url}', 'valueset {http://hl7.org/fhir/SearchParameter/StructureDefinition-valueset}', 'version {http://hl7.org/fhir/SearchParameter/conformance-version}');
{$ENDIF}
{$IFDEF FHIR_STRUCTUREMAP}
  CODES_TSearchParamsStructureMap : Array[TSearchParamsStructureMap] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'context {http://hl7.org/fhir/SearchParameter/conformance-context}', 'context-quantity {http://hl7.org/fhir/SearchParameter/conformance-context-quantity}', 'context-type {http://hl7.org/fhir/SearchParameter/conformance-context-type}',
       'context-type-quantity {http://hl7.org/fhir/SearchParameter/conformance-context-type-quantity}', 'context-type-value {http://hl7.org/fhir/SearchParameter/conformance-context-type-value}', 'date {http://hl7.org/fhir/SearchParameter/conformance-date}', 'description {http://hl7.org/fhir/SearchParameter/conformance-description}', 'identifier {http://hl7.org/fhir/SearchParameter/conformance-identifier}', 'jurisdiction {http://hl7.org/fhir/SearchParameter/conformance-jurisdiction}', 'name {http://hl7.org/fhir/SearchParameter/conformance-name}', 'publisher {http://hl7.org/fhir/SearchParameter/conformance-publisher}', 'status {http://hl7.org/fhir/SearchParameter/conformance-status}', 'title {http://hl7.org/fhir/SearchParameter/conformance-title}', 'url {http://hl7.org/fhir/SearchParameter/conformance-url}', 'version {http://hl7.org/fhir/SearchParameter/conformance-version}');
{$ENDIF}
{$IFDEF FHIR_SUBSCRIPTION}
  CODES_TSearchParamsSubscription : Array[TSearchParamsSubscription] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'contact {http://hl7.org/fhir/SearchParameter/Subscription-contact}', 'payload {http://hl7.org/fhir/SearchParameter/Subscription-payload}', 'status {http://hl7.org/fhir/SearchParameter/Subscription-status}', 'type {http://hl7.org/fhir/SearchParameter/Subscription-type}',
       'url {http://hl7.org/fhir/SearchParameter/Subscription-url}');
{$ENDIF}
{$IFDEF FHIR_SUBSCRIPTIONSTATUS}
  CODES_TSearchParamsSubscriptionStatus : Array[TSearchParamsSubscriptionStatus] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}');
{$ENDIF}
{$IFDEF FHIR_SUBSCRIPTIONTOPIC}
  CODES_TSearchParamsSubscriptionTopic : Array[TSearchParamsSubscriptionTopic] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'date {http://hl7.org/fhir/SearchParameter/SubscriptionTopic-date}', 'derived-or-self {http://hl7.org/fhir/SearchParameter/SubscriptionTopic-derived-or-self}', 'identifier {http://hl7.org/fhir/SearchParameter/SubscriptionTopic-identifier}',
       'publisher {http://hl7.org/fhir/SearchParameter/SubscriptionTopic-publisher}', 'resource-type {http://hl7.org/fhir/SearchParameter/SubscriptionTopic-resource-type}', 'status {http://hl7.org/fhir/SearchParameter/SubscriptionTopic-status}', 'title {http://hl7.org/fhir/SearchParameter/SubscriptionTopic-title}', 'trigger-description {http://hl7.org/fhir/SearchParameter/SubscriptionTopic-trigger-description}', 'url {http://hl7.org/fhir/SearchParameter/SubscriptionTopic-url}', 'version {http://hl7.org/fhir/SearchParameter/SubscriptionTopic-version}');
{$ENDIF}
{$IFDEF FHIR_SUBSTANCE}
  CODES_TSearchParamsSubstance : Array[TSearchParamsSubstance] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'category {http://hl7.org/fhir/SearchParameter/Substance-category}', 'code {http://hl7.org/fhir/SearchParameter/Substance-code}', 'container-identifier {http://hl7.org/fhir/SearchParameter/Substance-container-identifier}', 'expiry {http://hl7.org/fhir/SearchParameter/Substance-expiry}',
       'identifier {http://hl7.org/fhir/SearchParameter/Substance-identifier}', 'quantity {http://hl7.org/fhir/SearchParameter/Substance-quantity}', 'status {http://hl7.org/fhir/SearchParameter/Substance-status}', 'substance-reference {http://hl7.org/fhir/SearchParameter/Substance-substance-reference}');
{$ENDIF}
{$IFDEF FHIR_SUBSTANCEDEFINITION}
  CODES_TSearchParamsSubstanceDefinition : Array[TSearchParamsSubstanceDefinition] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'category {http://hl7.org/fhir/SearchParameter/SubstanceDefinition-category}', 'code {http://hl7.org/fhir/SearchParameter/SubstanceDefinition-code}', 'domain {http://hl7.org/fhir/SearchParameter/SubstanceDefinition-domain}',
       'identifier {http://hl7.org/fhir/SearchParameter/SubstanceDefinition-identifier}', 'name {http://hl7.org/fhir/SearchParameter/SubstanceDefinition-name}');
{$ENDIF}
{$IFDEF FHIR_SUBSTANCENUCLEICACID}
  CODES_TSearchParamsSubstanceNucleicAcid : Array[TSearchParamsSubstanceNucleicAcid] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}');
{$ENDIF}
{$IFDEF FHIR_SUBSTANCEPOLYMER}
  CODES_TSearchParamsSubstancePolymer : Array[TSearchParamsSubstancePolymer] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}');
{$ENDIF}
{$IFDEF FHIR_SUBSTANCEPROTEIN}
  CODES_TSearchParamsSubstanceProtein : Array[TSearchParamsSubstanceProtein] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}');
{$ENDIF}
{$IFDEF FHIR_SUBSTANCEREFERENCEINFORMATION}
  CODES_TSearchParamsSubstanceReferenceInformation : Array[TSearchParamsSubstanceReferenceInformation] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}');
{$ENDIF}
{$IFDEF FHIR_SUBSTANCESOURCEMATERIAL}
  CODES_TSearchParamsSubstanceSourceMaterial : Array[TSearchParamsSubstanceSourceMaterial] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}');
{$ENDIF}
{$IFDEF FHIR_SUPPLYDELIVERY}
  CODES_TSearchParamsSupplyDelivery : Array[TSearchParamsSupplyDelivery] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'identifier {http://hl7.org/fhir/SearchParameter/clinical-identifier}', 'patient {http://hl7.org/fhir/SearchParameter/clinical-patient}', 'receiver {http://hl7.org/fhir/SearchParameter/SupplyDelivery-receiver}', 'status {http://hl7.org/fhir/SearchParameter/SupplyDelivery-status}',
       'supplier {http://hl7.org/fhir/SearchParameter/SupplyDelivery-supplier}');
{$ENDIF}
{$IFDEF FHIR_SUPPLYREQUEST}
  CODES_TSearchParamsSupplyRequest : Array[TSearchParamsSupplyRequest] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'category {http://hl7.org/fhir/SearchParameter/SupplyRequest-category}', 'date {http://hl7.org/fhir/SearchParameter/clinical-date}', 'identifier {http://hl7.org/fhir/SearchParameter/clinical-identifier}', 'requester {http://hl7.org/fhir/SearchParameter/SupplyRequest-requester}',
       'status {http://hl7.org/fhir/SearchParameter/SupplyRequest-status}', 'subject {http://hl7.org/fhir/SearchParameter/SupplyRequest-subject}', 'supplier {http://hl7.org/fhir/SearchParameter/SupplyRequest-supplier}');
{$ENDIF}
{$IFDEF FHIR_TASK}
  CODES_TSearchParamsTask : Array[TSearchParamsTask] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'authored-on {http://hl7.org/fhir/SearchParameter/Task-authored-on}', 'based-on {http://hl7.org/fhir/SearchParameter/Task-based-on}', 'business-status {http://hl7.org/fhir/SearchParameter/Task-business-status}', 'code {http://hl7.org/fhir/SearchParameter/Task-code}',
       'encounter {http://hl7.org/fhir/SearchParameter/Task-encounter}', 'focus {http://hl7.org/fhir/SearchParameter/Task-focus}', 'group-identifier {http://hl7.org/fhir/SearchParameter/Task-group-identifier}', 'identifier {http://hl7.org/fhir/SearchParameter/Task-identifier}', 'intent {http://hl7.org/fhir/SearchParameter/Task-intent}', 'modified {http://hl7.org/fhir/SearchParameter/Task-modified}', 'owner {http://hl7.org/fhir/SearchParameter/Task-owner}', 'part-of {http://hl7.org/fhir/SearchParameter/Task-part-of}', 'patient {http://hl7.org/fhir/SearchParameter/Task-patient}', 'performer {http://hl7.org/fhir/SearchParameter/Task-performer}', 'period {http://hl7.org/fhir/SearchParameter/Task-period}', 'priority {http://hl7.org/fhir/SearchParameter/Task-priority}', 'requester {http://hl7.org/fhir/SearchParameter/Task-requester}', 'status {http://hl7.org/fhir/SearchParameter/Task-status}', 'subject {http://hl7.org/fhir/SearchParameter/Task-subject}');
{$ENDIF}
{$IFDEF FHIR_TERMINOLOGYCAPABILITIES}
  CODES_TSearchParamsTerminologyCapabilities : Array[TSearchParamsTerminologyCapabilities] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'context {http://hl7.org/fhir/SearchParameter/conformance-context}', 'context-quantity {http://hl7.org/fhir/SearchParameter/conformance-context-quantity}', 'context-type {http://hl7.org/fhir/SearchParameter/conformance-context-type}',
       'context-type-quantity {http://hl7.org/fhir/SearchParameter/conformance-context-type-quantity}', 'context-type-value {http://hl7.org/fhir/SearchParameter/conformance-context-type-value}', 'date {http://hl7.org/fhir/SearchParameter/conformance-date}', 'description {http://hl7.org/fhir/SearchParameter/conformance-description}', 'identifier {http://hl7.org/fhir/SearchParameter/conformance-identifier}', 'jurisdiction {http://hl7.org/fhir/SearchParameter/conformance-jurisdiction}', 'name {http://hl7.org/fhir/SearchParameter/conformance-name}', 'publisher {http://hl7.org/fhir/SearchParameter/conformance-publisher}', 'status {http://hl7.org/fhir/SearchParameter/conformance-status}', 'title {http://hl7.org/fhir/SearchParameter/conformance-title}', 'url {http://hl7.org/fhir/SearchParameter/conformance-url}', 'version {http://hl7.org/fhir/SearchParameter/conformance-version}');
{$ENDIF}
{$IFDEF FHIR_TESTREPORT}
  CODES_TSearchParamsTestReport : Array[TSearchParamsTestReport] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'identifier {http://hl7.org/fhir/SearchParameter/TestReport-identifier}', 'issued {http://hl7.org/fhir/SearchParameter/TestReport-issued}', 'participant {http://hl7.org/fhir/SearchParameter/TestReport-participant}', 'result {http://hl7.org/fhir/SearchParameter/TestReport-result}',
       'tester {http://hl7.org/fhir/SearchParameter/TestReport-tester}', 'testscript {http://hl7.org/fhir/SearchParameter/TestReport-testscript}');
{$ENDIF}
{$IFDEF FHIR_TESTSCRIPT}
  CODES_TSearchParamsTestScript : Array[TSearchParamsTestScript] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'context {http://hl7.org/fhir/SearchParameter/TestScript-context}', 'context-quantity {http://hl7.org/fhir/SearchParameter/TestScript-context-quantity}', 'context-type {http://hl7.org/fhir/SearchParameter/TestScript-context-type}',
       'context-type-quantity {http://hl7.org/fhir/SearchParameter/TestScript-context-type-quantity}', 'context-type-value {http://hl7.org/fhir/SearchParameter/TestScript-context-type-value}', 'date {http://hl7.org/fhir/SearchParameter/TestScript-date}', 'description {http://hl7.org/fhir/SearchParameter/TestScript-description}', 'identifier {http://hl7.org/fhir/SearchParameter/TestScript-identifier}', 'jurisdiction {http://hl7.org/fhir/SearchParameter/TestScript-jurisdiction}', 'name {http://hl7.org/fhir/SearchParameter/TestScript-name}', 'publisher {http://hl7.org/fhir/SearchParameter/TestScript-publisher}', 'status {http://hl7.org/fhir/SearchParameter/TestScript-status}', 'testscript-capability {http://hl7.org/fhir/SearchParameter/TestScript-testscript-capability}', 'title {http://hl7.org/fhir/SearchParameter/TestScript-title}', 'url {http://hl7.org/fhir/SearchParameter/TestScript-url}', 'version {http://hl7.org/fhir/SearchParameter/TestScript-version}');
{$ENDIF}
{$IFDEF FHIR_VALUESET}
  CODES_TSearchParamsValueSet : Array[TSearchParamsValueSet] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'code {http://hl7.org/fhir/SearchParameter/ValueSet-code}', 'context {http://hl7.org/fhir/SearchParameter/conformance-context}', 'context-quantity {http://hl7.org/fhir/SearchParameter/conformance-context-quantity}', 'context-type {http://hl7.org/fhir/SearchParameter/conformance-context-type}',
       'context-type-quantity {http://hl7.org/fhir/SearchParameter/conformance-context-type-quantity}', 'context-type-value {http://hl7.org/fhir/SearchParameter/conformance-context-type-value}', 'date {http://hl7.org/fhir/SearchParameter/conformance-date}', 'description {http://hl7.org/fhir/SearchParameter/conformance-description}', 'expansion {http://hl7.org/fhir/SearchParameter/ValueSet-expansion}', 'identifier {http://hl7.org/fhir/SearchParameter/conformance-identifier}', 'jurisdiction {http://hl7.org/fhir/SearchParameter/conformance-jurisdiction}', 'name {http://hl7.org/fhir/SearchParameter/conformance-name}', 'publisher {http://hl7.org/fhir/SearchParameter/conformance-publisher}', 'reference {http://hl7.org/fhir/SearchParameter/ValueSet-reference}', 'status {http://hl7.org/fhir/SearchParameter/conformance-status}', 'title {http://hl7.org/fhir/SearchParameter/conformance-title}', 'url {http://hl7.org/fhir/SearchParameter/conformance-url}',
       'version {http://hl7.org/fhir/SearchParameter/conformance-version}');
{$ENDIF}
{$IFDEF FHIR_VERIFICATIONRESULT}
  CODES_TSearchParamsVerificationResult : Array[TSearchParamsVerificationResult] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'target {http://hl7.org/fhir/SearchParameter/VerificationResult-target}');
{$ENDIF}
{$IFDEF FHIR_VISIONPRESCRIPTION}
  CODES_TSearchParamsVisionPrescription : Array[TSearchParamsVisionPrescription] of String = ('_content {http://hl7.org/fhir/SearchParameter/Resource-content}', '_filter {http://hl7.org/fhir/SearchParameter/filter}', '_id {http://hl7.org/fhir/SearchParameter/Resource-id}', '_lastUpdated {http://hl7.org/fhir/SearchParameter/Resource-lastUpdated}', '_profile {http://hl7.org/fhir/SearchParameter/Resource-profile}', '_query {http://hl7.org/fhir/SearchParameter/Resource-query}', '_security {http://hl7.org/fhir/SearchParameter/Resource-security}', '_source {http://hl7.org/fhir/SearchParameter/Resource-source}', '_tag {http://hl7.org/fhir/SearchParameter/Resource-tag}', '_text {http://hl7.org/fhir/SearchParameter/DomainResource-text}', 'datewritten {http://hl7.org/fhir/SearchParameter/VisionPrescription-datewritten}', 'encounter {http://hl7.org/fhir/SearchParameter/clinical-encounter}', 'identifier {http://hl7.org/fhir/SearchParameter/clinical-identifier}',
       'patient {http://hl7.org/fhir/SearchParameter/clinical-patient}', 'prescriber {http://hl7.org/fhir/SearchParameter/VisionPrescription-prescriber}', 'status {http://hl7.org/fhir/SearchParameter/VisionPrescription-status}');
{$ENDIF}

implementation

end.

