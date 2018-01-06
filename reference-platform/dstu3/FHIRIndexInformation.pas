unit FHIRIndexInformation;

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

{$IFNDEF FHIR3}
This is the dstu3 version of the FHIR code
{$ENDIF}


interface

// FHIR v3.0.1 generated 2017-04-27T17:09:41+10:00

uses
  SysUtils, Classes, StringSupport, DecimalSupport, AdvBuffers, DateSupport, FHIRIndexBase, FHIRResources, FHIRTypes, FHIRConstants, FHIRSupport;

Type

  TFHIRIndexBuilder = class (TAdvObject)
  private
    {$IFDEF FHIR_ACCOUNT}
    procedure buildIndexesForAccount(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_ACTIVITYDEFINITION}
    procedure buildIndexesForActivityDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_ADVERSEEVENT}
    procedure buildIndexesForAdverseEvent(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_ALLERGYINTOLERANCE}
    procedure buildIndexesForAllergyIntolerance(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_APPOINTMENT}
    procedure buildIndexesForAppointment(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_APPOINTMENTRESPONSE}
    procedure buildIndexesForAppointmentResponse(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_AUDITEVENT}
    procedure buildIndexesForAuditEvent(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_BASIC}
    procedure buildIndexesForBasic(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_BINARY}
    procedure buildIndexesForBinary(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_BODYSITE}
    procedure buildIndexesForBodySite(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_BUNDLE}
    procedure buildIndexesForBundle(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_CAPABILITYSTATEMENT}
    procedure buildIndexesForCapabilityStatement(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_CAREPLAN}
    procedure buildIndexesForCarePlan(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_CARETEAM}
    procedure buildIndexesForCareTeam(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_CHARGEITEM}
    procedure buildIndexesForChargeItem(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_CLAIM}
    procedure buildIndexesForClaim(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_CLAIMRESPONSE}
    procedure buildIndexesForClaimResponse(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_CLINICALIMPRESSION}
    procedure buildIndexesForClinicalImpression(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_CODESYSTEM}
    procedure buildIndexesForCodeSystem(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_COMMUNICATION}
    procedure buildIndexesForCommunication(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_COMMUNICATIONREQUEST}
    procedure buildIndexesForCommunicationRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_COMPARTMENTDEFINITION}
    procedure buildIndexesForCompartmentDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_COMPOSITION}
    procedure buildIndexesForComposition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_CONCEPTMAP}
    procedure buildIndexesForConceptMap(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_CONDITION}
    procedure buildIndexesForCondition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_CONSENT}
    procedure buildIndexesForConsent(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_CONTRACT}
    procedure buildIndexesForContract(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_COVERAGE}
    procedure buildIndexesForCoverage(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_DATAELEMENT}
    procedure buildIndexesForDataElement(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_DETECTEDISSUE}
    procedure buildIndexesForDetectedIssue(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_DEVICE}
    procedure buildIndexesForDevice(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_DEVICECOMPONENT}
    procedure buildIndexesForDeviceComponent(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_DEVICEMETRIC}
    procedure buildIndexesForDeviceMetric(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_DEVICEREQUEST}
    procedure buildIndexesForDeviceRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_DEVICEUSESTATEMENT}
    procedure buildIndexesForDeviceUseStatement(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_DIAGNOSTICREPORT}
    procedure buildIndexesForDiagnosticReport(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_DOCUMENTMANIFEST}
    procedure buildIndexesForDocumentManifest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_DOCUMENTREFERENCE}
    procedure buildIndexesForDocumentReference(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_ELIGIBILITYREQUEST}
    procedure buildIndexesForEligibilityRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_ELIGIBILITYRESPONSE}
    procedure buildIndexesForEligibilityResponse(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_ENCOUNTER}
    procedure buildIndexesForEncounter(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_ENDPOINT}
    procedure buildIndexesForEndpoint(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_ENROLLMENTREQUEST}
    procedure buildIndexesForEnrollmentRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_ENROLLMENTRESPONSE}
    procedure buildIndexesForEnrollmentResponse(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_EPISODEOFCARE}
    procedure buildIndexesForEpisodeOfCare(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_EXPANSIONPROFILE}
    procedure buildIndexesForExpansionProfile(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_EXPLANATIONOFBENEFIT}
    procedure buildIndexesForExplanationOfBenefit(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_FAMILYMEMBERHISTORY}
    procedure buildIndexesForFamilyMemberHistory(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_FLAG}
    procedure buildIndexesForFlag(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_GOAL}
    procedure buildIndexesForGoal(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_GRAPHDEFINITION}
    procedure buildIndexesForGraphDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_GROUP}
    procedure buildIndexesForGroup(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_GUIDANCERESPONSE}
    procedure buildIndexesForGuidanceResponse(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_HEALTHCARESERVICE}
    procedure buildIndexesForHealthcareService(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_IMAGINGMANIFEST}
    procedure buildIndexesForImagingManifest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_IMAGINGSTUDY}
    procedure buildIndexesForImagingStudy(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_IMMUNIZATION}
    procedure buildIndexesForImmunization(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_IMMUNIZATIONRECOMMENDATION}
    procedure buildIndexesForImmunizationRecommendation(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_IMPLEMENTATIONGUIDE}
    procedure buildIndexesForImplementationGuide(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_LIBRARY}
    procedure buildIndexesForLibrary(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_LINKAGE}
    procedure buildIndexesForLinkage(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_LIST}
    procedure buildIndexesForList(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_LOCATION}
    procedure buildIndexesForLocation(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_MEASURE}
    procedure buildIndexesForMeasure(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_MEASUREREPORT}
    procedure buildIndexesForMeasureReport(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_MEDIA}
    procedure buildIndexesForMedia(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_MEDICATION}
    procedure buildIndexesForMedication(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_MEDICATIONADMINISTRATION}
    procedure buildIndexesForMedicationAdministration(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_MEDICATIONDISPENSE}
    procedure buildIndexesForMedicationDispense(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_MEDICATIONREQUEST}
    procedure buildIndexesForMedicationRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_MEDICATIONSTATEMENT}
    procedure buildIndexesForMedicationStatement(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_MESSAGEDEFINITION}
    procedure buildIndexesForMessageDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_MESSAGEHEADER}
    procedure buildIndexesForMessageHeader(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_NAMINGSYSTEM}
    procedure buildIndexesForNamingSystem(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_NUTRITIONORDER}
    procedure buildIndexesForNutritionOrder(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_OBSERVATION}
    procedure buildIndexesForObservation(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_OPERATIONDEFINITION}
    procedure buildIndexesForOperationDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_OPERATIONOUTCOME}
    procedure buildIndexesForOperationOutcome(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_ORGANIZATION}
    procedure buildIndexesForOrganization(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_PATIENT}
    procedure buildIndexesForPatient(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_PAYMENTNOTICE}
    procedure buildIndexesForPaymentNotice(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_PAYMENTRECONCILIATION}
    procedure buildIndexesForPaymentReconciliation(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_PERSON}
    procedure buildIndexesForPerson(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_PLANDEFINITION}
    procedure buildIndexesForPlanDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_PRACTITIONER}
    procedure buildIndexesForPractitioner(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_PRACTITIONERROLE}
    procedure buildIndexesForPractitionerRole(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_PROCEDURE}
    procedure buildIndexesForProcedure(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_PROCEDUREREQUEST}
    procedure buildIndexesForProcedureRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_PROCESSREQUEST}
    procedure buildIndexesForProcessRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_PROCESSRESPONSE}
    procedure buildIndexesForProcessResponse(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_PROVENANCE}
    procedure buildIndexesForProvenance(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_QUESTIONNAIRE}
    procedure buildIndexesForQuestionnaire(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_QUESTIONNAIRERESPONSE}
    procedure buildIndexesForQuestionnaireResponse(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_REFERRALREQUEST}
    procedure buildIndexesForReferralRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_RELATEDPERSON}
    procedure buildIndexesForRelatedPerson(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_REQUESTGROUP}
    procedure buildIndexesForRequestGroup(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_RESEARCHSTUDY}
    procedure buildIndexesForResearchStudy(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_RESEARCHSUBJECT}
    procedure buildIndexesForResearchSubject(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_RISKASSESSMENT}
    procedure buildIndexesForRiskAssessment(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_SCHEDULE}
    procedure buildIndexesForSchedule(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_SEARCHPARAMETER}
    procedure buildIndexesForSearchParameter(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_SEQUENCE}
    procedure buildIndexesForSequence(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_SERVICEDEFINITION}
    procedure buildIndexesForServiceDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_SLOT}
    procedure buildIndexesForSlot(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_SPECIMEN}
    procedure buildIndexesForSpecimen(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_STRUCTUREDEFINITION}
    procedure buildIndexesForStructureDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_STRUCTUREMAP}
    procedure buildIndexesForStructureMap(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_SUBSCRIPTION}
    procedure buildIndexesForSubscription(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_SUBSTANCE}
    procedure buildIndexesForSubstance(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_SUPPLYDELIVERY}
    procedure buildIndexesForSupplyDelivery(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_SUPPLYREQUEST}
    procedure buildIndexesForSupplyRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_TASK}
    procedure buildIndexesForTask(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_TESTREPORT}
    procedure buildIndexesForTestReport(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_TESTSCRIPT}
    procedure buildIndexesForTestScript(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_VALUESET}
    procedure buildIndexesForValueSet(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_VISIONPRESCRIPTION}
    procedure buildIndexesForVisionPrescription(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
  public
    procedure registerIndexes(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
 end;

implementation

{$IFDEF FHIR_ACCOUNT}
procedure TFHIRIndexBuilder.buildIndexesForAccount(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Account', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Account', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('Account', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('Account', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('Account', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Account', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('Account', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('Account', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Account', 'balance', 'How much is in account?', SearchParamTypeQUANTITY, [], 'Account.balance', SearchXpathUsageNormal);
  indexes.add('Account', 'identifier', 'Account number', SearchParamTypeTOKEN, [], 'Account.identifier', SearchXpathUsageNormal);
  indexes.add('Account', 'name', 'Human-readable label', SearchParamTypeSTRING, [], 'Account.name', SearchXpathUsageNormal);
  indexes.add('Account', 'owner', 'Who is responsible?', SearchParamTypeREFERENCE, ['Organization'], 'Account.owner', SearchXpathUsageNormal);
  indexes.add('Account', 'patient', 'What is account tied to?', SearchParamTypeREFERENCE, ['Patient'], 'Account.subject', SearchXpathUsageNormal);
  indexes.add('Account', 'period', 'Transaction window', SearchParamTypeDATE, [], 'Account.period', SearchXpathUsageNormal);
  indexes.add('Account', 'status', 'active | inactive | entered-in-error', SearchParamTypeTOKEN, [], 'Account.status', SearchXpathUsageNormal);
  indexes.add('Account', 'subject', 'What is account tied to?', SearchParamTypeREFERENCE, ['Practitioner', 'Organization', 'Device', 'Patient', 'HealthcareService', 'Location'], 'Account.subject', SearchXpathUsageNormal);
  indexes.add('Account', 'type', 'E.g. patient, expense, depreciation', SearchParamTypeTOKEN, [], 'Account.type', SearchXpathUsageNormal);
  compartments.register(frtDevice, 'Account', ['subject']);
  compartments.register(frtPatient, 'Account', ['subject']);
  compartments.register(frtPractitioner, 'Account', ['subject']);
end;
{$ENDIF}

{$IFDEF FHIR_ACTIVITYDEFINITION}
procedure TFHIRIndexBuilder.buildIndexesForActivityDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ActivityDefinition', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('ActivityDefinition', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('ActivityDefinition', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('ActivityDefinition', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('ActivityDefinition', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ActivityDefinition', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('ActivityDefinition', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('ActivityDefinition', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('ActivityDefinition', 'composed-of', 'What resource is being referenced', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'ActivityDefinition.relatedArtifact.where(type=''composed-of'').resource', SearchXpathUsageNormal);
  indexes.add('ActivityDefinition', 'date', 'The activity definition publication date', SearchParamTypeDATE, [], 'ActivityDefinition.date', SearchXpathUsageNormal);
  indexes.add('ActivityDefinition', 'depends-on', 'What resource is being referenced', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'ActivityDefinition.relatedArtifact.where(type=''depends-on'').resource | ActivityDefinition.library', SearchXpathUsageNormal);
  indexes.add('ActivityDefinition', 'derived-from', 'What resource is being referenced', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'ActivityDefinition.relatedArtifact.where(type=''derived-from'').resource', SearchXpathUsageNormal);
  indexes.add('ActivityDefinition', 'description', 'The description of the activity definition', SearchParamTypeSTRING, [], 'ActivityDefinition.description', SearchXpathUsageNormal);
  indexes.add('ActivityDefinition', 'effective', 'The time during which the activity definition is intended to be in use', SearchParamTypeDATE, [], 'ActivityDefinition.effectivePeriod', SearchXpathUsageNormal);
  indexes.add('ActivityDefinition', 'identifier', 'External identifier for the activity definition', SearchParamTypeTOKEN, [], 'ActivityDefinition.identifier', SearchXpathUsageNormal);
  indexes.add('ActivityDefinition', 'jurisdiction', 'Intended jurisdiction for the activity definition', SearchParamTypeTOKEN, [], 'ActivityDefinition.jurisdiction', SearchXpathUsageNormal);
  indexes.add('ActivityDefinition', 'name', 'Computationally friendly name of the activity definition', SearchParamTypeSTRING, [], 'ActivityDefinition.name', SearchXpathUsageNormal);
  indexes.add('ActivityDefinition', 'predecessor', 'What resource is being referenced', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'ActivityDefinition.relatedArtifact.where(type=''predecessor'').resource', SearchXpathUsageNormal);
  indexes.add('ActivityDefinition', 'publisher', 'Name of the publisher of the activity definition', SearchParamTypeSTRING, [], 'ActivityDefinition.publisher', SearchXpathUsageNormal);
  indexes.add('ActivityDefinition', 'status', 'The current status of the activity definition', SearchParamTypeTOKEN, [], 'ActivityDefinition.status', SearchXpathUsageNormal);
  indexes.add('ActivityDefinition', 'successor', 'What resource is being referenced', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'ActivityDefinition.relatedArtifact.where(type=''successor'').resource', SearchXpathUsageNormal);
  indexes.add('ActivityDefinition', 'title', 'The human-friendly name of the activity definition', SearchParamTypeSTRING, [], 'ActivityDefinition.title', SearchXpathUsageNormal);
  indexes.add('ActivityDefinition', 'topic', 'Topics associated with the module', SearchParamTypeTOKEN, [], 'ActivityDefinition.topic', SearchXpathUsageNormal);
  indexes.add('ActivityDefinition', 'url', 'The uri that identifies the activity definition', SearchParamTypeURI, [], 'ActivityDefinition.url', SearchXpathUsageNormal);
  indexes.add('ActivityDefinition', 'version', 'The business version of the activity definition', SearchParamTypeTOKEN, [], 'ActivityDefinition.version', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_ADVERSEEVENT}
procedure TFHIRIndexBuilder.buildIndexesForAdverseEvent(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('AdverseEvent', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('AdverseEvent', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('AdverseEvent', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('AdverseEvent', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('AdverseEvent', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('AdverseEvent', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('AdverseEvent', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('AdverseEvent', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('AdverseEvent', 'category', 'AE | PAE  An adverse event is an event that caused harm to a patient,  an adverse reaction is a something that is a subject-specific event that is a result of an exposure to a medication, food, device or environmental substance, a potential adverse e'+'vent is something that occurred and that could have caused harm to a patient but did not', SearchParamTypeTOKEN, [], 'AdverseEvent.category', SearchXpathUsageNormal);
  indexes.add('AdverseEvent', 'date', 'When the event occurred', SearchParamTypeDATE, [], 'AdverseEvent.date', SearchXpathUsageNormal);
  indexes.add('AdverseEvent', 'location', 'Location where adverse event occurred', SearchParamTypeREFERENCE, ['Location'], 'AdverseEvent.location', SearchXpathUsageNormal);
  indexes.add('AdverseEvent', 'reaction', 'Adverse Reaction Events linked to exposure to substance', SearchParamTypeREFERENCE, ['Condition'], 'AdverseEvent.reaction', SearchXpathUsageNormal);
  indexes.add('AdverseEvent', 'recorder', 'Who recorded the adverse event', SearchParamTypeREFERENCE, ['Practitioner', 'Patient', 'RelatedPerson'], 'AdverseEvent.recorder', SearchXpathUsageNormal);
  indexes.add('AdverseEvent', 'seriousness', 'Mild | Moderate | Severe', SearchParamTypeTOKEN, [], 'AdverseEvent.seriousness', SearchXpathUsageNormal);
  indexes.add('AdverseEvent', 'study', 'AdverseEvent.study', SearchParamTypeREFERENCE, ['ResearchStudy'], 'AdverseEvent.study', SearchXpathUsageNormal);
  indexes.add('AdverseEvent', 'subject', 'Subject or group impacted by event', SearchParamTypeREFERENCE, ['Device', 'Medication', 'Patient', 'ResearchSubject'], 'AdverseEvent.subject', SearchXpathUsageNormal);
  indexes.add('AdverseEvent', 'substance', 'Refers to the specific entity that caused the adverse event', SearchParamTypeREFERENCE, ['Device', 'Medication', 'Substance', 'MedicationAdministration', 'MedicationStatement'], 'AdverseEvent.suspectEntity.instance', SearchXpathUsageNormal);
  indexes.add('AdverseEvent', 'type', 'actual | potential', SearchParamTypeTOKEN, [], 'AdverseEvent.type', SearchXpathUsageNormal);
  compartments.register(frtPatient, 'AdverseEvent', ['subject']);
  compartments.register(frtPractitioner, 'AdverseEvent', ['recorder']);
  compartments.register(frtRelatedPerson, 'AdverseEvent', ['recorder']);
end;
{$ENDIF}

{$IFDEF FHIR_ALLERGYINTOLERANCE}
procedure TFHIRIndexBuilder.buildIndexesForAllergyIntolerance(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('AllergyIntolerance', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('AllergyIntolerance', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('AllergyIntolerance', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('AllergyIntolerance', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('AllergyIntolerance', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('AllergyIntolerance', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('AllergyIntolerance', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('AllergyIntolerance', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('AllergyIntolerance', 'asserter', 'Source of the information about the allergy', SearchParamTypeREFERENCE, ['Practitioner', 'Patient', 'RelatedPerson'], 'AllergyIntolerance.asserter', SearchXpathUsageNormal);
  indexes.add('AllergyIntolerance', 'category', 'food | medication | environment | biologic', SearchParamTypeTOKEN, [], 'AllergyIntolerance.category', SearchXpathUsageNormal);
  indexes.add('AllergyIntolerance', 'clinical-status', 'active | inactive | resolved', SearchParamTypeTOKEN, [], 'AllergyIntolerance.clinicalStatus', SearchXpathUsageNormal);
  indexes.add('AllergyIntolerance', 'code', 'Code that identifies the allergy or intolerance', SearchParamTypeTOKEN, [], 'FamilyMemberHistory.condition.code | DeviceRequest.code.as(CodeableConcept) | AllergyIntolerance.code | AllergyIntolerance.reaction.substance | Procedure.code | List.code | ProcedureRequest.code | Observation.code | DiagnosticReport.code | Condition.'+'code', SearchXpathUsageNormal);
  indexes.add('AllergyIntolerance', 'criticality', 'low | high | unable-to-assess', SearchParamTypeTOKEN, [], 'AllergyIntolerance.criticality', SearchXpathUsageNormal);
  indexes.add('AllergyIntolerance', 'date', 'Date record was believed accurate', SearchParamTypeDATE, [], 'Consent.dateTime | SupplyRequest.authoredOn | RiskAssessment.occurrence.as(DateTime) | CareTeam.period | FamilyMemberHistory.date | Encounter.period | AllergyIntolerance.assertedDate | CarePlan.period | EpisodeOfCare.period | Procedure.performed | Li'+'st.date | Immunization.date | Flag.period | Observation.effective | DiagnosticReport.effective | Composition.date | DetectedIssue.date | ClinicalImpression.date', SearchXpathUsageNormal);
  indexes.add('AllergyIntolerance', 'identifier', 'External ids for this item', SearchParamTypeTOKEN, [], 'DocumentManifest.masterIdentifier | DocumentManifest.identifier | Goal.identifier | Consent.identifier | DocumentReference.masterIdentifier | DocumentReference.identifier | SupplyRequest.identifier | RiskAssessment.identifier | CareTeam.identifier | '+'ImagingStudy.identifier | FamilyMemberHistory.identifier | Encounter.identifier | DeviceRequest.identifier | AllergyIntolerance.identifier | CarePlan.identifier | EpisodeOfCare.identifier | Procedure.identifier | List.identifier | Immunization.identi'+'fier | VisionPrescription.identifier | ProcedureRequest.identifier | Observation.identifier | DiagnosticReport.identifier | NutritionOrder.identifier | Condition.identifier | Composition.identifier | DetectedIssue.identifier | SupplyDelivery.identifi'+'er', SearchXpathUsageNormal);
  indexes.add('AllergyIntolerance', 'last-date', 'Date(/time) of last known occurrence of a reaction', SearchParamTypeDATE, [], 'AllergyIntolerance.lastOccurrence', SearchXpathUsageNormal);
  indexes.add('AllergyIntolerance', 'manifestation', 'Clinical symptoms/signs associated with the Event', SearchParamTypeTOKEN, [], 'AllergyIntolerance.reaction.manifestation', SearchXpathUsageNormal);
  indexes.add('AllergyIntolerance', 'onset', 'Date(/time) when manifestations showed', SearchParamTypeDATE, [], 'AllergyIntolerance.reaction.onset', SearchXpathUsageNormal);
  indexes.add('AllergyIntolerance', 'patient', 'Who the sensitivity is for', SearchParamTypeREFERENCE, ['Group', 'Patient'], 'ReferralRequest.subject | DocumentManifest.subject | Goal.subject | Consent.patient | DocumentReference.subject | ImagingManifest.patient | RiskAssessment.subject | CareTeam.subject | ImagingStudy.patient | FamilyMemberHistory.patient | Encounter.sub'+'ject | DeviceUseStatement.subject | DeviceRequest.subject | AllergyIntolerance.patient | CarePlan.subject | EpisodeOfCare.patient | Procedure.subject | List.subject | Immunization.patient | VisionPrescription.patient | ProcedureRequest.subject | Flag'+'.subject | Observation.subject | DiagnosticReport.subject | NutritionOrder.patient | Condition.subject | Composition.subject | DetectedIssue.patient | SupplyDelivery.patient | ClinicalImpression.subject', SearchXpathUsageNormal);
  indexes.add('AllergyIntolerance', 'recorder', 'Who recorded the sensitivity', SearchParamTypeREFERENCE, ['Practitioner', 'Patient'], 'AllergyIntolerance.recorder', SearchXpathUsageNormal);
  indexes.add('AllergyIntolerance', 'route', 'How the subject was exposed to the substance', SearchParamTypeTOKEN, [], 'AllergyIntolerance.reaction.exposureRoute', SearchXpathUsageNormal);
  indexes.add('AllergyIntolerance', 'severity', 'mild | moderate | severe (of event as a whole)', SearchParamTypeTOKEN, [], 'AllergyIntolerance.reaction.severity', SearchXpathUsageNormal);
  indexes.add('AllergyIntolerance', 'type', 'allergy | intolerance - Underlying mechanism (if known)', SearchParamTypeTOKEN, [], 'ReferralRequest.type | DocumentManifest.type | DocumentReference.type | Encounter.type | AllergyIntolerance.type | EpisodeOfCare.type | Composition.type', SearchXpathUsageNormal);
  indexes.add('AllergyIntolerance', 'verification-status', 'unconfirmed | confirmed | refuted | entered-in-error', SearchParamTypeTOKEN, [], 'AllergyIntolerance.verificationStatus', SearchXpathUsageNormal);
  compartments.register(frtPatient, 'AllergyIntolerance', ['patient', 'recorder', 'asserter']);
  compartments.register(frtPractitioner, 'AllergyIntolerance', ['recorder', 'asserter']);
  compartments.register(frtRelatedPerson, 'AllergyIntolerance', ['asserter']);
end;
{$ENDIF}

{$IFDEF FHIR_APPOINTMENT}
procedure TFHIRIndexBuilder.buildIndexesForAppointment(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Appointment', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Appointment', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('Appointment', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('Appointment', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('Appointment', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Appointment', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('Appointment', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('Appointment', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Appointment', 'actor', 'Any one of the individuals participating in the appointment', SearchParamTypeREFERENCE, ['Practitioner', 'Device', 'Patient', 'HealthcareService', 'RelatedPerson', 'Location'], 'Appointment.participant.actor', SearchXpathUsageNormal);
  indexes.add('Appointment', 'appointment-type', 'The style of appointment or patient that has been booked in the slot (not service type)', SearchParamTypeTOKEN, [], 'Appointment.appointmentType', SearchXpathUsageNormal);
  indexes.add('Appointment', 'date', 'Appointment date/time.', SearchParamTypeDATE, [], 'Appointment.start', SearchXpathUsageNormal);
  indexes.add('Appointment', 'identifier', 'An Identifier of the Appointment', SearchParamTypeTOKEN, [], 'Appointment.identifier', SearchXpathUsageNormal);
  indexes.add('Appointment', 'incomingreferral', 'The ReferralRequest provided as information to allocate to the Encounter', SearchParamTypeREFERENCE, ['ReferralRequest'], 'Appointment.incomingReferral', SearchXpathUsageNormal);
  indexes.add('Appointment', 'location', 'This location is listed in the participants of the appointment', SearchParamTypeREFERENCE, ['Location'], 'Appointment.participant.actor', SearchXpathUsageNormal);
  indexes.add('Appointment', 'part-status', 'The Participation status of the subject, or other participant on the appointment. Can be used to locate participants that have not responded to meeting requests.', SearchParamTypeTOKEN, [], 'Appointment.participant.status', SearchXpathUsageNormal);
  indexes.add('Appointment', 'patient', 'One of the individuals of the appointment is this patient', SearchParamTypeREFERENCE, ['Patient'], 'Appointment.participant.actor', SearchXpathUsageNormal);
  indexes.add('Appointment', 'practitioner', 'One of the individuals of the appointment is this practitioner', SearchParamTypeREFERENCE, ['Practitioner'], 'Appointment.participant.actor', SearchXpathUsageNormal);
  indexes.add('Appointment', 'service-type', 'The specific service that is to be performed during this appointment', SearchParamTypeTOKEN, [], 'Appointment.serviceType', SearchXpathUsageNormal);
  indexes.add('Appointment', 'status', 'The overall status of the appointment', SearchParamTypeTOKEN, [], 'Appointment.status', SearchXpathUsageNormal);
  compartments.register(frtDevice, 'Appointment', ['actor']);
  compartments.register(frtPatient, 'Appointment', ['actor']);
  compartments.register(frtPractitioner, 'Appointment', ['actor']);
  compartments.register(frtRelatedPerson, 'Appointment', ['actor']);
end;
{$ENDIF}

{$IFDEF FHIR_APPOINTMENTRESPONSE}
procedure TFHIRIndexBuilder.buildIndexesForAppointmentResponse(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('AppointmentResponse', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('AppointmentResponse', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('AppointmentResponse', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('AppointmentResponse', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('AppointmentResponse', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('AppointmentResponse', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('AppointmentResponse', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('AppointmentResponse', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('AppointmentResponse', 'actor', 'The Person, Location/HealthcareService or Device that this appointment response replies for', SearchParamTypeREFERENCE, ['Practitioner', 'Device', 'Patient', 'HealthcareService', 'RelatedPerson', 'Location'], 'AppointmentResponse.actor', SearchXpathUsageNormal);
  indexes.add('AppointmentResponse', 'appointment', 'The appointment that the response is attached to', SearchParamTypeREFERENCE, ['Appointment'], 'AppointmentResponse.appointment', SearchXpathUsageNormal);
  indexes.add('AppointmentResponse', 'identifier', 'An Identifier in this appointment response', SearchParamTypeTOKEN, [], 'AppointmentResponse.identifier', SearchXpathUsageNormal);
  indexes.add('AppointmentResponse', 'location', 'This Response is for this Location', SearchParamTypeREFERENCE, ['Location'], 'AppointmentResponse.actor', SearchXpathUsageNormal);
  indexes.add('AppointmentResponse', 'part-status', 'The participants acceptance status for this appointment', SearchParamTypeTOKEN, [], 'AppointmentResponse.participantStatus', SearchXpathUsageNormal);
  indexes.add('AppointmentResponse', 'patient', 'This Response is for this Patient', SearchParamTypeREFERENCE, ['Patient'], 'AppointmentResponse.actor', SearchXpathUsageNormal);
  indexes.add('AppointmentResponse', 'practitioner', 'This Response is for this Practitioner', SearchParamTypeREFERENCE, ['Practitioner'], 'AppointmentResponse.actor', SearchXpathUsageNormal);
  compartments.register(frtDevice, 'AppointmentResponse', ['actor']);
  compartments.register(frtPatient, 'AppointmentResponse', ['actor']);
  compartments.register(frtPractitioner, 'AppointmentResponse', ['actor']);
  compartments.register(frtRelatedPerson, 'AppointmentResponse', ['actor']);
end;
{$ENDIF}

{$IFDEF FHIR_AUDITEVENT}
procedure TFHIRIndexBuilder.buildIndexesForAuditEvent(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('AuditEvent', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('AuditEvent', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('AuditEvent', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('AuditEvent', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('AuditEvent', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('AuditEvent', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('AuditEvent', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('AuditEvent', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('AuditEvent', 'action', 'Type of action performed during the event', SearchParamTypeTOKEN, [], 'AuditEvent.action', SearchXpathUsageNormal);
  indexes.add('AuditEvent', 'address', 'Identifier for the network access point of the user device', SearchParamTypeSTRING, [], 'AuditEvent.agent.network.address', SearchXpathUsageNormal);
  indexes.add('AuditEvent', 'agent', 'Direct reference to resource', SearchParamTypeREFERENCE, ['Practitioner', 'Organization', 'Device', 'Patient', 'RelatedPerson'], 'AuditEvent.agent.reference', SearchXpathUsageNormal);
  indexes.add('AuditEvent', 'agent-name', 'Human-meaningful name for the agent', SearchParamTypeSTRING, [], 'AuditEvent.agent.name', SearchXpathUsageNormal);
  indexes.add('AuditEvent', 'agent-role', 'Agent role in the event', SearchParamTypeTOKEN, [], 'AuditEvent.agent.role', SearchXpathUsageNormal);
  indexes.add('AuditEvent', 'altid', 'Alternative User id e.g. authentication', SearchParamTypeTOKEN, [], 'AuditEvent.agent.altId', SearchXpathUsageNormal);
  indexes.add('AuditEvent', 'date', 'Time when the event occurred on source', SearchParamTypeDATE, [], 'AuditEvent.recorded', SearchXpathUsageNormal);
  indexes.add('AuditEvent', 'entity', 'Specific instance of resource', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'AuditEvent.entity.reference', SearchXpathUsageNormal);
  indexes.add('AuditEvent', 'entity-id', 'Specific instance of object', SearchParamTypeTOKEN, [], 'AuditEvent.entity.identifier', SearchXpathUsageNormal);
  indexes.add('AuditEvent', 'entity-name', 'Descriptor for entity', SearchParamTypeSTRING, [], 'AuditEvent.entity.name', SearchXpathUsageNormal);
  indexes.add('AuditEvent', 'entity-role', 'What role the entity played', SearchParamTypeTOKEN, [], 'AuditEvent.entity.role', SearchXpathUsageNormal);
  indexes.add('AuditEvent', 'entity-type', 'Type of entity involved', SearchParamTypeTOKEN, [], 'AuditEvent.entity.type', SearchXpathUsageNormal);
  indexes.add('AuditEvent', 'outcome', 'Whether the event succeeded or failed', SearchParamTypeTOKEN, [], 'AuditEvent.outcome', SearchXpathUsageNormal);
  indexes.add('AuditEvent', 'patient', 'Direct reference to resource', SearchParamTypeREFERENCE, ['Patient'], 'AuditEvent.agent.reference | AuditEvent.entity.reference', SearchXpathUsageNormal);
  indexes.add('AuditEvent', 'policy', 'Policy that authorized event', SearchParamTypeURI, [], 'AuditEvent.agent.policy', SearchXpathUsageNormal);
  indexes.add('AuditEvent', 'site', 'Logical source location within the enterprise', SearchParamTypeTOKEN, [], 'AuditEvent.source.site', SearchXpathUsageNormal);
  indexes.add('AuditEvent', 'source', 'The identity of source detecting the event', SearchParamTypeTOKEN, [], 'AuditEvent.source.identifier', SearchXpathUsageNormal);
  indexes.add('AuditEvent', 'subtype', 'More specific type/id for the event', SearchParamTypeTOKEN, [], 'AuditEvent.subtype', SearchXpathUsageNormal);
  indexes.add('AuditEvent', 'type', 'Type/identifier of event', SearchParamTypeTOKEN, [], 'AuditEvent.type', SearchXpathUsageNormal);
  indexes.add('AuditEvent', 'user', 'Unique identifier for the user', SearchParamTypeTOKEN, [], 'AuditEvent.agent.userId', SearchXpathUsageNormal);
  compartments.register(frtDevice, 'AuditEvent', ['agent']);
  compartments.register(frtPatient, 'AuditEvent', ['patient', 'agent.patient', 'entity.patient']);
  compartments.register(frtPractitioner, 'AuditEvent', ['agent']);
end;
{$ENDIF}

{$IFDEF FHIR_BASIC}
procedure TFHIRIndexBuilder.buildIndexesForBasic(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Basic', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Basic', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('Basic', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('Basic', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('Basic', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Basic', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('Basic', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('Basic', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Basic', 'author', 'Who created', SearchParamTypeREFERENCE, ['Practitioner', 'Patient', 'RelatedPerson'], 'Basic.author', SearchXpathUsageNormal);
  indexes.add('Basic', 'code', 'Kind of Resource', SearchParamTypeTOKEN, [], 'Basic.code', SearchXpathUsageNormal);
  indexes.add('Basic', 'created', 'When created', SearchParamTypeDATE, [], 'Basic.created', SearchXpathUsageNormal);
  indexes.add('Basic', 'identifier', 'Business identifier', SearchParamTypeTOKEN, [], 'Basic.identifier', SearchXpathUsageNormal);
  indexes.add('Basic', 'patient', 'Identifies the focus of this resource', SearchParamTypeREFERENCE, ['Patient'], 'Basic.subject', SearchXpathUsageNormal);
  indexes.add('Basic', 'subject', 'Identifies the focus of this resource', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'Basic.subject', SearchXpathUsageNormal);
  compartments.register(frtPatient, 'Basic', ['patient', 'author']);
  compartments.register(frtPractitioner, 'Basic', ['author']);
  compartments.register(frtRelatedPerson, 'Basic', ['author']);
end;
{$ENDIF}

{$IFDEF FHIR_BINARY}
procedure TFHIRIndexBuilder.buildIndexesForBinary(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Binary', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Binary', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('Binary', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('Binary', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('Binary', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Binary', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('Binary', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('Binary', 'contenttype', 'MimeType of the binary content', SearchParamTypeTOKEN, [], 'Binary.contentType', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_BODYSITE}
procedure TFHIRIndexBuilder.buildIndexesForBodySite(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('BodySite', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('BodySite', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('BodySite', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('BodySite', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('BodySite', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('BodySite', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('BodySite', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('BodySite', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('BodySite', 'code', 'Named anatomical location', SearchParamTypeTOKEN, [], 'BodySite.code', SearchXpathUsageNormal);
  indexes.add('BodySite', 'identifier', 'Identifier for this instance of the anatomical location', SearchParamTypeTOKEN, [], 'BodySite.identifier', SearchXpathUsageNormal);
  indexes.add('BodySite', 'patient', 'Patient to whom bodysite belongs', SearchParamTypeREFERENCE, ['Patient'], 'BodySite.patient', SearchXpathUsageNormal);
  compartments.register(frtPatient, 'BodySite', ['patient']);
end;
{$ENDIF}

{$IFDEF FHIR_BUNDLE}
procedure TFHIRIndexBuilder.buildIndexesForBundle(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Bundle', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Bundle', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('Bundle', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('Bundle', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('Bundle', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Bundle', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('Bundle', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('Bundle', 'composition', 'The first resource in the bundle, if the bundle type is "document" - this is a composition, and this parameter provides access to searches its contents', SearchParamTypeREFERENCE, ['Composition'], 'Bundle.entry[0].resource', SearchXpathUsageNormal);
  indexes.add('Bundle', 'identifier', 'Persistent identifier for the bundle', SearchParamTypeTOKEN, [], 'Bundle.identifier', SearchXpathUsageNormal);
  indexes.add('Bundle', 'message', 'The first resource in the bundle, if the bundle type is "message" - this is a message header, and this parameter provides access to search its contents', SearchParamTypeREFERENCE, ['MessageHeader'], 'Bundle.entry[0].resource', SearchXpathUsageNormal);
  indexes.add('Bundle', 'type', 'document | message | transaction | transaction-response | batch | batch-response | history | searchset | collection', SearchParamTypeTOKEN, [], 'Bundle.type', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_CAPABILITYSTATEMENT}
procedure TFHIRIndexBuilder.buildIndexesForCapabilityStatement(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('CapabilityStatement', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('CapabilityStatement', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('CapabilityStatement', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('CapabilityStatement', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('CapabilityStatement', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('CapabilityStatement', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('CapabilityStatement', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('CapabilityStatement', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('CapabilityStatement', 'date', 'The capability statement publication date', SearchParamTypeDATE, [], 'CapabilityStatement.date', SearchXpathUsageNormal);
  indexes.add('CapabilityStatement', 'description', 'The description of the capability statement', SearchParamTypeSTRING, [], 'CapabilityStatement.description', SearchXpathUsageNormal);
  indexes.add('CapabilityStatement', 'event', 'Event code in a capability statement', SearchParamTypeTOKEN, [], 'CapabilityStatement.messaging.event.code', SearchXpathUsageNormal);
  indexes.add('CapabilityStatement', 'fhirversion', 'The version of FHIR', SearchParamTypeTOKEN, [], 'CapabilityStatement.version', SearchXpathUsageNormal);
  indexes.add('CapabilityStatement', 'format', 'formats supported (xml | json | ttl | mime type)', SearchParamTypeTOKEN, [], 'CapabilityStatement.format', SearchXpathUsageNormal);
  indexes.add('CapabilityStatement', 'guide', 'Implementation guides supported', SearchParamTypeURI, [], 'CapabilityStatement.implementationGuide', SearchXpathUsageNormal);
  indexes.add('CapabilityStatement', 'jurisdiction', 'Intended jurisdiction for the capability statement', SearchParamTypeTOKEN, [], 'CapabilityStatement.jurisdiction', SearchXpathUsageNormal);
  indexes.add('CapabilityStatement', 'mode', 'Mode - restful (server/client) or messaging (sender/receiver)', SearchParamTypeTOKEN, [], 'CapabilityStatement.rest.mode', SearchXpathUsageNormal);
  indexes.add('CapabilityStatement', 'name', 'Computationally friendly name of the capability statement', SearchParamTypeSTRING, [], 'CapabilityStatement.name', SearchXpathUsageNormal);
  indexes.add('CapabilityStatement', 'publisher', 'Name of the publisher of the capability statement', SearchParamTypeSTRING, [], 'CapabilityStatement.publisher', SearchXpathUsageNormal);
  indexes.add('CapabilityStatement', 'resource', 'Name of a resource mentioned in a capability statement', SearchParamTypeTOKEN, [], 'CapabilityStatement.rest.resource.type', SearchXpathUsageNormal);
  indexes.add('CapabilityStatement', 'resource-profile', 'A profile id invoked in a capability statement', SearchParamTypeREFERENCE, ['StructureDefinition'], 'CapabilityStatement.rest.resource.profile', SearchXpathUsageNormal);
  indexes.add('CapabilityStatement', 'security-service', 'OAuth | SMART-on-FHIR | NTLM | Basic | Kerberos | Certificates', SearchParamTypeTOKEN, [], 'CapabilityStatement.rest.security.service', SearchXpathUsageNormal);
  indexes.add('CapabilityStatement', 'software', 'Part of a the name of a software application', SearchParamTypeSTRING, [], 'CapabilityStatement.software.name', SearchXpathUsageNormal);
  indexes.add('CapabilityStatement', 'status', 'The current status of the capability statement', SearchParamTypeTOKEN, [], 'CapabilityStatement.status', SearchXpathUsageNormal);
  indexes.add('CapabilityStatement', 'supported-profile', 'Profiles for use cases supported', SearchParamTypeREFERENCE, ['StructureDefinition'], 'CapabilityStatement.profile', SearchXpathUsageNormal);
  indexes.add('CapabilityStatement', 'title', 'The human-friendly name of the capability statement', SearchParamTypeSTRING, [], 'CapabilityStatement.title', SearchXpathUsageNormal);
  indexes.add('CapabilityStatement', 'url', 'The uri that identifies the capability statement', SearchParamTypeURI, [], 'CapabilityStatement.url', SearchXpathUsageNormal);
  indexes.add('CapabilityStatement', 'version', 'The business version of the capability statement', SearchParamTypeTOKEN, [], 'CapabilityStatement.version', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_CAREPLAN}
procedure TFHIRIndexBuilder.buildIndexesForCarePlan(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('CarePlan', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('CarePlan', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('CarePlan', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('CarePlan', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('CarePlan', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('CarePlan', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('CarePlan', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('CarePlan', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('CarePlan', 'activity-code', 'Detail type of activity', SearchParamTypeTOKEN, [], 'CarePlan.activity.detail.code', SearchXpathUsageNormal);
  indexes.add('CarePlan', 'activity-date', 'Specified date occurs within period specified by CarePlan.activity.timingSchedule', SearchParamTypeDATE, [], 'CarePlan.activity.detail.scheduled', SearchXpathUsageNormal);
  indexes.add('CarePlan', 'activity-reference', 'Activity details defined in specific resource', SearchParamTypeREFERENCE, ['Appointment', 'ReferralRequest', 'MedicationRequest', 'Task', 'NutritionOrder', 'RequestGroup', 'VisionPrescription', 'ProcedureRequest', 'DeviceRequest', 'CommunicationRequest'], 'CarePlan.activity.reference', SearchXpathUsageNormal);
  indexes.add('CarePlan', 'based-on', 'Fulfills care plan', SearchParamTypeREFERENCE, ['CarePlan'], 'CarePlan.basedOn', SearchXpathUsageNormal);
  indexes.add('CarePlan', 'care-team', 'Who''s involved in plan?', SearchParamTypeREFERENCE, ['CareTeam'], 'CarePlan.careTeam', SearchXpathUsageNormal);
  indexes.add('CarePlan', 'category', 'Type of plan', SearchParamTypeTOKEN, [], 'CarePlan.category', SearchXpathUsageNormal);
  indexes.add('CarePlan', 'condition', 'Health issues this plan addresses', SearchParamTypeREFERENCE, ['Condition'], 'CarePlan.addresses', SearchXpathUsageNormal);
  indexes.add('CarePlan', 'context', 'Created in context of', SearchParamTypeREFERENCE, ['EpisodeOfCare', 'Encounter'], 'CarePlan.context', SearchXpathUsageNormal);
  indexes.add('CarePlan', 'date', 'Time period plan covers', SearchParamTypeDATE, [], 'Consent.dateTime | SupplyRequest.authoredOn | RiskAssessment.occurrence.as(DateTime) | CareTeam.period | FamilyMemberHistory.date | Encounter.period | AllergyIntolerance.assertedDate | CarePlan.period | EpisodeOfCare.period | Procedure.performed | Li'+'st.date | Immunization.date | Flag.period | Observation.effective | DiagnosticReport.effective | Composition.date | DetectedIssue.date | ClinicalImpression.date', SearchXpathUsageNormal);
  indexes.add('CarePlan', 'definition', 'Protocol or definition', SearchParamTypeREFERENCE, ['Questionnaire', 'PlanDefinition'], 'CarePlan.definition', SearchXpathUsageNormal);
  indexes.add('CarePlan', 'encounter', 'Created in context of', SearchParamTypeREFERENCE, ['Encounter'], 'CarePlan.context', SearchXpathUsageNormal);
  indexes.add('CarePlan', 'goal', 'Desired outcome of plan', SearchParamTypeREFERENCE, ['Goal'], 'CarePlan.goal', SearchXpathUsageNormal);
  indexes.add('CarePlan', 'identifier', 'External Ids for this plan', SearchParamTypeTOKEN, [], 'DocumentManifest.masterIdentifier | DocumentManifest.identifier | Goal.identifier | Consent.identifier | DocumentReference.masterIdentifier | DocumentReference.identifier | SupplyRequest.identifier | RiskAssessment.identifier | CareTeam.identifier | '+'ImagingStudy.identifier | FamilyMemberHistory.identifier | Encounter.identifier | DeviceRequest.identifier | AllergyIntolerance.identifier | CarePlan.identifier | EpisodeOfCare.identifier | Procedure.identifier | List.identifier | Immunization.identi'+'fier | VisionPrescription.identifier | ProcedureRequest.identifier | Observation.identifier | DiagnosticReport.identifier | NutritionOrder.identifier | Condition.identifier | Composition.identifier | DetectedIssue.identifier | SupplyDelivery.identifi'+'er', SearchXpathUsageNormal);
  indexes.add('CarePlan', 'intent', 'proposal | plan | order | option', SearchParamTypeTOKEN, [], 'CarePlan.intent', SearchXpathUsageNormal);
  indexes.add('CarePlan', 'part-of', 'Part of referenced CarePlan', SearchParamTypeREFERENCE, ['CarePlan'], 'CarePlan.partOf', SearchXpathUsageNormal);
  indexes.add('CarePlan', 'patient', 'Who care plan is for', SearchParamTypeREFERENCE, ['Group', 'Patient'], 'ReferralRequest.subject | DocumentManifest.subject | Goal.subject | Consent.patient | DocumentReference.subject | ImagingManifest.patient | RiskAssessment.subject | CareTeam.subject | ImagingStudy.patient | FamilyMemberHistory.patient | Encounter.sub'+'ject | DeviceUseStatement.subject | DeviceRequest.subject | AllergyIntolerance.patient | CarePlan.subject | EpisodeOfCare.patient | Procedure.subject | List.subject | Immunization.patient | VisionPrescription.patient | ProcedureRequest.subject | Flag'+'.subject | Observation.subject | DiagnosticReport.subject | NutritionOrder.patient | Condition.subject | Composition.subject | DetectedIssue.patient | SupplyDelivery.patient | ClinicalImpression.subject', SearchXpathUsageNormal);
  indexes.add('CarePlan', 'performer', 'Matches if the practitioner is listed as a performer in any of the "simple" activities.  (For performers of the detailed activities, chain through the activitydetail search parameter.)', SearchParamTypeREFERENCE, ['Practitioner', 'Organization', 'CareTeam', 'Patient', 'RelatedPerson'], 'CarePlan.activity.detail.performer', SearchXpathUsageNormal);
  indexes.add('CarePlan', 'replaces', 'CarePlan replaced by this CarePlan', SearchParamTypeREFERENCE, ['CarePlan'], 'CarePlan.replaces', SearchXpathUsageNormal);
  indexes.add('CarePlan', 'status', 'draft | active | suspended | completed | entered-in-error | cancelled | unknown', SearchParamTypeTOKEN, [], 'CarePlan.status', SearchXpathUsageNormal);
  indexes.add('CarePlan', 'subject', 'Who care plan is for', SearchParamTypeREFERENCE, ['Group', 'Patient'], 'CarePlan.subject', SearchXpathUsageNormal);
  compartments.register(frtPatient, 'CarePlan', ['patient', 'performer']);
  compartments.register(frtPractitioner, 'CarePlan', ['performer']);
  compartments.register(frtRelatedPerson, 'CarePlan', ['performer']);
end;
{$ENDIF}

{$IFDEF FHIR_CARETEAM}
procedure TFHIRIndexBuilder.buildIndexesForCareTeam(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('CareTeam', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('CareTeam', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('CareTeam', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('CareTeam', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('CareTeam', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('CareTeam', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('CareTeam', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('CareTeam', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('CareTeam', 'category', 'Type of team', SearchParamTypeTOKEN, [], 'CareTeam.category', SearchXpathUsageNormal);
  indexes.add('CareTeam', 'context', 'Encounter or episode associated with CareTeam', SearchParamTypeREFERENCE, ['EpisodeOfCare', 'Encounter'], 'CareTeam.context', SearchXpathUsageNormal);
  indexes.add('CareTeam', 'date', 'Time period team covers', SearchParamTypeDATE, [], 'Consent.dateTime | SupplyRequest.authoredOn | RiskAssessment.occurrence.as(DateTime) | CareTeam.period | FamilyMemberHistory.date | Encounter.period | AllergyIntolerance.assertedDate | CarePlan.period | EpisodeOfCare.period | Procedure.performed | Li'+'st.date | Immunization.date | Flag.period | Observation.effective | DiagnosticReport.effective | Composition.date | DetectedIssue.date | ClinicalImpression.date', SearchXpathUsageNormal);
  indexes.add('CareTeam', 'encounter', 'Encounter or episode associated with CareTeam', SearchParamTypeREFERENCE, ['Encounter'], 'CareTeam.context', SearchXpathUsageNormal);
  indexes.add('CareTeam', 'identifier', 'External Ids for this team', SearchParamTypeTOKEN, [], 'DocumentManifest.masterIdentifier | DocumentManifest.identifier | Goal.identifier | Consent.identifier | DocumentReference.masterIdentifier | DocumentReference.identifier | SupplyRequest.identifier | RiskAssessment.identifier | CareTeam.identifier | '+'ImagingStudy.identifier | FamilyMemberHistory.identifier | Encounter.identifier | DeviceRequest.identifier | AllergyIntolerance.identifier | CarePlan.identifier | EpisodeOfCare.identifier | Procedure.identifier | List.identifier | Immunization.identi'+'fier | VisionPrescription.identifier | ProcedureRequest.identifier | Observation.identifier | DiagnosticReport.identifier | NutritionOrder.identifier | Condition.identifier | Composition.identifier | DetectedIssue.identifier | SupplyDelivery.identifi'+'er', SearchXpathUsageNormal);
  indexes.add('CareTeam', 'participant', 'Who is involved', SearchParamTypeREFERENCE, ['Practitioner', 'Organization', 'CareTeam', 'Patient', 'RelatedPerson'], 'CareTeam.participant.member', SearchXpathUsageNormal);
  indexes.add('CareTeam', 'patient', 'Who care team is for', SearchParamTypeREFERENCE, ['Group', 'Patient'], 'ReferralRequest.subject | DocumentManifest.subject | Goal.subject | Consent.patient | DocumentReference.subject | ImagingManifest.patient | RiskAssessment.subject | CareTeam.subject | ImagingStudy.patient | FamilyMemberHistory.patient | Encounter.sub'+'ject | DeviceUseStatement.subject | DeviceRequest.subject | AllergyIntolerance.patient | CarePlan.subject | EpisodeOfCare.patient | Procedure.subject | List.subject | Immunization.patient | VisionPrescription.patient | ProcedureRequest.subject | Flag'+'.subject | Observation.subject | DiagnosticReport.subject | NutritionOrder.patient | Condition.subject | Composition.subject | DetectedIssue.patient | SupplyDelivery.patient | ClinicalImpression.subject', SearchXpathUsageNormal);
  indexes.add('CareTeam', 'status', 'proposed | active | suspended | inactive | entered-in-error', SearchParamTypeTOKEN, [], 'CareTeam.status', SearchXpathUsageNormal);
  indexes.add('CareTeam', 'subject', 'Who care team is for', SearchParamTypeREFERENCE, ['Group', 'Patient'], 'CareTeam.subject', SearchXpathUsageNormal);
  compartments.register(frtPatient, 'CareTeam', ['patient', 'participant']);
  compartments.register(frtPractitioner, 'CareTeam', ['participant']);
  compartments.register(frtRelatedPerson, 'CareTeam', ['participant']);
end;
{$ENDIF}

{$IFDEF FHIR_CHARGEITEM}
procedure TFHIRIndexBuilder.buildIndexesForChargeItem(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ChargeItem', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('ChargeItem', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('ChargeItem', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('ChargeItem', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('ChargeItem', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ChargeItem', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('ChargeItem', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('ChargeItem', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('ChargeItem', 'account', 'Account to place this charge', SearchParamTypeREFERENCE, ['Account'], 'ChargeItem.account', SearchXpathUsageNormal);
  indexes.add('ChargeItem', 'code', 'A code that identifies the charge, like a billing code', SearchParamTypeTOKEN, [], 'ChargeItem.code', SearchXpathUsageNormal);
  indexes.add('ChargeItem', 'context', 'Encounter / Episode associated with event', SearchParamTypeREFERENCE, ['EpisodeOfCare', 'Encounter'], 'ChargeItem.context', SearchXpathUsageNormal);
  indexes.add('ChargeItem', 'entered-date', 'Date the charge item was entered', SearchParamTypeDATE, [], 'ChargeItem.enteredDate', SearchXpathUsageNormal);
  indexes.add('ChargeItem', 'enterer', 'Individual who was entering', SearchParamTypeREFERENCE, ['Practitioner', 'Organization', 'Device', 'Patient', 'RelatedPerson'], 'ChargeItem.enterer', SearchXpathUsageNormal);
  indexes.add('ChargeItem', 'factor-override', 'Factor overriding the associated rules', SearchParamTypeNUMBER, [], 'ChargeItem.factorOverride', SearchXpathUsageNormal);
  indexes.add('ChargeItem', 'identifier', 'Business Identifier for item', SearchParamTypeTOKEN, [], 'ChargeItem.identifier', SearchXpathUsageNormal);
  indexes.add('ChargeItem', 'occurrence', 'When the charged service was applied', SearchParamTypeDATE, [], 'ChargeItem.occurrence', SearchXpathUsageNormal);
  indexes.add('ChargeItem', 'participant-actor', 'Individual who was performing', SearchParamTypeREFERENCE, ['Practitioner', 'Organization', 'Device', 'Patient', 'RelatedPerson'], 'ChargeItem.participant.actor', SearchXpathUsageNormal);
  indexes.add('ChargeItem', 'participant-role', 'What type of performance was done', SearchParamTypeTOKEN, [], 'ChargeItem.participant.role', SearchXpathUsageNormal);
  indexes.add('ChargeItem', 'patient', 'Individual service was done for/to', SearchParamTypeREFERENCE, ['Patient'], 'ChargeItem.subject', SearchXpathUsageNormal);
  indexes.add('ChargeItem', 'performing-organization', 'Organization providing the charged sevice', SearchParamTypeREFERENCE, ['Organization'], 'ChargeItem.performingOrganization', SearchXpathUsageNormal);
  indexes.add('ChargeItem', 'price-override', 'Price overriding the associated rules', SearchParamTypeQUANTITY, [], 'ChargeItem.priceOverride', SearchXpathUsageNormal);
  indexes.add('ChargeItem', 'quantity', 'Quantity of which the charge item has been serviced', SearchParamTypeQUANTITY, [], 'ChargeItem.quantity', SearchXpathUsageNormal);
  indexes.add('ChargeItem', 'requesting-organization', 'Organization requesting the charged service', SearchParamTypeREFERENCE, ['Organization'], 'ChargeItem.requestingOrganization', SearchXpathUsageNormal);
  indexes.add('ChargeItem', 'service', 'Which rendered service is being charged?', SearchParamTypeREFERENCE, ['Immunization', 'MedicationDispense', 'SupplyDelivery', 'Observation', 'DiagnosticReport', 'ImagingStudy', 'MedicationAdministration', 'Procedure'], 'ChargeItem.service', SearchXpathUsageNormal);
  indexes.add('ChargeItem', 'subject', 'Individual service was done for/to', SearchParamTypeREFERENCE, ['Group', 'Patient'], 'ChargeItem.subject', SearchXpathUsageNormal);
  compartments.register(frtDevice, 'ChargeItem', ['enterer', 'participant-actor']);
  compartments.register(frtEncounter, 'ChargeItem', ['context']);
  compartments.register(frtPatient, 'ChargeItem', ['subject']);
  compartments.register(frtPractitioner, 'ChargeItem', ['enterer', 'participant-actor']);
  compartments.register(frtRelatedPerson, 'ChargeItem', ['enterer', 'participant-actor']);
end;
{$ENDIF}

{$IFDEF FHIR_CLAIM}
procedure TFHIRIndexBuilder.buildIndexesForClaim(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Claim', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Claim', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('Claim', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('Claim', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('Claim', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Claim', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('Claim', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('Claim', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Claim', 'care-team', 'Member of the CareTeam', SearchParamTypeREFERENCE, ['Practitioner', 'Organization'], 'Claim.careTeam.provider', SearchXpathUsageNormal);
  indexes.add('Claim', 'created', 'The creation date for the Claim', SearchParamTypeDATE, [], 'Claim.created', SearchXpathUsageNormal);
  indexes.add('Claim', 'encounter', 'Encounters associated with a billed line item', SearchParamTypeREFERENCE, ['Encounter'], 'Claim.item.encounter', SearchXpathUsageNormal);
  indexes.add('Claim', 'enterer', 'The party responsible for the entry of the Claim', SearchParamTypeREFERENCE, ['Practitioner'], 'Claim.enterer', SearchXpathUsageNormal);
  indexes.add('Claim', 'facility', 'Facility responsible for the goods and services', SearchParamTypeREFERENCE, ['Location'], 'Claim.facility', SearchXpathUsageNormal);
  indexes.add('Claim', 'identifier', 'The primary identifier of the financial resource', SearchParamTypeTOKEN, [], 'Claim.identifier', SearchXpathUsageNormal);
  indexes.add('Claim', 'insurer', 'The target payor/insurer for the Claim', SearchParamTypeREFERENCE, ['Organization'], 'Claim.insurer', SearchXpathUsageNormal);
  indexes.add('Claim', 'organization', 'The reference to the providing organization', SearchParamTypeREFERENCE, ['Organization'], 'Claim.organization', SearchXpathUsageNormal);
  indexes.add('Claim', 'patient', 'Patient receiving the services', SearchParamTypeREFERENCE, ['Patient'], 'Claim.patient', SearchXpathUsageNormal);
  indexes.add('Claim', 'payee', 'The party receiving any payment for the Claim', SearchParamTypeREFERENCE, ['Practitioner', 'Organization', 'Patient', 'RelatedPerson'], 'Claim.payee.party', SearchXpathUsageNormal);
  indexes.add('Claim', 'priority', 'Processing priority requested', SearchParamTypeTOKEN, [], 'Claim.priority', SearchXpathUsageNormal);
  indexes.add('Claim', 'provider', 'Provider responsible for the Claim', SearchParamTypeREFERENCE, ['Practitioner'], 'Claim.provider', SearchXpathUsageNormal);
  indexes.add('Claim', 'use', 'The kind of financial resource', SearchParamTypeTOKEN, [], 'Claim.use', SearchXpathUsageNormal);
  compartments.register(frtEncounter, 'Claim', ['encounter']);
  compartments.register(frtPatient, 'Claim', ['patient', 'payee']);
  compartments.register(frtPractitioner, 'Claim', ['enterer', 'provider', 'payee', 'care-team']);
  compartments.register(frtRelatedPerson, 'Claim', ['payee']);
end;
{$ENDIF}

{$IFDEF FHIR_CLAIMRESPONSE}
procedure TFHIRIndexBuilder.buildIndexesForClaimResponse(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ClaimResponse', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('ClaimResponse', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('ClaimResponse', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('ClaimResponse', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('ClaimResponse', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ClaimResponse', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('ClaimResponse', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('ClaimResponse', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('ClaimResponse', 'created', 'The creation date', SearchParamTypeDATE, [], 'ClaimResponse.created', SearchXpathUsageNormal);
  indexes.add('ClaimResponse', 'disposition', 'The contents of the disposition message', SearchParamTypeSTRING, [], 'ClaimResponse.disposition', SearchXpathUsageNormal);
  indexes.add('ClaimResponse', 'identifier', 'The identity of the claimresponse', SearchParamTypeTOKEN, [], 'ClaimResponse.identifier', SearchXpathUsageNormal);
  indexes.add('ClaimResponse', 'insurer', 'The organization who generated this resource', SearchParamTypeREFERENCE, ['Organization'], 'ClaimResponse.insurer', SearchXpathUsageNormal);
  indexes.add('ClaimResponse', 'outcome', 'The processing outcome', SearchParamTypeTOKEN, [], 'ClaimResponse.outcome', SearchXpathUsageNormal);
  indexes.add('ClaimResponse', 'patient', 'The subject of care.', SearchParamTypeREFERENCE, ['Patient'], 'ClaimResponse.patient', SearchXpathUsageNormal);
  indexes.add('ClaimResponse', 'payment-date', 'The expected paymentDate', SearchParamTypeDATE, [], 'ClaimResponse.payment.date', SearchXpathUsageNormal);
  indexes.add('ClaimResponse', 'request', 'The claim reference', SearchParamTypeREFERENCE, ['Claim'], 'ClaimResponse.request', SearchXpathUsageNormal);
  indexes.add('ClaimResponse', 'request-provider', 'The Provider of the claim', SearchParamTypeREFERENCE, ['Practitioner'], 'ClaimResponse.requestProvider', SearchXpathUsageNormal);
  compartments.register(frtPatient, 'ClaimResponse', ['patient']);
  compartments.register(frtPractitioner, 'ClaimResponse', ['request-provider']);
end;
{$ENDIF}

{$IFDEF FHIR_CLINICALIMPRESSION}
procedure TFHIRIndexBuilder.buildIndexesForClinicalImpression(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ClinicalImpression', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('ClinicalImpression', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('ClinicalImpression', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('ClinicalImpression', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('ClinicalImpression', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ClinicalImpression', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('ClinicalImpression', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('ClinicalImpression', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('ClinicalImpression', 'action', 'Action taken as part of assessment procedure', SearchParamTypeREFERENCE, ['Appointment', 'ReferralRequest', 'MedicationRequest', 'ProcedureRequest', 'Procedure'], 'ClinicalImpression.action', SearchXpathUsageNormal);
  indexes.add('ClinicalImpression', 'assessor', 'The clinician performing the assessment', SearchParamTypeREFERENCE, ['Practitioner'], 'ClinicalImpression.assessor', SearchXpathUsageNormal);
  indexes.add('ClinicalImpression', 'context', 'Encounter or Episode created from', SearchParamTypeREFERENCE, ['EpisodeOfCare', 'Encounter'], 'ClinicalImpression.context', SearchXpathUsageNormal);
  indexes.add('ClinicalImpression', 'date', 'When the assessment was documented', SearchParamTypeDATE, [], 'Consent.dateTime | SupplyRequest.authoredOn | RiskAssessment.occurrence.as(DateTime) | CareTeam.period | FamilyMemberHistory.date | Encounter.period | AllergyIntolerance.assertedDate | CarePlan.period | EpisodeOfCare.period | Procedure.performed | Li'+'st.date | Immunization.date | Flag.period | Observation.effective | DiagnosticReport.effective | Composition.date | DetectedIssue.date | ClinicalImpression.date', SearchXpathUsageNormal);
  indexes.add('ClinicalImpression', 'finding-code', 'What was found', SearchParamTypeTOKEN, [], 'ClinicalImpression.finding.item.as(CodeableConcept)', SearchXpathUsageNormal);
  indexes.add('ClinicalImpression', 'finding-ref', 'What was found', SearchParamTypeREFERENCE, ['Condition', 'Observation'], 'ClinicalImpression.finding.item.as(Reference)', SearchXpathUsageNormal);
  indexes.add('ClinicalImpression', 'identifier', 'Business identifier', SearchParamTypeTOKEN, [], 'ClinicalImpression.identifier', SearchXpathUsageNormal);
  indexes.add('ClinicalImpression', 'investigation', 'Record of a specific investigation', SearchParamTypeREFERENCE, ['RiskAssessment', 'FamilyMemberHistory', 'Observation', 'DiagnosticReport', 'ImagingStudy', 'QuestionnaireResponse'], 'ClinicalImpression.investigation.item', SearchXpathUsageNormal);
  indexes.add('ClinicalImpression', 'patient', 'Patient or group assessed', SearchParamTypeREFERENCE, ['Group', 'Patient'], 'ReferralRequest.subject | DocumentManifest.subject | Goal.subject | Consent.patient | DocumentReference.subject | ImagingManifest.patient | RiskAssessment.subject | CareTeam.subject | ImagingStudy.patient | FamilyMemberHistory.patient | Encounter.sub'+'ject | DeviceUseStatement.subject | DeviceRequest.subject | AllergyIntolerance.patient | CarePlan.subject | EpisodeOfCare.patient | Procedure.subject | List.subject | Immunization.patient | VisionPrescription.patient | ProcedureRequest.subject | Flag'+'.subject | Observation.subject | DiagnosticReport.subject | NutritionOrder.patient | Condition.subject | Composition.subject | DetectedIssue.patient | SupplyDelivery.patient | ClinicalImpression.subject', SearchXpathUsageNormal);
  indexes.add('ClinicalImpression', 'previous', 'Reference to last assessment', SearchParamTypeREFERENCE, ['ClinicalImpression'], 'ClinicalImpression.previous', SearchXpathUsageNormal);
  indexes.add('ClinicalImpression', 'problem', 'Relevant impressions of patient state', SearchParamTypeREFERENCE, ['Condition', 'AllergyIntolerance'], 'ClinicalImpression.problem', SearchXpathUsageNormal);
  indexes.add('ClinicalImpression', 'status', 'draft | completed | entered-in-error', SearchParamTypeTOKEN, [], 'ClinicalImpression.status', SearchXpathUsageNormal);
  indexes.add('ClinicalImpression', 'subject', 'Patient or group assessed', SearchParamTypeREFERENCE, ['Group', 'Patient'], 'ClinicalImpression.subject', SearchXpathUsageNormal);
  compartments.register(frtEncounter, 'ClinicalImpression', ['context']);
  compartments.register(frtPatient, 'ClinicalImpression', ['subject']);
  compartments.register(frtPractitioner, 'ClinicalImpression', ['assessor']);
end;
{$ENDIF}

{$IFDEF FHIR_CODESYSTEM}
procedure TFHIRIndexBuilder.buildIndexesForCodeSystem(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('CodeSystem', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('CodeSystem', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('CodeSystem', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('CodeSystem', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('CodeSystem', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('CodeSystem', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('CodeSystem', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('CodeSystem', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('CodeSystem', 'code', 'A code defined in the code system', SearchParamTypeTOKEN, [], 'CodeSystem.concept.code', SearchXpathUsageNormal);
  indexes.add('CodeSystem', 'content-mode', 'not-present | example | fragment | complete', SearchParamTypeTOKEN, [], 'CodeSystem.content', SearchXpathUsageNormal);
  indexes.add('CodeSystem', 'date', 'The code system publication date', SearchParamTypeDATE, [], 'CodeSystem.date', SearchXpathUsageNormal);
  indexes.add('CodeSystem', 'description', 'The description of the code system', SearchParamTypeSTRING, [], 'CodeSystem.description', SearchXpathUsageNormal);
  indexes.add('CodeSystem', 'identifier', 'External identifier for the code system', SearchParamTypeTOKEN, [], 'CodeSystem.identifier', SearchXpathUsageNormal);
  indexes.add('CodeSystem', 'jurisdiction', 'Intended jurisdiction for the code system', SearchParamTypeTOKEN, [], 'CodeSystem.jurisdiction', SearchXpathUsageNormal);
  indexes.add('CodeSystem', 'language', 'A language in which a designation is provided', SearchParamTypeTOKEN, [], 'CodeSystem.concept.designation.language', SearchXpathUsageNormal);
  indexes.add('CodeSystem', 'name', 'Computationally friendly name of the code system', SearchParamTypeSTRING, [], 'CodeSystem.name', SearchXpathUsageNormal);
  indexes.add('CodeSystem', 'publisher', 'Name of the publisher of the code system', SearchParamTypeSTRING, [], 'CodeSystem.publisher', SearchXpathUsageNormal);
  indexes.add('CodeSystem', 'status', 'The current status of the code system', SearchParamTypeTOKEN, [], 'CodeSystem.status', SearchXpathUsageNormal);
  indexes.add('CodeSystem', 'system', 'The system for any codes defined by this code system (same as ''url'')', SearchParamTypeURI, [], 'CodeSystem.url', SearchXpathUsageNormal);
  indexes.add('CodeSystem', 'title', 'The human-friendly name of the code system', SearchParamTypeSTRING, [], 'CodeSystem.title', SearchXpathUsageNormal);
  indexes.add('CodeSystem', 'url', 'The uri that identifies the code system', SearchParamTypeURI, [], 'CodeSystem.url', SearchXpathUsageNormal);
  indexes.add('CodeSystem', 'version', 'The business version of the code system', SearchParamTypeTOKEN, [], 'CodeSystem.version', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_COMMUNICATION}
procedure TFHIRIndexBuilder.buildIndexesForCommunication(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Communication', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Communication', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('Communication', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('Communication', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('Communication', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Communication', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('Communication', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('Communication', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Communication', 'based-on', 'Request fulfilled by this communication', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'Communication.basedOn', SearchXpathUsageNormal);
  indexes.add('Communication', 'category', 'Message category', SearchParamTypeTOKEN, [], 'Communication.category', SearchXpathUsageNormal);
  indexes.add('Communication', 'context', 'Encounter or episode leading to message', SearchParamTypeREFERENCE, ['EpisodeOfCare', 'Encounter'], 'Communication.context', SearchXpathUsageNormal);
  indexes.add('Communication', 'definition', 'Instantiates protocol or definition', SearchParamTypeREFERENCE, ['PlanDefinition', 'ActivityDefinition'], 'Communication.definition', SearchXpathUsageNormal);
  indexes.add('Communication', 'encounter', 'Encounter leading to message', SearchParamTypeREFERENCE, ['Encounter'], 'Communication.context', SearchXpathUsageNormal);
  indexes.add('Communication', 'identifier', 'Unique identifier', SearchParamTypeTOKEN, [], 'Communication.identifier', SearchXpathUsageNormal);
  indexes.add('Communication', 'medium', 'A channel of communication', SearchParamTypeTOKEN, [], 'Communication.medium', SearchXpathUsageNormal);
  indexes.add('Communication', 'part-of', 'Part of this action', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'Communication.partOf', SearchXpathUsageNormal);
  indexes.add('Communication', 'patient', 'Focus of message', SearchParamTypeREFERENCE, ['Patient'], 'Communication.subject', SearchXpathUsageNormal);
  indexes.add('Communication', 'received', 'When received', SearchParamTypeDATE, [], 'Communication.received', SearchXpathUsageNormal);
  indexes.add('Communication', 'recipient', 'Message recipient', SearchParamTypeREFERENCE, ['Practitioner', 'Group', 'Organization', 'Device', 'Patient', 'RelatedPerson'], 'Communication.recipient', SearchXpathUsageNormal);
  indexes.add('Communication', 'sender', 'Message sender', SearchParamTypeREFERENCE, ['Practitioner', 'Organization', 'Device', 'Patient', 'RelatedPerson'], 'Communication.sender', SearchXpathUsageNormal);
  indexes.add('Communication', 'sent', 'When sent', SearchParamTypeDATE, [], 'Communication.sent', SearchXpathUsageNormal);
  indexes.add('Communication', 'status', 'preparation | in-progress | suspended | aborted | completed | entered-in-error', SearchParamTypeTOKEN, [], 'Communication.status', SearchXpathUsageNormal);
  indexes.add('Communication', 'subject', 'Focus of message', SearchParamTypeREFERENCE, ['Group', 'Patient'], 'Communication.subject', SearchXpathUsageNormal);
  compartments.register(frtDevice, 'Communication', ['sender', 'recipient']);
  compartments.register(frtEncounter, 'Communication', ['context']);
  compartments.register(frtPatient, 'Communication', ['subject', 'sender', 'recipient']);
  compartments.register(frtPractitioner, 'Communication', ['sender', 'recipient']);
  compartments.register(frtRelatedPerson, 'Communication', ['sender', 'recipient']);
end;
{$ENDIF}

{$IFDEF FHIR_COMMUNICATIONREQUEST}
procedure TFHIRIndexBuilder.buildIndexesForCommunicationRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('CommunicationRequest', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('CommunicationRequest', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('CommunicationRequest', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('CommunicationRequest', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('CommunicationRequest', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('CommunicationRequest', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('CommunicationRequest', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('CommunicationRequest', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('CommunicationRequest', 'authored', 'When request transitioned to being actionable', SearchParamTypeDATE, [], 'CommunicationRequest.authoredOn', SearchXpathUsageNormal);
  indexes.add('CommunicationRequest', 'based-on', 'Fulfills plan or proposal', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'CommunicationRequest.basedOn', SearchXpathUsageNormal);
  indexes.add('CommunicationRequest', 'category', 'Message category', SearchParamTypeTOKEN, [], 'CommunicationRequest.category', SearchXpathUsageNormal);
  indexes.add('CommunicationRequest', 'context', 'Encounter or episode leading to message', SearchParamTypeREFERENCE, ['EpisodeOfCare', 'Encounter'], 'CommunicationRequest.context', SearchXpathUsageNormal);
  indexes.add('CommunicationRequest', 'encounter', 'Encounter leading to message', SearchParamTypeREFERENCE, ['Encounter'], 'CommunicationRequest.context', SearchXpathUsageNormal);
  indexes.add('CommunicationRequest', 'group-identifier', 'Composite request this is part of', SearchParamTypeTOKEN, [], 'CommunicationRequest.groupIdentifier', SearchXpathUsageNormal);
  indexes.add('CommunicationRequest', 'identifier', 'Unique identifier', SearchParamTypeTOKEN, [], 'CommunicationRequest.identifier', SearchXpathUsageNormal);
  indexes.add('CommunicationRequest', 'medium', 'A channel of communication', SearchParamTypeTOKEN, [], 'CommunicationRequest.medium', SearchXpathUsageNormal);
  indexes.add('CommunicationRequest', 'occurrence', 'When scheduled', SearchParamTypeDATE, [], 'CommunicationRequest.occurrence.as(DateTime)', SearchXpathUsageNormal);
  indexes.add('CommunicationRequest', 'patient', 'Focus of message', SearchParamTypeREFERENCE, ['Patient'], 'CommunicationRequest.subject', SearchXpathUsageNormal);
  indexes.add('CommunicationRequest', 'priority', 'Message urgency', SearchParamTypeTOKEN, [], 'CommunicationRequest.priority', SearchXpathUsageNormal);
  indexes.add('CommunicationRequest', 'recipient', 'Message recipient', SearchParamTypeREFERENCE, ['Practitioner', 'Group', 'Organization', 'CareTeam', 'Device', 'Patient', 'RelatedPerson'], 'CommunicationRequest.recipient', SearchXpathUsageNormal);
  indexes.add('CommunicationRequest', 'replaces', 'Request(s) replaced by this request', SearchParamTypeREFERENCE, ['CommunicationRequest'], 'CommunicationRequest.replaces', SearchXpathUsageNormal);
  indexes.add('CommunicationRequest', 'requester', 'Individual making the request', SearchParamTypeREFERENCE, ['Practitioner', 'Organization', 'Device', 'Patient', 'RelatedPerson'], 'CommunicationRequest.requester.agent', SearchXpathUsageNormal);
  indexes.add('CommunicationRequest', 'sender', 'Message sender', SearchParamTypeREFERENCE, ['Practitioner', 'Organization', 'Device', 'Patient', 'RelatedPerson'], 'CommunicationRequest.sender', SearchXpathUsageNormal);
  indexes.add('CommunicationRequest', 'status', 'draft | active | suspended | cancelled | completed | entered-in-error | unknown', SearchParamTypeTOKEN, [], 'CommunicationRequest.status', SearchXpathUsageNormal);
  indexes.add('CommunicationRequest', 'subject', 'Focus of message', SearchParamTypeREFERENCE, ['Group', 'Patient'], 'CommunicationRequest.subject', SearchXpathUsageNormal);
  compartments.register(frtDevice, 'CommunicationRequest', ['sender', 'recipient']);
  compartments.register(frtEncounter, 'CommunicationRequest', ['context']);
  compartments.register(frtPatient, 'CommunicationRequest', ['subject', 'sender', 'recipient', 'requester']);
  compartments.register(frtPractitioner, 'CommunicationRequest', ['sender', 'recipient', 'requester']);
  compartments.register(frtRelatedPerson, 'CommunicationRequest', ['sender', 'recipient', 'requester']);
end;
{$ENDIF}

{$IFDEF FHIR_COMPARTMENTDEFINITION}
procedure TFHIRIndexBuilder.buildIndexesForCompartmentDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('CompartmentDefinition', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('CompartmentDefinition', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('CompartmentDefinition', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('CompartmentDefinition', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('CompartmentDefinition', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('CompartmentDefinition', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('CompartmentDefinition', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('CompartmentDefinition', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('CompartmentDefinition', 'code', 'Patient | Encounter | RelatedPerson | Practitioner | Device', SearchParamTypeTOKEN, [], 'CompartmentDefinition.code', SearchXpathUsageNormal);
  indexes.add('CompartmentDefinition', 'date', 'The compartment definition publication date', SearchParamTypeDATE, [], 'CompartmentDefinition.date', SearchXpathUsageNormal);
  indexes.add('CompartmentDefinition', 'description', 'The description of the compartment definition', SearchParamTypeSTRING, [], 'CompartmentDefinition.description', SearchXpathUsageNormal);
  indexes.add('CompartmentDefinition', 'jurisdiction', 'Intended jurisdiction for the compartment definition', SearchParamTypeTOKEN, [], 'CompartmentDefinition.jurisdiction', SearchXpathUsageNormal);
  indexes.add('CompartmentDefinition', 'name', 'Computationally friendly name of the compartment definition', SearchParamTypeSTRING, [], 'CompartmentDefinition.name', SearchXpathUsageNormal);
  indexes.add('CompartmentDefinition', 'publisher', 'Name of the publisher of the compartment definition', SearchParamTypeSTRING, [], 'CompartmentDefinition.publisher', SearchXpathUsageNormal);
  indexes.add('CompartmentDefinition', 'resource', 'Name of resource type', SearchParamTypeTOKEN, [], 'CompartmentDefinition.resource.code', SearchXpathUsageNormal);
  indexes.add('CompartmentDefinition', 'status', 'The current status of the compartment definition', SearchParamTypeTOKEN, [], 'CompartmentDefinition.status', SearchXpathUsageNormal);
  indexes.add('CompartmentDefinition', 'title', 'The human-friendly name of the compartment definition', SearchParamTypeSTRING, [], 'CompartmentDefinition.title', SearchXpathUsageNormal);
  indexes.add('CompartmentDefinition', 'url', 'The uri that identifies the compartment definition', SearchParamTypeURI, [], 'CompartmentDefinition.url', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_COMPOSITION}
procedure TFHIRIndexBuilder.buildIndexesForComposition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Composition', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Composition', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('Composition', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('Composition', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('Composition', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Composition', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('Composition', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('Composition', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Composition', 'attester', 'Who attested the composition', SearchParamTypeREFERENCE, ['Practitioner', 'Organization', 'Patient'], 'Composition.attester.party', SearchXpathUsageNormal);
  indexes.add('Composition', 'author', 'Who and/or what authored the composition', SearchParamTypeREFERENCE, ['Practitioner', 'Device', 'Patient', 'RelatedPerson'], 'Composition.author', SearchXpathUsageNormal);
  indexes.add('Composition', 'class', 'Categorization of Composition', SearchParamTypeTOKEN, [], 'Composition.class', SearchXpathUsageNormal);
  indexes.add('Composition', 'confidentiality', 'As defined by affinity domain', SearchParamTypeTOKEN, [], 'Composition.confidentiality', SearchXpathUsageNormal);
  indexes.add('Composition', 'context', 'Code(s) that apply to the event being documented', SearchParamTypeTOKEN, [], 'Composition.event.code', SearchXpathUsageNormal);
  indexes.add('Composition', 'date', 'Composition editing time', SearchParamTypeDATE, [], 'Consent.dateTime | SupplyRequest.authoredOn | RiskAssessment.occurrence.as(DateTime) | CareTeam.period | FamilyMemberHistory.date | Encounter.period | AllergyIntolerance.assertedDate | CarePlan.period | EpisodeOfCare.period | Procedure.performed | Li'+'st.date | Immunization.date | Flag.period | Observation.effective | DiagnosticReport.effective | Composition.date | DetectedIssue.date | ClinicalImpression.date', SearchXpathUsageNormal);
  indexes.add('Composition', 'encounter', 'Context of the Composition', SearchParamTypeREFERENCE, ['EpisodeOfCare', 'Encounter'], 'DocumentReference.context.encounter | RiskAssessment.context | DeviceRequest.context | Procedure.context | List.encounter | VisionPrescription.encounter | ProcedureRequest.context | Flag.encounter | Observation.context | DiagnosticReport.context | Nu'+'tritionOrder.encounter | Composition.encounter', SearchXpathUsageNormal);
  indexes.add('Composition', 'entry', 'A reference to data that supports this section', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'Composition.section.entry', SearchXpathUsageNormal);
  indexes.add('Composition', 'identifier', 'Logical identifier of composition (version-independent)', SearchParamTypeTOKEN, [], 'DocumentManifest.masterIdentifier | DocumentManifest.identifier | Goal.identifier | Consent.identifier | DocumentReference.masterIdentifier | DocumentReference.identifier | SupplyRequest.identifier | RiskAssessment.identifier | CareTeam.identifier | '+'ImagingStudy.identifier | FamilyMemberHistory.identifier | Encounter.identifier | DeviceRequest.identifier | AllergyIntolerance.identifier | CarePlan.identifier | EpisodeOfCare.identifier | Procedure.identifier | List.identifier | Immunization.identi'+'fier | VisionPrescription.identifier | ProcedureRequest.identifier | Observation.identifier | DiagnosticReport.identifier | NutritionOrder.identifier | Condition.identifier | Composition.identifier | DetectedIssue.identifier | SupplyDelivery.identifi'+'er', SearchXpathUsageNormal);
  indexes.add('Composition', 'patient', 'Who and/or what the composition is about', SearchParamTypeREFERENCE, ['Patient'], 'ReferralRequest.subject | DocumentManifest.subject | Goal.subject | Consent.patient | DocumentReference.subject | ImagingManifest.patient | RiskAssessment.subject | CareTeam.subject | ImagingStudy.patient | FamilyMemberHistory.patient | Encounter.sub'+'ject | DeviceUseStatement.subject | DeviceRequest.subject | AllergyIntolerance.patient | CarePlan.subject | EpisodeOfCare.patient | Procedure.subject | List.subject | Immunization.patient | VisionPrescription.patient | ProcedureRequest.subject | Flag'+'.subject | Observation.subject | DiagnosticReport.subject | NutritionOrder.patient | Condition.subject | Composition.subject | DetectedIssue.patient | SupplyDelivery.patient | ClinicalImpression.subject', SearchXpathUsageNormal);
  indexes.add('Composition', 'period', 'The period covered by the documentation', SearchParamTypeDATE, [], 'Composition.event.period', SearchXpathUsageNormal);
  indexes.add('Composition', 'related-id', 'Target of the relationship', SearchParamTypeTOKEN, [], 'Composition.relatesTo.target.as(Identifier)', SearchXpathUsageNormal);
  indexes.add('Composition', 'related-ref', 'Target of the relationship', SearchParamTypeREFERENCE, ['Composition'], 'Composition.relatesTo.target.as(Reference)', SearchXpathUsageNormal);
  indexes.add('Composition', 'section', 'Classification of section (recommended)', SearchParamTypeTOKEN, [], 'Composition.section.code', SearchXpathUsageNormal);
  indexes.add('Composition', 'status', 'preliminary | final | amended | entered-in-error', SearchParamTypeTOKEN, [], 'Composition.status', SearchXpathUsageNormal);
  indexes.add('Composition', 'subject', 'Who and/or what the composition is about', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'Composition.subject', SearchXpathUsageNormal);
  indexes.add('Composition', 'title', 'Human Readable name/title', SearchParamTypeSTRING, [], 'Composition.title', SearchXpathUsageNormal);
  indexes.add('Composition', 'type', 'Kind of composition (LOINC if possible)', SearchParamTypeTOKEN, [], 'ReferralRequest.type | DocumentManifest.type | DocumentReference.type | Encounter.type | AllergyIntolerance.type | EpisodeOfCare.type | Composition.type', SearchXpathUsageNormal);
  compartments.register(frtDevice, 'Composition', ['author']);
  compartments.register(frtEncounter, 'Composition', ['encounter']);
  compartments.register(frtPatient, 'Composition', ['subject', 'author', 'attester']);
  compartments.register(frtPractitioner, 'Composition', ['subject', 'author', 'attester']);
  compartments.register(frtRelatedPerson, 'Composition', ['author']);
end;
{$ENDIF}

{$IFDEF FHIR_CONCEPTMAP}
procedure TFHIRIndexBuilder.buildIndexesForConceptMap(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ConceptMap', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('ConceptMap', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('ConceptMap', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('ConceptMap', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('ConceptMap', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ConceptMap', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('ConceptMap', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('ConceptMap', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('ConceptMap', 'date', 'The concept map publication date', SearchParamTypeDATE, [], 'ConceptMap.date', SearchXpathUsageNormal);
  indexes.add('ConceptMap', 'dependson', 'Reference to property mapping depends on', SearchParamTypeURI, [], 'ConceptMap.group.element.target.dependsOn.property', SearchXpathUsageNormal);
  indexes.add('ConceptMap', 'description', 'The description of the concept map', SearchParamTypeSTRING, [], 'ConceptMap.description', SearchXpathUsageNormal);
  indexes.add('ConceptMap', 'identifier', 'External identifier for the concept map', SearchParamTypeTOKEN, [], 'ConceptMap.identifier', SearchXpathUsageNormal);
  indexes.add('ConceptMap', 'jurisdiction', 'Intended jurisdiction for the concept map', SearchParamTypeTOKEN, [], 'ConceptMap.jurisdiction', SearchXpathUsageNormal);
  indexes.add('ConceptMap', 'name', 'Computationally friendly name of the concept map', SearchParamTypeSTRING, [], 'ConceptMap.name', SearchXpathUsageNormal);
  indexes.add('ConceptMap', 'other', 'Canonical URL for other concept map', SearchParamTypeURI, [], 'ConceptMap.group.unmapped.url', SearchXpathUsageNormal);
  indexes.add('ConceptMap', 'product', 'Reference to property mapping depends on', SearchParamTypeURI, [], 'ConceptMap.group.element.target.product.property', SearchXpathUsageNormal);
  indexes.add('ConceptMap', 'publisher', 'Name of the publisher of the concept map', SearchParamTypeSTRING, [], 'ConceptMap.publisher', SearchXpathUsageNormal);
  indexes.add('ConceptMap', 'source', 'Identifies the source of the concepts which are being mapped', SearchParamTypeREFERENCE, ['ValueSet'], 'ConceptMap.source.as(Reference)', SearchXpathUsageNormal);
  indexes.add('ConceptMap', 'source-code', 'Identifies element being mapped', SearchParamTypeTOKEN, [], 'ConceptMap.group.element.code', SearchXpathUsageNormal);
  indexes.add('ConceptMap', 'source-system', 'Code System (if value set crosses code systems)', SearchParamTypeURI, [], 'ConceptMap.group.source', SearchXpathUsageNormal);
  indexes.add('ConceptMap', 'source-uri', 'Identifies the source of the concepts which are being mapped', SearchParamTypeREFERENCE, ['ValueSet'], 'ConceptMap.source.as(Uri)', SearchXpathUsageNormal);
  indexes.add('ConceptMap', 'status', 'The current status of the concept map', SearchParamTypeTOKEN, [], 'ConceptMap.status', SearchXpathUsageNormal);
  indexes.add('ConceptMap', 'target', 'Provides context to the mappings', SearchParamTypeREFERENCE, ['ValueSet'], 'ConceptMap.target.as(Reference)', SearchXpathUsageNormal);
  indexes.add('ConceptMap', 'target-code', 'Code that identifies the target element', SearchParamTypeTOKEN, [], 'ConceptMap.group.element.target.code', SearchXpathUsageNormal);
  indexes.add('ConceptMap', 'target-system', 'System of the target (if necessary)', SearchParamTypeURI, [], 'ConceptMap.group.target', SearchXpathUsageNormal);
  indexes.add('ConceptMap', 'target-uri', 'Provides context to the mappings', SearchParamTypeREFERENCE, ['ValueSet'], 'ConceptMap.target.as(Uri)', SearchXpathUsageNormal);
  indexes.add('ConceptMap', 'title', 'The human-friendly name of the concept map', SearchParamTypeSTRING, [], 'ConceptMap.title', SearchXpathUsageNormal);
  indexes.add('ConceptMap', 'url', 'The uri that identifies the concept map', SearchParamTypeURI, [], 'ConceptMap.url', SearchXpathUsageNormal);
  indexes.add('ConceptMap', 'version', 'The business version of the concept map', SearchParamTypeTOKEN, [], 'ConceptMap.version', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_CONDITION}
procedure TFHIRIndexBuilder.buildIndexesForCondition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Condition', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Condition', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('Condition', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('Condition', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('Condition', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Condition', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('Condition', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('Condition', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Condition', 'abatement-age', 'Abatement as age or age range', SearchParamTypeQUANTITY, [], 'Condition.abatement.as(Age) | Condition.abatement.as(Range) | Condition.abatement.as(Age)', SearchXpathUsageNormal);
  indexes.add('Condition', 'abatement-boolean', 'Abatement boolean (boolean is true or non-boolean values are present)', SearchParamTypeTOKEN, [], 'Condition.abatement.as(boolean) | Condition.abatement.is(dateTime) | Condition.abatement.is(Age) | Condition.abatement.is(Period) | Condition.abatement.is(Range) | Condition.abatement.is(string)', SearchXpathUsageNormal);
  indexes.add('Condition', 'abatement-date', 'Date-related abatements (dateTime and period)', SearchParamTypeDATE, [], 'Condition.abatement.as(dateTime) | Condition.abatement.as(Period)', SearchXpathUsageNormal);
  indexes.add('Condition', 'abatement-string', 'Abatement as a string', SearchParamTypeSTRING, [], 'Condition.abatement.as(string)', SearchXpathUsageNormal);
  indexes.add('Condition', 'asserted-date', 'Date record was believed accurate', SearchParamTypeDATE, [], 'Condition.assertedDate', SearchXpathUsageNormal);
  indexes.add('Condition', 'asserter', 'Person who asserts this condition', SearchParamTypeREFERENCE, ['Practitioner', 'Patient', 'RelatedPerson'], 'Condition.asserter', SearchXpathUsageNormal);
  indexes.add('Condition', 'body-site', 'Anatomical location, if relevant', SearchParamTypeTOKEN, [], 'Condition.bodySite', SearchXpathUsageNormal);
  indexes.add('Condition', 'category', 'The category of the condition', SearchParamTypeTOKEN, [], 'Condition.category', SearchXpathUsageNormal);
  indexes.add('Condition', 'clinical-status', 'The clinical status of the condition', SearchParamTypeTOKEN, [], 'Condition.clinicalStatus', SearchXpathUsageNormal);
  indexes.add('Condition', 'code', 'Code for the condition', SearchParamTypeTOKEN, [], 'FamilyMemberHistory.condition.code | DeviceRequest.code.as(CodeableConcept) | AllergyIntolerance.code | AllergyIntolerance.reaction.substance | Procedure.code | List.code | ProcedureRequest.code | Observation.code | DiagnosticReport.code | Condition.'+'code', SearchXpathUsageNormal);
  indexes.add('Condition', 'context', 'Encounter or episode when condition first asserted', SearchParamTypeREFERENCE, ['EpisodeOfCare', 'Encounter'], 'Condition.context', SearchXpathUsageNormal);
  indexes.add('Condition', 'encounter', 'Encounter when condition first asserted', SearchParamTypeREFERENCE, ['Encounter'], 'Condition.context', SearchXpathUsageNormal);
  indexes.add('Condition', 'evidence', 'Manifestation/symptom', SearchParamTypeTOKEN, [], 'Condition.evidence.code', SearchXpathUsageNormal);
  indexes.add('Condition', 'evidence-detail', 'Supporting information found elsewhere', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'Condition.evidence.detail', SearchXpathUsageNormal);
  indexes.add('Condition', 'identifier', 'A unique identifier of the condition record', SearchParamTypeTOKEN, [], 'DocumentManifest.masterIdentifier | DocumentManifest.identifier | Goal.identifier | Consent.identifier | DocumentReference.masterIdentifier | DocumentReference.identifier | SupplyRequest.identifier | RiskAssessment.identifier | CareTeam.identifier | '+'ImagingStudy.identifier | FamilyMemberHistory.identifier | Encounter.identifier | DeviceRequest.identifier | AllergyIntolerance.identifier | CarePlan.identifier | EpisodeOfCare.identifier | Procedure.identifier | List.identifier | Immunization.identi'+'fier | VisionPrescription.identifier | ProcedureRequest.identifier | Observation.identifier | DiagnosticReport.identifier | NutritionOrder.identifier | Condition.identifier | Composition.identifier | DetectedIssue.identifier | SupplyDelivery.identifi'+'er', SearchXpathUsageNormal);
  indexes.add('Condition', 'onset-age', 'Onsets as age or age range', SearchParamTypeQUANTITY, [], 'Condition.onset.as(Age) | Condition.onset.as(Range)', SearchXpathUsageNormal);
  indexes.add('Condition', 'onset-date', 'Date related onsets (dateTime and Period)', SearchParamTypeDATE, [], 'Condition.onset.as(dateTime) | Condition.onset.as(Period)', SearchXpathUsageNormal);
  indexes.add('Condition', 'onset-info', 'Onsets as a string', SearchParamTypeSTRING, [], 'Condition.onset.as(string)', SearchXpathUsageNormal);
  indexes.add('Condition', 'patient', 'Who has the condition?', SearchParamTypeREFERENCE, ['Group', 'Patient'], 'ReferralRequest.subject | DocumentManifest.subject | Goal.subject | Consent.patient | DocumentReference.subject | ImagingManifest.patient | RiskAssessment.subject | CareTeam.subject | ImagingStudy.patient | FamilyMemberHistory.patient | Encounter.sub'+'ject | DeviceUseStatement.subject | DeviceRequest.subject | AllergyIntolerance.patient | CarePlan.subject | EpisodeOfCare.patient | Procedure.subject | List.subject | Immunization.patient | VisionPrescription.patient | ProcedureRequest.subject | Flag'+'.subject | Observation.subject | DiagnosticReport.subject | NutritionOrder.patient | Condition.subject | Composition.subject | DetectedIssue.patient | SupplyDelivery.patient | ClinicalImpression.subject', SearchXpathUsageNormal);
  indexes.add('Condition', 'severity', 'The severity of the condition', SearchParamTypeTOKEN, [], 'Condition.severity', SearchXpathUsageNormal);
  indexes.add('Condition', 'stage', 'Simple summary (disease specific)', SearchParamTypeTOKEN, [], 'Condition.stage.summary', SearchXpathUsageNormal);
  indexes.add('Condition', 'subject', 'Who has the condition?', SearchParamTypeREFERENCE, ['Group', 'Patient'], 'Condition.subject', SearchXpathUsageNormal);
  indexes.add('Condition', 'verification-status', 'provisional | differential | confirmed | refuted | entered-in-error | unknown', SearchParamTypeTOKEN, [], 'Condition.verificationStatus', SearchXpathUsageNormal);
  compartments.register(frtEncounter, 'Condition', ['context']);
  compartments.register(frtPatient, 'Condition', ['patient', 'asserter']);
  compartments.register(frtPractitioner, 'Condition', ['asserter']);
  compartments.register(frtRelatedPerson, 'Condition', ['asserter']);
end;
{$ENDIF}

{$IFDEF FHIR_CONSENT}
procedure TFHIRIndexBuilder.buildIndexesForConsent(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Consent', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Consent', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('Consent', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('Consent', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('Consent', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Consent', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('Consent', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('Consent', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Consent', 'action', 'Actions controlled by this consent', SearchParamTypeTOKEN, [], 'Consent.action | Consent.except.action', SearchXpathUsageNormal);
  indexes.add('Consent', 'actor', 'Resource for the actor (or group, by role)', SearchParamTypeREFERENCE, ['Practitioner', 'Group', 'Organization', 'CareTeam', 'Device', 'Patient', 'RelatedPerson'], 'Consent.actor.reference | Consent.except.actor.reference', SearchXpathUsageNormal);
  indexes.add('Consent', 'category', 'Classification of the consent statement - for indexing/retrieval', SearchParamTypeTOKEN, [], 'Consent.category', SearchXpathUsageNormal);
  indexes.add('Consent', 'consentor', 'Who is agreeing to the policy and exceptions', SearchParamTypeREFERENCE, ['Practitioner', 'Organization', 'Patient', 'RelatedPerson'], 'Consent.consentingParty', SearchXpathUsageNormal);
  indexes.add('Consent', 'data', 'The actual data reference', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'Consent.data.reference | Consent.except.data.reference', SearchXpathUsageNormal);
  indexes.add('Consent', 'date', 'When this Consent was created or indexed', SearchParamTypeDATE, [], 'Consent.dateTime | SupplyRequest.authoredOn | RiskAssessment.occurrence.as(DateTime) | CareTeam.period | FamilyMemberHistory.date | Encounter.period | AllergyIntolerance.assertedDate | CarePlan.period | EpisodeOfCare.period | Procedure.performed | Li'+'st.date | Immunization.date | Flag.period | Observation.effective | DiagnosticReport.effective | Composition.date | DetectedIssue.date | ClinicalImpression.date', SearchXpathUsageNormal);
  indexes.add('Consent', 'identifier', 'Identifier for this record (external references)', SearchParamTypeTOKEN, [], 'DocumentManifest.masterIdentifier | DocumentManifest.identifier | Goal.identifier | Consent.identifier | DocumentReference.masterIdentifier | DocumentReference.identifier | SupplyRequest.identifier | RiskAssessment.identifier | CareTeam.identifier | '+'ImagingStudy.identifier | FamilyMemberHistory.identifier | Encounter.identifier | DeviceRequest.identifier | AllergyIntolerance.identifier | CarePlan.identifier | EpisodeOfCare.identifier | Procedure.identifier | List.identifier | Immunization.identi'+'fier | VisionPrescription.identifier | ProcedureRequest.identifier | Observation.identifier | DiagnosticReport.identifier | NutritionOrder.identifier | Condition.identifier | Composition.identifier | DetectedIssue.identifier | SupplyDelivery.identifi'+'er', SearchXpathUsageNormal);
  indexes.add('Consent', 'organization', 'Custodian of the consent', SearchParamTypeREFERENCE, ['Organization'], 'Consent.organization', SearchXpathUsageNormal);
  indexes.add('Consent', 'patient', 'Who the consent applies to', SearchParamTypeREFERENCE, ['Group', 'Patient'], 'ReferralRequest.subject | DocumentManifest.subject | Goal.subject | Consent.patient | DocumentReference.subject | ImagingManifest.patient | RiskAssessment.subject | CareTeam.subject | ImagingStudy.patient | FamilyMemberHistory.patient | Encounter.sub'+'ject | DeviceUseStatement.subject | DeviceRequest.subject | AllergyIntolerance.patient | CarePlan.subject | EpisodeOfCare.patient | Procedure.subject | List.subject | Immunization.patient | VisionPrescription.patient | ProcedureRequest.subject | Flag'+'.subject | Observation.subject | DiagnosticReport.subject | NutritionOrder.patient | Condition.subject | Composition.subject | DetectedIssue.patient | SupplyDelivery.patient | ClinicalImpression.subject', SearchXpathUsageNormal);
  indexes.add('Consent', 'period', 'Period that this consent applies', SearchParamTypeDATE, [], 'Consent.period', SearchXpathUsageNormal);
  indexes.add('Consent', 'purpose', 'Context of activities for which the agreement is made', SearchParamTypeTOKEN, [], 'Consent.purpose | Consent.except.purpose', SearchXpathUsageNormal);
  indexes.add('Consent', 'securitylabel', 'Security Labels that define affected resources', SearchParamTypeTOKEN, [], 'Consent.securityLabel | Consent.except.securityLabel', SearchXpathUsageNormal);
  indexes.add('Consent', 'source', 'Source from which this consent is taken', SearchParamTypeREFERENCE, ['Consent', 'Contract', 'QuestionnaireResponse', 'DocumentReference'], 'Consent.source', SearchXpathUsageNormal);
  indexes.add('Consent', 'status', 'draft | proposed | active | rejected | inactive | entered-in-error', SearchParamTypeTOKEN, [], 'Consent.status', SearchXpathUsageNormal);
  compartments.register(frtPatient, 'Consent', ['patient']);
end;
{$ENDIF}

{$IFDEF FHIR_CONTRACT}
procedure TFHIRIndexBuilder.buildIndexesForContract(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Contract', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Contract', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('Contract', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('Contract', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('Contract', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Contract', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('Contract', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('Contract', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Contract', 'agent', 'Agent to the Contact', SearchParamTypeREFERENCE, ['Practitioner', 'Group', 'Organization', 'Device', 'Patient', 'Substance', 'Contract', 'RelatedPerson', 'Location'], 'Contract.agent.actor', SearchXpathUsageNormal);
  indexes.add('Contract', 'authority', 'The authority of the contract', SearchParamTypeREFERENCE, ['Organization'], 'Contract.authority', SearchXpathUsageNormal);
  indexes.add('Contract', 'domain', 'The domain of the contract', SearchParamTypeREFERENCE, ['Location'], 'Contract.domain', SearchXpathUsageNormal);
  indexes.add('Contract', 'identifier', 'The identity of the contract', SearchParamTypeTOKEN, [], 'Contract.identifier', SearchXpathUsageNormal);
  indexes.add('Contract', 'issued', 'The date/time the contract was issued', SearchParamTypeDATE, [], 'Contract.issued', SearchXpathUsageNormal);
  indexes.add('Contract', 'patient', 'The identity of the subject of the contract (if a patient)', SearchParamTypeREFERENCE, ['Patient'], 'Contract.subject', SearchXpathUsageNormal);
  indexes.add('Contract', 'signer', 'Contract Signatory Party', SearchParamTypeREFERENCE, ['Practitioner', 'Organization', 'Patient', 'RelatedPerson'], 'Contract.signer.party', SearchXpathUsageNormal);
  indexes.add('Contract', 'subject', 'The identity of the subject of the contract', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'Contract.subject', SearchXpathUsageNormal);
  indexes.add('Contract', 'term-topic', 'The identity of the topic of the contract terms', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'Contract.term.topic', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_COVERAGE}
procedure TFHIRIndexBuilder.buildIndexesForCoverage(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Coverage', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Coverage', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('Coverage', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('Coverage', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('Coverage', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Coverage', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('Coverage', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('Coverage', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Coverage', 'beneficiary', 'Covered party', SearchParamTypeREFERENCE, ['Patient'], 'Coverage.beneficiary', SearchXpathUsageNormal);
  indexes.add('Coverage', 'class', 'Class identifier', SearchParamTypeSTRING, [], 'Coverage.grouping.class', SearchXpathUsageNormal);
  indexes.add('Coverage', 'dependent', 'Dependent number', SearchParamTypeSTRING, [], 'Coverage.dependent', SearchXpathUsageNormal);
  indexes.add('Coverage', 'group', 'Group identifier', SearchParamTypeSTRING, [], 'Coverage.grouping.group', SearchXpathUsageNormal);
  indexes.add('Coverage', 'identifier', 'The primary identifier of the insured and the coverage', SearchParamTypeTOKEN, [], 'Coverage.identifier', SearchXpathUsageNormal);
  indexes.add('Coverage', 'payor', 'The identity of the insurer or party paying for services', SearchParamTypeREFERENCE, ['Organization', 'Patient', 'RelatedPerson'], 'Coverage.payor', SearchXpathUsageNormal);
  indexes.add('Coverage', 'plan', 'A plan or policy identifier', SearchParamTypeSTRING, [], 'Coverage.grouping.plan', SearchXpathUsageNormal);
  indexes.add('Coverage', 'policy-holder', 'Reference to the policyholder', SearchParamTypeREFERENCE, ['Organization', 'Patient', 'RelatedPerson'], 'Coverage.policyHolder', SearchXpathUsageNormal);
  indexes.add('Coverage', 'sequence', 'Sequence number', SearchParamTypeSTRING, [], 'Coverage.sequence', SearchXpathUsageNormal);
  indexes.add('Coverage', 'subclass', 'Sub-class identifier', SearchParamTypeSTRING, [], 'Coverage.grouping.subClass', SearchXpathUsageNormal);
  indexes.add('Coverage', 'subgroup', 'Sub-group identifier', SearchParamTypeSTRING, [], 'Coverage.grouping.subGroup', SearchXpathUsageNormal);
  indexes.add('Coverage', 'subplan', 'Sub-plan identifier', SearchParamTypeSTRING, [], 'Coverage.grouping.subPlan', SearchXpathUsageNormal);
  indexes.add('Coverage', 'subscriber', 'Reference to the subscriber', SearchParamTypeREFERENCE, ['Patient', 'RelatedPerson'], 'Coverage.subscriber', SearchXpathUsageNormal);
  indexes.add('Coverage', 'type', 'The kind of coverage (health plan, auto, Workers Compensation)', SearchParamTypeTOKEN, [], 'Coverage.type', SearchXpathUsageNormal);
  compartments.register(frtPatient, 'Coverage', ['policy-holder', 'subscriber', 'beneficiary', 'payor']);
  compartments.register(frtRelatedPerson, 'Coverage', ['policy-holder', 'subscriber', 'payor']);
end;
{$ENDIF}

{$IFDEF FHIR_DATAELEMENT}
procedure TFHIRIndexBuilder.buildIndexesForDataElement(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('DataElement', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('DataElement', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('DataElement', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('DataElement', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('DataElement', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DataElement', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('DataElement', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('DataElement', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('DataElement', 'code', 'A code for the data element (server may choose to do subsumption)', SearchParamTypeTOKEN, [], 'DataElement.element.code', SearchXpathUsageNormal);
  indexes.add('DataElement', 'date', 'The data element publication date', SearchParamTypeDATE, [], 'DataElement.date', SearchXpathUsageNormal);
  indexes.add('DataElement', 'description', 'Text search in the description of the data element.  This corresponds to the definition of the first DataElement.element.', SearchParamTypeSTRING, [], 'DataElement.element.definition', SearchXpathUsageNormal);
  indexes.add('DataElement', 'identifier', 'External identifier for the data element', SearchParamTypeTOKEN, [], 'DataElement.identifier', SearchXpathUsageNormal);
  indexes.add('DataElement', 'jurisdiction', 'Intended jurisdiction for the data element', SearchParamTypeTOKEN, [], 'DataElement.jurisdiction', SearchXpathUsageNormal);
  indexes.add('DataElement', 'name', 'Computationally friendly name of the data element', SearchParamTypeSTRING, [], 'DataElement.name', SearchXpathUsageNormal);
  indexes.add('DataElement', 'objectClass', 'Matches on the 11179-objectClass extension value', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DataElement', 'objectClassProperty', 'Matches on the 11179-objectClassProperty extension value', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DataElement', 'publisher', 'Name of the publisher of the data element', SearchParamTypeSTRING, [], 'DataElement.publisher', SearchXpathUsageNormal);
  indexes.add('DataElement', 'status', 'The current status of the data element', SearchParamTypeTOKEN, [], 'DataElement.status', SearchXpathUsageNormal);
  indexes.add('DataElement', 'stringency', 'The stringency of the data element definition', SearchParamTypeTOKEN, [], 'DataElement.stringency', SearchXpathUsageNormal);
  indexes.add('DataElement', 'title', 'The human-friendly name of the data element', SearchParamTypeSTRING, [], 'DataElement.title', SearchXpathUsageNormal);
  indexes.add('DataElement', 'url', 'The uri that identifies the data element', SearchParamTypeURI, [], 'DataElement.url', SearchXpathUsageNormal);
  indexes.add('DataElement', 'version', 'The business version of the data element', SearchParamTypeTOKEN, [], 'DataElement.version', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_DETECTEDISSUE}
procedure TFHIRIndexBuilder.buildIndexesForDetectedIssue(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('DetectedIssue', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('DetectedIssue', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('DetectedIssue', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('DetectedIssue', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('DetectedIssue', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DetectedIssue', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('DetectedIssue', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('DetectedIssue', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('DetectedIssue', 'author', 'The provider or device that identified the issue', SearchParamTypeREFERENCE, ['Practitioner', 'Device'], 'DetectedIssue.author', SearchXpathUsageNormal);
  indexes.add('DetectedIssue', 'category', 'Issue Category, e.g. drug-drug, duplicate therapy, etc.', SearchParamTypeTOKEN, [], 'DetectedIssue.category', SearchXpathUsageNormal);
  indexes.add('DetectedIssue', 'date', 'When identified', SearchParamTypeDATE, [], 'Consent.dateTime | SupplyRequest.authoredOn | RiskAssessment.occurrence.as(DateTime) | CareTeam.period | FamilyMemberHistory.date | Encounter.period | AllergyIntolerance.assertedDate | CarePlan.period | EpisodeOfCare.period | Procedure.performed | Li'+'st.date | Immunization.date | Flag.period | Observation.effective | DiagnosticReport.effective | Composition.date | DetectedIssue.date | ClinicalImpression.date', SearchXpathUsageNormal);
  indexes.add('DetectedIssue', 'identifier', 'Unique id for the detected issue', SearchParamTypeTOKEN, [], 'DocumentManifest.masterIdentifier | DocumentManifest.identifier | Goal.identifier | Consent.identifier | DocumentReference.masterIdentifier | DocumentReference.identifier | SupplyRequest.identifier | RiskAssessment.identifier | CareTeam.identifier | '+'ImagingStudy.identifier | FamilyMemberHistory.identifier | Encounter.identifier | DeviceRequest.identifier | AllergyIntolerance.identifier | CarePlan.identifier | EpisodeOfCare.identifier | Procedure.identifier | List.identifier | Immunization.identi'+'fier | VisionPrescription.identifier | ProcedureRequest.identifier | Observation.identifier | DiagnosticReport.identifier | NutritionOrder.identifier | Condition.identifier | Composition.identifier | DetectedIssue.identifier | SupplyDelivery.identifi'+'er', SearchXpathUsageNormal);
  indexes.add('DetectedIssue', 'implicated', 'Problem resource', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'DetectedIssue.implicated', SearchXpathUsageNormal);
  indexes.add('DetectedIssue', 'patient', 'Associated patient', SearchParamTypeREFERENCE, ['Group', 'Patient'], 'ReferralRequest.subject | DocumentManifest.subject | Goal.subject | Consent.patient | DocumentReference.subject | ImagingManifest.patient | RiskAssessment.subject | CareTeam.subject | ImagingStudy.patient | FamilyMemberHistory.patient | Encounter.sub'+'ject | DeviceUseStatement.subject | DeviceRequest.subject | AllergyIntolerance.patient | CarePlan.subject | EpisodeOfCare.patient | Procedure.subject | List.subject | Immunization.patient | VisionPrescription.patient | ProcedureRequest.subject | Flag'+'.subject | Observation.subject | DiagnosticReport.subject | NutritionOrder.patient | Condition.subject | Composition.subject | DetectedIssue.patient | SupplyDelivery.patient | ClinicalImpression.subject', SearchXpathUsageNormal);
  compartments.register(frtDevice, 'DetectedIssue', ['author']);
  compartments.register(frtPatient, 'DetectedIssue', ['patient']);
  compartments.register(frtPractitioner, 'DetectedIssue', ['author']);
end;
{$ENDIF}

{$IFDEF FHIR_DEVICE}
procedure TFHIRIndexBuilder.buildIndexesForDevice(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Device', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Device', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('Device', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('Device', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('Device', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Device', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('Device', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('Device', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Device', 'device-name', 'A server defined search that may match any of the string fields in the Device.udi.name  or Device.type.coding.display or  Device.type.text', SearchParamTypeSTRING, [], 'Device.udi.name | Device.type.text | Device.type.coding.display', SearchXpathUsageNormal);
  indexes.add('Device', 'identifier', 'Instance id from manufacturer, owner, and others', SearchParamTypeTOKEN, [], 'Device.identifier', SearchXpathUsageNormal);
  indexes.add('Device', 'location', 'A location, where the resource is found', SearchParamTypeREFERENCE, ['Location'], 'Device.location', SearchXpathUsageNormal);
  indexes.add('Device', 'manufacturer', 'The manufacturer of the device', SearchParamTypeSTRING, [], 'Device.manufacturer', SearchXpathUsageNormal);
  indexes.add('Device', 'model', 'The model of the device', SearchParamTypeSTRING, [], 'Device.model', SearchXpathUsageNormal);
  indexes.add('Device', 'organization', 'The organization responsible for the device', SearchParamTypeREFERENCE, ['Organization'], 'Device.owner', SearchXpathUsageNormal);
  indexes.add('Device', 'patient', 'Patient information, if the resource is affixed to a person', SearchParamTypeREFERENCE, ['Patient'], 'Device.patient', SearchXpathUsageNormal);
  indexes.add('Device', 'status', 'active | inactive | entered-in-error | unknown', SearchParamTypeTOKEN, [], 'Device.status', SearchXpathUsageNormal);
  indexes.add('Device', 'type', 'The type of the device', SearchParamTypeTOKEN, [], 'Device.type', SearchXpathUsageNormal);
  indexes.add('Device', 'udi-carrier', 'UDI Barcode (RFID or other technology) string either in HRF format or AIDC format converted to base64 string.', SearchParamTypeSTRING, [], 'Device.udi.carrierHRF | Device.udi.carrierAIDC', SearchXpathUsageNormal);
  indexes.add('Device', 'udi-di', 'The udi Device Identifier (DI)', SearchParamTypeSTRING, [], 'Device.udi.deviceIdentifier', SearchXpathUsageNormal);
  indexes.add('Device', 'url', 'Network address to contact device', SearchParamTypeURI, [], 'Device.url', SearchXpathUsageNormal);
  compartments.register(frtDevice, 'Device', ['{def}']);
end;
{$ENDIF}

{$IFDEF FHIR_DEVICECOMPONENT}
procedure TFHIRIndexBuilder.buildIndexesForDeviceComponent(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('DeviceComponent', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('DeviceComponent', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('DeviceComponent', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('DeviceComponent', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('DeviceComponent', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DeviceComponent', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('DeviceComponent', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('DeviceComponent', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('DeviceComponent', 'identifier', 'The identifier of the component', SearchParamTypeTOKEN, [], 'DeviceComponent.identifier', SearchXpathUsageNormal);
  indexes.add('DeviceComponent', 'parent', 'The parent DeviceComponent resource', SearchParamTypeREFERENCE, ['DeviceComponent'], 'DeviceComponent.parent', SearchXpathUsageNormal);
  indexes.add('DeviceComponent', 'source', 'The device source', SearchParamTypeREFERENCE, ['Device'], 'DeviceComponent.source', SearchXpathUsageNormal);
  indexes.add('DeviceComponent', 'type', 'The device component type', SearchParamTypeTOKEN, [], 'DeviceComponent.type', SearchXpathUsageNormal);
  compartments.register(frtDevice, 'DeviceComponent', ['source']);
end;
{$ENDIF}

{$IFDEF FHIR_DEVICEMETRIC}
procedure TFHIRIndexBuilder.buildIndexesForDeviceMetric(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('DeviceMetric', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('DeviceMetric', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('DeviceMetric', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('DeviceMetric', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('DeviceMetric', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DeviceMetric', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('DeviceMetric', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('DeviceMetric', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('DeviceMetric', 'category', 'The category of the metric', SearchParamTypeTOKEN, [], 'DeviceMetric.category', SearchXpathUsageNormal);
  indexes.add('DeviceMetric', 'identifier', 'The identifier of the metric', SearchParamTypeTOKEN, [], 'DeviceMetric.identifier', SearchXpathUsageNormal);
  indexes.add('DeviceMetric', 'parent', 'The parent DeviceMetric resource', SearchParamTypeREFERENCE, ['DeviceComponent'], 'DeviceMetric.parent', SearchXpathUsageNormal);
  indexes.add('DeviceMetric', 'source', 'The device resource', SearchParamTypeREFERENCE, ['Device'], 'DeviceMetric.source', SearchXpathUsageNormal);
  indexes.add('DeviceMetric', 'type', 'The component type', SearchParamTypeTOKEN, [], 'DeviceMetric.type', SearchXpathUsageNormal);
  compartments.register(frtDevice, 'DeviceMetric', ['source']);
end;
{$ENDIF}

{$IFDEF FHIR_DEVICEREQUEST}
procedure TFHIRIndexBuilder.buildIndexesForDeviceRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('DeviceRequest', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('DeviceRequest', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('DeviceRequest', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('DeviceRequest', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('DeviceRequest', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DeviceRequest', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('DeviceRequest', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('DeviceRequest', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('DeviceRequest', 'authored-on', 'When the request transitioned to being actionable', SearchParamTypeDATE, [], 'DeviceRequest.authoredOn', SearchXpathUsageNormal);
  indexes.add('DeviceRequest', 'based-on', 'Plan/proposal/order fulfilled by this request', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'DeviceRequest.basedOn', SearchXpathUsageNormal);
  indexes.add('DeviceRequest', 'code', 'Code for what is being requested/ordered', SearchParamTypeTOKEN, [], 'FamilyMemberHistory.condition.code | DeviceRequest.code.as(CodeableConcept) | AllergyIntolerance.code | AllergyIntolerance.reaction.substance | Procedure.code | List.code | ProcedureRequest.code | Observation.code | DiagnosticReport.code | Condition.'+'code', SearchXpathUsageNormal);
  indexes.add('DeviceRequest', 'definition', 'Protocol or definition followed by this request', SearchParamTypeREFERENCE, ['PlanDefinition', 'ActivityDefinition'], 'DeviceRequest.definition', SearchXpathUsageNormal);
  indexes.add('DeviceRequest', 'device', 'Reference to resource that is being requested/ordered', SearchParamTypeREFERENCE, ['Device'], 'DeviceRequest.code.as(Reference)', SearchXpathUsageNormal);
  indexes.add('DeviceRequest', 'encounter', 'Encounter or Episode during which request was created', SearchParamTypeREFERENCE, ['EpisodeOfCare', 'Encounter'], 'DocumentReference.context.encounter | RiskAssessment.context | DeviceRequest.context | Procedure.context | List.encounter | VisionPrescription.encounter | ProcedureRequest.context | Flag.encounter | Observation.context | DiagnosticReport.context | Nu'+'tritionOrder.encounter | Composition.encounter', SearchXpathUsageNormal);
  indexes.add('DeviceRequest', 'event-date', 'When service should occur', SearchParamTypeDATE, [], 'DeviceRequest.occurrence.as(DateTime) | DeviceRequest.occurrence.as(Period)', SearchXpathUsageNormal);
  indexes.add('DeviceRequest', 'group-identifier', 'Composite request this is part of', SearchParamTypeTOKEN, [], 'DeviceRequest.groupIdentifier', SearchXpathUsageNormal);
  indexes.add('DeviceRequest', 'identifier', 'Business identifier for request/order', SearchParamTypeTOKEN, [], 'DocumentManifest.masterIdentifier | DocumentManifest.identifier | Goal.identifier | Consent.identifier | DocumentReference.masterIdentifier | DocumentReference.identifier | SupplyRequest.identifier | RiskAssessment.identifier | CareTeam.identifier | '+'ImagingStudy.identifier | FamilyMemberHistory.identifier | Encounter.identifier | DeviceRequest.identifier | AllergyIntolerance.identifier | CarePlan.identifier | EpisodeOfCare.identifier | Procedure.identifier | List.identifier | Immunization.identi'+'fier | VisionPrescription.identifier | ProcedureRequest.identifier | Observation.identifier | DiagnosticReport.identifier | NutritionOrder.identifier | Condition.identifier | Composition.identifier | DetectedIssue.identifier | SupplyDelivery.identifi'+'er', SearchXpathUsageNormal);
  indexes.add('DeviceRequest', 'intent', 'proposal | plan | original-order |reflex-order', SearchParamTypeTOKEN, [], 'DeviceRequest.intent', SearchXpathUsageNormal);
  indexes.add('DeviceRequest', 'patient', 'Individual the service is ordered for', SearchParamTypeREFERENCE, ['Group', 'Patient'], 'ReferralRequest.subject | DocumentManifest.subject | Goal.subject | Consent.patient | DocumentReference.subject | ImagingManifest.patient | RiskAssessment.subject | CareTeam.subject | ImagingStudy.patient | FamilyMemberHistory.patient | Encounter.sub'+'ject | DeviceUseStatement.subject | DeviceRequest.subject | AllergyIntolerance.patient | CarePlan.subject | EpisodeOfCare.patient | Procedure.subject | List.subject | Immunization.patient | VisionPrescription.patient | ProcedureRequest.subject | Flag'+'.subject | Observation.subject | DiagnosticReport.subject | NutritionOrder.patient | Condition.subject | Composition.subject | DetectedIssue.patient | SupplyDelivery.patient | ClinicalImpression.subject', SearchXpathUsageNormal);
  indexes.add('DeviceRequest', 'performer', 'Desired performer for service', SearchParamTypeREFERENCE, ['Practitioner', 'Organization', 'Device', 'Patient', 'HealthcareService', 'RelatedPerson'], 'DeviceRequest.performer', SearchXpathUsageNormal);
  indexes.add('DeviceRequest', 'priorrequest', 'Request takes the place of referenced completed or terminated requests', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'DeviceRequest.priorRequest', SearchXpathUsageNormal);
  indexes.add('DeviceRequest', 'requester', 'Who/what is requesting service?', SearchParamTypeREFERENCE, ['Practitioner', 'Organization', 'Device'], 'DeviceRequest.requester.agent', SearchXpathUsageNormal);
  indexes.add('DeviceRequest', 'status', 'entered-in-error | draft | active |suspended | completed?', SearchParamTypeTOKEN, [], 'DeviceRequest.status', SearchXpathUsageNormal);
  indexes.add('DeviceRequest', 'subject', 'Individual the service is ordered for', SearchParamTypeREFERENCE, ['Group', 'Device', 'Patient', 'Location'], 'DeviceRequest.subject', SearchXpathUsageNormal);
  compartments.register(frtDevice, 'DeviceRequest', ['device', 'subject', 'requester', 'performer']);
  compartments.register(frtEncounter, 'DeviceRequest', ['encounter']);
  compartments.register(frtPatient, 'DeviceRequest', ['subject', 'requester', 'performer']);
  compartments.register(frtPractitioner, 'DeviceRequest', ['requester', 'performer']);
end;
{$ENDIF}

{$IFDEF FHIR_DEVICEUSESTATEMENT}
procedure TFHIRIndexBuilder.buildIndexesForDeviceUseStatement(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('DeviceUseStatement', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('DeviceUseStatement', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('DeviceUseStatement', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('DeviceUseStatement', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('DeviceUseStatement', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DeviceUseStatement', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('DeviceUseStatement', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('DeviceUseStatement', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('DeviceUseStatement', 'device', 'Search by device', SearchParamTypeREFERENCE, ['Device'], 'DeviceUseStatement.device', SearchXpathUsageNormal);
  indexes.add('DeviceUseStatement', 'identifier', 'Search by identifier', SearchParamTypeTOKEN, [], 'DeviceUseStatement.identifier', SearchXpathUsageNormal);
  indexes.add('DeviceUseStatement', 'patient', 'Search by subject - a patient', SearchParamTypeREFERENCE, ['Group', 'Patient'], 'ReferralRequest.subject | DocumentManifest.subject | Goal.subject | Consent.patient | DocumentReference.subject | ImagingManifest.patient | RiskAssessment.subject | CareTeam.subject | ImagingStudy.patient | FamilyMemberHistory.patient | Encounter.sub'+'ject | DeviceUseStatement.subject | DeviceRequest.subject | AllergyIntolerance.patient | CarePlan.subject | EpisodeOfCare.patient | Procedure.subject | List.subject | Immunization.patient | VisionPrescription.patient | ProcedureRequest.subject | Flag'+'.subject | Observation.subject | DiagnosticReport.subject | NutritionOrder.patient | Condition.subject | Composition.subject | DetectedIssue.patient | SupplyDelivery.patient | ClinicalImpression.subject', SearchXpathUsageNormal);
  indexes.add('DeviceUseStatement', 'subject', 'Search by subject', SearchParamTypeREFERENCE, ['Group', 'Patient'], 'DeviceUseStatement.subject', SearchXpathUsageNormal);
  compartments.register(frtDevice, 'DeviceUseStatement', ['device']);
  compartments.register(frtPatient, 'DeviceUseStatement', ['subject']);
end;
{$ENDIF}

{$IFDEF FHIR_DIAGNOSTICREPORT}
procedure TFHIRIndexBuilder.buildIndexesForDiagnosticReport(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('DiagnosticReport', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('DiagnosticReport', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('DiagnosticReport', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('DiagnosticReport', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('DiagnosticReport', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DiagnosticReport', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('DiagnosticReport', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('DiagnosticReport', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('DiagnosticReport', 'based-on', 'Reference to the procedure request.', SearchParamTypeREFERENCE, ['ReferralRequest', 'CarePlan', 'MedicationRequest', 'NutritionOrder', 'ProcedureRequest', 'ImmunizationRecommendation'], 'DiagnosticReport.basedOn', SearchXpathUsageNormal);
  indexes.add('DiagnosticReport', 'category', 'Which diagnostic discipline/department created the report', SearchParamTypeTOKEN, [], 'DiagnosticReport.category', SearchXpathUsageNormal);
  indexes.add('DiagnosticReport', 'code', 'The code for the report as a whole, as opposed to codes for the atomic results, which are the names on the observation resource referred to from the result', SearchParamTypeTOKEN, [], 'FamilyMemberHistory.condition.code | DeviceRequest.code.as(CodeableConcept) | AllergyIntolerance.code | AllergyIntolerance.reaction.substance | Procedure.code | List.code | ProcedureRequest.code | Observation.code | DiagnosticReport.code | Condition.'+'code', SearchXpathUsageNormal);
  indexes.add('DiagnosticReport', 'context', 'Healthcare event (Episode of Care or Encounter) related to the report', SearchParamTypeREFERENCE, ['EpisodeOfCare', 'Encounter'], 'DiagnosticReport.context', SearchXpathUsageNormal);
  indexes.add('DiagnosticReport', 'date', 'The clinically relevant time of the report', SearchParamTypeDATE, [], 'Consent.dateTime | SupplyRequest.authoredOn | RiskAssessment.occurrence.as(DateTime) | CareTeam.period | FamilyMemberHistory.date | Encounter.period | AllergyIntolerance.assertedDate | CarePlan.period | EpisodeOfCare.period | Procedure.performed | Li'+'st.date | Immunization.date | Flag.period | Observation.effective | DiagnosticReport.effective | Composition.date | DetectedIssue.date | ClinicalImpression.date', SearchXpathUsageNormal);
  indexes.add('DiagnosticReport', 'diagnosis', 'A coded diagnosis on the report', SearchParamTypeTOKEN, [], 'DiagnosticReport.codedDiagnosis', SearchXpathUsageNormal);
  indexes.add('DiagnosticReport', 'encounter', 'The Encounter when the order was made', SearchParamTypeREFERENCE, ['EpisodeOfCare', 'Encounter'], 'DocumentReference.context.encounter | RiskAssessment.context | DeviceRequest.context | Procedure.context | List.encounter | VisionPrescription.encounter | ProcedureRequest.context | Flag.encounter | Observation.context | DiagnosticReport.context | Nu'+'tritionOrder.encounter | Composition.encounter', SearchXpathUsageNormal);
  indexes.add('DiagnosticReport', 'identifier', 'An identifier for the report', SearchParamTypeTOKEN, [], 'DocumentManifest.masterIdentifier | DocumentManifest.identifier | Goal.identifier | Consent.identifier | DocumentReference.masterIdentifier | DocumentReference.identifier | SupplyRequest.identifier | RiskAssessment.identifier | CareTeam.identifier | '+'ImagingStudy.identifier | FamilyMemberHistory.identifier | Encounter.identifier | DeviceRequest.identifier | AllergyIntolerance.identifier | CarePlan.identifier | EpisodeOfCare.identifier | Procedure.identifier | List.identifier | Immunization.identi'+'fier | VisionPrescription.identifier | ProcedureRequest.identifier | Observation.identifier | DiagnosticReport.identifier | NutritionOrder.identifier | Condition.identifier | Composition.identifier | DetectedIssue.identifier | SupplyDelivery.identifi'+'er', SearchXpathUsageNormal);
  indexes.add('DiagnosticReport', 'image', 'A reference to the image source.', SearchParamTypeREFERENCE, ['Media'], 'DiagnosticReport.image.link', SearchXpathUsageNormal);
  indexes.add('DiagnosticReport', 'issued', 'When the report was issued', SearchParamTypeDATE, [], 'DiagnosticReport.issued', SearchXpathUsageNormal);
  indexes.add('DiagnosticReport', 'patient', 'The subject of the report if a patient', SearchParamTypeREFERENCE, ['Group', 'Patient'], 'ReferralRequest.subject | DocumentManifest.subject | Goal.subject | Consent.patient | DocumentReference.subject | ImagingManifest.patient | RiskAssessment.subject | CareTeam.subject | ImagingStudy.patient | FamilyMemberHistory.patient | Encounter.sub'+'ject | DeviceUseStatement.subject | DeviceRequest.subject | AllergyIntolerance.patient | CarePlan.subject | EpisodeOfCare.patient | Procedure.subject | List.subject | Immunization.patient | VisionPrescription.patient | ProcedureRequest.subject | Flag'+'.subject | Observation.subject | DiagnosticReport.subject | NutritionOrder.patient | Condition.subject | Composition.subject | DetectedIssue.patient | SupplyDelivery.patient | ClinicalImpression.subject', SearchXpathUsageNormal);
  indexes.add('DiagnosticReport', 'performer', 'Who was the source of the report (organization)', SearchParamTypeREFERENCE, ['Practitioner', 'Organization'], 'DiagnosticReport.performer.actor', SearchXpathUsageNormal);
  indexes.add('DiagnosticReport', 'result', 'Link to an atomic result (observation resource)', SearchParamTypeREFERENCE, ['Observation'], 'DiagnosticReport.result', SearchXpathUsageNormal);
  indexes.add('DiagnosticReport', 'specimen', 'The specimen details', SearchParamTypeREFERENCE, ['Specimen'], 'DiagnosticReport.specimen', SearchXpathUsageNormal);
  indexes.add('DiagnosticReport', 'status', 'The status of the report', SearchParamTypeTOKEN, [], 'DiagnosticReport.status', SearchXpathUsageNormal);
  indexes.add('DiagnosticReport', 'subject', 'The subject of the report', SearchParamTypeREFERENCE, ['Group', 'Device', 'Patient', 'Location'], 'DiagnosticReport.subject', SearchXpathUsageNormal);
  compartments.register(frtDevice, 'DiagnosticReport', ['subject']);
  compartments.register(frtEncounter, 'DiagnosticReport', ['encounter']);
  compartments.register(frtPatient, 'DiagnosticReport', ['subject']);
  compartments.register(frtPractitioner, 'DiagnosticReport', ['performer']);
end;
{$ENDIF}

{$IFDEF FHIR_DOCUMENTMANIFEST}
procedure TFHIRIndexBuilder.buildIndexesForDocumentManifest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('DocumentManifest', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('DocumentManifest', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('DocumentManifest', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('DocumentManifest', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('DocumentManifest', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DocumentManifest', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('DocumentManifest', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('DocumentManifest', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('DocumentManifest', 'author', 'Who and/or what authored the manifest', SearchParamTypeREFERENCE, ['Practitioner', 'Organization', 'Device', 'Patient', 'RelatedPerson'], 'DocumentManifest.author', SearchXpathUsageNormal);
  indexes.add('DocumentManifest', 'content-ref', 'Contents of this set of documents', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'DocumentManifest.content.p.as(Reference)', SearchXpathUsageNormal);
  indexes.add('DocumentManifest', 'created', 'When this document manifest created', SearchParamTypeDATE, [], 'DocumentManifest.created', SearchXpathUsageNormal);
  indexes.add('DocumentManifest', 'description', 'Human-readable description (title)', SearchParamTypeSTRING, [], 'DocumentManifest.description', SearchXpathUsageNormal);
  indexes.add('DocumentManifest', 'identifier', 'Unique Identifier for the set of documents', SearchParamTypeTOKEN, [], 'DocumentManifest.masterIdentifier | DocumentManifest.identifier | Goal.identifier | Consent.identifier | DocumentReference.masterIdentifier | DocumentReference.identifier | SupplyRequest.identifier | RiskAssessment.identifier | CareTeam.identifier | '+'ImagingStudy.identifier | FamilyMemberHistory.identifier | Encounter.identifier | DeviceRequest.identifier | AllergyIntolerance.identifier | CarePlan.identifier | EpisodeOfCare.identifier | Procedure.identifier | List.identifier | Immunization.identi'+'fier | VisionPrescription.identifier | ProcedureRequest.identifier | Observation.identifier | DiagnosticReport.identifier | NutritionOrder.identifier | Condition.identifier | Composition.identifier | DetectedIssue.identifier | SupplyDelivery.identifi'+'er', SearchXpathUsageNormal);
  indexes.add('DocumentManifest', 'patient', 'The subject of the set of documents', SearchParamTypeREFERENCE, ['Group', 'Patient'], 'ReferralRequest.subject | DocumentManifest.subject | Goal.subject | Consent.patient | DocumentReference.subject | ImagingManifest.patient | RiskAssessment.subject | CareTeam.subject | ImagingStudy.patient | FamilyMemberHistory.patient | Encounter.sub'+'ject | DeviceUseStatement.subject | DeviceRequest.subject | AllergyIntolerance.patient | CarePlan.subject | EpisodeOfCare.patient | Procedure.subject | List.subject | Immunization.patient | VisionPrescription.patient | ProcedureRequest.subject | Flag'+'.subject | Observation.subject | DiagnosticReport.subject | NutritionOrder.patient | Condition.subject | Composition.subject | DetectedIssue.patient | SupplyDelivery.patient | ClinicalImpression.subject', SearchXpathUsageNormal);
  indexes.add('DocumentManifest', 'recipient', 'Intended to get notified about this set of documents', SearchParamTypeREFERENCE, ['Practitioner', 'Organization', 'Patient', 'RelatedPerson'], 'DocumentManifest.recipient', SearchXpathUsageNormal);
  indexes.add('DocumentManifest', 'related-id', 'Identifiers of things that are related', SearchParamTypeTOKEN, [], 'DocumentManifest.related.identifier', SearchXpathUsageNormal);
  indexes.add('DocumentManifest', 'related-ref', 'Related Resource', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'DocumentManifest.related.ref', SearchXpathUsageNormal);
  indexes.add('DocumentManifest', 'source', 'The source system/application/software', SearchParamTypeURI, [], 'DocumentManifest.source', SearchXpathUsageNormal);
  indexes.add('DocumentManifest', 'status', 'current | superseded | entered-in-error', SearchParamTypeTOKEN, [], 'DocumentManifest.status', SearchXpathUsageNormal);
  indexes.add('DocumentManifest', 'subject', 'The subject of the set of documents', SearchParamTypeREFERENCE, ['Practitioner', 'Group', 'Device', 'Patient'], 'DocumentManifest.subject', SearchXpathUsageNormal);
  indexes.add('DocumentManifest', 'type', 'Kind of document set', SearchParamTypeTOKEN, [], 'ReferralRequest.type | DocumentManifest.type | DocumentReference.type | Encounter.type | AllergyIntolerance.type | EpisodeOfCare.type | Composition.type', SearchXpathUsageNormal);
  compartments.register(frtDevice, 'DocumentManifest', ['subject', 'author']);
  compartments.register(frtPatient, 'DocumentManifest', ['subject', 'author', 'recipient']);
  compartments.register(frtPractitioner, 'DocumentManifest', ['subject', 'author', 'recipient']);
  compartments.register(frtRelatedPerson, 'DocumentManifest', ['author']);
end;
{$ENDIF}

{$IFDEF FHIR_DOCUMENTREFERENCE}
procedure TFHIRIndexBuilder.buildIndexesForDocumentReference(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('DocumentReference', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('DocumentReference', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('DocumentReference', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('DocumentReference', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('DocumentReference', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DocumentReference', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('DocumentReference', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('DocumentReference', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('DocumentReference', 'authenticator', 'Who/what authenticated the document', SearchParamTypeREFERENCE, ['Practitioner', 'Organization'], 'DocumentReference.authenticator', SearchXpathUsageNormal);
  indexes.add('DocumentReference', 'author', 'Who and/or what authored the document', SearchParamTypeREFERENCE, ['Practitioner', 'Organization', 'Device', 'Patient', 'RelatedPerson'], 'DocumentReference.author', SearchXpathUsageNormal);
  indexes.add('DocumentReference', 'class', 'Categorization of document', SearchParamTypeTOKEN, [], 'DocumentReference.class', SearchXpathUsageNormal);
  indexes.add('DocumentReference', 'created', 'Document creation time', SearchParamTypeDATE, [], 'DocumentReference.created', SearchXpathUsageNormal);
  indexes.add('DocumentReference', 'custodian', 'Organization which maintains the document', SearchParamTypeREFERENCE, ['Organization'], 'DocumentReference.custodian', SearchXpathUsageNormal);
  indexes.add('DocumentReference', 'description', 'Human-readable description (title)', SearchParamTypeSTRING, [], 'DocumentReference.description', SearchXpathUsageNormal);
  indexes.add('DocumentReference', 'encounter', 'Context of the document  content', SearchParamTypeREFERENCE, ['EpisodeOfCare', 'Encounter'], 'DocumentReference.context.encounter | RiskAssessment.context | DeviceRequest.context | Procedure.context | List.encounter | VisionPrescription.encounter | ProcedureRequest.context | Flag.encounter | Observation.context | DiagnosticReport.context | Nu'+'tritionOrder.encounter | Composition.encounter', SearchXpathUsageNormal);
  indexes.add('DocumentReference', 'event', 'Main clinical acts documented', SearchParamTypeTOKEN, [], 'DocumentReference.context.event', SearchXpathUsageNormal);
  indexes.add('DocumentReference', 'facility', 'Kind of facility where patient was seen', SearchParamTypeTOKEN, [], 'DocumentReference.context.facilityType', SearchXpathUsageNormal);
  indexes.add('DocumentReference', 'format', 'Format/content rules for the document', SearchParamTypeTOKEN, [], 'DocumentReference.content.format', SearchXpathUsageNormal);
  indexes.add('DocumentReference', 'identifier', 'Master Version Specific Identifier', SearchParamTypeTOKEN, [], 'DocumentManifest.masterIdentifier | DocumentManifest.identifier | Goal.identifier | Consent.identifier | DocumentReference.masterIdentifier | DocumentReference.identifier | SupplyRequest.identifier | RiskAssessment.identifier | CareTeam.identifier | '+'ImagingStudy.identifier | FamilyMemberHistory.identifier | Encounter.identifier | DeviceRequest.identifier | AllergyIntolerance.identifier | CarePlan.identifier | EpisodeOfCare.identifier | Procedure.identifier | List.identifier | Immunization.identi'+'fier | VisionPrescription.identifier | ProcedureRequest.identifier | Observation.identifier | DiagnosticReport.identifier | NutritionOrder.identifier | Condition.identifier | Composition.identifier | DetectedIssue.identifier | SupplyDelivery.identifi'+'er', SearchXpathUsageNormal);
  indexes.add('DocumentReference', 'indexed', 'When this document reference was created', SearchParamTypeDATE, [], 'DocumentReference.indexed', SearchXpathUsageNormal);
  indexes.add('DocumentReference', 'language', 'Human language of the content (BCP-47)', SearchParamTypeTOKEN, [], 'DocumentReference.content.attachment.language', SearchXpathUsageNormal);
  indexes.add('DocumentReference', 'location', 'Uri where the data can be found', SearchParamTypeURI, [], 'DocumentReference.content.attachment.url', SearchXpathUsageNormal);
  indexes.add('DocumentReference', 'patient', 'Who/what is the subject of the document', SearchParamTypeREFERENCE, ['Group', 'Patient'], 'ReferralRequest.subject | DocumentManifest.subject | Goal.subject | Consent.patient | DocumentReference.subject | ImagingManifest.patient | RiskAssessment.subject | CareTeam.subject | ImagingStudy.patient | FamilyMemberHistory.patient | Encounter.sub'+'ject | DeviceUseStatement.subject | DeviceRequest.subject | AllergyIntolerance.patient | CarePlan.subject | EpisodeOfCare.patient | Procedure.subject | List.subject | Immunization.patient | VisionPrescription.patient | ProcedureRequest.subject | Flag'+'.subject | Observation.subject | DiagnosticReport.subject | NutritionOrder.patient | Condition.subject | Composition.subject | DetectedIssue.patient | SupplyDelivery.patient | ClinicalImpression.subject', SearchXpathUsageNormal);
  indexes.add('DocumentReference', 'period', 'Time of service that is being documented', SearchParamTypeDATE, [], 'DocumentReference.context.period', SearchXpathUsageNormal);
  indexes.add('DocumentReference', 'related-id', 'Identifier of related objects or events', SearchParamTypeTOKEN, [], 'DocumentReference.context.related.identifier', SearchXpathUsageNormal);
  indexes.add('DocumentReference', 'related-ref', 'Related Resource', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'DocumentReference.context.related.ref', SearchXpathUsageNormal);
  indexes.add('DocumentReference', 'relatesto', 'Target of the relationship', SearchParamTypeREFERENCE, ['DocumentReference'], 'DocumentReference.relatesTo.target', SearchXpathUsageNormal);
  indexes.add('DocumentReference', 'relation', 'replaces | transforms | signs | appends', SearchParamTypeTOKEN, [], 'DocumentReference.relatesTo.code', SearchXpathUsageNormal);
  indexes.add('DocumentReference', 'relationship', 'Combination of relation and relatesTo', SearchParamTypeCOMPOSITE, [], 'DocumentReference.relatesTo', SearchXpathUsageNormal);
  indexes.add('DocumentReference', 'securitylabel', 'Document security-tags', SearchParamTypeTOKEN, [], 'DocumentReference.securityLabel', SearchXpathUsageNormal);
  indexes.add('DocumentReference', 'setting', 'Additional details about where the content was created (e.g. clinical specialty)', SearchParamTypeTOKEN, [], 'DocumentReference.context.practiceSetting', SearchXpathUsageNormal);
  indexes.add('DocumentReference', 'status', 'current | superseded | entered-in-error', SearchParamTypeTOKEN, [], 'DocumentReference.status', SearchXpathUsageNormal);
  indexes.add('DocumentReference', 'subject', 'Who/what is the subject of the document', SearchParamTypeREFERENCE, ['Practitioner', 'Group', 'Device', 'Patient'], 'DocumentReference.subject', SearchXpathUsageNormal);
  indexes.add('DocumentReference', 'type', 'Kind of document (LOINC if possible)', SearchParamTypeTOKEN, [], 'ReferralRequest.type | DocumentManifest.type | DocumentReference.type | Encounter.type | AllergyIntolerance.type | EpisodeOfCare.type | Composition.type', SearchXpathUsageNormal);
  compartments.register(frtDevice, 'DocumentReference', ['subject', 'author']);
  compartments.register(frtEncounter, 'DocumentReference', ['encounter']);
  compartments.register(frtPatient, 'DocumentReference', ['subject', 'author']);
  compartments.register(frtPractitioner, 'DocumentReference', ['subject', 'author', 'authenticator']);
  compartments.register(frtRelatedPerson, 'DocumentReference', ['author']);
end;
{$ENDIF}

{$IFDEF FHIR_ELIGIBILITYREQUEST}
procedure TFHIRIndexBuilder.buildIndexesForEligibilityRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('EligibilityRequest', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('EligibilityRequest', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('EligibilityRequest', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('EligibilityRequest', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('EligibilityRequest', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('EligibilityRequest', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('EligibilityRequest', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('EligibilityRequest', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('EligibilityRequest', 'created', 'The creation date for the EOB', SearchParamTypeDATE, [], 'EligibilityRequest.created', SearchXpathUsageNormal);
  indexes.add('EligibilityRequest', 'enterer', 'The party who is responsible for the request', SearchParamTypeREFERENCE, ['Practitioner'], 'EligibilityRequest.enterer', SearchXpathUsageNormal);
  indexes.add('EligibilityRequest', 'facility', 'Facility responsible for the goods and services', SearchParamTypeREFERENCE, ['Location'], 'EligibilityRequest.facility', SearchXpathUsageNormal);
  indexes.add('EligibilityRequest', 'identifier', 'The business identifier of the Eligibility', SearchParamTypeTOKEN, [], 'EligibilityRequest.identifier', SearchXpathUsageNormal);
  indexes.add('EligibilityRequest', 'organization', 'The reference to the providing organization', SearchParamTypeREFERENCE, ['Organization'], 'EligibilityRequest.organization', SearchXpathUsageNormal);
  indexes.add('EligibilityRequest', 'patient', 'The reference to the patient', SearchParamTypeREFERENCE, ['Patient'], 'EligibilityRequest.patient', SearchXpathUsageNormal);
  indexes.add('EligibilityRequest', 'provider', 'The reference to the provider', SearchParamTypeREFERENCE, ['Practitioner'], 'EligibilityRequest.provider', SearchXpathUsageNormal);
  compartments.register(frtPatient, 'EligibilityRequest', ['patient']);
  compartments.register(frtPractitioner, 'EligibilityRequest', ['enterer', 'provider']);
end;
{$ENDIF}

{$IFDEF FHIR_ELIGIBILITYRESPONSE}
procedure TFHIRIndexBuilder.buildIndexesForEligibilityResponse(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('EligibilityResponse', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('EligibilityResponse', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('EligibilityResponse', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('EligibilityResponse', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('EligibilityResponse', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('EligibilityResponse', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('EligibilityResponse', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('EligibilityResponse', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('EligibilityResponse', 'created', 'The creation date', SearchParamTypeDATE, [], 'EligibilityResponse.created', SearchXpathUsageNormal);
  indexes.add('EligibilityResponse', 'disposition', 'The contents of the disposition message', SearchParamTypeSTRING, [], 'EligibilityResponse.disposition', SearchXpathUsageNormal);
  indexes.add('EligibilityResponse', 'identifier', 'The business identifier', SearchParamTypeTOKEN, [], 'EligibilityResponse.identifier', SearchXpathUsageNormal);
  indexes.add('EligibilityResponse', 'insurer', 'The organization which generated this resource', SearchParamTypeREFERENCE, ['Organization'], 'EligibilityResponse.insurer', SearchXpathUsageNormal);
  indexes.add('EligibilityResponse', 'outcome', 'The processing outcome', SearchParamTypeTOKEN, [], 'EligibilityResponse.outcome', SearchXpathUsageNormal);
  indexes.add('EligibilityResponse', 'request', 'The EligibilityRequest reference', SearchParamTypeREFERENCE, ['EligibilityRequest'], 'EligibilityResponse.request', SearchXpathUsageNormal);
  indexes.add('EligibilityResponse', 'request-organization', 'The EligibilityRequest organization', SearchParamTypeREFERENCE, ['Organization'], 'EligibilityResponse.requestOrganization', SearchXpathUsageNormal);
  indexes.add('EligibilityResponse', 'request-provider', 'The EligibilityRequest provider', SearchParamTypeREFERENCE, ['Practitioner'], 'EligibilityResponse.requestProvider', SearchXpathUsageNormal);
  compartments.register(frtPractitioner, 'EligibilityResponse', ['request-provider']);
end;
{$ENDIF}

{$IFDEF FHIR_ENCOUNTER}
procedure TFHIRIndexBuilder.buildIndexesForEncounter(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Encounter', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Encounter', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('Encounter', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('Encounter', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('Encounter', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Encounter', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('Encounter', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('Encounter', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Encounter', 'appointment', 'The appointment that scheduled this encounter', SearchParamTypeREFERENCE, ['Appointment'], 'Encounter.appointment', SearchXpathUsageNormal);
  indexes.add('Encounter', 'class', 'inpatient | outpatient | ambulatory | emergency +', SearchParamTypeTOKEN, [], 'Encounter.class', SearchXpathUsageNormal);
  indexes.add('Encounter', 'date', 'A date within the period the Encounter lasted', SearchParamTypeDATE, [], 'Consent.dateTime | SupplyRequest.authoredOn | RiskAssessment.occurrence.as(DateTime) | CareTeam.period | FamilyMemberHistory.date | Encounter.period | AllergyIntolerance.assertedDate | CarePlan.period | EpisodeOfCare.period | Procedure.performed | Li'+'st.date | Immunization.date | Flag.period | Observation.effective | DiagnosticReport.effective | Composition.date | DetectedIssue.date | ClinicalImpression.date', SearchXpathUsageNormal);
  indexes.add('Encounter', 'diagnosis', 'Reason the encounter takes place (resource)', SearchParamTypeREFERENCE, ['Condition', 'Procedure'], 'Encounter.diagnosis.condition', SearchXpathUsageNormal);
  indexes.add('Encounter', 'episodeofcare', 'Episode(s) of care that this encounter should be recorded against', SearchParamTypeREFERENCE, ['EpisodeOfCare'], 'Encounter.episodeOfCare', SearchXpathUsageNormal);
  indexes.add('Encounter', 'identifier', 'Identifier(s) by which this encounter is known', SearchParamTypeTOKEN, [], 'DocumentManifest.masterIdentifier | DocumentManifest.identifier | Goal.identifier | Consent.identifier | DocumentReference.masterIdentifier | DocumentReference.identifier | SupplyRequest.identifier | RiskAssessment.identifier | CareTeam.identifier | '+'ImagingStudy.identifier | FamilyMemberHistory.identifier | Encounter.identifier | DeviceRequest.identifier | AllergyIntolerance.identifier | CarePlan.identifier | EpisodeOfCare.identifier | Procedure.identifier | List.identifier | Immunization.identi'+'fier | VisionPrescription.identifier | ProcedureRequest.identifier | Observation.identifier | DiagnosticReport.identifier | NutritionOrder.identifier | Condition.identifier | Composition.identifier | DetectedIssue.identifier | SupplyDelivery.identifi'+'er', SearchXpathUsageNormal);
  indexes.add('Encounter', 'incomingreferral', 'The ReferralRequest that initiated this encounter', SearchParamTypeREFERENCE, ['ReferralRequest'], 'Encounter.incomingReferral', SearchXpathUsageNormal);
  indexes.add('Encounter', 'length', 'Length of encounter in days', SearchParamTypeNUMBER, [], 'Encounter.length', SearchXpathUsageNormal);
  indexes.add('Encounter', 'location', 'Location the encounter takes place', SearchParamTypeREFERENCE, ['Location'], 'Encounter.location.location', SearchXpathUsageNormal);
  indexes.add('Encounter', 'location-period', 'Time period during which the patient was present at the location', SearchParamTypeDATE, [], 'Encounter.location.period', SearchXpathUsageNormal);
  indexes.add('Encounter', 'part-of', 'Another Encounter this encounter is part of', SearchParamTypeREFERENCE, ['Encounter'], 'Encounter.partOf', SearchXpathUsageNormal);
  indexes.add('Encounter', 'participant', 'Persons involved in the encounter other than the patient', SearchParamTypeREFERENCE, ['Practitioner', 'RelatedPerson'], 'Encounter.participant.individual', SearchXpathUsageNormal);
  indexes.add('Encounter', 'participant-type', 'Role of participant in encounter', SearchParamTypeTOKEN, [], 'Encounter.participant.type', SearchXpathUsageNormal);
  indexes.add('Encounter', 'patient', 'The patient ro group present at the encounter', SearchParamTypeREFERENCE, ['Group', 'Patient'], 'ReferralRequest.subject | DocumentManifest.subject | Goal.subject | Consent.patient | DocumentReference.subject | ImagingManifest.patient | RiskAssessment.subject | CareTeam.subject | ImagingStudy.patient | FamilyMemberHistory.patient | Encounter.sub'+'ject | DeviceUseStatement.subject | DeviceRequest.subject | AllergyIntolerance.patient | CarePlan.subject | EpisodeOfCare.patient | Procedure.subject | List.subject | Immunization.patient | VisionPrescription.patient | ProcedureRequest.subject | Flag'+'.subject | Observation.subject | DiagnosticReport.subject | NutritionOrder.patient | Condition.subject | Composition.subject | DetectedIssue.patient | SupplyDelivery.patient | ClinicalImpression.subject', SearchXpathUsageNormal);
  indexes.add('Encounter', 'practitioner', 'Persons involved in the encounter other than the patient', SearchParamTypeREFERENCE, ['Practitioner'], 'Encounter.participant.individual', SearchXpathUsageNormal);
  indexes.add('Encounter', 'reason', 'Reason the encounter takes place (code)', SearchParamTypeTOKEN, [], 'Encounter.reason', SearchXpathUsageNormal);
  indexes.add('Encounter', 'service-provider', 'The custodian organization of this Encounter record', SearchParamTypeREFERENCE, ['Organization'], 'Encounter.serviceProvider', SearchXpathUsageNormal);
  indexes.add('Encounter', 'special-arrangement', 'Wheelchair, translator, stretcher, etc.', SearchParamTypeTOKEN, [], 'Encounter.hospitalization.specialArrangement', SearchXpathUsageNormal);
  indexes.add('Encounter', 'status', 'planned | arrived | triaged | in-progress | onleave | finished | cancelled +', SearchParamTypeTOKEN, [], 'Encounter.status', SearchXpathUsageNormal);
  indexes.add('Encounter', 'subject', 'The patient ro group present at the encounter', SearchParamTypeREFERENCE, ['Group', 'Patient'], 'Encounter.subject', SearchXpathUsageNormal);
  indexes.add('Encounter', 'type', 'Specific type of encounter', SearchParamTypeTOKEN, [], 'ReferralRequest.type | DocumentManifest.type | DocumentReference.type | Encounter.type | AllergyIntolerance.type | EpisodeOfCare.type | Composition.type', SearchXpathUsageNormal);
  compartments.register(frtEncounter, 'Encounter', ['{def}']);
  compartments.register(frtPatient, 'Encounter', ['patient']);
  compartments.register(frtPractitioner, 'Encounter', ['practitioner', 'participant']);
  compartments.register(frtRelatedPerson, 'Encounter', ['participant']);
end;
{$ENDIF}

{$IFDEF FHIR_ENDPOINT}
procedure TFHIRIndexBuilder.buildIndexesForEndpoint(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Endpoint', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Endpoint', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('Endpoint', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('Endpoint', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('Endpoint', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Endpoint', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('Endpoint', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('Endpoint', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Endpoint', 'connection-type', 'Protocol/Profile/Standard to be used with this endpoint connection', SearchParamTypeTOKEN, [], 'Endpoint.connectionType', SearchXpathUsageNormal);
  indexes.add('Endpoint', 'identifier', 'Identifies this endpoint across multiple systems', SearchParamTypeTOKEN, [], 'Endpoint.identifier', SearchXpathUsageNormal);
  indexes.add('Endpoint', 'name', 'A name that this endpoint can be identified by', SearchParamTypeSTRING, [], 'Endpoint.name', SearchXpathUsageNormal);
  indexes.add('Endpoint', 'organization', 'The organization that is managing the endpoint', SearchParamTypeREFERENCE, ['Organization'], 'Endpoint.managingOrganization', SearchXpathUsageNormal);
  indexes.add('Endpoint', 'payload-type', 'The type of content that may be used at this endpoint (e.g. XDS Discharge summaries)', SearchParamTypeTOKEN, [], 'Endpoint.payloadType', SearchXpathUsageNormal);
  indexes.add('Endpoint', 'status', 'The current status of the Endpoint (usually expected to be active)', SearchParamTypeTOKEN, [], 'Endpoint.status', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_ENROLLMENTREQUEST}
procedure TFHIRIndexBuilder.buildIndexesForEnrollmentRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('EnrollmentRequest', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('EnrollmentRequest', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('EnrollmentRequest', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('EnrollmentRequest', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('EnrollmentRequest', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('EnrollmentRequest', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('EnrollmentRequest', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('EnrollmentRequest', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('EnrollmentRequest', 'identifier', 'The business identifier of the Enrollment', SearchParamTypeTOKEN, [], 'EnrollmentRequest.identifier', SearchXpathUsageNormal);
  indexes.add('EnrollmentRequest', 'organization', 'The organization who generated this resource', SearchParamTypeREFERENCE, ['Organization'], 'EnrollmentRequest.organization', SearchXpathUsageNormal);
  indexes.add('EnrollmentRequest', 'patient', 'The party to be enrolled', SearchParamTypeREFERENCE, ['Patient'], 'EnrollmentRequest.subject', SearchXpathUsageNormal);
  indexes.add('EnrollmentRequest', 'subject', 'The party to be enrolled', SearchParamTypeREFERENCE, ['Patient'], 'EnrollmentRequest.subject', SearchXpathUsageNormal);
  compartments.register(frtPatient, 'EnrollmentRequest', ['subject']);
end;
{$ENDIF}

{$IFDEF FHIR_ENROLLMENTRESPONSE}
procedure TFHIRIndexBuilder.buildIndexesForEnrollmentResponse(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('EnrollmentResponse', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('EnrollmentResponse', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('EnrollmentResponse', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('EnrollmentResponse', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('EnrollmentResponse', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('EnrollmentResponse', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('EnrollmentResponse', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('EnrollmentResponse', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('EnrollmentResponse', 'identifier', 'The business identifier of the EnrollmentResponse', SearchParamTypeTOKEN, [], 'EnrollmentResponse.identifier', SearchXpathUsageNormal);
  indexes.add('EnrollmentResponse', 'organization', 'The organization who generated this resource', SearchParamTypeREFERENCE, ['Organization'], 'EnrollmentResponse.organization', SearchXpathUsageNormal);
  indexes.add('EnrollmentResponse', 'request', 'The reference to the claim', SearchParamTypeREFERENCE, ['EnrollmentRequest'], 'EnrollmentResponse.request', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_EPISODEOFCARE}
procedure TFHIRIndexBuilder.buildIndexesForEpisodeOfCare(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('EpisodeOfCare', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('EpisodeOfCare', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('EpisodeOfCare', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('EpisodeOfCare', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('EpisodeOfCare', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('EpisodeOfCare', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('EpisodeOfCare', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('EpisodeOfCare', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('EpisodeOfCare', 'care-manager', 'Care manager/care co-ordinator for the patient', SearchParamTypeREFERENCE, ['Practitioner'], 'EpisodeOfCare.careManager', SearchXpathUsageNormal);
  indexes.add('EpisodeOfCare', 'condition', 'Conditions/problems/diagnoses this episode of care is for', SearchParamTypeREFERENCE, ['Condition'], 'EpisodeOfCare.diagnosis.condition', SearchXpathUsageNormal);
  indexes.add('EpisodeOfCare', 'date', 'The provided date search value falls within the episode of care''s period', SearchParamTypeDATE, [], 'Consent.dateTime | SupplyRequest.authoredOn | RiskAssessment.occurrence.as(DateTime) | CareTeam.period | FamilyMemberHistory.date | Encounter.period | AllergyIntolerance.assertedDate | CarePlan.period | EpisodeOfCare.period | Procedure.performed | Li'+'st.date | Immunization.date | Flag.period | Observation.effective | DiagnosticReport.effective | Composition.date | DetectedIssue.date | ClinicalImpression.date', SearchXpathUsageNormal);
  indexes.add('EpisodeOfCare', 'identifier', 'Business Identifier(s) relevant for this EpisodeOfCare', SearchParamTypeTOKEN, [], 'DocumentManifest.masterIdentifier | DocumentManifest.identifier | Goal.identifier | Consent.identifier | DocumentReference.masterIdentifier | DocumentReference.identifier | SupplyRequest.identifier | RiskAssessment.identifier | CareTeam.identifier | '+'ImagingStudy.identifier | FamilyMemberHistory.identifier | Encounter.identifier | DeviceRequest.identifier | AllergyIntolerance.identifier | CarePlan.identifier | EpisodeOfCare.identifier | Procedure.identifier | List.identifier | Immunization.identi'+'fier | VisionPrescription.identifier | ProcedureRequest.identifier | Observation.identifier | DiagnosticReport.identifier | NutritionOrder.identifier | Condition.identifier | Composition.identifier | DetectedIssue.identifier | SupplyDelivery.identifi'+'er', SearchXpathUsageNormal);
  indexes.add('EpisodeOfCare', 'incomingreferral', 'Incoming Referral Request', SearchParamTypeREFERENCE, ['ReferralRequest'], 'EpisodeOfCare.referralRequest', SearchXpathUsageNormal);
  indexes.add('EpisodeOfCare', 'organization', 'The organization that has assumed the specific responsibilities of this EpisodeOfCare', SearchParamTypeREFERENCE, ['Organization'], 'EpisodeOfCare.managingOrganization', SearchXpathUsageNormal);
  indexes.add('EpisodeOfCare', 'patient', 'The patient who is the focus of this episode of care', SearchParamTypeREFERENCE, ['Group', 'Patient'], 'ReferralRequest.subject | DocumentManifest.subject | Goal.subject | Consent.patient | DocumentReference.subject | ImagingManifest.patient | RiskAssessment.subject | CareTeam.subject | ImagingStudy.patient | FamilyMemberHistory.patient | Encounter.sub'+'ject | DeviceUseStatement.subject | DeviceRequest.subject | AllergyIntolerance.patient | CarePlan.subject | EpisodeOfCare.patient | Procedure.subject | List.subject | Immunization.patient | VisionPrescription.patient | ProcedureRequest.subject | Flag'+'.subject | Observation.subject | DiagnosticReport.subject | NutritionOrder.patient | Condition.subject | Composition.subject | DetectedIssue.patient | SupplyDelivery.patient | ClinicalImpression.subject', SearchXpathUsageNormal);
  indexes.add('EpisodeOfCare', 'status', 'The current status of the Episode of Care as provided (does not check the status history collection)', SearchParamTypeTOKEN, [], 'EpisodeOfCare.status', SearchXpathUsageNormal);
  indexes.add('EpisodeOfCare', 'type', 'Type/class  - e.g. specialist referral, disease management', SearchParamTypeTOKEN, [], 'ReferralRequest.type | DocumentManifest.type | DocumentReference.type | Encounter.type | AllergyIntolerance.type | EpisodeOfCare.type | Composition.type', SearchXpathUsageNormal);
  compartments.register(frtPatient, 'EpisodeOfCare', ['patient']);
  compartments.register(frtPractitioner, 'EpisodeOfCare', ['care-manager']);
end;
{$ENDIF}

{$IFDEF FHIR_EXPANSIONPROFILE}
procedure TFHIRIndexBuilder.buildIndexesForExpansionProfile(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ExpansionProfile', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('ExpansionProfile', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('ExpansionProfile', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('ExpansionProfile', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('ExpansionProfile', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ExpansionProfile', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('ExpansionProfile', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('ExpansionProfile', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('ExpansionProfile', 'date', 'The expansion profile publication date', SearchParamTypeDATE, [], 'ExpansionProfile.date', SearchXpathUsageNormal);
  indexes.add('ExpansionProfile', 'description', 'The description of the expansion profile', SearchParamTypeSTRING, [], 'ExpansionProfile.description', SearchXpathUsageNormal);
  indexes.add('ExpansionProfile', 'identifier', 'External identifier for the expansion profile', SearchParamTypeTOKEN, [], 'ExpansionProfile.identifier', SearchXpathUsageNormal);
  indexes.add('ExpansionProfile', 'jurisdiction', 'Intended jurisdiction for the expansion profile', SearchParamTypeTOKEN, [], 'ExpansionProfile.jurisdiction', SearchXpathUsageNormal);
  indexes.add('ExpansionProfile', 'name', 'Computationally friendly name of the expansion profile', SearchParamTypeSTRING, [], 'ExpansionProfile.name', SearchXpathUsageNormal);
  indexes.add('ExpansionProfile', 'publisher', 'Name of the publisher of the expansion profile', SearchParamTypeSTRING, [], 'ExpansionProfile.publisher', SearchXpathUsageNormal);
  indexes.add('ExpansionProfile', 'status', 'The current status of the expansion profile', SearchParamTypeTOKEN, [], 'ExpansionProfile.status', SearchXpathUsageNormal);
  indexes.add('ExpansionProfile', 'url', 'The uri that identifies the expansion profile', SearchParamTypeURI, [], 'ExpansionProfile.url', SearchXpathUsageNormal);
  indexes.add('ExpansionProfile', 'version', 'The business version of the expansion profile', SearchParamTypeTOKEN, [], 'ExpansionProfile.version', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_EXPLANATIONOFBENEFIT}
procedure TFHIRIndexBuilder.buildIndexesForExplanationOfBenefit(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ExplanationOfBenefit', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('ExplanationOfBenefit', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('ExplanationOfBenefit', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('ExplanationOfBenefit', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('ExplanationOfBenefit', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ExplanationOfBenefit', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('ExplanationOfBenefit', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('ExplanationOfBenefit', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('ExplanationOfBenefit', 'care-team', 'Member of the CareTeam', SearchParamTypeREFERENCE, ['Practitioner', 'Organization'], 'ExplanationOfBenefit.careTeam.provider', SearchXpathUsageNormal);
  indexes.add('ExplanationOfBenefit', 'claim', 'The reference to the claim', SearchParamTypeREFERENCE, ['Claim'], 'ExplanationOfBenefit.claim', SearchXpathUsageNormal);
  indexes.add('ExplanationOfBenefit', 'coverage', 'The plan under which the claim was adjudicated', SearchParamTypeREFERENCE, ['Coverage'], 'ExplanationOfBenefit.insurance.coverage', SearchXpathUsageNormal);
  indexes.add('ExplanationOfBenefit', 'created', 'The creation date for the EOB', SearchParamTypeDATE, [], 'ExplanationOfBenefit.created', SearchXpathUsageNormal);
  indexes.add('ExplanationOfBenefit', 'disposition', 'The contents of the disposition message', SearchParamTypeSTRING, [], 'ExplanationOfBenefit.disposition', SearchXpathUsageNormal);
  indexes.add('ExplanationOfBenefit', 'encounter', 'Encounters associated with a billed line item', SearchParamTypeREFERENCE, ['Encounter'], 'ExplanationOfBenefit.item.encounter', SearchXpathUsageNormal);
  indexes.add('ExplanationOfBenefit', 'enterer', 'The party responsible for the entry of the Claim', SearchParamTypeREFERENCE, ['Practitioner'], 'ExplanationOfBenefit.enterer', SearchXpathUsageNormal);
  indexes.add('ExplanationOfBenefit', 'facility', 'Facility responsible for the goods and services', SearchParamTypeREFERENCE, ['Location'], 'ExplanationOfBenefit.facility', SearchXpathUsageNormal);
  indexes.add('ExplanationOfBenefit', 'identifier', 'The business identifier of the Explanation of Benefit', SearchParamTypeTOKEN, [], 'ExplanationOfBenefit.identifier', SearchXpathUsageNormal);
  indexes.add('ExplanationOfBenefit', 'organization', 'The reference to the providing organization', SearchParamTypeREFERENCE, ['Organization'], 'ExplanationOfBenefit.organization', SearchXpathUsageNormal);
  indexes.add('ExplanationOfBenefit', 'patient', 'The reference to the patient', SearchParamTypeREFERENCE, ['Patient'], 'ExplanationOfBenefit.patient', SearchXpathUsageNormal);
  indexes.add('ExplanationOfBenefit', 'payee', 'The party receiving any payment for the Claim', SearchParamTypeREFERENCE, ['Practitioner', 'Organization', 'Patient', 'RelatedPerson'], 'ExplanationOfBenefit.payee.party', SearchXpathUsageNormal);
  indexes.add('ExplanationOfBenefit', 'provider', 'The reference to the provider', SearchParamTypeREFERENCE, ['Practitioner'], 'ExplanationOfBenefit.provider', SearchXpathUsageNormal);
  compartments.register(frtEncounter, 'ExplanationOfBenefit', ['encounter']);
  compartments.register(frtPatient, 'ExplanationOfBenefit', ['patient', 'payee']);
  compartments.register(frtPractitioner, 'ExplanationOfBenefit', ['enterer', 'provider', 'payee', 'care-team']);
  compartments.register(frtRelatedPerson, 'ExplanationOfBenefit', ['payee']);
end;
{$ENDIF}

{$IFDEF FHIR_FAMILYMEMBERHISTORY}
procedure TFHIRIndexBuilder.buildIndexesForFamilyMemberHistory(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('FamilyMemberHistory', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('FamilyMemberHistory', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('FamilyMemberHistory', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('FamilyMemberHistory', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('FamilyMemberHistory', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('FamilyMemberHistory', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('FamilyMemberHistory', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('FamilyMemberHistory', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('FamilyMemberHistory', 'code', 'A search by a condition code', SearchParamTypeTOKEN, [], 'FamilyMemberHistory.condition.code | DeviceRequest.code.as(CodeableConcept) | AllergyIntolerance.code | AllergyIntolerance.reaction.substance | Procedure.code | List.code | ProcedureRequest.code | Observation.code | DiagnosticReport.code | Condition.'+'code', SearchXpathUsageNormal);
  indexes.add('FamilyMemberHistory', 'date', 'When history was captured/updated', SearchParamTypeDATE, [], 'Consent.dateTime | SupplyRequest.authoredOn | RiskAssessment.occurrence.as(DateTime) | CareTeam.period | FamilyMemberHistory.date | Encounter.period | AllergyIntolerance.assertedDate | CarePlan.period | EpisodeOfCare.period | Procedure.performed | Li'+'st.date | Immunization.date | Flag.period | Observation.effective | DiagnosticReport.effective | Composition.date | DetectedIssue.date | ClinicalImpression.date', SearchXpathUsageNormal);
  indexes.add('FamilyMemberHistory', 'definition', 'Instantiates protocol or definition', SearchParamTypeREFERENCE, ['Questionnaire', 'PlanDefinition'], 'FamilyMemberHistory.definition', SearchXpathUsageNormal);
  indexes.add('FamilyMemberHistory', 'gender', 'A search by a gender code of a family member', SearchParamTypeTOKEN, [], 'FamilyMemberHistory.gender', SearchXpathUsageNormal);
  indexes.add('FamilyMemberHistory', 'identifier', 'A search by a record identifier', SearchParamTypeTOKEN, [], 'DocumentManifest.masterIdentifier | DocumentManifest.identifier | Goal.identifier | Consent.identifier | DocumentReference.masterIdentifier | DocumentReference.identifier | SupplyRequest.identifier | RiskAssessment.identifier | CareTeam.identifier | '+'ImagingStudy.identifier | FamilyMemberHistory.identifier | Encounter.identifier | DeviceRequest.identifier | AllergyIntolerance.identifier | CarePlan.identifier | EpisodeOfCare.identifier | Procedure.identifier | List.identifier | Immunization.identi'+'fier | VisionPrescription.identifier | ProcedureRequest.identifier | Observation.identifier | DiagnosticReport.identifier | NutritionOrder.identifier | Condition.identifier | Composition.identifier | DetectedIssue.identifier | SupplyDelivery.identifi'+'er', SearchXpathUsageNormal);
  indexes.add('FamilyMemberHistory', 'patient', 'The identity of a subject to list family member history items for', SearchParamTypeREFERENCE, ['Group', 'Patient'], 'ReferralRequest.subject | DocumentManifest.subject | Goal.subject | Consent.patient | DocumentReference.subject | ImagingManifest.patient | RiskAssessment.subject | CareTeam.subject | ImagingStudy.patient | FamilyMemberHistory.patient | Encounter.sub'+'ject | DeviceUseStatement.subject | DeviceRequest.subject | AllergyIntolerance.patient | CarePlan.subject | EpisodeOfCare.patient | Procedure.subject | List.subject | Immunization.patient | VisionPrescription.patient | ProcedureRequest.subject | Flag'+'.subject | Observation.subject | DiagnosticReport.subject | NutritionOrder.patient | Condition.subject | Composition.subject | DetectedIssue.patient | SupplyDelivery.patient | ClinicalImpression.subject', SearchXpathUsageNormal);
  indexes.add('FamilyMemberHistory', 'relationship', 'A search by a relationship type', SearchParamTypeTOKEN, [], 'FamilyMemberHistory.relationship', SearchXpathUsageNormal);
  indexes.add('FamilyMemberHistory', 'status', 'partial | completed | entered-in-error | health-unknown', SearchParamTypeTOKEN, [], 'FamilyMemberHistory.status', SearchXpathUsageNormal);
  compartments.register(frtPatient, 'FamilyMemberHistory', ['patient']);
end;
{$ENDIF}

{$IFDEF FHIR_FLAG}
procedure TFHIRIndexBuilder.buildIndexesForFlag(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Flag', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Flag', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('Flag', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('Flag', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('Flag', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Flag', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('Flag', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('Flag', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Flag', 'author', 'Flag creator', SearchParamTypeREFERENCE, ['Practitioner', 'Organization', 'Device', 'Patient'], 'Flag.author', SearchXpathUsageNormal);
  indexes.add('Flag', 'date', 'Time period when flag is active', SearchParamTypeDATE, [], 'Consent.dateTime | SupplyRequest.authoredOn | RiskAssessment.occurrence.as(DateTime) | CareTeam.period | FamilyMemberHistory.date | Encounter.period | AllergyIntolerance.assertedDate | CarePlan.period | EpisodeOfCare.period | Procedure.performed | Li'+'st.date | Immunization.date | Flag.period | Observation.effective | DiagnosticReport.effective | Composition.date | DetectedIssue.date | ClinicalImpression.date', SearchXpathUsageNormal);
  indexes.add('Flag', 'encounter', 'Alert relevant during encounter', SearchParamTypeREFERENCE, ['EpisodeOfCare', 'Encounter'], 'DocumentReference.context.encounter | RiskAssessment.context | DeviceRequest.context | Procedure.context | List.encounter | VisionPrescription.encounter | ProcedureRequest.context | Flag.encounter | Observation.context | DiagnosticReport.context | Nu'+'tritionOrder.encounter | Composition.encounter', SearchXpathUsageNormal);
  indexes.add('Flag', 'identifier', 'Business identifier', SearchParamTypeTOKEN, [], 'Flag.identifier', SearchXpathUsageNormal);
  indexes.add('Flag', 'patient', 'The identity of a subject to list flags for', SearchParamTypeREFERENCE, ['Group', 'Patient'], 'ReferralRequest.subject | DocumentManifest.subject | Goal.subject | Consent.patient | DocumentReference.subject | ImagingManifest.patient | RiskAssessment.subject | CareTeam.subject | ImagingStudy.patient | FamilyMemberHistory.patient | Encounter.sub'+'ject | DeviceUseStatement.subject | DeviceRequest.subject | AllergyIntolerance.patient | CarePlan.subject | EpisodeOfCare.patient | Procedure.subject | List.subject | Immunization.patient | VisionPrescription.patient | ProcedureRequest.subject | Flag'+'.subject | Observation.subject | DiagnosticReport.subject | NutritionOrder.patient | Condition.subject | Composition.subject | DetectedIssue.patient | SupplyDelivery.patient | ClinicalImpression.subject', SearchXpathUsageNormal);
  indexes.add('Flag', 'subject', 'The identity of a subject to list flags for', SearchParamTypeREFERENCE, ['Practitioner', 'Group', 'Organization', 'Medication', 'Patient', 'PlanDefinition', 'Procedure', 'Location'], 'Flag.subject', SearchXpathUsageNormal);
  compartments.register(frtDevice, 'Flag', ['author']);
  compartments.register(frtPatient, 'Flag', ['patient']);
  compartments.register(frtPractitioner, 'Flag', ['author']);
end;
{$ENDIF}

{$IFDEF FHIR_GOAL}
procedure TFHIRIndexBuilder.buildIndexesForGoal(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Goal', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Goal', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('Goal', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('Goal', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('Goal', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Goal', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('Goal', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('Goal', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Goal', 'category', 'E.g. Treatment, dietary, behavioral, etc.', SearchParamTypeTOKEN, [], 'Goal.category', SearchXpathUsageNormal);
  indexes.add('Goal', 'identifier', 'External Ids for this goal', SearchParamTypeTOKEN, [], 'DocumentManifest.masterIdentifier | DocumentManifest.identifier | Goal.identifier | Consent.identifier | DocumentReference.masterIdentifier | DocumentReference.identifier | SupplyRequest.identifier | RiskAssessment.identifier | CareTeam.identifier | '+'ImagingStudy.identifier | FamilyMemberHistory.identifier | Encounter.identifier | DeviceRequest.identifier | AllergyIntolerance.identifier | CarePlan.identifier | EpisodeOfCare.identifier | Procedure.identifier | List.identifier | Immunization.identi'+'fier | VisionPrescription.identifier | ProcedureRequest.identifier | Observation.identifier | DiagnosticReport.identifier | NutritionOrder.identifier | Condition.identifier | Composition.identifier | DetectedIssue.identifier | SupplyDelivery.identifi'+'er', SearchXpathUsageNormal);
  indexes.add('Goal', 'patient', 'Who this goal is intended for', SearchParamTypeREFERENCE, ['Group', 'Patient'], 'ReferralRequest.subject | DocumentManifest.subject | Goal.subject | Consent.patient | DocumentReference.subject | ImagingManifest.patient | RiskAssessment.subject | CareTeam.subject | ImagingStudy.patient | FamilyMemberHistory.patient | Encounter.sub'+'ject | DeviceUseStatement.subject | DeviceRequest.subject | AllergyIntolerance.patient | CarePlan.subject | EpisodeOfCare.patient | Procedure.subject | List.subject | Immunization.patient | VisionPrescription.patient | ProcedureRequest.subject | Flag'+'.subject | Observation.subject | DiagnosticReport.subject | NutritionOrder.patient | Condition.subject | Composition.subject | DetectedIssue.patient | SupplyDelivery.patient | ClinicalImpression.subject', SearchXpathUsageNormal);
  indexes.add('Goal', 'start-date', 'When goal pursuit begins', SearchParamTypeDATE, [], 'Goal.start.as(Date)', SearchXpathUsageNormal);
  indexes.add('Goal', 'status', 'proposed | accepted | planned | in-progress | on-target | ahead-of-target | behind-target | sustaining | achieved | on-hold | cancelled | entered-in-error | rejected', SearchParamTypeTOKEN, [], 'Goal.status', SearchXpathUsageNormal);
  indexes.add('Goal', 'subject', 'Who this goal is intended for', SearchParamTypeREFERENCE, ['Group', 'Organization', 'Patient'], 'Goal.subject', SearchXpathUsageNormal);
  indexes.add('Goal', 'target-date', 'Reach goal on or before', SearchParamTypeDATE, [], 'Goal.target.due.as(Date)', SearchXpathUsageNormal);
  compartments.register(frtPatient, 'Goal', ['patient']);
end;
{$ENDIF}

{$IFDEF FHIR_GRAPHDEFINITION}
procedure TFHIRIndexBuilder.buildIndexesForGraphDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('GraphDefinition', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('GraphDefinition', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('GraphDefinition', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('GraphDefinition', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('GraphDefinition', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('GraphDefinition', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('GraphDefinition', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('GraphDefinition', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('GraphDefinition', 'date', 'The graph definition publication date', SearchParamTypeDATE, [], 'GraphDefinition.date', SearchXpathUsageNormal);
  indexes.add('GraphDefinition', 'description', 'The description of the graph definition', SearchParamTypeSTRING, [], 'GraphDefinition.description', SearchXpathUsageNormal);
  indexes.add('GraphDefinition', 'jurisdiction', 'Intended jurisdiction for the graph definition', SearchParamTypeTOKEN, [], 'GraphDefinition.jurisdiction', SearchXpathUsageNormal);
  indexes.add('GraphDefinition', 'name', 'Computationally friendly name of the graph definition', SearchParamTypeSTRING, [], 'GraphDefinition.name', SearchXpathUsageNormal);
  indexes.add('GraphDefinition', 'publisher', 'Name of the publisher of the graph definition', SearchParamTypeSTRING, [], 'GraphDefinition.publisher', SearchXpathUsageNormal);
  indexes.add('GraphDefinition', 'start', 'Type of resource at which the graph starts', SearchParamTypeTOKEN, [], 'GraphDefinition.start', SearchXpathUsageNormal);
  indexes.add('GraphDefinition', 'status', 'The current status of the graph definition', SearchParamTypeTOKEN, [], 'GraphDefinition.status', SearchXpathUsageNormal);
  indexes.add('GraphDefinition', 'url', 'The uri that identifies the graph definition', SearchParamTypeURI, [], 'GraphDefinition.url', SearchXpathUsageNormal);
  indexes.add('GraphDefinition', 'version', 'The business version of the graph definition', SearchParamTypeTOKEN, [], 'GraphDefinition.version', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_GROUP}
procedure TFHIRIndexBuilder.buildIndexesForGroup(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Group', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Group', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('Group', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('Group', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('Group', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Group', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('Group', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('Group', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Group', 'actual', 'Descriptive or actual', SearchParamTypeTOKEN, [], 'Group.actual', SearchXpathUsageNormal);
  indexes.add('Group', 'characteristic', 'Kind of characteristic', SearchParamTypeTOKEN, [], 'Group.characteristic.code', SearchXpathUsageNormal);
  indexes.add('Group', 'characteristic-value', 'A composite of both characteristic and value', SearchParamTypeCOMPOSITE, [], 'Group.characteristic', SearchXpathUsageNormal);
  indexes.add('Group', 'code', 'The kind of resources contained', SearchParamTypeTOKEN, [], 'Group.code', SearchXpathUsageNormal);
  indexes.add('Group', 'exclude', 'Group includes or excludes', SearchParamTypeTOKEN, [], 'Group.characteristic.exclude', SearchXpathUsageNormal);
  indexes.add('Group', 'identifier', 'Unique id', SearchParamTypeTOKEN, [], 'Group.identifier', SearchXpathUsageNormal);
  indexes.add('Group', 'member', 'Reference to the group member', SearchParamTypeREFERENCE, ['Practitioner', 'Device', 'Medication', 'Patient', 'Substance'], 'Group.member.entity', SearchXpathUsageNormal);
  indexes.add('Group', 'type', 'The type of resources the group contains', SearchParamTypeTOKEN, [], 'Group.type', SearchXpathUsageNormal);
  indexes.add('Group', 'value', 'Value held by characteristic', SearchParamTypeTOKEN, [], 'Group.characteristic.value', SearchXpathUsageNormal);
  compartments.register(frtDevice, 'Group', ['member']);
  compartments.register(frtPatient, 'Group', ['member']);
  compartments.register(frtPractitioner, 'Group', ['member']);
end;
{$ENDIF}

{$IFDEF FHIR_GUIDANCERESPONSE}
procedure TFHIRIndexBuilder.buildIndexesForGuidanceResponse(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('GuidanceResponse', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('GuidanceResponse', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('GuidanceResponse', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('GuidanceResponse', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('GuidanceResponse', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('GuidanceResponse', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('GuidanceResponse', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('GuidanceResponse', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('GuidanceResponse', 'identifier', 'The identifier of the guidance response', SearchParamTypeTOKEN, [], 'GuidanceResponse.identifier', SearchXpathUsageNormal);
  indexes.add('GuidanceResponse', 'patient', 'The identity of a patient to search for guidance response results', SearchParamTypeREFERENCE, ['Patient'], 'GuidanceResponse.subject', SearchXpathUsageNormal);
  indexes.add('GuidanceResponse', 'request', 'The identifier of the request associated with the response', SearchParamTypeTOKEN, [], 'GuidanceResponse.requestId', SearchXpathUsageNormal);
  indexes.add('GuidanceResponse', 'subject', 'The subject that the guidance response is about', SearchParamTypeREFERENCE, ['Group', 'Patient'], 'GuidanceResponse.subject', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_HEALTHCARESERVICE}
procedure TFHIRIndexBuilder.buildIndexesForHealthcareService(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('HealthcareService', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('HealthcareService', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('HealthcareService', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('HealthcareService', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('HealthcareService', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('HealthcareService', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('HealthcareService', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('HealthcareService', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('HealthcareService', 'active', 'The Healthcare Service is currently marked as active', SearchParamTypeTOKEN, [], 'HealthcareService.active', SearchXpathUsageNormal);
  indexes.add('HealthcareService', 'category', 'Service Category of the Healthcare Service', SearchParamTypeTOKEN, [], 'HealthcareService.category', SearchXpathUsageNormal);
  indexes.add('HealthcareService', 'characteristic', 'One of the HealthcareService''s characteristics', SearchParamTypeTOKEN, [], 'HealthcareService.characteristic', SearchXpathUsageNormal);
  indexes.add('HealthcareService', 'endpoint', 'Technical endpoints providing access to services operated for the location', SearchParamTypeREFERENCE, ['Endpoint'], 'HealthcareService.endpoint', SearchXpathUsageNormal);
  indexes.add('HealthcareService', 'identifier', 'External identifiers for this item', SearchParamTypeTOKEN, [], 'HealthcareService.identifier', SearchXpathUsageNormal);
  indexes.add('HealthcareService', 'location', 'The location of the Healthcare Service', SearchParamTypeREFERENCE, ['Location'], 'HealthcareService.location', SearchXpathUsageNormal);
  indexes.add('HealthcareService', 'name', 'A portion of the Healthcare service name', SearchParamTypeSTRING, [], 'HealthcareService.name', SearchXpathUsageNormal);
  indexes.add('HealthcareService', 'organization', 'The organization that provides this Healthcare Service', SearchParamTypeREFERENCE, ['Organization'], 'HealthcareService.providedBy', SearchXpathUsageNormal);
  indexes.add('HealthcareService', 'programname', 'One of the Program Names serviced by this HealthcareService', SearchParamTypeSTRING, [], 'HealthcareService.programName', SearchXpathUsageNormal);
  indexes.add('HealthcareService', 'type', 'The type of service provided by this healthcare service', SearchParamTypeTOKEN, [], 'HealthcareService.type', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_IMAGINGMANIFEST}
procedure TFHIRIndexBuilder.buildIndexesForImagingManifest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ImagingManifest', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('ImagingManifest', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('ImagingManifest', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('ImagingManifest', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('ImagingManifest', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ImagingManifest', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('ImagingManifest', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('ImagingManifest', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('ImagingManifest', 'author', 'Author of the ImagingManifest (or a DICOM Key Object Selection which it represents)', SearchParamTypeREFERENCE, ['Practitioner', 'Organization', 'Device', 'Patient', 'RelatedPerson'], 'ImagingManifest.author', SearchXpathUsageNormal);
  indexes.add('ImagingManifest', 'authoring-time', 'Time of the ImagingManifest (or a DICOM Key Object Selection which it represents) authoring', SearchParamTypeDATE, [], 'ImagingManifest.authoringTime', SearchXpathUsageNormal);
  indexes.add('ImagingManifest', 'endpoint', 'The endpoint for the study or series', SearchParamTypeREFERENCE, ['Endpoint'], 'ImagingManifest.study.endpoint | ImagingManifest.study.series.endpoint', SearchXpathUsageNormal);
  indexes.add('ImagingManifest', 'identifier', 'UID of the ImagingManifest (or a DICOM Key Object Selection which it represents)', SearchParamTypeTOKEN, [], 'ImagingManifest.identifier', SearchXpathUsageNormal);
  indexes.add('ImagingManifest', 'imaging-study', 'ImagingStudy resource selected in the ImagingManifest (or a DICOM Key Object Selection which it represents)', SearchParamTypeREFERENCE, ['ImagingStudy'], 'ImagingManifest.study.imagingStudy', SearchXpathUsageNormal);
  indexes.add('ImagingManifest', 'patient', 'Subject of the ImagingManifest (or a DICOM Key Object Selection which it represents)', SearchParamTypeREFERENCE, ['Group', 'Patient'], 'ReferralRequest.subject | DocumentManifest.subject | Goal.subject | Consent.patient | DocumentReference.subject | ImagingManifest.patient | RiskAssessment.subject | CareTeam.subject | ImagingStudy.patient | FamilyMemberHistory.patient | Encounter.sub'+'ject | DeviceUseStatement.subject | DeviceRequest.subject | AllergyIntolerance.patient | CarePlan.subject | EpisodeOfCare.patient | Procedure.subject | List.subject | Immunization.patient | VisionPrescription.patient | ProcedureRequest.subject | Flag'+'.subject | Observation.subject | DiagnosticReport.subject | NutritionOrder.patient | Condition.subject | Composition.subject | DetectedIssue.patient | SupplyDelivery.patient | ClinicalImpression.subject', SearchXpathUsageNormal);
  indexes.add('ImagingManifest', 'selected-study', 'Study selected in the ImagingManifest (or a DICOM Key Object Selection which it represents)', SearchParamTypeURI, [], 'ImagingManifest.study.uid', SearchXpathUsageNormal);
  compartments.register(frtDevice, 'ImagingManifest', ['author']);
  compartments.register(frtPatient, 'ImagingManifest', ['patient', 'author']);
  compartments.register(frtPractitioner, 'ImagingManifest', ['author']);
  compartments.register(frtRelatedPerson, 'ImagingManifest', ['author']);
end;
{$ENDIF}

{$IFDEF FHIR_IMAGINGSTUDY}
procedure TFHIRIndexBuilder.buildIndexesForImagingStudy(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ImagingStudy', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('ImagingStudy', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('ImagingStudy', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('ImagingStudy', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('ImagingStudy', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ImagingStudy', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('ImagingStudy', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('ImagingStudy', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('ImagingStudy', 'accession', 'The accession identifier for the study', SearchParamTypeTOKEN, [], 'ImagingStudy.accession', SearchXpathUsageNormal);
  indexes.add('ImagingStudy', 'basedon', 'The order for the image', SearchParamTypeREFERENCE, ['ReferralRequest', 'CarePlan', 'ProcedureRequest'], 'ImagingStudy.basedOn', SearchXpathUsageNormal);
  indexes.add('ImagingStudy', 'bodysite', 'The body site studied', SearchParamTypeTOKEN, [], 'ImagingStudy.series.bodySite', SearchXpathUsageNormal);
  indexes.add('ImagingStudy', 'context', 'The context of the study', SearchParamTypeREFERENCE, ['EpisodeOfCare', 'Encounter'], 'ImagingStudy.context', SearchXpathUsageNormal);
  indexes.add('ImagingStudy', 'dicom-class', 'The type of the instance', SearchParamTypeURI, [], 'ImagingStudy.series.instance.sopClass', SearchXpathUsageNormal);
  indexes.add('ImagingStudy', 'endpoint', 'The endpoint for te study or series', SearchParamTypeREFERENCE, ['Endpoint'], 'ImagingStudy.endpoint | ImagingStudy.series.endpoint', SearchXpathUsageNormal);
  indexes.add('ImagingStudy', 'identifier', 'Other identifiers for the Study', SearchParamTypeTOKEN, [], 'DocumentManifest.masterIdentifier | DocumentManifest.identifier | Goal.identifier | Consent.identifier | DocumentReference.masterIdentifier | DocumentReference.identifier | SupplyRequest.identifier | RiskAssessment.identifier | CareTeam.identifier | '+'ImagingStudy.identifier | FamilyMemberHistory.identifier | Encounter.identifier | DeviceRequest.identifier | AllergyIntolerance.identifier | CarePlan.identifier | EpisodeOfCare.identifier | Procedure.identifier | List.identifier | Immunization.identi'+'fier | VisionPrescription.identifier | ProcedureRequest.identifier | Observation.identifier | DiagnosticReport.identifier | NutritionOrder.identifier | Condition.identifier | Composition.identifier | DetectedIssue.identifier | SupplyDelivery.identifi'+'er', SearchXpathUsageNormal);
  indexes.add('ImagingStudy', 'modality', 'The modality of the series', SearchParamTypeTOKEN, [], 'ImagingStudy.series.modality', SearchXpathUsageNormal);
  indexes.add('ImagingStudy', 'patient', 'Who the study is about', SearchParamTypeREFERENCE, ['Group', 'Patient'], 'ReferralRequest.subject | DocumentManifest.subject | Goal.subject | Consent.patient | DocumentReference.subject | ImagingManifest.patient | RiskAssessment.subject | CareTeam.subject | ImagingStudy.patient | FamilyMemberHistory.patient | Encounter.sub'+'ject | DeviceUseStatement.subject | DeviceRequest.subject | AllergyIntolerance.patient | CarePlan.subject | EpisodeOfCare.patient | Procedure.subject | List.subject | Immunization.patient | VisionPrescription.patient | ProcedureRequest.subject | Flag'+'.subject | Observation.subject | DiagnosticReport.subject | NutritionOrder.patient | Condition.subject | Composition.subject | DetectedIssue.patient | SupplyDelivery.patient | ClinicalImpression.subject', SearchXpathUsageNormal);
  indexes.add('ImagingStudy', 'performer', 'The person who performed the study', SearchParamTypeREFERENCE, ['Practitioner'], 'ImagingStudy.series.performer', SearchXpathUsageNormal);
  indexes.add('ImagingStudy', 'reason', 'The reason for the study', SearchParamTypeTOKEN, [], 'ImagingStudy.reason', SearchXpathUsageNormal);
  indexes.add('ImagingStudy', 'series', 'The identifier of the series of images', SearchParamTypeURI, [], 'ImagingStudy.series.uid', SearchXpathUsageNormal);
  indexes.add('ImagingStudy', 'started', 'When the study was started', SearchParamTypeDATE, [], 'ImagingStudy.started', SearchXpathUsageNormal);
  indexes.add('ImagingStudy', 'study', 'The study identifier for the image', SearchParamTypeURI, [], 'ImagingStudy.uid', SearchXpathUsageNormal);
  indexes.add('ImagingStudy', 'uid', 'The instance unique identifier', SearchParamTypeURI, [], 'ImagingStudy.series.instance.uid', SearchXpathUsageNormal);
  compartments.register(frtPatient, 'ImagingStudy', ['patient']);
end;
{$ENDIF}

{$IFDEF FHIR_IMMUNIZATION}
procedure TFHIRIndexBuilder.buildIndexesForImmunization(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Immunization', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Immunization', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('Immunization', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('Immunization', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('Immunization', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Immunization', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('Immunization', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('Immunization', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Immunization', 'date', 'Vaccination  (non)-Administration Date', SearchParamTypeDATE, [], 'Consent.dateTime | SupplyRequest.authoredOn | RiskAssessment.occurrence.as(DateTime) | CareTeam.period | FamilyMemberHistory.date | Encounter.period | AllergyIntolerance.assertedDate | CarePlan.period | EpisodeOfCare.period | Procedure.performed | Li'+'st.date | Immunization.date | Flag.period | Observation.effective | DiagnosticReport.effective | Composition.date | DetectedIssue.date | ClinicalImpression.date', SearchXpathUsageNormal);
  indexes.add('Immunization', 'dose-sequence', 'Dose number within series', SearchParamTypeNUMBER, [], 'Immunization.vaccinationProtocol.doseSequence', SearchXpathUsageNormal);
  indexes.add('Immunization', 'identifier', 'Business identifier', SearchParamTypeTOKEN, [], 'DocumentManifest.masterIdentifier | DocumentManifest.identifier | Goal.identifier | Consent.identifier | DocumentReference.masterIdentifier | DocumentReference.identifier | SupplyRequest.identifier | RiskAssessment.identifier | CareTeam.identifier | '+'ImagingStudy.identifier | FamilyMemberHistory.identifier | Encounter.identifier | DeviceRequest.identifier | AllergyIntolerance.identifier | CarePlan.identifier | EpisodeOfCare.identifier | Procedure.identifier | List.identifier | Immunization.identi'+'fier | VisionPrescription.identifier | ProcedureRequest.identifier | Observation.identifier | DiagnosticReport.identifier | NutritionOrder.identifier | Condition.identifier | Composition.identifier | DetectedIssue.identifier | SupplyDelivery.identifi'+'er', SearchXpathUsageNormal);
  indexes.add('Immunization', 'location', 'The service delivery location or facility in which the vaccine was / was to be administered', SearchParamTypeREFERENCE, ['Location'], 'Immunization.location', SearchXpathUsageNormal);
  indexes.add('Immunization', 'lot-number', 'Vaccine Lot Number', SearchParamTypeSTRING, [], 'Immunization.lotNumber', SearchXpathUsageNormal);
  indexes.add('Immunization', 'manufacturer', 'Vaccine Manufacturer', SearchParamTypeREFERENCE, ['Organization'], 'Immunization.manufacturer', SearchXpathUsageNormal);
  indexes.add('Immunization', 'notgiven', 'Administrations which were not given', SearchParamTypeTOKEN, [], 'Immunization.notGiven', SearchXpathUsageNormal);
  indexes.add('Immunization', 'patient', 'The patient for the vaccination record', SearchParamTypeREFERENCE, ['Group', 'Patient'], 'ReferralRequest.subject | DocumentManifest.subject | Goal.subject | Consent.patient | DocumentReference.subject | ImagingManifest.patient | RiskAssessment.subject | CareTeam.subject | ImagingStudy.patient | FamilyMemberHistory.patient | Encounter.sub'+'ject | DeviceUseStatement.subject | DeviceRequest.subject | AllergyIntolerance.patient | CarePlan.subject | EpisodeOfCare.patient | Procedure.subject | List.subject | Immunization.patient | VisionPrescription.patient | ProcedureRequest.subject | Flag'+'.subject | Observation.subject | DiagnosticReport.subject | NutritionOrder.patient | Condition.subject | Composition.subject | DetectedIssue.patient | SupplyDelivery.patient | ClinicalImpression.subject', SearchXpathUsageNormal);
  indexes.add('Immunization', 'practitioner', 'The practitioner who played a role in the vaccination', SearchParamTypeREFERENCE, ['Practitioner'], 'Immunization.practitioner.actor', SearchXpathUsageNormal);
  indexes.add('Immunization', 'reaction', 'Additional information on reaction', SearchParamTypeREFERENCE, ['Observation'], 'Immunization.reaction.detail', SearchXpathUsageNormal);
  indexes.add('Immunization', 'reaction-date', 'When reaction started', SearchParamTypeDATE, [], 'Immunization.reaction.date', SearchXpathUsageNormal);
  indexes.add('Immunization', 'reason', 'Why immunization occurred', SearchParamTypeTOKEN, [], 'Immunization.explanation.reason', SearchXpathUsageNormal);
  indexes.add('Immunization', 'reason-not-given', 'Explanation of reason vaccination was not administered', SearchParamTypeTOKEN, [], 'Immunization.explanation.reasonNotGiven', SearchXpathUsageNormal);
  indexes.add('Immunization', 'status', 'Immunization event status', SearchParamTypeTOKEN, [], 'Immunization.status', SearchXpathUsageNormal);
  indexes.add('Immunization', 'vaccine-code', 'Vaccine Product Administered', SearchParamTypeTOKEN, [], 'Immunization.vaccineCode', SearchXpathUsageNormal);
  compartments.register(frtPatient, 'Immunization', ['patient']);
  compartments.register(frtPractitioner, 'Immunization', ['practitioner']);
end;
{$ENDIF}

{$IFDEF FHIR_IMMUNIZATIONRECOMMENDATION}
procedure TFHIRIndexBuilder.buildIndexesForImmunizationRecommendation(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ImmunizationRecommendation', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('ImmunizationRecommendation', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('ImmunizationRecommendation', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('ImmunizationRecommendation', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('ImmunizationRecommendation', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ImmunizationRecommendation', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('ImmunizationRecommendation', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('ImmunizationRecommendation', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('ImmunizationRecommendation', 'date', 'Date recommendation created', SearchParamTypeDATE, [], 'ImmunizationRecommendation.recommendation.date', SearchXpathUsageNormal);
  indexes.add('ImmunizationRecommendation', 'dose-number', 'Recommended dose number', SearchParamTypeNUMBER, [], 'ImmunizationRecommendation.recommendation.doseNumber', SearchXpathUsageNormal);
  indexes.add('ImmunizationRecommendation', 'dose-sequence', 'Dose number within sequence', SearchParamTypeNUMBER, [], 'ImmunizationRecommendation.recommendation.protocol.doseSequence', SearchXpathUsageNormal);
  indexes.add('ImmunizationRecommendation', 'identifier', 'Business identifier', SearchParamTypeTOKEN, [], 'ImmunizationRecommendation.identifier', SearchXpathUsageNormal);
  indexes.add('ImmunizationRecommendation', 'information', 'Patient observations supporting recommendation', SearchParamTypeREFERENCE, ['AllergyIntolerance', 'Observation'], 'ImmunizationRecommendation.recommendation.supportingPatientInformation', SearchXpathUsageNormal);
  indexes.add('ImmunizationRecommendation', 'patient', 'Who this profile is for', SearchParamTypeREFERENCE, ['Patient'], 'ImmunizationRecommendation.patient', SearchXpathUsageNormal);
  indexes.add('ImmunizationRecommendation', 'status', 'Vaccine administration status', SearchParamTypeTOKEN, [], 'ImmunizationRecommendation.recommendation.forecastStatus', SearchXpathUsageNormal);
  indexes.add('ImmunizationRecommendation', 'support', 'Past immunizations supporting recommendation', SearchParamTypeREFERENCE, ['Immunization'], 'ImmunizationRecommendation.recommendation.supportingImmunization', SearchXpathUsageNormal);
  indexes.add('ImmunizationRecommendation', 'target-disease', 'Disease to be immunized against', SearchParamTypeTOKEN, [], 'ImmunizationRecommendation.recommendation.targetDisease', SearchXpathUsageNormal);
  indexes.add('ImmunizationRecommendation', 'vaccine-type', 'Vaccine recommendation applies to', SearchParamTypeTOKEN, [], 'ImmunizationRecommendation.recommendation.vaccineCode', SearchXpathUsageNormal);
  compartments.register(frtPatient, 'ImmunizationRecommendation', ['patient']);
end;
{$ENDIF}

{$IFDEF FHIR_IMPLEMENTATIONGUIDE}
procedure TFHIRIndexBuilder.buildIndexesForImplementationGuide(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ImplementationGuide', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('ImplementationGuide', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('ImplementationGuide', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('ImplementationGuide', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('ImplementationGuide', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ImplementationGuide', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('ImplementationGuide', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('ImplementationGuide', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('ImplementationGuide', 'date', 'The implementation guide publication date', SearchParamTypeDATE, [], 'ImplementationGuide.date', SearchXpathUsageNormal);
  indexes.add('ImplementationGuide', 'dependency', 'Where to find dependency', SearchParamTypeURI, [], 'ImplementationGuide.dependency.uri', SearchXpathUsageNormal);
  indexes.add('ImplementationGuide', 'description', 'The description of the implementation guide', SearchParamTypeSTRING, [], 'ImplementationGuide.description', SearchXpathUsageNormal);
  indexes.add('ImplementationGuide', 'experimental', 'For testing purposes, not real usage', SearchParamTypeTOKEN, [], 'ImplementationGuide.experimental', SearchXpathUsageNormal);
  indexes.add('ImplementationGuide', 'jurisdiction', 'Intended jurisdiction for the implementation guide', SearchParamTypeTOKEN, [], 'ImplementationGuide.jurisdiction', SearchXpathUsageNormal);
  indexes.add('ImplementationGuide', 'name', 'Computationally friendly name of the implementation guide', SearchParamTypeSTRING, [], 'ImplementationGuide.name', SearchXpathUsageNormal);
  indexes.add('ImplementationGuide', 'publisher', 'Name of the publisher of the implementation guide', SearchParamTypeSTRING, [], 'ImplementationGuide.publisher', SearchXpathUsageNormal);
  indexes.add('ImplementationGuide', 'resource', 'Location of the resource', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'ImplementationGuide.package.resource.source', SearchXpathUsageNormal);
  indexes.add('ImplementationGuide', 'status', 'The current status of the implementation guide', SearchParamTypeTOKEN, [], 'ImplementationGuide.status', SearchXpathUsageNormal);
  indexes.add('ImplementationGuide', 'url', 'The uri that identifies the implementation guide', SearchParamTypeURI, [], 'ImplementationGuide.url', SearchXpathUsageNormal);
  indexes.add('ImplementationGuide', 'version', 'The business version of the implementation guide', SearchParamTypeTOKEN, [], 'ImplementationGuide.version', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_LIBRARY}
procedure TFHIRIndexBuilder.buildIndexesForLibrary(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Library', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Library', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('Library', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('Library', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('Library', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Library', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('Library', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('Library', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Library', 'composed-of', 'What resource is being referenced', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'Library.relatedArtifact.where(type=''composed-of'').resource', SearchXpathUsageNormal);
  indexes.add('Library', 'date', 'The library publication date', SearchParamTypeDATE, [], 'Library.date', SearchXpathUsageNormal);
  indexes.add('Library', 'depends-on', 'What resource is being referenced', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'Library.relatedArtifact.where(type=''depends-on'').resource', SearchXpathUsageNormal);
  indexes.add('Library', 'derived-from', 'What resource is being referenced', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'Library.relatedArtifact.where(type=''derived-from'').resource', SearchXpathUsageNormal);
  indexes.add('Library', 'description', 'The description of the library', SearchParamTypeSTRING, [], 'Library.description', SearchXpathUsageNormal);
  indexes.add('Library', 'effective', 'The time during which the library is intended to be in use', SearchParamTypeDATE, [], 'Library.effectivePeriod', SearchXpathUsageNormal);
  indexes.add('Library', 'identifier', 'External identifier for the library', SearchParamTypeTOKEN, [], 'Library.identifier', SearchXpathUsageNormal);
  indexes.add('Library', 'jurisdiction', 'Intended jurisdiction for the library', SearchParamTypeTOKEN, [], 'Library.jurisdiction', SearchXpathUsageNormal);
  indexes.add('Library', 'name', 'Computationally friendly name of the library', SearchParamTypeSTRING, [], 'Library.name', SearchXpathUsageNormal);
  indexes.add('Library', 'predecessor', 'What resource is being referenced', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'Library.relatedArtifact.where(type=''predecessor'').resource', SearchXpathUsageNormal);
  indexes.add('Library', 'publisher', 'Name of the publisher of the library', SearchParamTypeSTRING, [], 'Library.publisher', SearchXpathUsageNormal);
  indexes.add('Library', 'status', 'The current status of the library', SearchParamTypeTOKEN, [], 'Library.status', SearchXpathUsageNormal);
  indexes.add('Library', 'successor', 'What resource is being referenced', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'Library.relatedArtifact.where(type=''successor'').resource', SearchXpathUsageNormal);
  indexes.add('Library', 'title', 'The human-friendly name of the library', SearchParamTypeSTRING, [], 'Library.title', SearchXpathUsageNormal);
  indexes.add('Library', 'topic', 'Topics associated with the module', SearchParamTypeTOKEN, [], 'Library.topic', SearchXpathUsageNormal);
  indexes.add('Library', 'url', 'The uri that identifies the library', SearchParamTypeURI, [], 'Library.url', SearchXpathUsageNormal);
  indexes.add('Library', 'version', 'The business version of the library', SearchParamTypeTOKEN, [], 'Library.version', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_LINKAGE}
procedure TFHIRIndexBuilder.buildIndexesForLinkage(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Linkage', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Linkage', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('Linkage', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('Linkage', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('Linkage', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Linkage', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('Linkage', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('Linkage', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Linkage', 'author', 'Author of the Linkage', SearchParamTypeREFERENCE, ['Practitioner', 'Organization'], 'Linkage.author', SearchXpathUsageNormal);
  indexes.add('Linkage', 'item', 'Matches on any item in the Linkage', SearchParamTypeREFERENCE, [], 'Linkage.item.resource', SearchXpathUsageNormal);
  indexes.add('Linkage', 'source', 'Matches on any item in the Linkage with a type of ''source''', SearchParamTypeREFERENCE, [], 'Linkage.item.resource', SearchXpathUsageNormal);
  compartments.register(frtPractitioner, 'Linkage', ['author']);
end;
{$ENDIF}

{$IFDEF FHIR_LIST}
procedure TFHIRIndexBuilder.buildIndexesForList(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('List', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('List', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('List', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('List', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('List', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('List', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('List', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('List', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('List', 'code', 'What the purpose of this list is', SearchParamTypeTOKEN, [], 'FamilyMemberHistory.condition.code | DeviceRequest.code.as(CodeableConcept) | AllergyIntolerance.code | AllergyIntolerance.reaction.substance | Procedure.code | List.code | ProcedureRequest.code | Observation.code | DiagnosticReport.code | Condition.'+'code', SearchXpathUsageNormal);
  indexes.add('List', 'date', 'When the list was prepared', SearchParamTypeDATE, [], 'Consent.dateTime | SupplyRequest.authoredOn | RiskAssessment.occurrence.as(DateTime) | CareTeam.period | FamilyMemberHistory.date | Encounter.period | AllergyIntolerance.assertedDate | CarePlan.period | EpisodeOfCare.period | Procedure.performed | Li'+'st.date | Immunization.date | Flag.period | Observation.effective | DiagnosticReport.effective | Composition.date | DetectedIssue.date | ClinicalImpression.date', SearchXpathUsageNormal);
  indexes.add('List', 'empty-reason', 'Why list is empty', SearchParamTypeTOKEN, [], 'List.emptyReason', SearchXpathUsageNormal);
  indexes.add('List', 'encounter', 'Context in which list created', SearchParamTypeREFERENCE, ['EpisodeOfCare', 'Encounter'], 'DocumentReference.context.encounter | RiskAssessment.context | DeviceRequest.context | Procedure.context | List.encounter | VisionPrescription.encounter | ProcedureRequest.context | Flag.encounter | Observation.context | DiagnosticReport.context | Nu'+'tritionOrder.encounter | Composition.encounter', SearchXpathUsageNormal);
  indexes.add('List', 'identifier', 'Business identifier', SearchParamTypeTOKEN, [], 'DocumentManifest.masterIdentifier | DocumentManifest.identifier | Goal.identifier | Consent.identifier | DocumentReference.masterIdentifier | DocumentReference.identifier | SupplyRequest.identifier | RiskAssessment.identifier | CareTeam.identifier | '+'ImagingStudy.identifier | FamilyMemberHistory.identifier | Encounter.identifier | DeviceRequest.identifier | AllergyIntolerance.identifier | CarePlan.identifier | EpisodeOfCare.identifier | Procedure.identifier | List.identifier | Immunization.identi'+'fier | VisionPrescription.identifier | ProcedureRequest.identifier | Observation.identifier | DiagnosticReport.identifier | NutritionOrder.identifier | Condition.identifier | Composition.identifier | DetectedIssue.identifier | SupplyDelivery.identifi'+'er', SearchXpathUsageNormal);
  indexes.add('List', 'item', 'Actual entry', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'List.entry.item', SearchXpathUsageNormal);
  indexes.add('List', 'notes', 'The annotation  - text content', SearchParamTypeSTRING, [], 'List.note.text', SearchXpathUsageNormal);
  indexes.add('List', 'patient', 'If all resources have the same subject', SearchParamTypeREFERENCE, ['Group', 'Patient'], 'ReferralRequest.subject | DocumentManifest.subject | Goal.subject | Consent.patient | DocumentReference.subject | ImagingManifest.patient | RiskAssessment.subject | CareTeam.subject | ImagingStudy.patient | FamilyMemberHistory.patient | Encounter.sub'+'ject | DeviceUseStatement.subject | DeviceRequest.subject | AllergyIntolerance.patient | CarePlan.subject | EpisodeOfCare.patient | Procedure.subject | List.subject | Immunization.patient | VisionPrescription.patient | ProcedureRequest.subject | Flag'+'.subject | Observation.subject | DiagnosticReport.subject | NutritionOrder.patient | Condition.subject | Composition.subject | DetectedIssue.patient | SupplyDelivery.patient | ClinicalImpression.subject', SearchXpathUsageNormal);
  indexes.add('List', 'source', 'Who and/or what defined the list contents (aka Author)', SearchParamTypeREFERENCE, ['Practitioner', 'Device', 'Patient'], 'List.source', SearchXpathUsageNormal);
  indexes.add('List', 'status', 'current | retired | entered-in-error', SearchParamTypeTOKEN, [], 'List.status', SearchXpathUsageNormal);
  indexes.add('List', 'subject', 'If all resources have the same subject', SearchParamTypeREFERENCE, ['Group', 'Device', 'Patient', 'Location'], 'List.subject', SearchXpathUsageNormal);
  indexes.add('List', 'title', 'Descriptive name for the list', SearchParamTypeSTRING, [], 'List.title', SearchXpathUsageNormal);
  compartments.register(frtDevice, 'List', ['subject', 'source']);
  compartments.register(frtPatient, 'List', ['subject', 'source']);
  compartments.register(frtPractitioner, 'List', ['source']);
end;
{$ENDIF}

{$IFDEF FHIR_LOCATION}
procedure TFHIRIndexBuilder.buildIndexesForLocation(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Location', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Location', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('Location', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('Location', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('Location', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Location', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('Location', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('Location', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Location', 'address', 'A (part of the) address of the location', SearchParamTypeSTRING, [], 'Location.address', SearchXpathUsageNormal);
  indexes.add('Location', 'address-city', 'A city specified in an address', SearchParamTypeSTRING, [], 'Location.address.city', SearchXpathUsageNormal);
  indexes.add('Location', 'address-country', 'A country specified in an address', SearchParamTypeSTRING, [], 'Location.address.country', SearchXpathUsageNormal);
  indexes.add('Location', 'address-postalcode', 'A postal code specified in an address', SearchParamTypeSTRING, [], 'Location.address.postalCode', SearchXpathUsageNormal);
  indexes.add('Location', 'address-state', 'A state specified in an address', SearchParamTypeSTRING, [], 'Location.address.state', SearchXpathUsageNormal);
  indexes.add('Location', 'address-use', 'A use code specified in an address', SearchParamTypeTOKEN, [], 'Location.address.use', SearchXpathUsageNormal);
  indexes.add('Location', 'endpoint', 'Technical endpoints providing access to services operated for the location', SearchParamTypeREFERENCE, ['Endpoint'], 'Location.endpoint', SearchXpathUsageNormal);
  indexes.add('Location', 'identifier', 'An identifier for the location', SearchParamTypeTOKEN, [], 'Location.identifier', SearchXpathUsageNormal);
  indexes.add('Location', 'name', 'A portion of the location''s name or alias', SearchParamTypeSTRING, [], 'Location.name | Location.alias', SearchXpathUsageNormal);
  indexes.add('Location', 'near', 'The coordinates expressed as [latitude]:[longitude] (using the WGS84 datum, see notes) to find locations near to (servers may search using a square rather than a circle for efficiency)  Requires the near-distance parameter to be provided also', SearchParamTypeTOKEN, [], 'Location.position', SearchXpathUsageNearby);
  indexes.add('Location', 'near-distance', 'A distance quantity to limit the near search to locations within a specific distance  Requires the near parameter to also be included', SearchParamTypeQUANTITY, [], 'Location.position', SearchXpathUsageDistance);
  indexes.add('Location', 'operational-status', 'Searches for locations (typically bed/room) that have an operational status (e.g. contaminated, housekeeping)', SearchParamTypeTOKEN, [], 'Location.operationalStatus', SearchXpathUsageNormal);
  indexes.add('Location', 'organization', 'Searches for locations that are managed by the provided organization', SearchParamTypeREFERENCE, ['Organization'], 'Location.managingOrganization', SearchXpathUsageNormal);
  indexes.add('Location', 'partof', 'A location of which this location is a part', SearchParamTypeREFERENCE, ['Location'], 'Location.partOf', SearchXpathUsageNormal);
  indexes.add('Location', 'status', 'Searches for locations with a specific kind of status', SearchParamTypeTOKEN, [], 'Location.status', SearchXpathUsageNormal);
  indexes.add('Location', 'type', 'A code for the type of location', SearchParamTypeTOKEN, [], 'Location.type', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_MEASURE}
procedure TFHIRIndexBuilder.buildIndexesForMeasure(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Measure', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Measure', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('Measure', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('Measure', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('Measure', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Measure', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('Measure', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('Measure', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Measure', 'composed-of', 'What resource is being referenced', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'Measure.relatedArtifact.where(type=''composed-of'').resource', SearchXpathUsageNormal);
  indexes.add('Measure', 'date', 'The measure publication date', SearchParamTypeDATE, [], 'Measure.date', SearchXpathUsageNormal);
  indexes.add('Measure', 'depends-on', 'What resource is being referenced', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'Measure.relatedArtifact.where(type=''depends-on'').resource | Measure.library', SearchXpathUsageNormal);
  indexes.add('Measure', 'derived-from', 'What resource is being referenced', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'Measure.relatedArtifact.where(type=''derived-from'').resource', SearchXpathUsageNormal);
  indexes.add('Measure', 'description', 'The description of the measure', SearchParamTypeSTRING, [], 'Measure.description', SearchXpathUsageNormal);
  indexes.add('Measure', 'effective', 'The time during which the measure is intended to be in use', SearchParamTypeDATE, [], 'Measure.effectivePeriod', SearchXpathUsageNormal);
  indexes.add('Measure', 'identifier', 'External identifier for the measure', SearchParamTypeTOKEN, [], 'Measure.identifier', SearchXpathUsageNormal);
  indexes.add('Measure', 'jurisdiction', 'Intended jurisdiction for the measure', SearchParamTypeTOKEN, [], 'Measure.jurisdiction', SearchXpathUsageNormal);
  indexes.add('Measure', 'name', 'Computationally friendly name of the measure', SearchParamTypeSTRING, [], 'Measure.name', SearchXpathUsageNormal);
  indexes.add('Measure', 'predecessor', 'What resource is being referenced', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'Measure.relatedArtifact.where(type=''predecessor'').resource', SearchXpathUsageNormal);
  indexes.add('Measure', 'publisher', 'Name of the publisher of the measure', SearchParamTypeSTRING, [], 'Measure.publisher', SearchXpathUsageNormal);
  indexes.add('Measure', 'status', 'The current status of the measure', SearchParamTypeTOKEN, [], 'Measure.status', SearchXpathUsageNormal);
  indexes.add('Measure', 'successor', 'What resource is being referenced', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'Measure.relatedArtifact.where(type=''successor'').resource', SearchXpathUsageNormal);
  indexes.add('Measure', 'title', 'The human-friendly name of the measure', SearchParamTypeSTRING, [], 'Measure.title', SearchXpathUsageNormal);
  indexes.add('Measure', 'topic', 'Topics associated with the module', SearchParamTypeTOKEN, [], 'Measure.topic', SearchXpathUsageNormal);
  indexes.add('Measure', 'url', 'The uri that identifies the measure', SearchParamTypeURI, [], 'Measure.url', SearchXpathUsageNormal);
  indexes.add('Measure', 'version', 'The business version of the measure', SearchParamTypeTOKEN, [], 'Measure.version', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_MEASUREREPORT}
procedure TFHIRIndexBuilder.buildIndexesForMeasureReport(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('MeasureReport', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('MeasureReport', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('MeasureReport', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('MeasureReport', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('MeasureReport', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('MeasureReport', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('MeasureReport', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('MeasureReport', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('MeasureReport', 'identifier', 'External identifier of the measure report to be returned', SearchParamTypeTOKEN, [], 'MeasureReport.identifier', SearchXpathUsageNormal);
  indexes.add('MeasureReport', 'patient', 'The identity of a patient to search for individual measure report results for', SearchParamTypeREFERENCE, ['Patient'], 'MeasureReport.patient', SearchXpathUsageNormal);
  indexes.add('MeasureReport', 'status', 'The status of the measure report', SearchParamTypeTOKEN, [], 'MeasureReport.status', SearchXpathUsageNormal);
  compartments.register(frtPatient, 'MeasureReport', ['patient']);
end;
{$ENDIF}

{$IFDEF FHIR_MEDIA}
procedure TFHIRIndexBuilder.buildIndexesForMedia(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Media', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Media', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('Media', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('Media', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('Media', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Media', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('Media', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('Media', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Media', 'based-on', 'Procedure that caused this media to be created', SearchParamTypeREFERENCE, ['ProcedureRequest'], 'Media.basedOn', SearchXpathUsageNormal);
  indexes.add('Media', 'context', 'Encounter / Episode associated with media', SearchParamTypeREFERENCE, ['EpisodeOfCare', 'Encounter'], 'Media.context', SearchXpathUsageNormal);
  indexes.add('Media', 'created', 'Date attachment was first created', SearchParamTypeDATE, [], 'Media.content.creation', SearchXpathUsageNormal);
  indexes.add('Media', 'date', 'When Media was collected', SearchParamTypeDATE, [], 'Media.occurrence', SearchXpathUsageNormal);
  indexes.add('Media', 'device', 'Observing Device', SearchParamTypeREFERENCE, ['Device', 'DeviceMetric'], 'Media.device', SearchXpathUsageNormal);
  indexes.add('Media', 'identifier', 'Identifier(s) for the image', SearchParamTypeTOKEN, [], 'Media.identifier', SearchXpathUsageNormal);
  indexes.add('Media', 'operator', 'The person who generated the image', SearchParamTypeREFERENCE, ['Practitioner'], 'Media.operator', SearchXpathUsageNormal);
  indexes.add('Media', 'patient', 'Who/What this Media is a record of', SearchParamTypeREFERENCE, ['Patient'], 'Media.subject', SearchXpathUsageNormal);
  indexes.add('Media', 'site', 'Body part in media', SearchParamTypeTOKEN, [], 'Media.bodySite', SearchXpathUsageNormal);
  indexes.add('Media', 'subject', 'Who/What this Media is a record of', SearchParamTypeREFERENCE, ['Practitioner', 'Group', 'Specimen', 'Device', 'Patient'], 'Media.subject', SearchXpathUsageNormal);
  indexes.add('Media', 'subtype', 'The type of acquisition equipment/process', SearchParamTypeTOKEN, [], 'Media.subtype', SearchXpathUsageNormal);
  indexes.add('Media', 'type', 'photo | video | audio', SearchParamTypeTOKEN, [], 'Media.type', SearchXpathUsageNormal);
  indexes.add('Media', 'view', 'Imaging view, e.g. Lateral or Antero-posterior', SearchParamTypeTOKEN, [], 'Media.view', SearchXpathUsageNormal);
  compartments.register(frtDevice, 'Media', ['subject']);
  compartments.register(frtPatient, 'Media', ['subject']);
  compartments.register(frtPractitioner, 'Media', ['subject', 'operator']);
end;
{$ENDIF}

{$IFDEF FHIR_MEDICATION}
procedure TFHIRIndexBuilder.buildIndexesForMedication(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Medication', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Medication', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('Medication', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('Medication', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('Medication', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Medication', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('Medication', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('Medication', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Medication', 'code', 'Codes that identify this medication', SearchParamTypeTOKEN, [], 'Medication.code | MedicationRequest.medication.as(CodeableConcept) | MedicationAdministration.medication.as(CodeableConcept) | MedicationStatement.medication.as(CodeableConcept) | MedicationDispense.medication.as(CodeableConcept)', SearchXpathUsageNormal);
  indexes.add('Medication', 'container', 'E.g. box, vial, blister-pack', SearchParamTypeTOKEN, [], 'Medication.package.container', SearchXpathUsageNormal);
  indexes.add('Medication', 'form', 'powder | tablets | capsule +', SearchParamTypeTOKEN, [], 'Medication.form', SearchXpathUsageNormal);
  indexes.add('Medication', 'ingredient', 'The product contained', SearchParamTypeREFERENCE, ['Medication', 'Substance'], 'Medication.ingredient.item.as(Reference)', SearchXpathUsageNormal);
  indexes.add('Medication', 'ingredient-code', 'The product contained', SearchParamTypeTOKEN, [], 'Medication.ingredient.item.as(CodeableConcept)', SearchXpathUsageNormal);
  indexes.add('Medication', 'manufacturer', 'Manufacturer of the item', SearchParamTypeREFERENCE, ['Organization'], 'Medication.manufacturer', SearchXpathUsageNormal);
  indexes.add('Medication', 'over-the-counter', 'True if medication does not require a prescription', SearchParamTypeTOKEN, [], 'Medication.isOverTheCounter', SearchXpathUsageNormal);
  indexes.add('Medication', 'package-item', 'The item in the package', SearchParamTypeREFERENCE, ['Medication'], 'Medication.package.content.item.as(Reference)', SearchXpathUsageNormal);
  indexes.add('Medication', 'package-item-code', 'The item in the package', SearchParamTypeTOKEN, [], 'Medication.package.content.item.as(CodeableConcept)', SearchXpathUsageNormal);
  indexes.add('Medication', 'status', 'active | inactive | entered-in-error', SearchParamTypeTOKEN, [], 'Medication.status', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_MEDICATIONADMINISTRATION}
procedure TFHIRIndexBuilder.buildIndexesForMedicationAdministration(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('MedicationAdministration', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('MedicationAdministration', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('MedicationAdministration', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('MedicationAdministration', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('MedicationAdministration', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('MedicationAdministration', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('MedicationAdministration', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('MedicationAdministration', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('MedicationAdministration', 'code', 'Return administrations of this medication code', SearchParamTypeTOKEN, [], 'Medication.code | MedicationRequest.medication.as(CodeableConcept) | MedicationAdministration.medication.as(CodeableConcept) | MedicationStatement.medication.as(CodeableConcept) | MedicationDispense.medication.as(CodeableConcept)', SearchXpathUsageNormal);
  indexes.add('MedicationAdministration', 'context', 'Return administrations that share this encounter or episode of care', SearchParamTypeREFERENCE, ['EpisodeOfCare', 'Encounter'], 'MedicationAdministration.context', SearchXpathUsageNormal);
  indexes.add('MedicationAdministration', 'device', 'Return administrations with this administration device identity', SearchParamTypeREFERENCE, ['Device'], 'MedicationAdministration.device', SearchXpathUsageNormal);
  indexes.add('MedicationAdministration', 'effective-time', 'Date administration happened (or did not happen)', SearchParamTypeDATE, [], 'MedicationAdministration.effective', SearchXpathUsageNormal);
  indexes.add('MedicationAdministration', 'identifier', 'Return administrations with this external identifier', SearchParamTypeTOKEN, [], 'MedicationRequest.identifier | MedicationAdministration.identifier | MedicationStatement.identifier | MedicationDispense.identifier', SearchXpathUsageNormal);
  indexes.add('MedicationAdministration', 'medication', 'Return administrations of this medication resource', SearchParamTypeREFERENCE, ['Medication'], 'MedicationRequest.medication.as(Reference) | MedicationAdministration.medication.as(Reference) | MedicationStatement.medication.as(Reference) | MedicationDispense.medication.as(Reference)', SearchXpathUsageNormal);
  indexes.add('MedicationAdministration', 'not-given', 'Administrations that were not made', SearchParamTypeTOKEN, [], 'MedicationAdministration.notGiven', SearchXpathUsageNormal);
  indexes.add('MedicationAdministration', 'patient', 'The identity of a patient to list administrations  for', SearchParamTypeREFERENCE, ['Patient'], 'MedicationRequest.subject | MedicationAdministration.subject | MedicationStatement.subject | MedicationDispense.subject', SearchXpathUsageNormal);
  indexes.add('MedicationAdministration', 'performer', 'The identify of the individual who administered the medication', SearchParamTypeREFERENCE, ['Practitioner', 'Device', 'Patient', 'RelatedPerson'], 'MedicationAdministration.performer.actor', SearchXpathUsageNormal);
  indexes.add('MedicationAdministration', 'prescription', 'The identity of a prescription to list administrations from', SearchParamTypeREFERENCE, ['MedicationRequest'], 'MedicationAdministration.prescription | MedicationDispense.authorizingPrescription', SearchXpathUsageNormal);
  indexes.add('MedicationAdministration', 'reason-given', 'Reasons for administering the medication', SearchParamTypeTOKEN, [], 'MedicationAdministration.reasonCode', SearchXpathUsageNormal);
  indexes.add('MedicationAdministration', 'reason-not-given', 'Reasons for not administering the medication', SearchParamTypeTOKEN, [], 'MedicationAdministration.reasonNotGiven', SearchXpathUsageNormal);
  indexes.add('MedicationAdministration', 'status', 'MedicationAdministration event status (for example one of active/paused/completed/nullified)', SearchParamTypeTOKEN, [], 'MedicationRequest.status | MedicationAdministration.status | MedicationStatement.status | MedicationDispense.status', SearchXpathUsageNormal);
  indexes.add('MedicationAdministration', 'subject', 'The identify of the individual or group to list administrations for', SearchParamTypeREFERENCE, ['Group', 'Patient'], 'MedicationAdministration.subject', SearchXpathUsageNormal);
  compartments.register(frtDevice, 'MedicationAdministration', ['device']);
  compartments.register(frtEncounter, 'MedicationAdministration', ['context']);
  compartments.register(frtPatient, 'MedicationAdministration', ['patient', 'performer', 'subject']);
  compartments.register(frtPractitioner, 'MedicationAdministration', ['performer']);
  compartments.register(frtRelatedPerson, 'MedicationAdministration', ['performer']);
end;
{$ENDIF}

{$IFDEF FHIR_MEDICATIONDISPENSE}
procedure TFHIRIndexBuilder.buildIndexesForMedicationDispense(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('MedicationDispense', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('MedicationDispense', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('MedicationDispense', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('MedicationDispense', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('MedicationDispense', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('MedicationDispense', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('MedicationDispense', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('MedicationDispense', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('MedicationDispense', 'code', 'Return dispenses of this medicine code', SearchParamTypeTOKEN, [], 'Medication.code | MedicationRequest.medication.as(CodeableConcept) | MedicationAdministration.medication.as(CodeableConcept) | MedicationStatement.medication.as(CodeableConcept) | MedicationDispense.medication.as(CodeableConcept)', SearchXpathUsageNormal);
  indexes.add('MedicationDispense', 'context', 'Returns dispenses with a specific context (episode or episode of care)', SearchParamTypeREFERENCE, ['EpisodeOfCare', 'Encounter'], 'MedicationDispense.context', SearchXpathUsageNormal);
  indexes.add('MedicationDispense', 'destination', 'Return dispenses that should be sent to a specific destination', SearchParamTypeREFERENCE, ['Location'], 'MedicationDispense.destination', SearchXpathUsageNormal);
  indexes.add('MedicationDispense', 'identifier', 'Return dispenses with this external identifier', SearchParamTypeTOKEN, [], 'MedicationRequest.identifier | MedicationAdministration.identifier | MedicationStatement.identifier | MedicationDispense.identifier', SearchXpathUsageNormal);
  indexes.add('MedicationDispense', 'medication', 'Return dispenses of this medicine resource', SearchParamTypeREFERENCE, ['Medication'], 'MedicationRequest.medication.as(Reference) | MedicationAdministration.medication.as(Reference) | MedicationStatement.medication.as(Reference) | MedicationDispense.medication.as(Reference)', SearchXpathUsageNormal);
  indexes.add('MedicationDispense', 'patient', 'The identity of a patient to list dispenses  for', SearchParamTypeREFERENCE, ['Patient'], 'MedicationRequest.subject | MedicationAdministration.subject | MedicationStatement.subject | MedicationDispense.subject', SearchXpathUsageNormal);
  indexes.add('MedicationDispense', 'performer', 'Return dispenses performed by a specific individual', SearchParamTypeREFERENCE, ['Practitioner', 'Organization', 'Device', 'Patient', 'RelatedPerson'], 'MedicationDispense.performer.actor', SearchXpathUsageNormal);
  indexes.add('MedicationDispense', 'prescription', 'The identity of a prescription to list dispenses from', SearchParamTypeREFERENCE, ['MedicationRequest'], 'MedicationAdministration.prescription | MedicationDispense.authorizingPrescription', SearchXpathUsageNormal);
  indexes.add('MedicationDispense', 'receiver', 'The identity of a receiver to list dispenses for', SearchParamTypeREFERENCE, ['Practitioner', 'Patient'], 'MedicationDispense.receiver', SearchXpathUsageNormal);
  indexes.add('MedicationDispense', 'responsibleparty', 'Return dispenses with the specified responsible party', SearchParamTypeREFERENCE, ['Practitioner'], 'MedicationDispense.substitution.responsibleParty', SearchXpathUsageNormal);
  indexes.add('MedicationDispense', 'status', 'Return dispenses with a specified dispense status', SearchParamTypeTOKEN, [], 'MedicationRequest.status | MedicationAdministration.status | MedicationStatement.status | MedicationDispense.status', SearchXpathUsageNormal);
  indexes.add('MedicationDispense', 'subject', 'The identity of a patient to list dispenses  for', SearchParamTypeREFERENCE, ['Group', 'Patient'], 'MedicationDispense.subject', SearchXpathUsageNormal);
  indexes.add('MedicationDispense', 'type', 'Return dispenses of a specific type', SearchParamTypeTOKEN, [], 'MedicationDispense.type', SearchXpathUsageNormal);
  indexes.add('MedicationDispense', 'whenhandedover', 'Returns dispenses handed over on this date', SearchParamTypeDATE, [], 'MedicationDispense.whenHandedOver', SearchXpathUsageNormal);
  indexes.add('MedicationDispense', 'whenprepared', 'Returns dispenses prepared on this date', SearchParamTypeDATE, [], 'MedicationDispense.whenPrepared', SearchXpathUsageNormal);
  compartments.register(frtPatient, 'MedicationDispense', ['subject', 'patient', 'receiver']);
  compartments.register(frtPractitioner, 'MedicationDispense', ['performer', 'receiver']);
end;
{$ENDIF}

{$IFDEF FHIR_MEDICATIONREQUEST}
procedure TFHIRIndexBuilder.buildIndexesForMedicationRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('MedicationRequest', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('MedicationRequest', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('MedicationRequest', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('MedicationRequest', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('MedicationRequest', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('MedicationRequest', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('MedicationRequest', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('MedicationRequest', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('MedicationRequest', 'authoredon', 'Return prescriptions written on this date', SearchParamTypeDATE, [], 'MedicationRequest.authoredOn', SearchXpathUsageNormal);
  indexes.add('MedicationRequest', 'category', 'Returns prescriptions with different categories', SearchParamTypeTOKEN, [], 'MedicationRequest.category', SearchXpathUsageNormal);
  indexes.add('MedicationRequest', 'code', 'Return prescriptions of this medication code', SearchParamTypeTOKEN, [], 'Medication.code | MedicationRequest.medication.as(CodeableConcept) | MedicationAdministration.medication.as(CodeableConcept) | MedicationStatement.medication.as(CodeableConcept) | MedicationDispense.medication.as(CodeableConcept)', SearchXpathUsageNormal);
  indexes.add('MedicationRequest', 'context', 'Return prescriptions with this encounter or episode of care identifier', SearchParamTypeREFERENCE, ['EpisodeOfCare', 'Encounter'], 'MedicationRequest.context', SearchXpathUsageNormal);
  indexes.add('MedicationRequest', 'date', 'Returns medication request to be administered on a specific date', SearchParamTypeDATE, [], 'MedicationRequest.dosageInstruction.timing.event', SearchXpathUsageNormal);
  indexes.add('MedicationRequest', 'identifier', 'Return prescriptions with this external identifier', SearchParamTypeTOKEN, [], 'MedicationRequest.identifier | MedicationAdministration.identifier | MedicationStatement.identifier | MedicationDispense.identifier', SearchXpathUsageNormal);
  indexes.add('MedicationRequest', 'intended-dispenser', 'Returns prescriptions intended to be dispensed by this Organization', SearchParamTypeREFERENCE, ['Organization'], 'MedicationRequest.dispenseRequest.performer', SearchXpathUsageNormal);
  indexes.add('MedicationRequest', 'intent', 'Returns prescriptions with different intents', SearchParamTypeTOKEN, [], 'MedicationRequest.intent', SearchXpathUsageNormal);
  indexes.add('MedicationRequest', 'medication', 'Return prescriptions of this medication reference', SearchParamTypeREFERENCE, ['Medication'], 'MedicationRequest.medication.as(Reference) | MedicationAdministration.medication.as(Reference) | MedicationStatement.medication.as(Reference) | MedicationDispense.medication.as(Reference)', SearchXpathUsageNormal);
  indexes.add('MedicationRequest', 'patient', 'Returns prescriptions for a specific patient', SearchParamTypeREFERENCE, ['Patient'], 'MedicationRequest.subject | MedicationAdministration.subject | MedicationStatement.subject | MedicationDispense.subject', SearchXpathUsageNormal);
  indexes.add('MedicationRequest', 'priority', 'Returns prescriptions with different priorities', SearchParamTypeTOKEN, [], 'MedicationRequest.priority', SearchXpathUsageNormal);
  indexes.add('MedicationRequest', 'requester', 'Returns prescriptions prescribed by this prescriber', SearchParamTypeREFERENCE, ['Practitioner', 'Organization', 'Device', 'Patient', 'RelatedPerson'], 'MedicationRequest.requester.agent', SearchXpathUsageNormal);
  indexes.add('MedicationRequest', 'status', 'Status of the prescription', SearchParamTypeTOKEN, [], 'MedicationRequest.status | MedicationAdministration.status | MedicationStatement.status | MedicationDispense.status', SearchXpathUsageNormal);
  indexes.add('MedicationRequest', 'subject', 'The identity of a patient to list orders  for', SearchParamTypeREFERENCE, ['Group', 'Patient'], 'MedicationRequest.subject', SearchXpathUsageNormal);
  compartments.register(frtEncounter, 'MedicationRequest', ['context']);
  compartments.register(frtPatient, 'MedicationRequest', ['subject']);
  compartments.register(frtPractitioner, 'MedicationRequest', ['requester']);
end;
{$ENDIF}

{$IFDEF FHIR_MEDICATIONSTATEMENT}
procedure TFHIRIndexBuilder.buildIndexesForMedicationStatement(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('MedicationStatement', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('MedicationStatement', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('MedicationStatement', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('MedicationStatement', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('MedicationStatement', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('MedicationStatement', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('MedicationStatement', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('MedicationStatement', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('MedicationStatement', 'category', 'Returns statements of this category of medicationstatement', SearchParamTypeTOKEN, [], 'MedicationStatement.category', SearchXpathUsageNormal);
  indexes.add('MedicationStatement', 'code', 'Return statements of this medication code', SearchParamTypeTOKEN, [], 'Medication.code | MedicationRequest.medication.as(CodeableConcept) | MedicationAdministration.medication.as(CodeableConcept) | MedicationStatement.medication.as(CodeableConcept) | MedicationDispense.medication.as(CodeableConcept)', SearchXpathUsageNormal);
  indexes.add('MedicationStatement', 'context', 'Returns statements for a specific context (episode or episode of Care).', SearchParamTypeREFERENCE, ['EpisodeOfCare', 'Encounter'], 'MedicationStatement.context', SearchXpathUsageNormal);
  indexes.add('MedicationStatement', 'effective', 'Date when patient was taking (or not taking) the medication', SearchParamTypeDATE, [], 'MedicationStatement.effective', SearchXpathUsageNormal);
  indexes.add('MedicationStatement', 'identifier', 'Return statements with this external identifier', SearchParamTypeTOKEN, [], 'MedicationRequest.identifier | MedicationAdministration.identifier | MedicationStatement.identifier | MedicationDispense.identifier', SearchXpathUsageNormal);
  indexes.add('MedicationStatement', 'medication', 'Return statements of this medication reference', SearchParamTypeREFERENCE, ['Medication'], 'MedicationRequest.medication.as(Reference) | MedicationAdministration.medication.as(Reference) | MedicationStatement.medication.as(Reference) | MedicationDispense.medication.as(Reference)', SearchXpathUsageNormal);
  indexes.add('MedicationStatement', 'part-of', 'Returns statements that are part of another event.', SearchParamTypeREFERENCE, ['MedicationDispense', 'Observation', 'MedicationAdministration', 'Procedure', 'MedicationStatement'], 'MedicationStatement.partOf', SearchXpathUsageNormal);
  indexes.add('MedicationStatement', 'patient', 'Returns statements for a specific patient.', SearchParamTypeREFERENCE, ['Patient'], 'MedicationRequest.subject | MedicationAdministration.subject | MedicationStatement.subject | MedicationDispense.subject', SearchXpathUsageNormal);
  indexes.add('MedicationStatement', 'source', 'Who or where the information in the statement came from', SearchParamTypeREFERENCE, ['Practitioner', 'Organization', 'Patient', 'RelatedPerson'], 'MedicationStatement.informationSource', SearchXpathUsageNormal);
  indexes.add('MedicationStatement', 'status', 'Return statements that match the given status', SearchParamTypeTOKEN, [], 'MedicationRequest.status | MedicationAdministration.status | MedicationStatement.status | MedicationDispense.status', SearchXpathUsageNormal);
  indexes.add('MedicationStatement', 'subject', 'The identity of a patient, animal or group to list statements for', SearchParamTypeREFERENCE, ['Group', 'Patient'], 'MedicationStatement.subject', SearchXpathUsageNormal);
  compartments.register(frtPatient, 'MedicationStatement', ['subject']);
  compartments.register(frtPractitioner, 'MedicationStatement', ['source']);
  compartments.register(frtRelatedPerson, 'MedicationStatement', ['source']);
end;
{$ENDIF}

{$IFDEF FHIR_MESSAGEDEFINITION}
procedure TFHIRIndexBuilder.buildIndexesForMessageDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('MessageDefinition', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('MessageDefinition', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('MessageDefinition', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('MessageDefinition', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('MessageDefinition', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('MessageDefinition', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('MessageDefinition', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('MessageDefinition', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('MessageDefinition', 'category', 'The behavior associated with the message', SearchParamTypeTOKEN, [], 'MessageDefinition.category', SearchXpathUsageNormal);
  indexes.add('MessageDefinition', 'date', 'The message definition publication date', SearchParamTypeDATE, [], 'MessageDefinition.date', SearchXpathUsageNormal);
  indexes.add('MessageDefinition', 'description', 'The description of the message definition', SearchParamTypeSTRING, [], 'MessageDefinition.description', SearchXpathUsageNormal);
  indexes.add('MessageDefinition', 'event', 'The event that triggers the message', SearchParamTypeTOKEN, [], 'MessageDefinition.event', SearchXpathUsageNormal);
  indexes.add('MessageDefinition', 'focus', 'A resource that is a permitted focus of the message', SearchParamTypeTOKEN, [], 'MessageDefinition.focus.code', SearchXpathUsageNormal);
  indexes.add('MessageDefinition', 'identifier', 'External identifier for the message definition', SearchParamTypeTOKEN, [], 'MessageDefinition.identifier', SearchXpathUsageNormal);
  indexes.add('MessageDefinition', 'jurisdiction', 'Intended jurisdiction for the message definition', SearchParamTypeTOKEN, [], 'MessageDefinition.jurisdiction', SearchXpathUsageNormal);
  indexes.add('MessageDefinition', 'name', 'Computationally friendly name of the message definition', SearchParamTypeSTRING, [], 'MessageDefinition.name', SearchXpathUsageNormal);
  indexes.add('MessageDefinition', 'publisher', 'Name of the publisher of the message definition', SearchParamTypeSTRING, [], 'MessageDefinition.publisher', SearchXpathUsageNormal);
  indexes.add('MessageDefinition', 'status', 'The current status of the message definition', SearchParamTypeTOKEN, [], 'MessageDefinition.status', SearchXpathUsageNormal);
  indexes.add('MessageDefinition', 'title', 'The human-friendly name of the message definition', SearchParamTypeSTRING, [], 'MessageDefinition.title', SearchXpathUsageNormal);
  indexes.add('MessageDefinition', 'url', 'The uri that identifies the message definition', SearchParamTypeURI, [], 'MessageDefinition.url', SearchXpathUsageNormal);
  indexes.add('MessageDefinition', 'version', 'The business version of the message definition', SearchParamTypeTOKEN, [], 'MessageDefinition.version', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_MESSAGEHEADER}
procedure TFHIRIndexBuilder.buildIndexesForMessageHeader(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('MessageHeader', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('MessageHeader', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('MessageHeader', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('MessageHeader', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('MessageHeader', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('MessageHeader', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('MessageHeader', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('MessageHeader', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('MessageHeader', 'author', 'The source of the decision', SearchParamTypeREFERENCE, ['Practitioner'], 'MessageHeader.author', SearchXpathUsageNormal);
  indexes.add('MessageHeader', 'code', 'ok | transient-error | fatal-error', SearchParamTypeTOKEN, [], 'MessageHeader.response.code', SearchXpathUsageNormal);
  indexes.add('MessageHeader', 'destination', 'Name of system', SearchParamTypeSTRING, [], 'MessageHeader.destination.name', SearchXpathUsageNormal);
  indexes.add('MessageHeader', 'destination-uri', 'Actual destination address or id', SearchParamTypeURI, [], 'MessageHeader.destination.endpoint', SearchXpathUsageNormal);
  indexes.add('MessageHeader', 'enterer', 'The source of the data entry', SearchParamTypeREFERENCE, ['Practitioner'], 'MessageHeader.enterer', SearchXpathUsageNormal);
  indexes.add('MessageHeader', 'event', 'Code for the event this message represents', SearchParamTypeTOKEN, [], 'MessageHeader.event', SearchXpathUsageNormal);
  indexes.add('MessageHeader', 'focus', 'The actual content of the message', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'MessageHeader.focus', SearchXpathUsageNormal);
  indexes.add('MessageHeader', 'receiver', 'Intended "real-world" recipient for the data', SearchParamTypeREFERENCE, ['Practitioner', 'Organization'], 'MessageHeader.receiver', SearchXpathUsageNormal);
  indexes.add('MessageHeader', 'response-id', 'Id of original message', SearchParamTypeTOKEN, [], 'MessageHeader.response.identifier', SearchXpathUsageNormal);
  indexes.add('MessageHeader', 'responsible', 'Final responsibility for event', SearchParamTypeREFERENCE, ['Practitioner', 'Organization'], 'MessageHeader.responsible', SearchXpathUsageNormal);
  indexes.add('MessageHeader', 'sender', 'Real world sender of the message', SearchParamTypeREFERENCE, ['Practitioner', 'Organization'], 'MessageHeader.sender', SearchXpathUsageNormal);
  indexes.add('MessageHeader', 'source', 'Name of system', SearchParamTypeSTRING, [], 'MessageHeader.source.name', SearchXpathUsageNormal);
  indexes.add('MessageHeader', 'source-uri', 'Actual message source address or id', SearchParamTypeURI, [], 'MessageHeader.source.endpoint', SearchXpathUsageNormal);
  indexes.add('MessageHeader', 'target', 'Particular delivery destination within the destination', SearchParamTypeREFERENCE, ['Device'], 'MessageHeader.destination.target', SearchXpathUsageNormal);
  indexes.add('MessageHeader', 'timestamp', 'Time that the message was sent', SearchParamTypeDATE, [], 'MessageHeader.timestamp', SearchXpathUsageNormal);
  compartments.register(frtDevice, 'MessageHeader', ['target']);
  compartments.register(frtPractitioner, 'MessageHeader', ['receiver', 'author', 'responsible', 'enterer']);
end;
{$ENDIF}

{$IFDEF FHIR_NAMINGSYSTEM}
procedure TFHIRIndexBuilder.buildIndexesForNamingSystem(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('NamingSystem', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('NamingSystem', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('NamingSystem', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('NamingSystem', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('NamingSystem', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('NamingSystem', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('NamingSystem', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('NamingSystem', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('NamingSystem', 'contact', 'Name of an individual to contact', SearchParamTypeSTRING, [], 'NamingSystem.contact.name', SearchXpathUsageNormal);
  indexes.add('NamingSystem', 'date', 'The naming system publication date', SearchParamTypeDATE, [], 'NamingSystem.date', SearchXpathUsageNormal);
  indexes.add('NamingSystem', 'description', 'The description of the naming system', SearchParamTypeSTRING, [], 'NamingSystem.description', SearchXpathUsageNormal);
  indexes.add('NamingSystem', 'id-type', 'oid | uuid | uri | other', SearchParamTypeTOKEN, [], 'NamingSystem.uniqueId.type', SearchXpathUsageNormal);
  indexes.add('NamingSystem', 'jurisdiction', 'Intended jurisdiction for the naming system', SearchParamTypeTOKEN, [], 'NamingSystem.jurisdiction', SearchXpathUsageNormal);
  indexes.add('NamingSystem', 'kind', 'codesystem | identifier | root', SearchParamTypeTOKEN, [], 'NamingSystem.kind', SearchXpathUsageNormal);
  indexes.add('NamingSystem', 'name', 'Computationally friendly name of the naming system', SearchParamTypeSTRING, [], 'NamingSystem.name', SearchXpathUsageNormal);
  indexes.add('NamingSystem', 'period', 'When is identifier valid?', SearchParamTypeDATE, [], 'NamingSystem.uniqueId.period', SearchXpathUsageNormal);
  indexes.add('NamingSystem', 'publisher', 'Name of the publisher of the naming system', SearchParamTypeSTRING, [], 'NamingSystem.publisher', SearchXpathUsageNormal);
  indexes.add('NamingSystem', 'replaced-by', 'Use this instead', SearchParamTypeREFERENCE, ['NamingSystem'], 'NamingSystem.replacedBy', SearchXpathUsageNormal);
  indexes.add('NamingSystem', 'responsible', 'Who maintains system namespace?', SearchParamTypeSTRING, [], 'NamingSystem.responsible', SearchXpathUsageNormal);
  indexes.add('NamingSystem', 'status', 'The current status of the naming system', SearchParamTypeTOKEN, [], 'NamingSystem.status', SearchXpathUsageNormal);
  indexes.add('NamingSystem', 'telecom', 'Contact details for individual or organization', SearchParamTypeTOKEN, [], 'NamingSystem.contact.telecom', SearchXpathUsageNormal);
  indexes.add('NamingSystem', 'type', 'e.g. driver,  provider,  patient, bank etc.', SearchParamTypeTOKEN, [], 'NamingSystem.type', SearchXpathUsageNormal);
  indexes.add('NamingSystem', 'value', 'The unique identifier', SearchParamTypeSTRING, [], 'NamingSystem.uniqueId.value', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_NUTRITIONORDER}
procedure TFHIRIndexBuilder.buildIndexesForNutritionOrder(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('NutritionOrder', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('NutritionOrder', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('NutritionOrder', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('NutritionOrder', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('NutritionOrder', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('NutritionOrder', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('NutritionOrder', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('NutritionOrder', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('NutritionOrder', 'additive', 'Type of module component to add to the feeding', SearchParamTypeTOKEN, [], 'NutritionOrder.enteralFormula.additiveType', SearchXpathUsageNormal);
  indexes.add('NutritionOrder', 'datetime', 'Return nutrition orders requested on this date', SearchParamTypeDATE, [], 'NutritionOrder.dateTime', SearchXpathUsageNormal);
  indexes.add('NutritionOrder', 'encounter', 'Return nutrition orders with this encounter identifier', SearchParamTypeREFERENCE, ['EpisodeOfCare', 'Encounter'], 'DocumentReference.context.encounter | RiskAssessment.context | DeviceRequest.context | Procedure.context | List.encounter | VisionPrescription.encounter | ProcedureRequest.context | Flag.encounter | Observation.context | DiagnosticReport.context | Nu'+'tritionOrder.encounter | Composition.encounter', SearchXpathUsageNormal);
  indexes.add('NutritionOrder', 'formula', 'Type of enteral or infant formula', SearchParamTypeTOKEN, [], 'NutritionOrder.enteralFormula.baseFormulaType', SearchXpathUsageNormal);
  indexes.add('NutritionOrder', 'identifier', 'Return nutrition orders with this external identifier', SearchParamTypeTOKEN, [], 'DocumentManifest.masterIdentifier | DocumentManifest.identifier | Goal.identifier | Consent.identifier | DocumentReference.masterIdentifier | DocumentReference.identifier | SupplyRequest.identifier | RiskAssessment.identifier | CareTeam.identifier | '+'ImagingStudy.identifier | FamilyMemberHistory.identifier | Encounter.identifier | DeviceRequest.identifier | AllergyIntolerance.identifier | CarePlan.identifier | EpisodeOfCare.identifier | Procedure.identifier | List.identifier | Immunization.identi'+'fier | VisionPrescription.identifier | ProcedureRequest.identifier | Observation.identifier | DiagnosticReport.identifier | NutritionOrder.identifier | Condition.identifier | Composition.identifier | DetectedIssue.identifier | SupplyDelivery.identifi'+'er', SearchXpathUsageNormal);
  indexes.add('NutritionOrder', 'oraldiet', 'Type of diet that can be consumed orally (i.e., take via the mouth).', SearchParamTypeTOKEN, [], 'NutritionOrder.oralDiet.type', SearchXpathUsageNormal);
  indexes.add('NutritionOrder', 'patient', 'The identity of the person who requires the diet, formula or nutritional supplement', SearchParamTypeREFERENCE, ['Group', 'Patient'], 'ReferralRequest.subject | DocumentManifest.subject | Goal.subject | Consent.patient | DocumentReference.subject | ImagingManifest.patient | RiskAssessment.subject | CareTeam.subject | ImagingStudy.patient | FamilyMemberHistory.patient | Encounter.sub'+'ject | DeviceUseStatement.subject | DeviceRequest.subject | AllergyIntolerance.patient | CarePlan.subject | EpisodeOfCare.patient | Procedure.subject | List.subject | Immunization.patient | VisionPrescription.patient | ProcedureRequest.subject | Flag'+'.subject | Observation.subject | DiagnosticReport.subject | NutritionOrder.patient | Condition.subject | Composition.subject | DetectedIssue.patient | SupplyDelivery.patient | ClinicalImpression.subject', SearchXpathUsageNormal);
  indexes.add('NutritionOrder', 'provider', 'The identify of the provider who placed the nutrition order', SearchParamTypeREFERENCE, ['Practitioner'], 'NutritionOrder.orderer', SearchXpathUsageNormal);
  indexes.add('NutritionOrder', 'status', 'Status of the nutrition order.', SearchParamTypeTOKEN, [], 'NutritionOrder.status', SearchXpathUsageNormal);
  indexes.add('NutritionOrder', 'supplement', 'Type of supplement product requested', SearchParamTypeTOKEN, [], 'NutritionOrder.supplement.type', SearchXpathUsageNormal);
  compartments.register(frtEncounter, 'NutritionOrder', ['encounter']);
  compartments.register(frtPatient, 'NutritionOrder', ['patient']);
  compartments.register(frtPractitioner, 'NutritionOrder', ['provider']);
end;
{$ENDIF}

{$IFDEF FHIR_OBSERVATION}
procedure TFHIRIndexBuilder.buildIndexesForObservation(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Observation', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Observation', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('Observation', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('Observation', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('Observation', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Observation', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('Observation', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('Observation', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Observation', 'based-on', 'Reference to the test or procedure request.', SearchParamTypeREFERENCE, ['ReferralRequest', 'CarePlan', 'MedicationRequest', 'NutritionOrder', 'ProcedureRequest', 'DeviceRequest', 'ImmunizationRecommendation'], 'Observation.basedOn', SearchXpathUsageNormal);
  indexes.add('Observation', 'category', 'The classification of the type of observation', SearchParamTypeTOKEN, [], 'Observation.category', SearchXpathUsageNormal);
  indexes.add('Observation', 'code', 'The code of the observation type', SearchParamTypeTOKEN, [], 'FamilyMemberHistory.condition.code | DeviceRequest.code.as(CodeableConcept) | AllergyIntolerance.code | AllergyIntolerance.reaction.substance | Procedure.code | List.code | ProcedureRequest.code | Observation.code | DiagnosticReport.code | Condition.'+'code', SearchXpathUsageNormal);
  indexes.add('Observation', 'code-value-concept', 'Code and coded value parameter pair', SearchParamTypeCOMPOSITE, [], 'Observation', SearchXpathUsageNormal);
  indexes.add('Observation', 'code-value-date', 'Code and date/time value parameter pair', SearchParamTypeCOMPOSITE, [], 'Observation', SearchXpathUsageNormal);
  indexes.add('Observation', 'code-value-quantity', 'Code and quantity value parameter pair', SearchParamTypeCOMPOSITE, [], 'Observation', SearchXpathUsageNormal);
  indexes.add('Observation', 'code-value-string', 'Code and string value parameter pair', SearchParamTypeCOMPOSITE, [], 'Observation', SearchXpathUsageNormal);
  indexes.add('Observation', 'combo-code', 'The code of the observation type or component type', SearchParamTypeTOKEN, [], 'Observation.code | Observation.component.code', SearchXpathUsageNormal);
  indexes.add('Observation', 'combo-code-value-concept', 'Code and coded value parameter pair, including in components', SearchParamTypeCOMPOSITE, [], 'Observation | Observation.component', SearchXpathUsageNormal);
  indexes.add('Observation', 'combo-code-value-quantity', 'Code and quantity value parameter pair, including in components', SearchParamTypeCOMPOSITE, [], 'Observation | Observation.component', SearchXpathUsageNormal);
  indexes.add('Observation', 'combo-data-absent-reason', 'The reason why the expected value in the element Observation.value[x] or Observation.component.value[x] is missing.', SearchParamTypeTOKEN, [], 'Observation.dataAbsentReason | Observation.component.dataAbsentReason', SearchXpathUsageNormal);
  indexes.add('Observation', 'combo-value-concept', 'The value or component value of the observation, if the value is a CodeableConcept', SearchParamTypeTOKEN, [], 'Observation.value.as(CodeableConcept) | Observation.component.value.as(CodeableConcept)', SearchXpathUsageNormal);
  indexes.add('Observation', 'combo-value-quantity', 'The value or component value of the observation, if the value is a Quantity, or a SampledData (just search on the bounds of the values in sampled data)', SearchParamTypeQUANTITY, [], 'Observation.value.as(Quantity) | Observation.component.value.as(Quantity)', SearchXpathUsageNormal);
  indexes.add('Observation', 'component-code', 'The component code of the observation type', SearchParamTypeTOKEN, [], 'Observation.component.code', SearchXpathUsageNormal);
  indexes.add('Observation', 'component-code-value-concept', 'Component code and component coded value parameter pair', SearchParamTypeCOMPOSITE, [], 'Observation.component', SearchXpathUsageNormal);
  indexes.add('Observation', 'component-code-value-quantity', 'Component code and component quantity value parameter pair', SearchParamTypeCOMPOSITE, [], 'Observation.component', SearchXpathUsageNormal);
  indexes.add('Observation', 'component-data-absent-reason', 'The reason why the expected value in the element Observation.component.value[x] is missing.', SearchParamTypeTOKEN, [], 'Observation.component.dataAbsentReason', SearchXpathUsageNormal);
  indexes.add('Observation', 'component-value-concept', 'The value of the component observation, if the value is a CodeableConcept', SearchParamTypeTOKEN, [], 'Observation.component.value.as(CodeableConcept)', SearchXpathUsageNormal);
  indexes.add('Observation', 'component-value-quantity', 'The value of the component observation, if the value is a Quantity, or a SampledData (just search on the bounds of the values in sampled data)', SearchParamTypeQUANTITY, [], 'Observation.component.value.as(Quantity)', SearchXpathUsageNormal);
  indexes.add('Observation', 'context', 'Healthcare event  (Episode-of-care or Encounter) related to the observation', SearchParamTypeREFERENCE, ['EpisodeOfCare', 'Encounter'], 'Observation.context', SearchXpathUsageNormal);
  indexes.add('Observation', 'data-absent-reason', 'The reason why the expected value in the element Observation.value[x] is missing.', SearchParamTypeTOKEN, [], 'Observation.dataAbsentReason', SearchXpathUsageNormal);
  indexes.add('Observation', 'date', 'Obtained date/time. If the obtained element is a period, a date that falls in the period', SearchParamTypeDATE, [], 'Consent.dateTime | SupplyRequest.authoredOn | RiskAssessment.occurrence.as(DateTime) | CareTeam.period | FamilyMemberHistory.date | Encounter.period | AllergyIntolerance.assertedDate | CarePlan.period | EpisodeOfCare.period | Procedure.performed | Li'+'st.date | Immunization.date | Flag.period | Observation.effective | DiagnosticReport.effective | Composition.date | DetectedIssue.date | ClinicalImpression.date', SearchXpathUsageNormal);
  indexes.add('Observation', 'device', 'The Device that generated the observation data.', SearchParamTypeREFERENCE, ['Device', 'DeviceMetric'], 'Observation.device', SearchXpathUsageNormal);
  indexes.add('Observation', 'dna-variant', 'search for extension http://hl7.org/fhir/StructureDefinition/observation-geneticsDNASequenceVariantName', SearchParamTypeTOKEN, [], 'Observation.extension(''http://hl7.org/fhir/StructureDefinition/observation-geneticsDNASequenceVariantName'').value', SearchXpathUsageNormal);
  indexes.add('Observation', 'encounter', 'Encounter related to the observation', SearchParamTypeREFERENCE, ['EpisodeOfCare', 'Encounter'], 'DocumentReference.context.encounter | RiskAssessment.context | DeviceRequest.context | Procedure.context | List.encounter | VisionPrescription.encounter | ProcedureRequest.context | Flag.encounter | Observation.context | DiagnosticReport.context | Nu'+'tritionOrder.encounter | Composition.encounter', SearchXpathUsageNormal);
  indexes.add('Observation', 'gene-dnavariant', 'search for extension http://hl7.org/fhir/StructureDefinition/observation-geneticsDNAVariantId', SearchParamTypeTOKEN, [], 'Observation.extension(''http://hl7.org/fhir/StructureDefinition/observation-geneticsDNAVariantId'').value', SearchXpathUsageNormal);
  indexes.add('Observation', 'gene-identifier', 'search for extension http://hl7.org/fhir/StructureDefinition/observation-geneticsGene', SearchParamTypeTOKEN, [], 'Observation.extension(''http://hl7.org/fhir/StructureDefinition/observation-geneticsGene'').value', SearchXpathUsageNormal);
  indexes.add('Observation', 'identifier', 'The unique id for a particular observation', SearchParamTypeTOKEN, [], 'DocumentManifest.masterIdentifier | DocumentManifest.identifier | Goal.identifier | Consent.identifier | DocumentReference.masterIdentifier | DocumentReference.identifier | SupplyRequest.identifier | RiskAssessment.identifier | CareTeam.identifier | '+'ImagingStudy.identifier | FamilyMemberHistory.identifier | Encounter.identifier | DeviceRequest.identifier | AllergyIntolerance.identifier | CarePlan.identifier | EpisodeOfCare.identifier | Procedure.identifier | List.identifier | Immunization.identi'+'fier | VisionPrescription.identifier | ProcedureRequest.identifier | Observation.identifier | DiagnosticReport.identifier | NutritionOrder.identifier | Condition.identifier | Composition.identifier | DetectedIssue.identifier | SupplyDelivery.identifi'+'er', SearchXpathUsageNormal);
  indexes.add('Observation', 'method', 'The method used for the observation', SearchParamTypeTOKEN, [], 'Observation.method', SearchXpathUsageNormal);
  indexes.add('Observation', 'patient', 'The subject that the observation is about (if patient)', SearchParamTypeREFERENCE, ['Group', 'Patient'], 'ReferralRequest.subject | DocumentManifest.subject | Goal.subject | Consent.patient | DocumentReference.subject | ImagingManifest.patient | RiskAssessment.subject | CareTeam.subject | ImagingStudy.patient | FamilyMemberHistory.patient | Encounter.sub'+'ject | DeviceUseStatement.subject | DeviceRequest.subject | AllergyIntolerance.patient | CarePlan.subject | EpisodeOfCare.patient | Procedure.subject | List.subject | Immunization.patient | VisionPrescription.patient | ProcedureRequest.subject | Flag'+'.subject | Observation.subject | DiagnosticReport.subject | NutritionOrder.patient | Condition.subject | Composition.subject | DetectedIssue.patient | SupplyDelivery.patient | ClinicalImpression.subject', SearchXpathUsageNormal);
  indexes.add('Observation', 'performer', 'Who performed the observation', SearchParamTypeREFERENCE, ['Practitioner', 'Organization', 'Patient', 'RelatedPerson'], 'Observation.performer', SearchXpathUsageNormal);
  indexes.add('Observation', 'related', 'Related Observations - search on related-type and related-target together', SearchParamTypeCOMPOSITE, [], 'Observation.related', SearchXpathUsageNormal);
  indexes.add('Observation', 'related-target', 'Resource that is related to this one', SearchParamTypeREFERENCE, ['Observation', 'Sequence', 'QuestionnaireResponse'], 'Observation.related.target', SearchXpathUsageNormal);
  indexes.add('Observation', 'related-type', 'has-member | derived-from | sequel-to | replaces | qualified-by | interfered-by', SearchParamTypeTOKEN, [], 'Observation.related.type', SearchXpathUsageNormal);
  indexes.add('Observation', 'specimen', 'Specimen used for this observation', SearchParamTypeREFERENCE, ['Specimen'], 'Observation.specimen', SearchXpathUsageNormal);
  indexes.add('Observation', 'status', 'The status of the observation', SearchParamTypeTOKEN, [], 'Observation.status', SearchXpathUsageNormal);
  indexes.add('Observation', 'subject', 'The subject that the observation is about', SearchParamTypeREFERENCE, ['Group', 'Device', 'Patient', 'Location'], 'Observation.subject', SearchXpathUsageNormal);
  indexes.add('Observation', 'value-concept', 'The value of the observation, if the value is a CodeableConcept', SearchParamTypeTOKEN, [], 'Observation.value.as(CodeableConcept)', SearchXpathUsageNormal);
  indexes.add('Observation', 'value-date', 'The value of the observation, if the value is a date or period of time', SearchParamTypeDATE, [], 'Observation.value.as(DateTime) | Observation.value.as(Period)', SearchXpathUsageNormal);
  indexes.add('Observation', 'value-quantity', 'The value of the observation, if the value is a Quantity, or a SampledData (just search on the bounds of the values in sampled data)', SearchParamTypeQUANTITY, [], 'Observation.value.as(Quantity)', SearchXpathUsageNormal);
  indexes.add('Observation', 'value-string', 'The value of the observation, if the value is a string, and also searches in CodeableConcept.text', SearchParamTypeSTRING, [], 'Observation.value.as(String)', SearchXpathUsageNormal);
  compartments.register(frtDevice, 'Observation', ['subject', 'device']);
  compartments.register(frtEncounter, 'Observation', ['encounter']);
  compartments.register(frtPatient, 'Observation', ['subject', 'performer']);
  compartments.register(frtPractitioner, 'Observation', ['performer']);
  compartments.register(frtRelatedPerson, 'Observation', ['performer']);
end;
{$ENDIF}

{$IFDEF FHIR_OPERATIONDEFINITION}
procedure TFHIRIndexBuilder.buildIndexesForOperationDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('OperationDefinition', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('OperationDefinition', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('OperationDefinition', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('OperationDefinition', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('OperationDefinition', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('OperationDefinition', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('OperationDefinition', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('OperationDefinition', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('OperationDefinition', 'base', 'Marks this as a profile of the base', SearchParamTypeREFERENCE, ['OperationDefinition'], 'OperationDefinition.base', SearchXpathUsageNormal);
  indexes.add('OperationDefinition', 'code', 'Name used to invoke the operation', SearchParamTypeTOKEN, [], 'OperationDefinition.code', SearchXpathUsageNormal);
  indexes.add('OperationDefinition', 'date', 'The operation definition publication date', SearchParamTypeDATE, [], 'OperationDefinition.date', SearchXpathUsageNormal);
  indexes.add('OperationDefinition', 'description', 'The description of the operation definition', SearchParamTypeSTRING, [], 'OperationDefinition.description', SearchXpathUsageNormal);
  indexes.add('OperationDefinition', 'instance', 'Invoke on an instance?', SearchParamTypeTOKEN, [], 'OperationDefinition.instance', SearchXpathUsageNormal);
  indexes.add('OperationDefinition', 'jurisdiction', 'Intended jurisdiction for the operation definition', SearchParamTypeTOKEN, [], 'OperationDefinition.jurisdiction', SearchXpathUsageNormal);
  indexes.add('OperationDefinition', 'kind', 'operation | query', SearchParamTypeTOKEN, [], 'OperationDefinition.kind', SearchXpathUsageNormal);
  indexes.add('OperationDefinition', 'name', 'Computationally friendly name of the operation definition', SearchParamTypeSTRING, [], 'OperationDefinition.name', SearchXpathUsageNormal);
  indexes.add('OperationDefinition', 'param-profile', 'Profile on the type', SearchParamTypeREFERENCE, ['StructureDefinition'], 'OperationDefinition.parameter.profile', SearchXpathUsageNormal);
  indexes.add('OperationDefinition', 'publisher', 'Name of the publisher of the operation definition', SearchParamTypeSTRING, [], 'OperationDefinition.publisher', SearchXpathUsageNormal);
  indexes.add('OperationDefinition', 'status', 'The current status of the operation definition', SearchParamTypeTOKEN, [], 'OperationDefinition.status', SearchXpathUsageNormal);
  indexes.add('OperationDefinition', 'system', 'Invoke at the system level?', SearchParamTypeTOKEN, [], 'OperationDefinition.system', SearchXpathUsageNormal);
  indexes.add('OperationDefinition', 'type', 'Invole at the type level?', SearchParamTypeTOKEN, [], 'OperationDefinition.type', SearchXpathUsageNormal);
  indexes.add('OperationDefinition', 'url', 'The uri that identifies the operation definition', SearchParamTypeURI, [], 'OperationDefinition.url', SearchXpathUsageNormal);
  indexes.add('OperationDefinition', 'version', 'The business version of the operation definition', SearchParamTypeTOKEN, [], 'OperationDefinition.version', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_OPERATIONOUTCOME}
procedure TFHIRIndexBuilder.buildIndexesForOperationOutcome(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('OperationOutcome', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('OperationOutcome', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('OperationOutcome', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('OperationOutcome', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('OperationOutcome', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('OperationOutcome', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('OperationOutcome', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('OperationOutcome', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_ORGANIZATION}
procedure TFHIRIndexBuilder.buildIndexesForOrganization(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Organization', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Organization', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('Organization', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('Organization', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('Organization', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Organization', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('Organization', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('Organization', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Organization', 'active', 'A server defined search that may match any of the string fields in the Address, including line, city, state, country, postalCode, and/or text', SearchParamTypeTOKEN, [], 'Organization.active', SearchXpathUsageNormal);
  indexes.add('Organization', 'address', 'A (part of the) address of the organization', SearchParamTypeSTRING, [], 'Organization.address', SearchXpathUsageNormal);
  indexes.add('Organization', 'address-city', 'A city specified in an address', SearchParamTypeSTRING, [], 'Organization.address.city', SearchXpathUsageNormal);
  indexes.add('Organization', 'address-country', 'A country specified in an address', SearchParamTypeSTRING, [], 'Organization.address.country', SearchXpathUsageNormal);
  indexes.add('Organization', 'address-postalcode', 'A postal code specified in an address', SearchParamTypeSTRING, [], 'Organization.address.postalCode', SearchXpathUsageNormal);
  indexes.add('Organization', 'address-state', 'A state specified in an address', SearchParamTypeSTRING, [], 'Organization.address.state', SearchXpathUsageNormal);
  indexes.add('Organization', 'address-use', 'A use code specified in an address', SearchParamTypeTOKEN, [], 'Organization.address.use', SearchXpathUsageNormal);
  indexes.add('Organization', 'endpoint', 'Technical endpoints providing access to services operated for the organization', SearchParamTypeREFERENCE, ['Endpoint'], 'Organization.endpoint', SearchXpathUsageNormal);
  indexes.add('Organization', 'identifier', 'Any identifier for the organization (not the accreditation issuer''s identifier)', SearchParamTypeTOKEN, [], 'Organization.identifier', SearchXpathUsageNormal);
  indexes.add('Organization', 'name', 'A portion of the organization''s name or alias', SearchParamTypeSTRING, [], 'Organization.name | Organization.alias', SearchXpathUsageNormal);
  indexes.add('Organization', 'partof', 'An organization of which this organization forms a part', SearchParamTypeREFERENCE, ['Organization'], 'Organization.partOf', SearchXpathUsageNormal);
  indexes.add('Organization', 'phonetic', 'A portion of the organization''s name using some kind of phonetic matching algorithm', SearchParamTypeSTRING, [], 'Organization.name', SearchXpathUsagePhonetic);
  indexes.add('Organization', 'type', 'A code for the type of organization', SearchParamTypeTOKEN, [], 'Organization.type', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_PATIENT}
procedure TFHIRIndexBuilder.buildIndexesForPatient(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Patient', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Patient', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('Patient', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('Patient', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('Patient', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Patient', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('Patient', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('Patient', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Patient', 'active', 'Whether the patient record is active', SearchParamTypeTOKEN, [], 'Patient.active', SearchXpathUsageNormal);
  indexes.add('Patient', 'address', 'A server defined search that may match any of the string fields in the Address, including line, city, state, country, postalCode, and/or text', SearchParamTypeSTRING, [], 'RelatedPerson.address | Practitioner.address | Person.address | Patient.address', SearchXpathUsageNormal);
  indexes.add('Patient', 'address-city', 'A city specified in an address', SearchParamTypeSTRING, [], 'RelatedPerson.address.city | Practitioner.address.city | Person.address.city | Patient.address.city', SearchXpathUsageNormal);
  indexes.add('Patient', 'address-country', 'A country specified in an address', SearchParamTypeSTRING, [], 'RelatedPerson.address.country | Practitioner.address.country | Person.address.country | Patient.address.country', SearchXpathUsageNormal);
  indexes.add('Patient', 'address-postalcode', 'A postalCode specified in an address', SearchParamTypeSTRING, [], 'RelatedPerson.address.postalCode | Practitioner.address.postalCode | Person.address.postalCode | Patient.address.postalCode', SearchXpathUsageNormal);
  indexes.add('Patient', 'address-state', 'A state specified in an address', SearchParamTypeSTRING, [], 'RelatedPerson.address.state | Practitioner.address.state | Person.address.state | Patient.address.state', SearchXpathUsageNormal);
  indexes.add('Patient', 'address-use', 'A use code specified in an address', SearchParamTypeTOKEN, [], 'RelatedPerson.address.use | Practitioner.address.use | Person.address.use | Patient.address.use', SearchXpathUsageNormal);
  indexes.add('Patient', 'animal-breed', 'The breed for animal patients', SearchParamTypeTOKEN, [], 'Patient.animal.breed', SearchXpathUsageNormal);
  indexes.add('Patient', 'animal-species', 'The species for animal patients', SearchParamTypeTOKEN, [], 'Patient.animal.species', SearchXpathUsageNormal);
  indexes.add('Patient', 'birthdate', 'The patient''s date of birth', SearchParamTypeDATE, [], 'RelatedPerson.birthDate | Person.birthDate | Patient.birthDate', SearchXpathUsageNormal);
  indexes.add('Patient', 'death-date', 'The date of death has been provided and satisfies this search value', SearchParamTypeDATE, [], 'Patient.deceased.as(DateTime)', SearchXpathUsageNormal);
  indexes.add('Patient', 'deceased', 'This patient has been marked as deceased, or as a death date entered', SearchParamTypeTOKEN, [], 'Patient.deceased.exists()', SearchXpathUsageNormal);
  indexes.add('Patient', 'email', 'A value in an email contact', SearchParamTypeTOKEN, [], 'PractitionerRole.telecom.where(system=''email'') | RelatedPerson.telecom.where(system=''email'') | Practitioner.telecom.where(system=''email'') | Person.telecom.where(system=''email'') | Patient.telecom.where(system=''email'')', SearchXpathUsageNormal);
  indexes.add('Patient', 'family', 'A portion of the family name of the patient', SearchParamTypeSTRING, [], 'Practitioner.name.family | Patient.name.family', SearchXpathUsageNormal);
  indexes.add('Patient', 'gender', 'Gender of the patient', SearchParamTypeTOKEN, [], 'RelatedPerson.gender | Practitioner.gender | Person.gender | Patient.gender', SearchXpathUsageNormal);
  indexes.add('Patient', 'general-practitioner', 'Patient''s nominated general practitioner, not the organization that manages the record', SearchParamTypeREFERENCE, ['Practitioner', 'Organization'], 'Patient.generalPractitioner', SearchXpathUsageNormal);
  indexes.add('Patient', 'given', 'A portion of the given name of the patient', SearchParamTypeSTRING, [], 'Practitioner.name.given | Patient.name.given', SearchXpathUsageNormal);
  indexes.add('Patient', 'identifier', 'A patient identifier', SearchParamTypeTOKEN, [], 'Patient.identifier', SearchXpathUsageNormal);
  indexes.add('Patient', 'language', 'Language code (irrespective of use value)', SearchParamTypeTOKEN, [], 'Patient.communication.language', SearchXpathUsageNormal);
  indexes.add('Patient', 'link', 'All patients linked to the given patient', SearchParamTypeREFERENCE, ['Patient', 'RelatedPerson'], 'Patient.link.other', SearchXpathUsageNormal);
  indexes.add('Patient', 'name', 'A server defined search that may match any of the string fields in the HumanName, including family, give, prefix, suffix, suffix, and/or text', SearchParamTypeSTRING, [], 'Patient.name', SearchXpathUsageNormal);
  indexes.add('Patient', 'organization', 'The organization at which this person is a patient', SearchParamTypeREFERENCE, ['Organization'], 'Patient.managingOrganization', SearchXpathUsageNormal);
  indexes.add('Patient', 'phone', 'A value in a phone contact', SearchParamTypeTOKEN, [], 'PractitionerRole.telecom.where(system=''phone'') | RelatedPerson.telecom.where(system=''phone'') | Practitioner.telecom.where(system=''phone'') | Person.telecom.where(system=''phone'') | Patient.telecom.where(system=''phone'')', SearchXpathUsageNormal);
  indexes.add('Patient', 'phonetic', 'A portion of either family or given name using some kind of phonetic matching algorithm', SearchParamTypeSTRING, [], 'RelatedPerson.name | Practitioner.name | Person.name | Patient.name', SearchXpathUsagePhonetic);
  indexes.add('Patient', 'telecom', 'The value in any kind of telecom details of the patient', SearchParamTypeTOKEN, [], 'PractitionerRole.telecom | RelatedPerson.telecom | Practitioner.telecom | Person.telecom | Patient.telecom', SearchXpathUsageNormal);
  compartments.register(frtPatient, 'Patient', ['link']);
  compartments.register(frtPractitioner, 'Patient', ['general-practitioner']);
  compartments.register(frtRelatedPerson, 'Patient', ['link']);
end;
{$ENDIF}

{$IFDEF FHIR_PAYMENTNOTICE}
procedure TFHIRIndexBuilder.buildIndexesForPaymentNotice(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('PaymentNotice', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('PaymentNotice', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('PaymentNotice', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('PaymentNotice', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('PaymentNotice', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('PaymentNotice', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('PaymentNotice', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('PaymentNotice', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('PaymentNotice', 'created', 'Creation date fro the notice', SearchParamTypeDATE, [], 'PaymentNotice.created', SearchXpathUsageNormal);
  indexes.add('PaymentNotice', 'identifier', 'The business identifier of the notice', SearchParamTypeTOKEN, [], 'PaymentNotice.identifier', SearchXpathUsageNormal);
  indexes.add('PaymentNotice', 'organization', 'The organization who generated this resource', SearchParamTypeREFERENCE, ['Organization'], 'PaymentNotice.organization', SearchXpathUsageNormal);
  indexes.add('PaymentNotice', 'payment-status', 'The type of payment notice', SearchParamTypeTOKEN, [], 'PaymentNotice.paymentStatus', SearchXpathUsageNormal);
  indexes.add('PaymentNotice', 'provider', 'The reference to the provider', SearchParamTypeREFERENCE, ['Practitioner'], 'PaymentNotice.provider', SearchXpathUsageNormal);
  indexes.add('PaymentNotice', 'request', 'The Claim', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'PaymentNotice.request', SearchXpathUsageNormal);
  indexes.add('PaymentNotice', 'response', 'The ClaimResponse', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'PaymentNotice.response', SearchXpathUsageNormal);
  indexes.add('PaymentNotice', 'statusdate', 'The date of the payment action', SearchParamTypeDATE, [], 'PaymentNotice.statusDate', SearchXpathUsageNormal);
  compartments.register(frtPractitioner, 'PaymentNotice', ['provider']);
end;
{$ENDIF}

{$IFDEF FHIR_PAYMENTRECONCILIATION}
procedure TFHIRIndexBuilder.buildIndexesForPaymentReconciliation(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('PaymentReconciliation', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('PaymentReconciliation', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('PaymentReconciliation', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('PaymentReconciliation', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('PaymentReconciliation', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('PaymentReconciliation', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('PaymentReconciliation', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('PaymentReconciliation', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('PaymentReconciliation', 'created', 'The creation date', SearchParamTypeDATE, [], 'PaymentReconciliation.created', SearchXpathUsageNormal);
  indexes.add('PaymentReconciliation', 'disposition', 'The contents of the disposition message', SearchParamTypeSTRING, [], 'PaymentReconciliation.disposition', SearchXpathUsageNormal);
  indexes.add('PaymentReconciliation', 'identifier', 'The business identifier of the Explanation of Benefit', SearchParamTypeTOKEN, [], 'PaymentReconciliation.identifier', SearchXpathUsageNormal);
  indexes.add('PaymentReconciliation', 'organization', 'The organization who generated this resource', SearchParamTypeREFERENCE, ['Organization'], 'PaymentReconciliation.organization', SearchXpathUsageNormal);
  indexes.add('PaymentReconciliation', 'outcome', 'The processing outcome', SearchParamTypeTOKEN, [], 'PaymentReconciliation.outcome', SearchXpathUsageNormal);
  indexes.add('PaymentReconciliation', 'request', 'The reference to the claim', SearchParamTypeREFERENCE, ['ProcessRequest'], 'PaymentReconciliation.request', SearchXpathUsageNormal);
  indexes.add('PaymentReconciliation', 'request-organization', 'The organization who generated this resource', SearchParamTypeREFERENCE, ['Organization'], 'PaymentReconciliation.requestOrganization', SearchXpathUsageNormal);
  indexes.add('PaymentReconciliation', 'request-provider', 'The reference to the provider who sumbitted the claim', SearchParamTypeREFERENCE, ['Practitioner'], 'PaymentReconciliation.requestProvider', SearchXpathUsageNormal);
  compartments.register(frtPractitioner, 'PaymentReconciliation', ['request-provider']);
end;
{$ENDIF}

{$IFDEF FHIR_PERSON}
procedure TFHIRIndexBuilder.buildIndexesForPerson(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Person', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Person', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('Person', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('Person', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('Person', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Person', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('Person', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('Person', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Person', 'address', 'A server defined search that may match any of the string fields in the Address, including line, city, state, country, postalCode, and/or text', SearchParamTypeSTRING, [], 'RelatedPerson.address | Practitioner.address | Person.address | Patient.address', SearchXpathUsageNormal);
  indexes.add('Person', 'address-city', 'A city specified in an address', SearchParamTypeSTRING, [], 'RelatedPerson.address.city | Practitioner.address.city | Person.address.city | Patient.address.city', SearchXpathUsageNormal);
  indexes.add('Person', 'address-country', 'A country specified in an address', SearchParamTypeSTRING, [], 'RelatedPerson.address.country | Practitioner.address.country | Person.address.country | Patient.address.country', SearchXpathUsageNormal);
  indexes.add('Person', 'address-postalcode', 'A postal code specified in an address', SearchParamTypeSTRING, [], 'RelatedPerson.address.postalCode | Practitioner.address.postalCode | Person.address.postalCode | Patient.address.postalCode', SearchXpathUsageNormal);
  indexes.add('Person', 'address-state', 'A state specified in an address', SearchParamTypeSTRING, [], 'RelatedPerson.address.state | Practitioner.address.state | Person.address.state | Patient.address.state', SearchXpathUsageNormal);
  indexes.add('Person', 'address-use', 'A use code specified in an address', SearchParamTypeTOKEN, [], 'RelatedPerson.address.use | Practitioner.address.use | Person.address.use | Patient.address.use', SearchXpathUsageNormal);
  indexes.add('Person', 'birthdate', 'The Related Person''s date of birth', SearchParamTypeDATE, [], 'RelatedPerson.birthDate | Person.birthDate | Patient.birthDate', SearchXpathUsageNormal);
  indexes.add('Person', 'email', 'A value in an email contact', SearchParamTypeTOKEN, [], 'PractitionerRole.telecom.where(system=''email'') | RelatedPerson.telecom.where(system=''email'') | Practitioner.telecom.where(system=''email'') | Person.telecom.where(system=''email'') | Patient.telecom.where(system=''email'')', SearchXpathUsageNormal);
  indexes.add('Person', 'gender', 'Gender of the related person', SearchParamTypeTOKEN, [], 'RelatedPerson.gender | Practitioner.gender | Person.gender | Patient.gender', SearchXpathUsageNormal);
  indexes.add('Person', 'identifier', 'A person Identifier', SearchParamTypeTOKEN, [], 'Person.identifier', SearchXpathUsageNormal);
  indexes.add('Person', 'link', 'Any link has this Patient, Person, RelatedPerson or Practitioner reference', SearchParamTypeREFERENCE, ['Practitioner', 'Patient', 'Person', 'RelatedPerson'], 'Person.link.target', SearchXpathUsageNormal);
  indexes.add('Person', 'name', 'A server defined search that may match any of the string fields in the HumanName, including family, give, prefix, suffix, suffix, and/or text', SearchParamTypeSTRING, [], 'Person.name', SearchXpathUsageNormal);
  indexes.add('Person', 'organization', 'The organization at which this person record is being managed', SearchParamTypeREFERENCE, ['Organization'], 'Person.managingOrganization', SearchXpathUsageNormal);
  indexes.add('Person', 'patient', 'The Person links to this Patient', SearchParamTypeREFERENCE, ['Patient'], 'Person.link.target', SearchXpathUsageNormal);
  indexes.add('Person', 'phone', 'A value in a phone contact', SearchParamTypeTOKEN, [], 'PractitionerRole.telecom.where(system=''phone'') | RelatedPerson.telecom.where(system=''phone'') | Practitioner.telecom.where(system=''phone'') | Person.telecom.where(system=''phone'') | Patient.telecom.where(system=''phone'')', SearchXpathUsageNormal);
  indexes.add('Person', 'phonetic', 'A portion of name using some kind of phonetic matching algorithm', SearchParamTypeSTRING, [], 'RelatedPerson.name | Practitioner.name | Person.name | Patient.name', SearchXpathUsagePhonetic);
  indexes.add('Person', 'practitioner', 'The Person links to this Practitioner', SearchParamTypeREFERENCE, ['Practitioner'], 'Person.link.target', SearchXpathUsageNormal);
  indexes.add('Person', 'relatedperson', 'The Person links to this RelatedPerson', SearchParamTypeREFERENCE, ['RelatedPerson'], 'Person.link.target', SearchXpathUsageNormal);
  indexes.add('Person', 'telecom', 'The value in any kind of contact', SearchParamTypeTOKEN, [], 'PractitionerRole.telecom | RelatedPerson.telecom | Practitioner.telecom | Person.telecom | Patient.telecom', SearchXpathUsageNormal);
  compartments.register(frtPatient, 'Person', ['patient']);
  compartments.register(frtPractitioner, 'Person', ['practitioner']);
  compartments.register(frtRelatedPerson, 'Person', ['link']);
end;
{$ENDIF}

{$IFDEF FHIR_PLANDEFINITION}
procedure TFHIRIndexBuilder.buildIndexesForPlanDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('PlanDefinition', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('PlanDefinition', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('PlanDefinition', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('PlanDefinition', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('PlanDefinition', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('PlanDefinition', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('PlanDefinition', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('PlanDefinition', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('PlanDefinition', 'composed-of', 'What resource is being referenced', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'PlanDefinition.relatedArtifact.where(type=''composed-of'').resource', SearchXpathUsageNormal);
  indexes.add('PlanDefinition', 'date', 'The plan definition publication date', SearchParamTypeDATE, [], 'PlanDefinition.date', SearchXpathUsageNormal);
  indexes.add('PlanDefinition', 'depends-on', 'What resource is being referenced', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'PlanDefinition.relatedArtifact.where(type=''depends-on'').resource | PlanDefinition.library', SearchXpathUsageNormal);
  indexes.add('PlanDefinition', 'derived-from', 'What resource is being referenced', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'PlanDefinition.relatedArtifact.where(type=''derived-from'').resource', SearchXpathUsageNormal);
  indexes.add('PlanDefinition', 'description', 'The description of the plan definition', SearchParamTypeSTRING, [], 'PlanDefinition.description', SearchXpathUsageNormal);
  indexes.add('PlanDefinition', 'effective', 'The time during which the plan definition is intended to be in use', SearchParamTypeDATE, [], 'PlanDefinition.effectivePeriod', SearchXpathUsageNormal);
  indexes.add('PlanDefinition', 'identifier', 'External identifier for the plan definition', SearchParamTypeTOKEN, [], 'PlanDefinition.identifier', SearchXpathUsageNormal);
  indexes.add('PlanDefinition', 'jurisdiction', 'Intended jurisdiction for the plan definition', SearchParamTypeTOKEN, [], 'PlanDefinition.jurisdiction', SearchXpathUsageNormal);
  indexes.add('PlanDefinition', 'name', 'Computationally friendly name of the plan definition', SearchParamTypeSTRING, [], 'PlanDefinition.name', SearchXpathUsageNormal);
  indexes.add('PlanDefinition', 'predecessor', 'What resource is being referenced', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'PlanDefinition.relatedArtifact.where(type=''predecessor'').resource', SearchXpathUsageNormal);
  indexes.add('PlanDefinition', 'publisher', 'Name of the publisher of the plan definition', SearchParamTypeSTRING, [], 'PlanDefinition.publisher', SearchXpathUsageNormal);
  indexes.add('PlanDefinition', 'status', 'The current status of the plan definition', SearchParamTypeTOKEN, [], 'PlanDefinition.status', SearchXpathUsageNormal);
  indexes.add('PlanDefinition', 'successor', 'What resource is being referenced', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'PlanDefinition.relatedArtifact.where(type=''successor'').resource', SearchXpathUsageNormal);
  indexes.add('PlanDefinition', 'title', 'The human-friendly name of the plan definition', SearchParamTypeSTRING, [], 'PlanDefinition.title', SearchXpathUsageNormal);
  indexes.add('PlanDefinition', 'topic', 'Topics associated with the module', SearchParamTypeTOKEN, [], 'PlanDefinition.topic', SearchXpathUsageNormal);
  indexes.add('PlanDefinition', 'url', 'The uri that identifies the plan definition', SearchParamTypeURI, [], 'PlanDefinition.url', SearchXpathUsageNormal);
  indexes.add('PlanDefinition', 'version', 'The business version of the plan definition', SearchParamTypeTOKEN, [], 'PlanDefinition.version', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_PRACTITIONER}
procedure TFHIRIndexBuilder.buildIndexesForPractitioner(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Practitioner', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Practitioner', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('Practitioner', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('Practitioner', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('Practitioner', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Practitioner', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('Practitioner', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('Practitioner', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Practitioner', 'active', 'Whether the practitioner record is active', SearchParamTypeTOKEN, [], 'Practitioner.active', SearchXpathUsageNormal);
  indexes.add('Practitioner', 'address', 'A server defined search that may match any of the string fields in the Address, including line, city, state, country, postalCode, and/or text', SearchParamTypeSTRING, [], 'RelatedPerson.address | Practitioner.address | Person.address | Patient.address', SearchXpathUsageNormal);
  indexes.add('Practitioner', 'address-city', 'A city specified in an address', SearchParamTypeSTRING, [], 'RelatedPerson.address.city | Practitioner.address.city | Person.address.city | Patient.address.city', SearchXpathUsageNormal);
  indexes.add('Practitioner', 'address-country', 'A country specified in an address', SearchParamTypeSTRING, [], 'RelatedPerson.address.country | Practitioner.address.country | Person.address.country | Patient.address.country', SearchXpathUsageNormal);
  indexes.add('Practitioner', 'address-postalcode', 'A postalCode specified in an address', SearchParamTypeSTRING, [], 'RelatedPerson.address.postalCode | Practitioner.address.postalCode | Person.address.postalCode | Patient.address.postalCode', SearchXpathUsageNormal);
  indexes.add('Practitioner', 'address-state', 'A state specified in an address', SearchParamTypeSTRING, [], 'RelatedPerson.address.state | Practitioner.address.state | Person.address.state | Patient.address.state', SearchXpathUsageNormal);
  indexes.add('Practitioner', 'address-use', 'A use code specified in an address', SearchParamTypeTOKEN, [], 'RelatedPerson.address.use | Practitioner.address.use | Person.address.use | Patient.address.use', SearchXpathUsageNormal);
  indexes.add('Practitioner', 'communication', 'One of the languages that the practitioner can communicate with', SearchParamTypeTOKEN, [], 'Practitioner.communication', SearchXpathUsageNormal);
  indexes.add('Practitioner', 'email', 'A value in an email contact', SearchParamTypeTOKEN, [], 'PractitionerRole.telecom.where(system=''email'') | RelatedPerson.telecom.where(system=''email'') | Practitioner.telecom.where(system=''email'') | Person.telecom.where(system=''email'') | Patient.telecom.where(system=''email'')', SearchXpathUsageNormal);
  indexes.add('Practitioner', 'family', 'A portion of the family name', SearchParamTypeSTRING, [], 'Practitioner.name.family | Patient.name.family', SearchXpathUsageNormal);
  indexes.add('Practitioner', 'gender', 'Gender of the practitioner', SearchParamTypeTOKEN, [], 'RelatedPerson.gender | Practitioner.gender | Person.gender | Patient.gender', SearchXpathUsageNormal);
  indexes.add('Practitioner', 'given', 'A portion of the given name', SearchParamTypeSTRING, [], 'Practitioner.name.given | Patient.name.given', SearchXpathUsageNormal);
  indexes.add('Practitioner', 'identifier', 'A practitioner''s Identifier', SearchParamTypeTOKEN, [], 'Practitioner.identifier', SearchXpathUsageNormal);
  indexes.add('Practitioner', 'name', 'A server defined search that may match any of the string fields in the HumanName, including family, give, prefix, suffix, suffix, and/or text', SearchParamTypeSTRING, [], 'Practitioner.name', SearchXpathUsageNormal);
  indexes.add('Practitioner', 'phone', 'A value in a phone contact', SearchParamTypeTOKEN, [], 'PractitionerRole.telecom.where(system=''phone'') | RelatedPerson.telecom.where(system=''phone'') | Practitioner.telecom.where(system=''phone'') | Person.telecom.where(system=''phone'') | Patient.telecom.where(system=''phone'')', SearchXpathUsageNormal);
  indexes.add('Practitioner', 'phonetic', 'A portion of either family or given name using some kind of phonetic matching algorithm', SearchParamTypeSTRING, [], 'RelatedPerson.name | Practitioner.name | Person.name | Patient.name', SearchXpathUsagePhonetic);
  indexes.add('Practitioner', 'telecom', 'The value in any kind of contact', SearchParamTypeTOKEN, [], 'PractitionerRole.telecom | RelatedPerson.telecom | Practitioner.telecom | Person.telecom | Patient.telecom', SearchXpathUsageNormal);
  compartments.register(frtPractitioner, 'Practitioner', ['{def}']);
end;
{$ENDIF}

{$IFDEF FHIR_PRACTITIONERROLE}
procedure TFHIRIndexBuilder.buildIndexesForPractitionerRole(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('PractitionerRole', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('PractitionerRole', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('PractitionerRole', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('PractitionerRole', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('PractitionerRole', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('PractitionerRole', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('PractitionerRole', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('PractitionerRole', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('PractitionerRole', 'active', 'Whether this practitioner''s record is in active use', SearchParamTypeTOKEN, [], 'PractitionerRole.active', SearchXpathUsageNormal);
  indexes.add('PractitionerRole', 'date', 'The period during which the practitioner is authorized to perform in these role(s)', SearchParamTypeDATE, [], 'PractitionerRole.period', SearchXpathUsageNormal);
  indexes.add('PractitionerRole', 'email', 'A value in an email contact', SearchParamTypeTOKEN, [], 'PractitionerRole.telecom.where(system=''email'') | RelatedPerson.telecom.where(system=''email'') | Practitioner.telecom.where(system=''email'') | Person.telecom.where(system=''email'') | Patient.telecom.where(system=''email'')', SearchXpathUsageNormal);
  indexes.add('PractitionerRole', 'endpoint', 'Technical endpoints providing access to services operated for the practitioner with this role', SearchParamTypeREFERENCE, ['Endpoint'], 'PractitionerRole.endpoint', SearchXpathUsageNormal);
  indexes.add('PractitionerRole', 'identifier', 'A practitioner''s Identifier', SearchParamTypeTOKEN, [], 'PractitionerRole.identifier', SearchXpathUsageNormal);
  indexes.add('PractitionerRole', 'location', 'One of the locations at which this practitioner provides care', SearchParamTypeREFERENCE, ['Location'], 'PractitionerRole.location', SearchXpathUsageNormal);
  indexes.add('PractitionerRole', 'organization', 'The identity of the organization the practitioner represents / acts on behalf of', SearchParamTypeREFERENCE, ['Organization'], 'PractitionerRole.organization', SearchXpathUsageNormal);
  indexes.add('PractitionerRole', 'phone', 'A value in a phone contact', SearchParamTypeTOKEN, [], 'PractitionerRole.telecom.where(system=''phone'') | RelatedPerson.telecom.where(system=''phone'') | Practitioner.telecom.where(system=''phone'') | Person.telecom.where(system=''phone'') | Patient.telecom.where(system=''phone'')', SearchXpathUsageNormal);
  indexes.add('PractitionerRole', 'practitioner', 'Practitioner that is able to provide the defined services for the organation', SearchParamTypeREFERENCE, ['Practitioner'], 'PractitionerRole.practitioner', SearchXpathUsageNormal);
  indexes.add('PractitionerRole', 'role', 'The practitioner can perform this role at for the organization', SearchParamTypeTOKEN, [], 'PractitionerRole.code', SearchXpathUsageNormal);
  indexes.add('PractitionerRole', 'service', 'The list of healthcare services that this worker provides for this role''s Organization/Location(s)', SearchParamTypeREFERENCE, ['HealthcareService'], 'PractitionerRole.healthcareService', SearchXpathUsageNormal);
  indexes.add('PractitionerRole', 'specialty', 'The practitioner has this specialty at an organization', SearchParamTypeTOKEN, [], 'PractitionerRole.specialty', SearchXpathUsageNormal);
  indexes.add('PractitionerRole', 'telecom', 'The value in any kind of contact', SearchParamTypeTOKEN, [], 'PractitionerRole.telecom | RelatedPerson.telecom | Practitioner.telecom | Person.telecom | Patient.telecom', SearchXpathUsageNormal);
  compartments.register(frtPractitioner, 'PractitionerRole', ['practitioner']);
end;
{$ENDIF}

{$IFDEF FHIR_PROCEDURE}
procedure TFHIRIndexBuilder.buildIndexesForProcedure(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Procedure', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Procedure', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('Procedure', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('Procedure', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('Procedure', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Procedure', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('Procedure', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('Procedure', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Procedure', 'based-on', 'A request for this procedure', SearchParamTypeREFERENCE, ['ReferralRequest', 'CarePlan', 'ProcedureRequest'], 'Procedure.basedOn', SearchXpathUsageNormal);
  indexes.add('Procedure', 'category', 'Classification of the procedure', SearchParamTypeTOKEN, [], 'Procedure.category', SearchXpathUsageNormal);
  indexes.add('Procedure', 'code', 'A code to identify a  procedure', SearchParamTypeTOKEN, [], 'FamilyMemberHistory.condition.code | DeviceRequest.code.as(CodeableConcept) | AllergyIntolerance.code | AllergyIntolerance.reaction.substance | Procedure.code | List.code | ProcedureRequest.code | Observation.code | DiagnosticReport.code | Condition.'+'code', SearchXpathUsageNormal);
  indexes.add('Procedure', 'context', 'Encounter or episode associated with the procedure', SearchParamTypeREFERENCE, ['EpisodeOfCare', 'Encounter'], 'Procedure.context', SearchXpathUsageNormal);
  indexes.add('Procedure', 'date', 'Date/Period the procedure was performed', SearchParamTypeDATE, [], 'Consent.dateTime | SupplyRequest.authoredOn | RiskAssessment.occurrence.as(DateTime) | CareTeam.period | FamilyMemberHistory.date | Encounter.period | AllergyIntolerance.assertedDate | CarePlan.period | EpisodeOfCare.period | Procedure.performed | Li'+'st.date | Immunization.date | Flag.period | Observation.effective | DiagnosticReport.effective | Composition.date | DetectedIssue.date | ClinicalImpression.date', SearchXpathUsageNormal);
  indexes.add('Procedure', 'definition', 'Instantiates protocol or definition', SearchParamTypeREFERENCE, ['PlanDefinition', 'HealthcareService', 'ActivityDefinition'], 'Procedure.definition', SearchXpathUsageNormal);
  indexes.add('Procedure', 'encounter', 'Search by encounter', SearchParamTypeREFERENCE, ['EpisodeOfCare', 'Encounter'], 'DocumentReference.context.encounter | RiskAssessment.context | DeviceRequest.context | Procedure.context | List.encounter | VisionPrescription.encounter | ProcedureRequest.context | Flag.encounter | Observation.context | DiagnosticReport.context | Nu'+'tritionOrder.encounter | Composition.encounter', SearchXpathUsageNormal);
  indexes.add('Procedure', 'identifier', 'A unique identifier for a procedure', SearchParamTypeTOKEN, [], 'DocumentManifest.masterIdentifier | DocumentManifest.identifier | Goal.identifier | Consent.identifier | DocumentReference.masterIdentifier | DocumentReference.identifier | SupplyRequest.identifier | RiskAssessment.identifier | CareTeam.identifier | '+'ImagingStudy.identifier | FamilyMemberHistory.identifier | Encounter.identifier | DeviceRequest.identifier | AllergyIntolerance.identifier | CarePlan.identifier | EpisodeOfCare.identifier | Procedure.identifier | List.identifier | Immunization.identi'+'fier | VisionPrescription.identifier | ProcedureRequest.identifier | Observation.identifier | DiagnosticReport.identifier | NutritionOrder.identifier | Condition.identifier | Composition.identifier | DetectedIssue.identifier | SupplyDelivery.identifi'+'er', SearchXpathUsageNormal);
  indexes.add('Procedure', 'location', 'Where the procedure happened', SearchParamTypeREFERENCE, ['Location'], 'Procedure.location', SearchXpathUsageNormal);
  indexes.add('Procedure', 'part-of', 'Part of referenced event', SearchParamTypeREFERENCE, ['Observation', 'Procedure', 'MedicationAdministration'], 'Procedure.partOf', SearchXpathUsageNormal);
  indexes.add('Procedure', 'patient', 'Search by subject - a patient', SearchParamTypeREFERENCE, ['Group', 'Patient'], 'ReferralRequest.subject | DocumentManifest.subject | Goal.subject | Consent.patient | DocumentReference.subject | ImagingManifest.patient | RiskAssessment.subject | CareTeam.subject | ImagingStudy.patient | FamilyMemberHistory.patient | Encounter.sub'+'ject | DeviceUseStatement.subject | DeviceRequest.subject | AllergyIntolerance.patient | CarePlan.subject | EpisodeOfCare.patient | Procedure.subject | List.subject | Immunization.patient | VisionPrescription.patient | ProcedureRequest.subject | Flag'+'.subject | Observation.subject | DiagnosticReport.subject | NutritionOrder.patient | Condition.subject | Composition.subject | DetectedIssue.patient | SupplyDelivery.patient | ClinicalImpression.subject', SearchXpathUsageNormal);
  indexes.add('Procedure', 'performer', 'The reference to the practitioner', SearchParamTypeREFERENCE, ['Practitioner', 'Organization', 'Device', 'Patient', 'RelatedPerson'], 'Procedure.performer.actor', SearchXpathUsageNormal);
  indexes.add('Procedure', 'status', 'preparation | in-progress | suspended | aborted | completed | entered-in-error | unknown', SearchParamTypeTOKEN, [], 'Procedure.status', SearchXpathUsageNormal);
  indexes.add('Procedure', 'subject', 'Search by subject', SearchParamTypeREFERENCE, ['Group', 'Patient'], 'Procedure.subject', SearchXpathUsageNormal);
  compartments.register(frtEncounter, 'Procedure', ['encounter']);
  compartments.register(frtPatient, 'Procedure', ['patient', 'performer']);
  compartments.register(frtPractitioner, 'Procedure', ['performer']);
  compartments.register(frtRelatedPerson, 'Procedure', ['performer']);
end;
{$ENDIF}

{$IFDEF FHIR_PROCEDUREREQUEST}
procedure TFHIRIndexBuilder.buildIndexesForProcedureRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ProcedureRequest', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('ProcedureRequest', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('ProcedureRequest', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('ProcedureRequest', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('ProcedureRequest', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ProcedureRequest', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('ProcedureRequest', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('ProcedureRequest', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('ProcedureRequest', 'authored', 'Date request signed', SearchParamTypeDATE, [], 'ProcedureRequest.authoredOn', SearchXpathUsageNormal);
  indexes.add('ProcedureRequest', 'based-on', 'What request fulfills', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'ProcedureRequest.basedOn', SearchXpathUsageNormal);
  indexes.add('ProcedureRequest', 'body-site', 'Where procedure is going to be done', SearchParamTypeTOKEN, [], 'ProcedureRequest.bodySite', SearchXpathUsageNormal);
  indexes.add('ProcedureRequest', 'code', 'What is being requested/ordered', SearchParamTypeTOKEN, [], 'FamilyMemberHistory.condition.code | DeviceRequest.code.as(CodeableConcept) | AllergyIntolerance.code | AllergyIntolerance.reaction.substance | Procedure.code | List.code | ProcedureRequest.code | Observation.code | DiagnosticReport.code | Condition.'+'code', SearchXpathUsageNormal);
  indexes.add('ProcedureRequest', 'context', 'Encounter or Episode during which request was created', SearchParamTypeREFERENCE, ['EpisodeOfCare', 'Encounter'], 'ProcedureRequest.context', SearchXpathUsageNormal);
  indexes.add('ProcedureRequest', 'definition', 'Protocol or definition', SearchParamTypeREFERENCE, ['PlanDefinition', 'ActivityDefinition'], 'ProcedureRequest.definition', SearchXpathUsageNormal);
  indexes.add('ProcedureRequest', 'encounter', 'An encounter in which this request is made', SearchParamTypeREFERENCE, ['EpisodeOfCare', 'Encounter'], 'DocumentReference.context.encounter | RiskAssessment.context | DeviceRequest.context | Procedure.context | List.encounter | VisionPrescription.encounter | ProcedureRequest.context | Flag.encounter | Observation.context | DiagnosticReport.context | Nu'+'tritionOrder.encounter | Composition.encounter', SearchXpathUsageNormal);
  indexes.add('ProcedureRequest', 'identifier', 'Identifiers assigned to this order', SearchParamTypeTOKEN, [], 'DocumentManifest.masterIdentifier | DocumentManifest.identifier | Goal.identifier | Consent.identifier | DocumentReference.masterIdentifier | DocumentReference.identifier | SupplyRequest.identifier | RiskAssessment.identifier | CareTeam.identifier | '+'ImagingStudy.identifier | FamilyMemberHistory.identifier | Encounter.identifier | DeviceRequest.identifier | AllergyIntolerance.identifier | CarePlan.identifier | EpisodeOfCare.identifier | Procedure.identifier | List.identifier | Immunization.identi'+'fier | VisionPrescription.identifier | ProcedureRequest.identifier | Observation.identifier | DiagnosticReport.identifier | NutritionOrder.identifier | Condition.identifier | Composition.identifier | DetectedIssue.identifier | SupplyDelivery.identifi'+'er', SearchXpathUsageNormal);
  indexes.add('ProcedureRequest', 'intent', 'proposal | plan | order +', SearchParamTypeTOKEN, [], 'ProcedureRequest.intent', SearchXpathUsageNormal);
  indexes.add('ProcedureRequest', 'occurrence', 'When procedure should occur', SearchParamTypeDATE, [], 'ProcedureRequest.occurrence', SearchXpathUsageNormal);
  indexes.add('ProcedureRequest', 'patient', 'Search by subject - a patient', SearchParamTypeREFERENCE, ['Group', 'Patient'], 'ReferralRequest.subject | DocumentManifest.subject | Goal.subject | Consent.patient | DocumentReference.subject | ImagingManifest.patient | RiskAssessment.subject | CareTeam.subject | ImagingStudy.patient | FamilyMemberHistory.patient | Encounter.sub'+'ject | DeviceUseStatement.subject | DeviceRequest.subject | AllergyIntolerance.patient | CarePlan.subject | EpisodeOfCare.patient | Procedure.subject | List.subject | Immunization.patient | VisionPrescription.patient | ProcedureRequest.subject | Flag'+'.subject | Observation.subject | DiagnosticReport.subject | NutritionOrder.patient | Condition.subject | Composition.subject | DetectedIssue.patient | SupplyDelivery.patient | ClinicalImpression.subject', SearchXpathUsageNormal);
  indexes.add('ProcedureRequest', 'performer', 'Requested perfomer', SearchParamTypeREFERENCE, ['Practitioner', 'Organization', 'Device', 'Patient', 'HealthcareService', 'RelatedPerson'], 'ProcedureRequest.performer', SearchXpathUsageNormal);
  indexes.add('ProcedureRequest', 'performer-type', 'Performer role', SearchParamTypeTOKEN, [], 'ProcedureRequest.performerType', SearchXpathUsageNormal);
  indexes.add('ProcedureRequest', 'priority', 'routine | urgent | asap | stat', SearchParamTypeTOKEN, [], 'ProcedureRequest.priority', SearchXpathUsageNormal);
  indexes.add('ProcedureRequest', 'replaces', 'What request replaces', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'ProcedureRequest.replaces', SearchXpathUsageNormal);
  indexes.add('ProcedureRequest', 'requester', 'Individual making the request', SearchParamTypeREFERENCE, ['Practitioner', 'Organization', 'Device'], 'ProcedureRequest.requester.agent', SearchXpathUsageNormal);
  indexes.add('ProcedureRequest', 'requisition', 'Composite Request ID', SearchParamTypeTOKEN, [], 'ProcedureRequest.requisition', SearchXpathUsageNormal);
  indexes.add('ProcedureRequest', 'specimen', 'Specimen to be tested', SearchParamTypeREFERENCE, ['Specimen'], 'ProcedureRequest.specimen', SearchXpathUsageNormal);
  indexes.add('ProcedureRequest', 'status', 'draft | active | suspended | completed | entered-in-error | cancelled', SearchParamTypeTOKEN, [], 'ProcedureRequest.status', SearchXpathUsageNormal);
  indexes.add('ProcedureRequest', 'subject', 'Search by subject', SearchParamTypeREFERENCE, ['Group', 'Device', 'Patient', 'Location'], 'ProcedureRequest.subject', SearchXpathUsageNormal);
  compartments.register(frtDevice, 'ProcedureRequest', ['performer', 'requester']);
  compartments.register(frtEncounter, 'ProcedureRequest', ['context']);
  compartments.register(frtPatient, 'ProcedureRequest', ['subject', 'performer']);
  compartments.register(frtPractitioner, 'ProcedureRequest', ['performer', 'requester']);
  compartments.register(frtRelatedPerson, 'ProcedureRequest', ['performer']);
end;
{$ENDIF}

{$IFDEF FHIR_PROCESSREQUEST}
procedure TFHIRIndexBuilder.buildIndexesForProcessRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ProcessRequest', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('ProcessRequest', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('ProcessRequest', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('ProcessRequest', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('ProcessRequest', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ProcessRequest', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('ProcessRequest', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('ProcessRequest', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('ProcessRequest', 'action', 'The action requested by this resource', SearchParamTypeTOKEN, [], 'ProcessRequest.action', SearchXpathUsageNormal);
  indexes.add('ProcessRequest', 'identifier', 'The business identifier of the ProcessRequest', SearchParamTypeTOKEN, [], 'ProcessRequest.identifier', SearchXpathUsageNormal);
  indexes.add('ProcessRequest', 'organization', 'The organization who generated this request', SearchParamTypeREFERENCE, ['Organization'], 'ProcessRequest.organization', SearchXpathUsageNormal);
  indexes.add('ProcessRequest', 'provider', 'The provider who regenerated this request', SearchParamTypeREFERENCE, ['Practitioner'], 'ProcessRequest.provider', SearchXpathUsageNormal);
  compartments.register(frtPractitioner, 'ProcessRequest', ['provider']);
end;
{$ENDIF}

{$IFDEF FHIR_PROCESSRESPONSE}
procedure TFHIRIndexBuilder.buildIndexesForProcessResponse(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ProcessResponse', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('ProcessResponse', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('ProcessResponse', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('ProcessResponse', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('ProcessResponse', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ProcessResponse', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('ProcessResponse', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('ProcessResponse', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('ProcessResponse', 'identifier', 'The business identifier of the Explanation of Benefit', SearchParamTypeTOKEN, [], 'ProcessResponse.identifier', SearchXpathUsageNormal);
  indexes.add('ProcessResponse', 'organization', 'The organization who generated this resource', SearchParamTypeREFERENCE, ['Organization'], 'ProcessResponse.organization', SearchXpathUsageNormal);
  indexes.add('ProcessResponse', 'request', 'The reference to the claim', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'ProcessResponse.request', SearchXpathUsageNormal);
  indexes.add('ProcessResponse', 'request-organization', 'The Organization who is responsible the request transaction', SearchParamTypeREFERENCE, ['Organization'], 'ProcessResponse.requestOrganization', SearchXpathUsageNormal);
  indexes.add('ProcessResponse', 'request-provider', 'The Provider who is responsible the request transaction', SearchParamTypeREFERENCE, ['Practitioner'], 'ProcessResponse.requestProvider', SearchXpathUsageNormal);
  compartments.register(frtPractitioner, 'ProcessResponse', ['request-provider']);
end;
{$ENDIF}

{$IFDEF FHIR_PROVENANCE}
procedure TFHIRIndexBuilder.buildIndexesForProvenance(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Provenance', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Provenance', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('Provenance', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('Provenance', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('Provenance', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Provenance', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('Provenance', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('Provenance', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Provenance', 'agent', 'Who participated', SearchParamTypeREFERENCE, ['Practitioner', 'Organization', 'Device', 'Patient', 'RelatedPerson'], 'Provenance.agent.who', SearchXpathUsageNormal);
  indexes.add('Provenance', 'agent-role', 'What the agents role was', SearchParamTypeTOKEN, [], 'Provenance.agent.role', SearchXpathUsageNormal);
  indexes.add('Provenance', 'end', 'End time with inclusive boundary, if not ongoing', SearchParamTypeDATE, [], 'Provenance.period.end', SearchXpathUsageNormal);
  indexes.add('Provenance', 'entity-id', 'Identity of entity', SearchParamTypeTOKEN, [], 'Provenance.entity.what.as(Identifier)', SearchXpathUsageNormal);
  indexes.add('Provenance', 'entity-ref', 'Identity of entity', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'Provenance.entity.what.as(Reference)', SearchXpathUsageNormal);
  indexes.add('Provenance', 'location', 'Where the activity occurred, if relevant', SearchParamTypeREFERENCE, ['Location'], 'Provenance.location', SearchXpathUsageNormal);
  indexes.add('Provenance', 'patient', 'Target Reference(s) (usually version specific)', SearchParamTypeREFERENCE, ['Patient'], 'Provenance.target', SearchXpathUsageNormal);
  indexes.add('Provenance', 'recorded', 'When the activity was recorded / updated', SearchParamTypeDATE, [], 'Provenance.recorded', SearchXpathUsageNormal);
  indexes.add('Provenance', 'signature-type', 'Indication of the reason the entity signed the object(s)', SearchParamTypeTOKEN, [], 'Provenance.signature.type', SearchXpathUsageNormal);
  indexes.add('Provenance', 'start', 'Starting time with inclusive boundary', SearchParamTypeDATE, [], 'Provenance.period.start', SearchXpathUsageNormal);
  indexes.add('Provenance', 'target', 'Target Reference(s) (usually version specific)', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'Provenance.target', SearchXpathUsageNormal);
  compartments.register(frtDevice, 'Provenance', ['agent']);
  compartments.register(frtPatient, 'Provenance', ['target.subject', 'target.patient', 'patient']);
  compartments.register(frtPractitioner, 'Provenance', ['agent']);
  compartments.register(frtRelatedPerson, 'Provenance', ['agent']);
end;
{$ENDIF}

{$IFDEF FHIR_QUESTIONNAIRE}
procedure TFHIRIndexBuilder.buildIndexesForQuestionnaire(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Questionnaire', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Questionnaire', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('Questionnaire', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('Questionnaire', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('Questionnaire', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Questionnaire', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('Questionnaire', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('Questionnaire', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Questionnaire', 'code', 'A code that corresponds to one of its items in the questionnaire', SearchParamTypeTOKEN, [], 'Questionnaire.item.code', SearchXpathUsageNormal);
  indexes.add('Questionnaire', 'date', 'The questionnaire publication date', SearchParamTypeDATE, [], 'Questionnaire.date', SearchXpathUsageNormal);
  indexes.add('Questionnaire', 'description', 'The description of the questionnaire', SearchParamTypeSTRING, [], 'Questionnaire.description', SearchXpathUsageNormal);
  indexes.add('Questionnaire', 'effective', 'The time during which the questionnaire is intended to be in use', SearchParamTypeDATE, [], 'Questionnaire.effectivePeriod', SearchXpathUsageNormal);
  indexes.add('Questionnaire', 'identifier', 'External identifier for the questionnaire', SearchParamTypeTOKEN, [], 'Questionnaire.identifier', SearchXpathUsageNormal);
  indexes.add('Questionnaire', 'jurisdiction', 'Intended jurisdiction for the questionnaire', SearchParamTypeTOKEN, [], 'Questionnaire.jurisdiction', SearchXpathUsageNormal);
  indexes.add('Questionnaire', 'name', 'Computationally friendly name of the questionnaire', SearchParamTypeSTRING, [], 'Questionnaire.name', SearchXpathUsageNormal);
  indexes.add('Questionnaire', 'publisher', 'Name of the publisher of the questionnaire', SearchParamTypeSTRING, [], 'Questionnaire.publisher', SearchXpathUsageNormal);
  indexes.add('Questionnaire', 'status', 'The current status of the questionnaire', SearchParamTypeTOKEN, [], 'Questionnaire.status', SearchXpathUsageNormal);
  indexes.add('Questionnaire', 'title', 'The human-friendly name of the questionnaire', SearchParamTypeSTRING, [], 'Questionnaire.title', SearchXpathUsageNormal);
  indexes.add('Questionnaire', 'url', 'The uri that identifies the questionnaire', SearchParamTypeURI, [], 'Questionnaire.url', SearchXpathUsageNormal);
  indexes.add('Questionnaire', 'version', 'The business version of the questionnaire', SearchParamTypeTOKEN, [], 'Questionnaire.version', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_QUESTIONNAIRERESPONSE}
procedure TFHIRIndexBuilder.buildIndexesForQuestionnaireResponse(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('QuestionnaireResponse', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('QuestionnaireResponse', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('QuestionnaireResponse', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('QuestionnaireResponse', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('QuestionnaireResponse', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('QuestionnaireResponse', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('QuestionnaireResponse', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('QuestionnaireResponse', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('QuestionnaireResponse', 'author', 'The author of the questionnaire response', SearchParamTypeREFERENCE, ['Practitioner', 'Device', 'Patient', 'RelatedPerson'], 'QuestionnaireResponse.author', SearchXpathUsageNormal);
  indexes.add('QuestionnaireResponse', 'authored', 'When the questionnaire response was last changed', SearchParamTypeDATE, [], 'QuestionnaireResponse.authored', SearchXpathUsageNormal);
  indexes.add('QuestionnaireResponse', 'based-on', 'Plan/proposal/order fulfilled by this questionnaire response', SearchParamTypeREFERENCE, ['ReferralRequest', 'CarePlan', 'ProcedureRequest'], 'QuestionnaireResponse.basedOn', SearchXpathUsageNormal);
  indexes.add('QuestionnaireResponse', 'context', 'Encounter or episode associated with the questionnaire response', SearchParamTypeREFERENCE, ['EpisodeOfCare', 'Encounter'], 'QuestionnaireResponse.context', SearchXpathUsageNormal);
  indexes.add('QuestionnaireResponse', 'identifier', 'The unique identifier for the questionnaire response', SearchParamTypeTOKEN, [], 'QuestionnaireResponse.identifier', SearchXpathUsageNormal);
  indexes.add('QuestionnaireResponse', 'parent', 'Procedure or observation this questionnaire response was performed as a part of', SearchParamTypeREFERENCE, ['Observation', 'Procedure'], 'QuestionnaireResponse.parent', SearchXpathUsageNormal);
  indexes.add('QuestionnaireResponse', 'patient', 'The patient that is the subject of the questionnaire response', SearchParamTypeREFERENCE, ['Patient'], 'QuestionnaireResponse.subject', SearchXpathUsageNormal);
  indexes.add('QuestionnaireResponse', 'questionnaire', 'The questionnaire the answers are provided for', SearchParamTypeREFERENCE, ['Questionnaire'], 'QuestionnaireResponse.questionnaire', SearchXpathUsageNormal);
  indexes.add('QuestionnaireResponse', 'source', 'The individual providing the information reflected in the questionnaire respose', SearchParamTypeREFERENCE, ['Practitioner', 'Patient', 'RelatedPerson'], 'QuestionnaireResponse.source', SearchXpathUsageNormal);
  indexes.add('QuestionnaireResponse', 'status', 'The status of the questionnaire response', SearchParamTypeTOKEN, [], 'QuestionnaireResponse.status', SearchXpathUsageNormal);
  indexes.add('QuestionnaireResponse', 'subject', 'The subject of the questionnaire response', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'QuestionnaireResponse.subject', SearchXpathUsageNormal);
  compartments.register(frtDevice, 'QuestionnaireResponse', ['author']);
  compartments.register(frtEncounter, 'QuestionnaireResponse', ['context']);
  compartments.register(frtPatient, 'QuestionnaireResponse', ['subject', 'author']);
  compartments.register(frtPractitioner, 'QuestionnaireResponse', ['author', 'source']);
  compartments.register(frtRelatedPerson, 'QuestionnaireResponse', ['author', 'source']);
end;
{$ENDIF}

{$IFDEF FHIR_REFERRALREQUEST}
procedure TFHIRIndexBuilder.buildIndexesForReferralRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ReferralRequest', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('ReferralRequest', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('ReferralRequest', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('ReferralRequest', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('ReferralRequest', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ReferralRequest', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('ReferralRequest', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('ReferralRequest', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('ReferralRequest', 'authored-on', 'Creation or activation date', SearchParamTypeDATE, [], 'ReferralRequest.authoredOn', SearchXpathUsageNormal);
  indexes.add('ReferralRequest', 'based-on', 'Request being fulfilled', SearchParamTypeREFERENCE, ['ReferralRequest', 'CarePlan', 'ProcedureRequest'], 'ReferralRequest.basedOn', SearchXpathUsageNormal);
  indexes.add('ReferralRequest', 'context', 'Part of encounter or episode of care', SearchParamTypeREFERENCE, ['EpisodeOfCare', 'Encounter'], 'ReferralRequest.context', SearchXpathUsageNormal);
  indexes.add('ReferralRequest', 'definition', 'Instantiates protocol or definition', SearchParamTypeREFERENCE, ['PlanDefinition', 'ActivityDefinition'], 'ReferralRequest.definition', SearchXpathUsageNormal);
  indexes.add('ReferralRequest', 'encounter', 'Originating encounter', SearchParamTypeREFERENCE, ['Encounter'], 'ReferralRequest.context', SearchXpathUsageNormal);
  indexes.add('ReferralRequest', 'group-identifier', 'Part of common request', SearchParamTypeTOKEN, [], 'ReferralRequest.groupIdentifier', SearchXpathUsageNormal);
  indexes.add('ReferralRequest', 'identifier', 'Business identifier', SearchParamTypeTOKEN, [], 'ReferralRequest.identifier', SearchXpathUsageNormal);
  indexes.add('ReferralRequest', 'intent', 'Proposal, plan or order', SearchParamTypeTOKEN, [], 'ReferralRequest.intent', SearchXpathUsageNormal);
  indexes.add('ReferralRequest', 'occurrence-date', 'When the service(s) requested in the referral should occur', SearchParamTypeDATE, [], 'ReferralRequest.occurrence', SearchXpathUsageNormal);
  indexes.add('ReferralRequest', 'patient', 'Who the referral is about', SearchParamTypeREFERENCE, ['Group', 'Patient'], 'ReferralRequest.subject | DocumentManifest.subject | Goal.subject | Consent.patient | DocumentReference.subject | ImagingManifest.patient | RiskAssessment.subject | CareTeam.subject | ImagingStudy.patient | FamilyMemberHistory.patient | Encounter.sub'+'ject | DeviceUseStatement.subject | DeviceRequest.subject | AllergyIntolerance.patient | CarePlan.subject | EpisodeOfCare.patient | Procedure.subject | List.subject | Immunization.patient | VisionPrescription.patient | ProcedureRequest.subject | Flag'+'.subject | Observation.subject | DiagnosticReport.subject | NutritionOrder.patient | Condition.subject | Composition.subject | DetectedIssue.patient | SupplyDelivery.patient | ClinicalImpression.subject', SearchXpathUsageNormal);
  indexes.add('ReferralRequest', 'priority', 'The priority assigned to the referral', SearchParamTypeTOKEN, [], 'ReferralRequest.priority', SearchXpathUsageNormal);
  indexes.add('ReferralRequest', 'recipient', 'The person that the referral was sent to', SearchParamTypeREFERENCE, ['Practitioner', 'Organization', 'HealthcareService'], 'ReferralRequest.recipient', SearchXpathUsageNormal);
  indexes.add('ReferralRequest', 'replaces', 'Request(s) replaced by this request', SearchParamTypeREFERENCE, ['ReferralRequest'], 'ReferralRequest.replaces', SearchXpathUsageNormal);
  indexes.add('ReferralRequest', 'requester', 'Individual making the request', SearchParamTypeREFERENCE, ['Practitioner', 'Organization', 'Device', 'Patient', 'RelatedPerson'], 'ReferralRequest.requester.agent', SearchXpathUsageNormal);
  indexes.add('ReferralRequest', 'service', 'Actions requested as part of the referral', SearchParamTypeTOKEN, [], 'ReferralRequest.serviceRequested', SearchXpathUsageNormal);
  indexes.add('ReferralRequest', 'specialty', 'The specialty that the referral is for', SearchParamTypeTOKEN, [], 'ReferralRequest.specialty', SearchXpathUsageNormal);
  indexes.add('ReferralRequest', 'status', 'The status of the referral', SearchParamTypeTOKEN, [], 'ReferralRequest.status', SearchXpathUsageNormal);
  indexes.add('ReferralRequest', 'subject', 'Patient referred to care or transfer', SearchParamTypeREFERENCE, ['Group', 'Patient'], 'ReferralRequest.subject', SearchXpathUsageNormal);
  indexes.add('ReferralRequest', 'type', 'The type of the referral', SearchParamTypeTOKEN, [], 'ReferralRequest.type | DocumentManifest.type | DocumentReference.type | Encounter.type | AllergyIntolerance.type | EpisodeOfCare.type | Composition.type', SearchXpathUsageNormal);
  compartments.register(frtPatient, 'ReferralRequest', ['patient', 'requester']);
  compartments.register(frtPractitioner, 'ReferralRequest', ['requester', 'recipient']);
end;
{$ENDIF}

{$IFDEF FHIR_RELATEDPERSON}
procedure TFHIRIndexBuilder.buildIndexesForRelatedPerson(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('RelatedPerson', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('RelatedPerson', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('RelatedPerson', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('RelatedPerson', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('RelatedPerson', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('RelatedPerson', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('RelatedPerson', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('RelatedPerson', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('RelatedPerson', 'active', 'Indicates if the related person record is active', SearchParamTypeTOKEN, [], 'RelatedPerson.active', SearchXpathUsageNormal);
  indexes.add('RelatedPerson', 'address', 'A server defined search that may match any of the string fields in the Address, including line, city, state, country, postalCode, and/or text', SearchParamTypeSTRING, [], 'RelatedPerson.address | Practitioner.address | Person.address | Patient.address', SearchXpathUsageNormal);
  indexes.add('RelatedPerson', 'address-city', 'A city specified in an address', SearchParamTypeSTRING, [], 'RelatedPerson.address.city | Practitioner.address.city | Person.address.city | Patient.address.city', SearchXpathUsageNormal);
  indexes.add('RelatedPerson', 'address-country', 'A country specified in an address', SearchParamTypeSTRING, [], 'RelatedPerson.address.country | Practitioner.address.country | Person.address.country | Patient.address.country', SearchXpathUsageNormal);
  indexes.add('RelatedPerson', 'address-postalcode', 'A postal code specified in an address', SearchParamTypeSTRING, [], 'RelatedPerson.address.postalCode | Practitioner.address.postalCode | Person.address.postalCode | Patient.address.postalCode', SearchXpathUsageNormal);
  indexes.add('RelatedPerson', 'address-state', 'A state specified in an address', SearchParamTypeSTRING, [], 'RelatedPerson.address.state | Practitioner.address.state | Person.address.state | Patient.address.state', SearchXpathUsageNormal);
  indexes.add('RelatedPerson', 'address-use', 'A use code specified in an address', SearchParamTypeTOKEN, [], 'RelatedPerson.address.use | Practitioner.address.use | Person.address.use | Patient.address.use', SearchXpathUsageNormal);
  indexes.add('RelatedPerson', 'birthdate', 'The Related Person''s date of birth', SearchParamTypeDATE, [], 'RelatedPerson.birthDate | Person.birthDate | Patient.birthDate', SearchXpathUsageNormal);
  indexes.add('RelatedPerson', 'email', 'A value in an email contact', SearchParamTypeTOKEN, [], 'PractitionerRole.telecom.where(system=''email'') | RelatedPerson.telecom.where(system=''email'') | Practitioner.telecom.where(system=''email'') | Person.telecom.where(system=''email'') | Patient.telecom.where(system=''email'')', SearchXpathUsageNormal);
  indexes.add('RelatedPerson', 'gender', 'Gender of the related person', SearchParamTypeTOKEN, [], 'RelatedPerson.gender | Practitioner.gender | Person.gender | Patient.gender', SearchXpathUsageNormal);
  indexes.add('RelatedPerson', 'identifier', 'An Identifier of the RelatedPerson', SearchParamTypeTOKEN, [], 'RelatedPerson.identifier', SearchXpathUsageNormal);
  indexes.add('RelatedPerson', 'name', 'A server defined search that may match any of the string fields in the HumanName, including family, give, prefix, suffix, suffix, and/or text', SearchParamTypeSTRING, [], 'RelatedPerson.name', SearchXpathUsageNormal);
  indexes.add('RelatedPerson', 'patient', 'The patient this related person is related to', SearchParamTypeREFERENCE, ['Patient'], 'RelatedPerson.patient', SearchXpathUsageNormal);
  indexes.add('RelatedPerson', 'phone', 'A value in a phone contact', SearchParamTypeTOKEN, [], 'PractitionerRole.telecom.where(system=''phone'') | RelatedPerson.telecom.where(system=''phone'') | Practitioner.telecom.where(system=''phone'') | Person.telecom.where(system=''phone'') | Patient.telecom.where(system=''phone'')', SearchXpathUsageNormal);
  indexes.add('RelatedPerson', 'phonetic', 'A portion of name using some kind of phonetic matching algorithm', SearchParamTypeSTRING, [], 'RelatedPerson.name | Practitioner.name | Person.name | Patient.name', SearchXpathUsagePhonetic);
  indexes.add('RelatedPerson', 'telecom', 'The value in any kind of contact', SearchParamTypeTOKEN, [], 'PractitionerRole.telecom | RelatedPerson.telecom | Practitioner.telecom | Person.telecom | Patient.telecom', SearchXpathUsageNormal);
  compartments.register(frtPatient, 'RelatedPerson', ['patient']);
  compartments.register(frtRelatedPerson, 'RelatedPerson', ['{def}']);
end;
{$ENDIF}

{$IFDEF FHIR_REQUESTGROUP}
procedure TFHIRIndexBuilder.buildIndexesForRequestGroup(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('RequestGroup', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('RequestGroup', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('RequestGroup', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('RequestGroup', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('RequestGroup', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('RequestGroup', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('RequestGroup', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('RequestGroup', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('RequestGroup', 'author', 'The author of the request group', SearchParamTypeREFERENCE, ['Practitioner', 'Device'], 'RequestGroup.author', SearchXpathUsageNormal);
  indexes.add('RequestGroup', 'authored', 'The date the request group was authored', SearchParamTypeDATE, [], 'RequestGroup.authoredOn', SearchXpathUsageNormal);
  indexes.add('RequestGroup', 'context', 'The context the request group applies to', SearchParamTypeREFERENCE, ['EpisodeOfCare', 'Encounter'], 'RequestGroup.context', SearchXpathUsageNormal);
  indexes.add('RequestGroup', 'definition', 'The definition from which the request group is realized', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'RequestGroup.definition', SearchXpathUsageNormal);
  indexes.add('RequestGroup', 'encounter', 'The encounter the request group applies to', SearchParamTypeREFERENCE, ['Encounter'], 'RequestGroup.context', SearchXpathUsageNormal);
  indexes.add('RequestGroup', 'group-identifier', 'The group identifier for the request group', SearchParamTypeTOKEN, [], 'RequestGroup.groupIdentifier', SearchXpathUsageNormal);
  indexes.add('RequestGroup', 'identifier', 'External identifiers for the request group', SearchParamTypeTOKEN, [], 'RequestGroup.identifier', SearchXpathUsageNormal);
  indexes.add('RequestGroup', 'intent', 'The intent of the request group', SearchParamTypeTOKEN, [], 'RequestGroup.intent', SearchXpathUsageNormal);
  indexes.add('RequestGroup', 'participant', 'The participant in the requests in the group', SearchParamTypeREFERENCE, ['Practitioner', 'Patient', 'Person', 'RelatedPerson'], 'RequestGroup.action.participant', SearchXpathUsageNormal);
  indexes.add('RequestGroup', 'patient', 'The identity of a patient to search for request groups', SearchParamTypeREFERENCE, ['Patient'], 'RequestGroup.subject', SearchXpathUsageNormal);
  indexes.add('RequestGroup', 'priority', 'The priority of the request group', SearchParamTypeTOKEN, [], 'RequestGroup.priority', SearchXpathUsageNormal);
  indexes.add('RequestGroup', 'status', 'The status of the request group', SearchParamTypeTOKEN, [], 'RequestGroup.status', SearchXpathUsageNormal);
  indexes.add('RequestGroup', 'subject', 'The subject that the request group is about', SearchParamTypeREFERENCE, ['Group', 'Patient'], 'RequestGroup.subject', SearchXpathUsageNormal);
  compartments.register(frtDevice, 'RequestGroup', ['author']);
  compartments.register(frtEncounter, 'RequestGroup', ['encounter']);
  compartments.register(frtPatient, 'RequestGroup', ['subject', 'participant']);
  compartments.register(frtPractitioner, 'RequestGroup', ['participant', 'author']);
  compartments.register(frtRelatedPerson, 'RequestGroup', ['participant']);
end;
{$ENDIF}

{$IFDEF FHIR_RESEARCHSTUDY}
procedure TFHIRIndexBuilder.buildIndexesForResearchStudy(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ResearchStudy', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('ResearchStudy', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('ResearchStudy', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('ResearchStudy', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('ResearchStudy', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ResearchStudy', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('ResearchStudy', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('ResearchStudy', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('ResearchStudy', 'category', 'Classifications for the study', SearchParamTypeTOKEN, [], 'ResearchStudy.category', SearchXpathUsageNormal);
  indexes.add('ResearchStudy', 'date', 'When the study began and ended', SearchParamTypeDATE, [], 'ResearchStudy.period', SearchXpathUsageNormal);
  indexes.add('ResearchStudy', 'focus', 'Drugs, devices, conditions, etc. under study', SearchParamTypeTOKEN, [], 'ResearchStudy.focus', SearchXpathUsageNormal);
  indexes.add('ResearchStudy', 'identifier', 'Business Identifier for study', SearchParamTypeTOKEN, [], 'ResearchStudy.identifier', SearchXpathUsageNormal);
  indexes.add('ResearchStudy', 'jurisdiction', 'Geographic region(s) for study', SearchParamTypeTOKEN, [], 'ResearchStudy.jurisdiction', SearchXpathUsageNormal);
  indexes.add('ResearchStudy', 'keyword', 'Used to search for the study', SearchParamTypeTOKEN, [], 'ResearchStudy.keyword', SearchXpathUsageNormal);
  indexes.add('ResearchStudy', 'partof', 'Part of larger study', SearchParamTypeREFERENCE, ['ResearchStudy'], 'ResearchStudy.partOf', SearchXpathUsageNormal);
  indexes.add('ResearchStudy', 'principalinvestigator', 'The individual responsible for the study', SearchParamTypeREFERENCE, ['Practitioner'], 'ResearchStudy.principalInvestigator', SearchXpathUsageNormal);
  indexes.add('ResearchStudy', 'protocol', 'Steps followed in executing study', SearchParamTypeREFERENCE, ['PlanDefinition'], 'ResearchStudy.protocol', SearchXpathUsageNormal);
  indexes.add('ResearchStudy', 'site', 'Location involved in study execution', SearchParamTypeREFERENCE, ['Location'], 'ResearchStudy.site', SearchXpathUsageNormal);
  indexes.add('ResearchStudy', 'sponsor', 'Organization responsible for the study', SearchParamTypeREFERENCE, ['Organization'], 'ResearchStudy.sponsor', SearchXpathUsageNormal);
  indexes.add('ResearchStudy', 'status', 'draft | in-progress | suspended | stopped | completed | entered-in-error', SearchParamTypeTOKEN, [], 'ResearchStudy.status', SearchXpathUsageNormal);
  indexes.add('ResearchStudy', 'title', 'Name for this study', SearchParamTypeSTRING, [], 'ResearchStudy.title', SearchXpathUsageNormal);
  compartments.register(frtPractitioner, 'ResearchStudy', ['principalinvestigator']);
end;
{$ENDIF}

{$IFDEF FHIR_RESEARCHSUBJECT}
procedure TFHIRIndexBuilder.buildIndexesForResearchSubject(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ResearchSubject', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('ResearchSubject', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('ResearchSubject', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('ResearchSubject', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('ResearchSubject', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ResearchSubject', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('ResearchSubject', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('ResearchSubject', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('ResearchSubject', 'date', 'Start and end of participation', SearchParamTypeDATE, [], 'ResearchSubject.period', SearchXpathUsageNormal);
  indexes.add('ResearchSubject', 'identifier', 'Business Identifier for research subject', SearchParamTypeTOKEN, [], 'ResearchSubject.identifier', SearchXpathUsageNormal);
  indexes.add('ResearchSubject', 'individual', 'Who is part of study', SearchParamTypeREFERENCE, ['Patient'], 'ResearchSubject.individual', SearchXpathUsageNormal);
  indexes.add('ResearchSubject', 'patient', 'Who is part of study', SearchParamTypeREFERENCE, ['Patient'], 'ResearchSubject.individual', SearchXpathUsageNormal);
  indexes.add('ResearchSubject', 'status', 'candidate | enrolled | active | suspended | withdrawn | completed', SearchParamTypeTOKEN, [], 'ResearchSubject.status', SearchXpathUsageNormal);
  compartments.register(frtPatient, 'ResearchSubject', ['individual']);
end;
{$ENDIF}

{$IFDEF FHIR_RISKASSESSMENT}
procedure TFHIRIndexBuilder.buildIndexesForRiskAssessment(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('RiskAssessment', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('RiskAssessment', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('RiskAssessment', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('RiskAssessment', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('RiskAssessment', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('RiskAssessment', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('RiskAssessment', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('RiskAssessment', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('RiskAssessment', 'condition', 'Condition assessed', SearchParamTypeREFERENCE, ['Condition'], 'RiskAssessment.condition', SearchXpathUsageNormal);
  indexes.add('RiskAssessment', 'date', 'When was assessment made?', SearchParamTypeDATE, [], 'Consent.dateTime | SupplyRequest.authoredOn | RiskAssessment.occurrence.as(DateTime) | CareTeam.period | FamilyMemberHistory.date | Encounter.period | AllergyIntolerance.assertedDate | CarePlan.period | EpisodeOfCare.period | Procedure.performed | Li'+'st.date | Immunization.date | Flag.period | Observation.effective | DiagnosticReport.effective | Composition.date | DetectedIssue.date | ClinicalImpression.date', SearchXpathUsageNormal);
  indexes.add('RiskAssessment', 'encounter', 'Where was assessment performed?', SearchParamTypeREFERENCE, ['EpisodeOfCare', 'Encounter'], 'DocumentReference.context.encounter | RiskAssessment.context | DeviceRequest.context | Procedure.context | List.encounter | VisionPrescription.encounter | ProcedureRequest.context | Flag.encounter | Observation.context | DiagnosticReport.context | Nu'+'tritionOrder.encounter | Composition.encounter', SearchXpathUsageNormal);
  indexes.add('RiskAssessment', 'identifier', 'Unique identifier for the assessment', SearchParamTypeTOKEN, [], 'DocumentManifest.masterIdentifier | DocumentManifest.identifier | Goal.identifier | Consent.identifier | DocumentReference.masterIdentifier | DocumentReference.identifier | SupplyRequest.identifier | RiskAssessment.identifier | CareTeam.identifier | '+'ImagingStudy.identifier | FamilyMemberHistory.identifier | Encounter.identifier | DeviceRequest.identifier | AllergyIntolerance.identifier | CarePlan.identifier | EpisodeOfCare.identifier | Procedure.identifier | List.identifier | Immunization.identi'+'fier | VisionPrescription.identifier | ProcedureRequest.identifier | Observation.identifier | DiagnosticReport.identifier | NutritionOrder.identifier | Condition.identifier | Composition.identifier | DetectedIssue.identifier | SupplyDelivery.identifi'+'er', SearchXpathUsageNormal);
  indexes.add('RiskAssessment', 'method', 'Evaluation mechanism', SearchParamTypeTOKEN, [], 'RiskAssessment.method', SearchXpathUsageNormal);
  indexes.add('RiskAssessment', 'patient', 'Who/what does assessment apply to?', SearchParamTypeREFERENCE, ['Group', 'Patient'], 'ReferralRequest.subject | DocumentManifest.subject | Goal.subject | Consent.patient | DocumentReference.subject | ImagingManifest.patient | RiskAssessment.subject | CareTeam.subject | ImagingStudy.patient | FamilyMemberHistory.patient | Encounter.sub'+'ject | DeviceUseStatement.subject | DeviceRequest.subject | AllergyIntolerance.patient | CarePlan.subject | EpisodeOfCare.patient | Procedure.subject | List.subject | Immunization.patient | VisionPrescription.patient | ProcedureRequest.subject | Flag'+'.subject | Observation.subject | DiagnosticReport.subject | NutritionOrder.patient | Condition.subject | Composition.subject | DetectedIssue.patient | SupplyDelivery.patient | ClinicalImpression.subject', SearchXpathUsageNormal);
  indexes.add('RiskAssessment', 'performer', 'Who did assessment?', SearchParamTypeREFERENCE, ['Practitioner', 'Device'], 'RiskAssessment.performer', SearchXpathUsageNormal);
  indexes.add('RiskAssessment', 'probability', 'Likelihood of specified outcome', SearchParamTypeNUMBER, [], 'RiskAssessment.prediction.probability', SearchXpathUsageNormal);
  indexes.add('RiskAssessment', 'risk', 'Likelihood of specified outcome as a qualitative value', SearchParamTypeTOKEN, [], 'RiskAssessment.prediction.qualitativeRisk', SearchXpathUsageNormal);
  indexes.add('RiskAssessment', 'subject', 'Who/what does assessment apply to?', SearchParamTypeREFERENCE, ['Group', 'Patient'], 'RiskAssessment.subject', SearchXpathUsageNormal);
  compartments.register(frtDevice, 'RiskAssessment', ['performer']);
  compartments.register(frtPatient, 'RiskAssessment', ['subject']);
  compartments.register(frtPractitioner, 'RiskAssessment', ['performer']);
end;
{$ENDIF}

{$IFDEF FHIR_SCHEDULE}
procedure TFHIRIndexBuilder.buildIndexesForSchedule(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Schedule', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Schedule', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('Schedule', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('Schedule', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('Schedule', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Schedule', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('Schedule', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('Schedule', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Schedule', 'active', 'Is the schedule in active use', SearchParamTypeTOKEN, [], 'Schedule.active', SearchXpathUsageNormal);
  indexes.add('Schedule', 'actor', 'The individual(HealthcareService, Practitioner, Location, ...) to find a Schedule for', SearchParamTypeREFERENCE, ['Practitioner', 'Device', 'Patient', 'HealthcareService', 'PractitionerRole', 'RelatedPerson', 'Location'], 'Schedule.actor', SearchXpathUsageNormal);
  indexes.add('Schedule', 'date', 'Search for Schedule resources that have a period that contains this date specified', SearchParamTypeDATE, [], 'Schedule.planningHorizon', SearchXpathUsageNormal);
  indexes.add('Schedule', 'identifier', 'A Schedule Identifier', SearchParamTypeTOKEN, [], 'Schedule.identifier', SearchXpathUsageNormal);
  indexes.add('Schedule', 'type', 'The type of appointments that can be booked into associated slot(s)', SearchParamTypeTOKEN, [], 'Schedule.serviceType', SearchXpathUsageNormal);
  compartments.register(frtDevice, 'Schedule', ['actor']);
  compartments.register(frtPatient, 'Schedule', ['actor']);
  compartments.register(frtPractitioner, 'Schedule', ['actor']);
  compartments.register(frtRelatedPerson, 'Schedule', ['actor']);
end;
{$ENDIF}

{$IFDEF FHIR_SEARCHPARAMETER}
procedure TFHIRIndexBuilder.buildIndexesForSearchParameter(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('SearchParameter', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('SearchParameter', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('SearchParameter', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('SearchParameter', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('SearchParameter', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('SearchParameter', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('SearchParameter', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('SearchParameter', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('SearchParameter', 'base', 'The resource type(s) this search parameter applies to', SearchParamTypeTOKEN, [], 'SearchParameter.base', SearchXpathUsageNormal);
  indexes.add('SearchParameter', 'code', 'Code used in URL', SearchParamTypeTOKEN, [], 'SearchParameter.code', SearchXpathUsageNormal);
  indexes.add('SearchParameter', 'component', 'Defines how the part works', SearchParamTypeREFERENCE, ['SearchParameter'], 'SearchParameter.component.definition', SearchXpathUsageNormal);
  indexes.add('SearchParameter', 'date', 'The search parameter publication date', SearchParamTypeDATE, [], 'SearchParameter.date', SearchXpathUsageNormal);
  indexes.add('SearchParameter', 'derived-from', 'Original Definition for the search parameter', SearchParamTypeURI, [], 'SearchParameter.derivedFrom', SearchXpathUsageNormal);
  indexes.add('SearchParameter', 'description', 'The description of the search parameter', SearchParamTypeSTRING, [], 'SearchParameter.description', SearchXpathUsageNormal);
  indexes.add('SearchParameter', 'jurisdiction', 'Intended jurisdiction for the search parameter', SearchParamTypeTOKEN, [], 'SearchParameter.jurisdiction', SearchXpathUsageNormal);
  indexes.add('SearchParameter', 'name', 'Computationally friendly name of the search parameter', SearchParamTypeSTRING, [], 'SearchParameter.name', SearchXpathUsageNormal);
  indexes.add('SearchParameter', 'publisher', 'Name of the publisher of the search parameter', SearchParamTypeSTRING, [], 'SearchParameter.publisher', SearchXpathUsageNormal);
  indexes.add('SearchParameter', 'status', 'The current status of the search parameter', SearchParamTypeTOKEN, [], 'SearchParameter.status', SearchXpathUsageNormal);
  indexes.add('SearchParameter', 'target', 'Types of resource (if a resource reference)', SearchParamTypeTOKEN, [], 'SearchParameter.target', SearchXpathUsageNormal);
  indexes.add('SearchParameter', 'type', 'number | date | string | token | reference | composite | quantity | uri', SearchParamTypeTOKEN, [], 'SearchParameter.type', SearchXpathUsageNormal);
  indexes.add('SearchParameter', 'url', 'The uri that identifies the search parameter', SearchParamTypeURI, [], 'SearchParameter.url', SearchXpathUsageNormal);
  indexes.add('SearchParameter', 'version', 'The business version of the search parameter', SearchParamTypeTOKEN, [], 'SearchParameter.version', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_SEQUENCE}
procedure TFHIRIndexBuilder.buildIndexesForSequence(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Sequence', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Sequence', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('Sequence', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('Sequence', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('Sequence', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Sequence', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('Sequence', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('Sequence', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Sequence', 'chromosome', 'Chromosome number of the reference sequence', SearchParamTypeTOKEN, [], 'Sequence.referenceSeq.chromosome', SearchXpathUsageNormal);
  indexes.add('Sequence', 'coordinate', 'Search parameter for region of the reference DNA sequence string. This will refer to part of a locus or part of a gene where search region will be represented in 1-based system. Since the coordinateSystem can either be 0-based or 1-based, this search'+' query will include the result of both coordinateSystem that contains the equivalent segment of the gene or whole genome sequence. For example, a search for sequence can be represented as `coordinate=1$lt345$gt123`, this means it will search for the '+'Sequence resource on chromosome 1 and with position >123 and <345, where in 1-based system resource, all strings within region 1:124-344 will be revealed, while in 0-based system resource, all strings within region 1:123-344 will be revealed. You may'+' want to check detail about 0-based v.s. 1-based above.', SearchParamTypeCOMPOSITE, [], 'Sequence.variant', SearchXpathUsageNormal);
  indexes.add('Sequence', 'end', 'End position (0-based exclusive, which menas the acid at this position will not be included, 1-based inclusive, which means the acid at this position will be included) of the reference sequence.', SearchParamTypeNUMBER, [], 'Sequence.referenceSeq.windowEnd', SearchXpathUsageNormal);
  indexes.add('Sequence', 'identifier', 'The unique identity for a particular sequence', SearchParamTypeTOKEN, [], 'Sequence.identifier', SearchXpathUsageNormal);
  indexes.add('Sequence', 'patient', 'The subject that the observation is about', SearchParamTypeREFERENCE, ['Patient'], 'Sequence.patient', SearchXpathUsageNormal);
  indexes.add('Sequence', 'start', 'Start position (0-based inclusive, 1-based inclusive, that means the nucleic acid or amino acid at this position will be included) of the reference sequence.', SearchParamTypeNUMBER, [], 'Sequence.referenceSeq.windowStart', SearchXpathUsageNormal);
  indexes.add('Sequence', 'type', 'Amino Acid Sequence/ DNA Sequence / RNA Sequence', SearchParamTypeTOKEN, [], 'Sequence.type', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_SERVICEDEFINITION}
procedure TFHIRIndexBuilder.buildIndexesForServiceDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ServiceDefinition', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('ServiceDefinition', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('ServiceDefinition', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('ServiceDefinition', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('ServiceDefinition', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ServiceDefinition', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('ServiceDefinition', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('ServiceDefinition', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('ServiceDefinition', 'composed-of', 'What resource is being referenced', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'ServiceDefinition.relatedArtifact.where(type=''composed-of'').resource', SearchXpathUsageNormal);
  indexes.add('ServiceDefinition', 'date', 'The service definition publication date', SearchParamTypeDATE, [], 'ServiceDefinition.date', SearchXpathUsageNormal);
  indexes.add('ServiceDefinition', 'depends-on', 'What resource is being referenced', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'ServiceDefinition.relatedArtifact.where(type=''depends-on'').resource', SearchXpathUsageNormal);
  indexes.add('ServiceDefinition', 'derived-from', 'What resource is being referenced', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'ServiceDefinition.relatedArtifact.where(type=''derived-from'').resource', SearchXpathUsageNormal);
  indexes.add('ServiceDefinition', 'description', 'The description of the service definition', SearchParamTypeSTRING, [], 'ServiceDefinition.description', SearchXpathUsageNormal);
  indexes.add('ServiceDefinition', 'effective', 'The time during which the service definition is intended to be in use', SearchParamTypeDATE, [], 'ServiceDefinition.effectivePeriod', SearchXpathUsageNormal);
  indexes.add('ServiceDefinition', 'identifier', 'External identifier for the service definition', SearchParamTypeTOKEN, [], 'ServiceDefinition.identifier', SearchXpathUsageNormal);
  indexes.add('ServiceDefinition', 'jurisdiction', 'Intended jurisdiction for the service definition', SearchParamTypeTOKEN, [], 'ServiceDefinition.jurisdiction', SearchXpathUsageNormal);
  indexes.add('ServiceDefinition', 'name', 'Computationally friendly name of the service definition', SearchParamTypeSTRING, [], 'ServiceDefinition.name', SearchXpathUsageNormal);
  indexes.add('ServiceDefinition', 'predecessor', 'What resource is being referenced', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'ServiceDefinition.relatedArtifact.where(type=''predecessor'').resource', SearchXpathUsageNormal);
  indexes.add('ServiceDefinition', 'publisher', 'Name of the publisher of the service definition', SearchParamTypeSTRING, [], 'ServiceDefinition.publisher', SearchXpathUsageNormal);
  indexes.add('ServiceDefinition', 'status', 'The current status of the service definition', SearchParamTypeTOKEN, [], 'ServiceDefinition.status', SearchXpathUsageNormal);
  indexes.add('ServiceDefinition', 'successor', 'What resource is being referenced', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'ServiceDefinition.relatedArtifact.where(type=''successor'').resource', SearchXpathUsageNormal);
  indexes.add('ServiceDefinition', 'title', 'The human-friendly name of the service definition', SearchParamTypeSTRING, [], 'ServiceDefinition.title', SearchXpathUsageNormal);
  indexes.add('ServiceDefinition', 'topic', 'Topics associated with the module', SearchParamTypeTOKEN, [], 'ServiceDefinition.topic', SearchXpathUsageNormal);
  indexes.add('ServiceDefinition', 'url', 'The uri that identifies the service definition', SearchParamTypeURI, [], 'ServiceDefinition.url', SearchXpathUsageNormal);
  indexes.add('ServiceDefinition', 'version', 'The business version of the service definition', SearchParamTypeTOKEN, [], 'ServiceDefinition.version', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_SLOT}
procedure TFHIRIndexBuilder.buildIndexesForSlot(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Slot', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Slot', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('Slot', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('Slot', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('Slot', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Slot', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('Slot', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('Slot', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Slot', 'identifier', 'A Slot Identifier', SearchParamTypeTOKEN, [], 'Slot.identifier', SearchXpathUsageNormal);
  indexes.add('Slot', 'schedule', 'The Schedule Resource that we are seeking a slot within', SearchParamTypeREFERENCE, ['Schedule'], 'Slot.schedule', SearchXpathUsageNormal);
  indexes.add('Slot', 'slot-type', 'The type of appointments that can be booked into the slot', SearchParamTypeTOKEN, [], 'Slot.serviceType', SearchXpathUsageNormal);
  indexes.add('Slot', 'start', 'Appointment date/time.', SearchParamTypeDATE, [], 'Slot.start', SearchXpathUsageNormal);
  indexes.add('Slot', 'status', 'The free/busy status of the appointment', SearchParamTypeTOKEN, [], 'Slot.status', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_SPECIMEN}
procedure TFHIRIndexBuilder.buildIndexesForSpecimen(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Specimen', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Specimen', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('Specimen', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('Specimen', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('Specimen', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Specimen', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('Specimen', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('Specimen', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Specimen', 'accession', 'The accession number associated with the specimen', SearchParamTypeTOKEN, [], 'Specimen.accessionIdentifier', SearchXpathUsageNormal);
  indexes.add('Specimen', 'bodysite', 'The code for the body site from where the specimen originated', SearchParamTypeTOKEN, [], 'Specimen.collection.bodySite', SearchXpathUsageNormal);
  indexes.add('Specimen', 'collected', 'The date the specimen was collected', SearchParamTypeDATE, [], 'Specimen.collection.collected', SearchXpathUsageNormal);
  indexes.add('Specimen', 'collector', 'Who collected the specimen', SearchParamTypeREFERENCE, ['Practitioner'], 'Specimen.collection.collector', SearchXpathUsageNormal);
  indexes.add('Specimen', 'container', 'The kind of specimen container', SearchParamTypeTOKEN, [], 'Specimen.container.type', SearchXpathUsageNormal);
  indexes.add('Specimen', 'container-id', 'The unique identifier associated with the specimen container', SearchParamTypeTOKEN, [], 'Specimen.container.identifier', SearchXpathUsageNormal);
  indexes.add('Specimen', 'identifier', 'The unique identifier associated with the specimen', SearchParamTypeTOKEN, [], 'Specimen.identifier', SearchXpathUsageNormal);
  indexes.add('Specimen', 'parent', 'The parent of the specimen', SearchParamTypeREFERENCE, ['Specimen'], 'Specimen.parent', SearchXpathUsageNormal);
  indexes.add('Specimen', 'patient', 'The patient the specimen comes from', SearchParamTypeREFERENCE, ['Patient'], 'Specimen.subject', SearchXpathUsageNormal);
  indexes.add('Specimen', 'status', 'available | unavailable | unsatisfactory | entered-in-error', SearchParamTypeTOKEN, [], 'Specimen.status', SearchXpathUsageNormal);
  indexes.add('Specimen', 'subject', 'The subject of the specimen', SearchParamTypeREFERENCE, ['Group', 'Device', 'Patient', 'Substance'], 'Specimen.subject', SearchXpathUsageNormal);
  indexes.add('Specimen', 'type', 'The specimen type', SearchParamTypeTOKEN, [], 'Specimen.type', SearchXpathUsageNormal);
  compartments.register(frtDevice, 'Specimen', ['subject']);
  compartments.register(frtPatient, 'Specimen', ['subject']);
  compartments.register(frtPractitioner, 'Specimen', ['collector']);
end;
{$ENDIF}

{$IFDEF FHIR_STRUCTUREDEFINITION}
procedure TFHIRIndexBuilder.buildIndexesForStructureDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('StructureDefinition', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('StructureDefinition', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('StructureDefinition', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('StructureDefinition', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('StructureDefinition', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('StructureDefinition', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('StructureDefinition', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('StructureDefinition', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('StructureDefinition', 'abstract', 'Whether the structure is abstract', SearchParamTypeTOKEN, [], 'StructureDefinition.abstract', SearchXpathUsageNormal);
  indexes.add('StructureDefinition', 'base', 'Definition that this type is constrained/specialized from', SearchParamTypeURI, [], 'StructureDefinition.baseDefinition', SearchXpathUsageNormal);
  indexes.add('StructureDefinition', 'base-path', 'Path that identifies the base element', SearchParamTypeTOKEN, [], 'StructureDefinition.snapshot.element.base.path | StructureDefinition.differential.element.base.path', SearchXpathUsageNormal);
  indexes.add('StructureDefinition', 'context-type', 'resource | datatype | extension', SearchParamTypeTOKEN, [], 'StructureDefinition.contextType', SearchXpathUsageNormal);
  indexes.add('StructureDefinition', 'date', 'The structure definition publication date', SearchParamTypeDATE, [], 'StructureDefinition.date', SearchXpathUsageNormal);
  indexes.add('StructureDefinition', 'derivation', 'specialization | constraint - How relates to base definition', SearchParamTypeTOKEN, [], 'StructureDefinition.derivation', SearchXpathUsageNormal);
  indexes.add('StructureDefinition', 'description', 'The description of the structure definition', SearchParamTypeSTRING, [], 'StructureDefinition.description', SearchXpathUsageNormal);
  indexes.add('StructureDefinition', 'experimental', 'For testing purposes, not real usage', SearchParamTypeTOKEN, [], 'StructureDefinition.experimental', SearchXpathUsageNormal);
  indexes.add('StructureDefinition', 'ext-context', 'Where the extension can be used in instances', SearchParamTypeSTRING, [], 'StructureDefinition.context', SearchXpathUsageNormal);
  indexes.add('StructureDefinition', 'identifier', 'External identifier for the structure definition', SearchParamTypeTOKEN, [], 'StructureDefinition.identifier', SearchXpathUsageNormal);
  indexes.add('StructureDefinition', 'jurisdiction', 'Intended jurisdiction for the structure definition', SearchParamTypeTOKEN, [], 'StructureDefinition.jurisdiction', SearchXpathUsageNormal);
  indexes.add('StructureDefinition', 'keyword', 'A code for the profile', SearchParamTypeTOKEN, [], 'StructureDefinition.keyword', SearchXpathUsageNormal);
  indexes.add('StructureDefinition', 'kind', 'primitive-type | complex-type | resource | logical', SearchParamTypeTOKEN, [], 'StructureDefinition.kind', SearchXpathUsageNormal);
  indexes.add('StructureDefinition', 'name', 'Computationally friendly name of the structure definition', SearchParamTypeSTRING, [], 'StructureDefinition.name', SearchXpathUsageNormal);
  indexes.add('StructureDefinition', 'path', 'A path that is constrained in the profile', SearchParamTypeTOKEN, [], 'StructureDefinition.snapshot.element.path | StructureDefinition.differential.element.path', SearchXpathUsageNormal);
  indexes.add('StructureDefinition', 'publisher', 'Name of the publisher of the structure definition', SearchParamTypeSTRING, [], 'StructureDefinition.publisher', SearchXpathUsageNormal);
  indexes.add('StructureDefinition', 'status', 'The current status of the structure definition', SearchParamTypeTOKEN, [], 'StructureDefinition.status', SearchXpathUsageNormal);
  indexes.add('StructureDefinition', 'title', 'The human-friendly name of the structure definition', SearchParamTypeSTRING, [], 'StructureDefinition.title', SearchXpathUsageNormal);
  indexes.add('StructureDefinition', 'type', 'Type defined or constrained by this structure', SearchParamTypeTOKEN, [], 'StructureDefinition.type', SearchXpathUsageNormal);
  indexes.add('StructureDefinition', 'url', 'The uri that identifies the structure definition', SearchParamTypeURI, [], 'StructureDefinition.url', SearchXpathUsageNormal);
  indexes.add('StructureDefinition', 'valueset', 'A vocabulary binding reference', SearchParamTypeREFERENCE, ['ValueSet'], 'StructureDefinition.snapshot.element.binding.valueSet', SearchXpathUsageNormal);
  indexes.add('StructureDefinition', 'version', 'The business version of the structure definition', SearchParamTypeTOKEN, [], 'StructureDefinition.version', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_STRUCTUREMAP}
procedure TFHIRIndexBuilder.buildIndexesForStructureMap(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('StructureMap', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('StructureMap', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('StructureMap', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('StructureMap', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('StructureMap', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('StructureMap', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('StructureMap', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('StructureMap', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('StructureMap', 'date', 'The structure map publication date', SearchParamTypeDATE, [], 'StructureMap.date', SearchXpathUsageNormal);
  indexes.add('StructureMap', 'description', 'The description of the structure map', SearchParamTypeSTRING, [], 'StructureMap.description', SearchXpathUsageNormal);
  indexes.add('StructureMap', 'identifier', 'External identifier for the structure map', SearchParamTypeTOKEN, [], 'StructureMap.identifier', SearchXpathUsageNormal);
  indexes.add('StructureMap', 'jurisdiction', 'Intended jurisdiction for the structure map', SearchParamTypeTOKEN, [], 'StructureMap.jurisdiction', SearchXpathUsageNormal);
  indexes.add('StructureMap', 'name', 'Computationally friendly name of the structure map', SearchParamTypeSTRING, [], 'StructureMap.name', SearchXpathUsageNormal);
  indexes.add('StructureMap', 'publisher', 'Name of the publisher of the structure map', SearchParamTypeSTRING, [], 'StructureMap.publisher', SearchXpathUsageNormal);
  indexes.add('StructureMap', 'status', 'The current status of the structure map', SearchParamTypeTOKEN, [], 'StructureMap.status', SearchXpathUsageNormal);
  indexes.add('StructureMap', 'title', 'The human-friendly name of the structure map', SearchParamTypeSTRING, [], 'StructureMap.title', SearchXpathUsageNormal);
  indexes.add('StructureMap', 'url', 'The uri that identifies the structure map', SearchParamTypeURI, [], 'StructureMap.url', SearchXpathUsageNormal);
  indexes.add('StructureMap', 'version', 'The business version of the structure map', SearchParamTypeTOKEN, [], 'StructureMap.version', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_SUBSCRIPTION}
procedure TFHIRIndexBuilder.buildIndexesForSubscription(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Subscription', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Subscription', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('Subscription', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('Subscription', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('Subscription', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Subscription', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('Subscription', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('Subscription', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Subscription', 'add-tag', 'A tag to be added to the resource matching the criteria', SearchParamTypeTOKEN, [], 'Subscription.tag', SearchXpathUsageNormal);
  indexes.add('Subscription', 'contact', 'Contact details for the subscription', SearchParamTypeTOKEN, [], 'Subscription.contact', SearchXpathUsageNormal);
  indexes.add('Subscription', 'criteria', 'The search rules used to determine when to send a notification', SearchParamTypeSTRING, [], 'Subscription.criteria', SearchXpathUsageNormal);
  indexes.add('Subscription', 'payload', 'The mime-type of the notification payload', SearchParamTypeSTRING, [], 'Subscription.channel.payload', SearchXpathUsageNormal);
  indexes.add('Subscription', 'status', 'The current state of the subscription', SearchParamTypeTOKEN, [], 'Subscription.status', SearchXpathUsageNormal);
  indexes.add('Subscription', 'type', 'The type of channel for the sent notifications', SearchParamTypeTOKEN, [], 'Subscription.channel.type', SearchXpathUsageNormal);
  indexes.add('Subscription', 'url', 'The uri that will receive the notifications', SearchParamTypeURI, [], 'Subscription.channel.endpoint', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_SUBSTANCE}
procedure TFHIRIndexBuilder.buildIndexesForSubstance(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Substance', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Substance', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('Substance', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('Substance', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('Substance', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Substance', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('Substance', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('Substance', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Substance', 'category', 'The category of the substance', SearchParamTypeTOKEN, [], 'Substance.category', SearchXpathUsageNormal);
  indexes.add('Substance', 'code', 'The code of the substance or ingredient', SearchParamTypeTOKEN, [], 'Substance.code | Substance.ingredient.substance.as(CodeableConcept)', SearchXpathUsageNormal);
  indexes.add('Substance', 'container-identifier', 'Identifier of the package/container', SearchParamTypeTOKEN, [], 'Substance.instance.identifier', SearchXpathUsageNormal);
  indexes.add('Substance', 'expiry', 'Expiry date of package or container of substance', SearchParamTypeDATE, [], 'Substance.instance.expiry', SearchXpathUsageNormal);
  indexes.add('Substance', 'identifier', 'Unique identifier for the substance', SearchParamTypeTOKEN, [], 'Substance.identifier', SearchXpathUsageNormal);
  indexes.add('Substance', 'quantity', 'Amount of substance in the package', SearchParamTypeQUANTITY, [], 'Substance.instance.quantity', SearchXpathUsageNormal);
  indexes.add('Substance', 'status', 'active | inactive | entered-in-error', SearchParamTypeTOKEN, [], 'Substance.status', SearchXpathUsageNormal);
  indexes.add('Substance', 'substance-reference', 'A component of the substance', SearchParamTypeREFERENCE, ['Substance'], 'Substance.ingredient.substance.as(Reference)', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_SUPPLYDELIVERY}
procedure TFHIRIndexBuilder.buildIndexesForSupplyDelivery(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('SupplyDelivery', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('SupplyDelivery', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('SupplyDelivery', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('SupplyDelivery', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('SupplyDelivery', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('SupplyDelivery', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('SupplyDelivery', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('SupplyDelivery', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('SupplyDelivery', 'identifier', 'External identifier', SearchParamTypeTOKEN, [], 'DocumentManifest.masterIdentifier | DocumentManifest.identifier | Goal.identifier | Consent.identifier | DocumentReference.masterIdentifier | DocumentReference.identifier | SupplyRequest.identifier | RiskAssessment.identifier | CareTeam.identifier | '+'ImagingStudy.identifier | FamilyMemberHistory.identifier | Encounter.identifier | DeviceRequest.identifier | AllergyIntolerance.identifier | CarePlan.identifier | EpisodeOfCare.identifier | Procedure.identifier | List.identifier | Immunization.identi'+'fier | VisionPrescription.identifier | ProcedureRequest.identifier | Observation.identifier | DiagnosticReport.identifier | NutritionOrder.identifier | Condition.identifier | Composition.identifier | DetectedIssue.identifier | SupplyDelivery.identifi'+'er', SearchXpathUsageNormal);
  indexes.add('SupplyDelivery', 'patient', 'Patient for whom the item is supplied', SearchParamTypeREFERENCE, ['Group', 'Patient'], 'ReferralRequest.subject | DocumentManifest.subject | Goal.subject | Consent.patient | DocumentReference.subject | ImagingManifest.patient | RiskAssessment.subject | CareTeam.subject | ImagingStudy.patient | FamilyMemberHistory.patient | Encounter.sub'+'ject | DeviceUseStatement.subject | DeviceRequest.subject | AllergyIntolerance.patient | CarePlan.subject | EpisodeOfCare.patient | Procedure.subject | List.subject | Immunization.patient | VisionPrescription.patient | ProcedureRequest.subject | Flag'+'.subject | Observation.subject | DiagnosticReport.subject | NutritionOrder.patient | Condition.subject | Composition.subject | DetectedIssue.patient | SupplyDelivery.patient | ClinicalImpression.subject', SearchXpathUsageNormal);
  indexes.add('SupplyDelivery', 'receiver', 'Who collected the Supply', SearchParamTypeREFERENCE, ['Practitioner'], 'SupplyDelivery.receiver', SearchXpathUsageNormal);
  indexes.add('SupplyDelivery', 'status', 'in-progress | completed | abandoned | entered-in-error', SearchParamTypeTOKEN, [], 'SupplyDelivery.status', SearchXpathUsageNormal);
  indexes.add('SupplyDelivery', 'supplier', 'Dispenser', SearchParamTypeREFERENCE, ['Practitioner', 'Organization'], 'SupplyDelivery.supplier', SearchXpathUsageNormal);
  compartments.register(frtPatient, 'SupplyDelivery', ['patient']);
  compartments.register(frtPractitioner, 'SupplyDelivery', ['supplier', 'receiver']);
end;
{$ENDIF}

{$IFDEF FHIR_SUPPLYREQUEST}
procedure TFHIRIndexBuilder.buildIndexesForSupplyRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('SupplyRequest', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('SupplyRequest', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('SupplyRequest', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('SupplyRequest', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('SupplyRequest', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('SupplyRequest', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('SupplyRequest', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('SupplyRequest', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('SupplyRequest', 'category', 'The kind of supply (central, non-stock, etc.)', SearchParamTypeTOKEN, [], 'SupplyRequest.category', SearchXpathUsageNormal);
  indexes.add('SupplyRequest', 'date', 'When the request was made', SearchParamTypeDATE, [], 'Consent.dateTime | SupplyRequest.authoredOn | RiskAssessment.occurrence.as(DateTime) | CareTeam.period | FamilyMemberHistory.date | Encounter.period | AllergyIntolerance.assertedDate | CarePlan.period | EpisodeOfCare.period | Procedure.performed | Li'+'st.date | Immunization.date | Flag.period | Observation.effective | DiagnosticReport.effective | Composition.date | DetectedIssue.date | ClinicalImpression.date', SearchXpathUsageNormal);
  indexes.add('SupplyRequest', 'identifier', 'Unique identifier', SearchParamTypeTOKEN, [], 'DocumentManifest.masterIdentifier | DocumentManifest.identifier | Goal.identifier | Consent.identifier | DocumentReference.masterIdentifier | DocumentReference.identifier | SupplyRequest.identifier | RiskAssessment.identifier | CareTeam.identifier | '+'ImagingStudy.identifier | FamilyMemberHistory.identifier | Encounter.identifier | DeviceRequest.identifier | AllergyIntolerance.identifier | CarePlan.identifier | EpisodeOfCare.identifier | Procedure.identifier | List.identifier | Immunization.identi'+'fier | VisionPrescription.identifier | ProcedureRequest.identifier | Observation.identifier | DiagnosticReport.identifier | NutritionOrder.identifier | Condition.identifier | Composition.identifier | DetectedIssue.identifier | SupplyDelivery.identifi'+'er', SearchXpathUsageNormal);
  indexes.add('SupplyRequest', 'requester', 'Individual making the request', SearchParamTypeREFERENCE, ['Practitioner', 'Organization', 'Device', 'Patient', 'RelatedPerson'], 'SupplyRequest.requester.agent', SearchXpathUsageNormal);
  indexes.add('SupplyRequest', 'status', 'draft | active | suspended +', SearchParamTypeTOKEN, [], 'SupplyRequest.status', SearchXpathUsageNormal);
  indexes.add('SupplyRequest', 'supplier', 'Who is intended to fulfill the request', SearchParamTypeREFERENCE, ['Organization'], 'SupplyRequest.supplier', SearchXpathUsageNormal);
  compartments.register(frtDevice, 'SupplyRequest', ['requester']);
  compartments.register(frtPatient, 'SupplyRequest', ['requester']);
  compartments.register(frtPractitioner, 'SupplyRequest', ['requester']);
  compartments.register(frtRelatedPerson, 'SupplyRequest', ['requester']);
end;
{$ENDIF}

{$IFDEF FHIR_TASK}
procedure TFHIRIndexBuilder.buildIndexesForTask(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Task', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Task', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('Task', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('Task', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('Task', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Task', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('Task', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('Task', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Task', 'authored-on', 'Search by creation date', SearchParamTypeDATE, [], 'Task.authoredOn', SearchXpathUsageNormal);
  indexes.add('Task', 'based-on', 'Search by requests this task is based on', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'Task.basedOn', SearchXpathUsageNormal);
  indexes.add('Task', 'business-status', 'Search by business status', SearchParamTypeTOKEN, [], 'Task.businessStatus', SearchXpathUsageNormal);
  indexes.add('Task', 'code', 'Search by task code', SearchParamTypeTOKEN, [], 'Task.code', SearchXpathUsageNormal);
  indexes.add('Task', 'context', 'Search by encounter or episode', SearchParamTypeREFERENCE, ['EpisodeOfCare', 'Encounter'], 'Task.context', SearchXpathUsageNormal);
  indexes.add('Task', 'focus', 'Search by task focus', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'Task.focus', SearchXpathUsageNormal);
  indexes.add('Task', 'group-identifier', 'Search by group identifier', SearchParamTypeTOKEN, [], 'Task.groupIdentifier', SearchXpathUsageNormal);
  indexes.add('Task', 'identifier', 'Search for a task instance by its business identifier', SearchParamTypeTOKEN, [], 'Task.identifier', SearchXpathUsageNormal);
  indexes.add('Task', 'intent', 'Search by task intent', SearchParamTypeTOKEN, [], 'Task.intent', SearchXpathUsageNormal);
  indexes.add('Task', 'modified', 'Search by last modification date', SearchParamTypeDATE, [], 'Task.lastModified', SearchXpathUsageNormal);
  indexes.add('Task', 'organization', 'Search by responsible organization', SearchParamTypeREFERENCE, ['Organization'], 'Task.requester.onBehalfOf', SearchXpathUsageNormal);
  indexes.add('Task', 'owner', 'Search by task owner', SearchParamTypeREFERENCE, ['Practitioner', 'Organization', 'Device', 'Patient', 'RelatedPerson'], 'Task.owner', SearchXpathUsageNormal);
  indexes.add('Task', 'part-of', 'Search by task this task is part of', SearchParamTypeREFERENCE, ['Task'], 'Task.partOf', SearchXpathUsageNormal);
  indexes.add('Task', 'patient', 'Search by patient', SearchParamTypeREFERENCE, ['Patient'], 'Task.for', SearchXpathUsageNormal);
  indexes.add('Task', 'performer', 'Search by recommended type of performer (e.g., Requester, Performer, Scheduler).', SearchParamTypeTOKEN, [], 'Task.performerType', SearchXpathUsageNormal);
  indexes.add('Task', 'period', 'Search by period Task is/was underway', SearchParamTypeDATE, [], 'Task.executionPeriod', SearchXpathUsageNormal);
  indexes.add('Task', 'priority', 'Search by task priority', SearchParamTypeTOKEN, [], 'Task.priority', SearchXpathUsageNormal);
  indexes.add('Task', 'requester', 'Search by task requester', SearchParamTypeREFERENCE, ['Practitioner', 'Organization', 'Device', 'Patient', 'RelatedPerson'], 'Task.requester.agent', SearchXpathUsageNormal);
  indexes.add('Task', 'status', 'Search by task status', SearchParamTypeTOKEN, [], 'Task.status', SearchXpathUsageNormal);
  indexes.add('Task', 'subject', 'Search by subject', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'Task.for', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_TESTREPORT}
procedure TFHIRIndexBuilder.buildIndexesForTestReport(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('TestReport', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('TestReport', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('TestReport', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('TestReport', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('TestReport', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('TestReport', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('TestReport', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('TestReport', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('TestReport', 'identifier', 'An external identifier for the test report', SearchParamTypeTOKEN, [], 'TestReport.identifier', SearchXpathUsageNormal);
  indexes.add('TestReport', 'issued', 'The test report generation date', SearchParamTypeDATE, [], 'TestReport.issued', SearchXpathUsageNormal);
  indexes.add('TestReport', 'participant', 'The reference to a participant in the test execution', SearchParamTypeURI, [], 'TestReport.participant.uri', SearchXpathUsageNormal);
  indexes.add('TestReport', 'result', 'The result disposition of the test execution', SearchParamTypeTOKEN, [], 'TestReport.result', SearchXpathUsageNormal);
  indexes.add('TestReport', 'tester', 'The name of the testing organization', SearchParamTypeSTRING, [], 'TestReport.tester', SearchXpathUsageNormal);
  indexes.add('TestReport', 'testscript', 'The test script executed to produce this report', SearchParamTypeREFERENCE, ['TestScript'], 'TestReport.testScript', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_TESTSCRIPT}
procedure TFHIRIndexBuilder.buildIndexesForTestScript(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('TestScript', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('TestScript', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('TestScript', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('TestScript', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('TestScript', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('TestScript', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('TestScript', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('TestScript', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('TestScript', 'date', 'The test script publication date', SearchParamTypeDATE, [], 'TestScript.date', SearchXpathUsageNormal);
  indexes.add('TestScript', 'description', 'The description of the test script', SearchParamTypeSTRING, [], 'TestScript.description', SearchXpathUsageNormal);
  indexes.add('TestScript', 'identifier', 'External identifier for the test script', SearchParamTypeTOKEN, [], 'TestScript.identifier', SearchXpathUsageNormal);
  indexes.add('TestScript', 'jurisdiction', 'Intended jurisdiction for the test script', SearchParamTypeTOKEN, [], 'TestScript.jurisdiction', SearchXpathUsageNormal);
  indexes.add('TestScript', 'name', 'Computationally friendly name of the test script', SearchParamTypeSTRING, [], 'TestScript.name', SearchXpathUsageNormal);
  indexes.add('TestScript', 'publisher', 'Name of the publisher of the test script', SearchParamTypeSTRING, [], 'TestScript.publisher', SearchXpathUsageNormal);
  indexes.add('TestScript', 'status', 'The current status of the test script', SearchParamTypeTOKEN, [], 'TestScript.status', SearchXpathUsageNormal);
  indexes.add('TestScript', 'testscript-capability', 'TestScript required and validated capability', SearchParamTypeSTRING, [], 'TestScript.metadata.capability.description', SearchXpathUsageNormal);
  indexes.add('TestScript', 'title', 'The human-friendly name of the test script', SearchParamTypeSTRING, [], 'TestScript.title', SearchXpathUsageNormal);
  indexes.add('TestScript', 'url', 'The uri that identifies the test script', SearchParamTypeURI, [], 'TestScript.url', SearchXpathUsageNormal);
  indexes.add('TestScript', 'version', 'The business version of the test script', SearchParamTypeTOKEN, [], 'TestScript.version', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_VALUESET}
procedure TFHIRIndexBuilder.buildIndexesForValueSet(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ValueSet', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('ValueSet', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('ValueSet', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('ValueSet', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('ValueSet', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ValueSet', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('ValueSet', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('ValueSet', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('ValueSet', 'date', 'The value set publication date', SearchParamTypeDATE, [], 'ValueSet.date', SearchXpathUsageNormal);
  indexes.add('ValueSet', 'description', 'The description of the value set', SearchParamTypeSTRING, [], 'ValueSet.description', SearchXpathUsageNormal);
  indexes.add('ValueSet', 'expansion', 'Uniquely identifies this expansion', SearchParamTypeURI, [], 'ValueSet.expansion.identifier', SearchXpathUsageNormal);
  indexes.add('ValueSet', 'identifier', 'External identifier for the value set', SearchParamTypeTOKEN, [], 'ValueSet.identifier', SearchXpathUsageNormal);
  indexes.add('ValueSet', 'jurisdiction', 'Intended jurisdiction for the value set', SearchParamTypeTOKEN, [], 'ValueSet.jurisdiction', SearchXpathUsageNormal);
  indexes.add('ValueSet', 'name', 'Computationally friendly name of the value set', SearchParamTypeSTRING, [], 'ValueSet.name', SearchXpathUsageNormal);
  indexes.add('ValueSet', 'publisher', 'Name of the publisher of the value set', SearchParamTypeSTRING, [], 'ValueSet.publisher', SearchXpathUsageNormal);
  indexes.add('ValueSet', 'reference', 'A code system included or excluded in the value set or an imported value set', SearchParamTypeURI, [], 'ValueSet.compose.include.system', SearchXpathUsageNormal);
  indexes.add('ValueSet', 'status', 'The current status of the value set', SearchParamTypeTOKEN, [], 'ValueSet.status', SearchXpathUsageNormal);
  indexes.add('ValueSet', 'title', 'The human-friendly name of the value set', SearchParamTypeSTRING, [], 'ValueSet.title', SearchXpathUsageNormal);
  indexes.add('ValueSet', 'url', 'The uri that identifies the value set', SearchParamTypeURI, [], 'ValueSet.url', SearchXpathUsageNormal);
  indexes.add('ValueSet', 'version', 'The business version of the value set', SearchParamTypeTOKEN, [], 'ValueSet.version', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_VISIONPRESCRIPTION}
procedure TFHIRIndexBuilder.buildIndexesForVisionPrescription(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('VisionPrescription', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('VisionPrescription', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], 'Resource.id', SearchXpathUsageNormal);
  indexes.add('VisionPrescription', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], 'Resource.meta.lastUpdated', SearchXpathUsageNormal);
  indexes.add('VisionPrescription', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], 'Resource.meta.profile', SearchXpathUsageNormal);
  indexes.add('VisionPrescription', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('VisionPrescription', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.security', SearchXpathUsageNormal);
  indexes.add('VisionPrescription', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], 'Resource.meta.tag', SearchXpathUsageNormal);
  indexes.add('VisionPrescription', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('VisionPrescription', 'datewritten', 'Return prescriptions written on this date', SearchParamTypeDATE, [], 'VisionPrescription.dateWritten', SearchXpathUsageNormal);
  indexes.add('VisionPrescription', 'encounter', 'Return prescriptions with this encounter identifier', SearchParamTypeREFERENCE, ['EpisodeOfCare', 'Encounter'], 'DocumentReference.context.encounter | RiskAssessment.context | DeviceRequest.context | Procedure.context | List.encounter | VisionPrescription.encounter | ProcedureRequest.context | Flag.encounter | Observation.context | DiagnosticReport.context | Nu'+'tritionOrder.encounter | Composition.encounter', SearchXpathUsageNormal);
  indexes.add('VisionPrescription', 'identifier', 'Return prescriptions with this external identifier', SearchParamTypeTOKEN, [], 'DocumentManifest.masterIdentifier | DocumentManifest.identifier | Goal.identifier | Consent.identifier | DocumentReference.masterIdentifier | DocumentReference.identifier | SupplyRequest.identifier | RiskAssessment.identifier | CareTeam.identifier | '+'ImagingStudy.identifier | FamilyMemberHistory.identifier | Encounter.identifier | DeviceRequest.identifier | AllergyIntolerance.identifier | CarePlan.identifier | EpisodeOfCare.identifier | Procedure.identifier | List.identifier | Immunization.identi'+'fier | VisionPrescription.identifier | ProcedureRequest.identifier | Observation.identifier | DiagnosticReport.identifier | NutritionOrder.identifier | Condition.identifier | Composition.identifier | DetectedIssue.identifier | SupplyDelivery.identifi'+'er', SearchXpathUsageNormal);
  indexes.add('VisionPrescription', 'patient', 'The identity of a patient to list dispenses for', SearchParamTypeREFERENCE, ['Group', 'Patient'], 'ReferralRequest.subject | DocumentManifest.subject | Goal.subject | Consent.patient | DocumentReference.subject | ImagingManifest.patient | RiskAssessment.subject | CareTeam.subject | ImagingStudy.patient | FamilyMemberHistory.patient | Encounter.sub'+'ject | DeviceUseStatement.subject | DeviceRequest.subject | AllergyIntolerance.patient | CarePlan.subject | EpisodeOfCare.patient | Procedure.subject | List.subject | Immunization.patient | VisionPrescription.patient | ProcedureRequest.subject | Flag'+'.subject | Observation.subject | DiagnosticReport.subject | NutritionOrder.patient | Condition.subject | Composition.subject | DetectedIssue.patient | SupplyDelivery.patient | ClinicalImpression.subject', SearchXpathUsageNormal);
  indexes.add('VisionPrescription', 'prescriber', 'Who authorizes the vision product', SearchParamTypeREFERENCE, ['Practitioner'], 'VisionPrescription.prescriber', SearchXpathUsageNormal);
  compartments.register(frtEncounter, 'VisionPrescription', ['encounter']);
  compartments.register(frtPatient, 'VisionPrescription', ['patient']);
  compartments.register(frtPractitioner, 'VisionPrescription', ['prescriber']);
end;
{$ENDIF}

procedure TFHIRIndexBuilder.registerIndexes(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  {$IFDEF FHIR_ACCOUNT}
  buildIndexesForAccount(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_ACTIVITYDEFINITION}
  buildIndexesForActivityDefinition(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_ADVERSEEVENT}
  buildIndexesForAdverseEvent(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_ALLERGYINTOLERANCE}
  buildIndexesForAllergyIntolerance(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_APPOINTMENT}
  buildIndexesForAppointment(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_APPOINTMENTRESPONSE}
  buildIndexesForAppointmentResponse(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_AUDITEVENT}
  buildIndexesForAuditEvent(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_BASIC}
  buildIndexesForBasic(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_BINARY}
  buildIndexesForBinary(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_BODYSITE}
  buildIndexesForBodySite(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_BUNDLE}
  buildIndexesForBundle(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_CAPABILITYSTATEMENT}
  buildIndexesForCapabilityStatement(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_CAREPLAN}
  buildIndexesForCarePlan(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_CARETEAM}
  buildIndexesForCareTeam(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_CHARGEITEM}
  buildIndexesForChargeItem(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_CLAIM}
  buildIndexesForClaim(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_CLAIMRESPONSE}
  buildIndexesForClaimResponse(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_CLINICALIMPRESSION}
  buildIndexesForClinicalImpression(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_CODESYSTEM}
  buildIndexesForCodeSystem(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_COMMUNICATION}
  buildIndexesForCommunication(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_COMMUNICATIONREQUEST}
  buildIndexesForCommunicationRequest(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_COMPARTMENTDEFINITION}
  buildIndexesForCompartmentDefinition(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_COMPOSITION}
  buildIndexesForComposition(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_CONCEPTMAP}
  buildIndexesForConceptMap(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_CONDITION}
  buildIndexesForCondition(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_CONSENT}
  buildIndexesForConsent(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_CONTRACT}
  buildIndexesForContract(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_COVERAGE}
  buildIndexesForCoverage(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_DATAELEMENT}
  buildIndexesForDataElement(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_DETECTEDISSUE}
  buildIndexesForDetectedIssue(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_DEVICE}
  buildIndexesForDevice(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_DEVICECOMPONENT}
  buildIndexesForDeviceComponent(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_DEVICEMETRIC}
  buildIndexesForDeviceMetric(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_DEVICEREQUEST}
  buildIndexesForDeviceRequest(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_DEVICEUSESTATEMENT}
  buildIndexesForDeviceUseStatement(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_DIAGNOSTICREPORT}
  buildIndexesForDiagnosticReport(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_DOCUMENTMANIFEST}
  buildIndexesForDocumentManifest(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_DOCUMENTREFERENCE}
  buildIndexesForDocumentReference(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_ELIGIBILITYREQUEST}
  buildIndexesForEligibilityRequest(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_ELIGIBILITYRESPONSE}
  buildIndexesForEligibilityResponse(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_ENCOUNTER}
  buildIndexesForEncounter(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_ENDPOINT}
  buildIndexesForEndpoint(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_ENROLLMENTREQUEST}
  buildIndexesForEnrollmentRequest(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_ENROLLMENTRESPONSE}
  buildIndexesForEnrollmentResponse(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_EPISODEOFCARE}
  buildIndexesForEpisodeOfCare(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_EXPANSIONPROFILE}
  buildIndexesForExpansionProfile(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_EXPLANATIONOFBENEFIT}
  buildIndexesForExplanationOfBenefit(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_FAMILYMEMBERHISTORY}
  buildIndexesForFamilyMemberHistory(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_FLAG}
  buildIndexesForFlag(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_GOAL}
  buildIndexesForGoal(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_GRAPHDEFINITION}
  buildIndexesForGraphDefinition(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_GROUP}
  buildIndexesForGroup(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_GUIDANCERESPONSE}
  buildIndexesForGuidanceResponse(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_HEALTHCARESERVICE}
  buildIndexesForHealthcareService(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_IMAGINGMANIFEST}
  buildIndexesForImagingManifest(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_IMAGINGSTUDY}
  buildIndexesForImagingStudy(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_IMMUNIZATION}
  buildIndexesForImmunization(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_IMMUNIZATIONRECOMMENDATION}
  buildIndexesForImmunizationRecommendation(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_IMPLEMENTATIONGUIDE}
  buildIndexesForImplementationGuide(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_LIBRARY}
  buildIndexesForLibrary(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_LINKAGE}
  buildIndexesForLinkage(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_LIST}
  buildIndexesForList(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_LOCATION}
  buildIndexesForLocation(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_MEASURE}
  buildIndexesForMeasure(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_MEASUREREPORT}
  buildIndexesForMeasureReport(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_MEDIA}
  buildIndexesForMedia(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_MEDICATION}
  buildIndexesForMedication(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_MEDICATIONADMINISTRATION}
  buildIndexesForMedicationAdministration(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_MEDICATIONDISPENSE}
  buildIndexesForMedicationDispense(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_MEDICATIONREQUEST}
  buildIndexesForMedicationRequest(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_MEDICATIONSTATEMENT}
  buildIndexesForMedicationStatement(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_MESSAGEDEFINITION}
  buildIndexesForMessageDefinition(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_MESSAGEHEADER}
  buildIndexesForMessageHeader(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_NAMINGSYSTEM}
  buildIndexesForNamingSystem(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_NUTRITIONORDER}
  buildIndexesForNutritionOrder(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_OBSERVATION}
  buildIndexesForObservation(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_OPERATIONDEFINITION}
  buildIndexesForOperationDefinition(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_OPERATIONOUTCOME}
  buildIndexesForOperationOutcome(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_ORGANIZATION}
  buildIndexesForOrganization(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_PATIENT}
  buildIndexesForPatient(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_PAYMENTNOTICE}
  buildIndexesForPaymentNotice(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_PAYMENTRECONCILIATION}
  buildIndexesForPaymentReconciliation(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_PERSON}
  buildIndexesForPerson(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_PLANDEFINITION}
  buildIndexesForPlanDefinition(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_PRACTITIONER}
  buildIndexesForPractitioner(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_PRACTITIONERROLE}
  buildIndexesForPractitionerRole(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_PROCEDURE}
  buildIndexesForProcedure(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_PROCEDUREREQUEST}
  buildIndexesForProcedureRequest(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_PROCESSREQUEST}
  buildIndexesForProcessRequest(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_PROCESSRESPONSE}
  buildIndexesForProcessResponse(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_PROVENANCE}
  buildIndexesForProvenance(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_QUESTIONNAIRE}
  buildIndexesForQuestionnaire(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_QUESTIONNAIRERESPONSE}
  buildIndexesForQuestionnaireResponse(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_REFERRALREQUEST}
  buildIndexesForReferralRequest(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_RELATEDPERSON}
  buildIndexesForRelatedPerson(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_REQUESTGROUP}
  buildIndexesForRequestGroup(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_RESEARCHSTUDY}
  buildIndexesForResearchStudy(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_RESEARCHSUBJECT}
  buildIndexesForResearchSubject(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_RISKASSESSMENT}
  buildIndexesForRiskAssessment(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_SCHEDULE}
  buildIndexesForSchedule(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_SEARCHPARAMETER}
  buildIndexesForSearchParameter(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_SEQUENCE}
  buildIndexesForSequence(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_SERVICEDEFINITION}
  buildIndexesForServiceDefinition(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_SLOT}
  buildIndexesForSlot(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_SPECIMEN}
  buildIndexesForSpecimen(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_STRUCTUREDEFINITION}
  buildIndexesForStructureDefinition(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_STRUCTUREMAP}
  buildIndexesForStructureMap(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_SUBSCRIPTION}
  buildIndexesForSubscription(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_SUBSTANCE}
  buildIndexesForSubstance(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_SUPPLYDELIVERY}
  buildIndexesForSupplyDelivery(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_SUPPLYREQUEST}
  buildIndexesForSupplyRequest(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_TASK}
  buildIndexesForTask(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_TESTREPORT}
  buildIndexesForTestReport(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_TESTSCRIPT}
  buildIndexesForTestScript(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_VALUESET}
  buildIndexesForValueSet(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_VISIONPRESCRIPTION}
  buildIndexesForVisionPrescription(Indexes, compartments);
  {$ENDIF}
end;

end.

