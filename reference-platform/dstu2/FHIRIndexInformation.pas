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

{$IFNDEF FHIR2}
This is the dstu2 version of the FHIR code
{$ENDIF}


interface

// FHIR v1.0.2 generated 2015-10-24T07:41:03+11:00

uses
  SysUtils, Classes, StringSupport, DecimalSupport, AdvBuffers, DateSupport, FHIRIndexBase, FHIRResources, FHIRTypes, FHIRConstants, FHIRSupport;

Type

  TFHIRIndexBuilder = class (TAdvObject)
  private
    {$IFDEF FHIR_ACCOUNT}
    procedure buildIndexesForAccount(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
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
    {$IFDEF FHIR_CAREPLAN}
    procedure buildIndexesForCarePlan(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
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
    {$IFDEF FHIR_COMMUNICATION}
    procedure buildIndexesForCommunication(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_COMMUNICATIONREQUEST}
    procedure buildIndexesForCommunicationRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
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
    {$IFDEF FHIR_CONFORMANCE}
    procedure buildIndexesForConformance(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
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
    {$IFDEF FHIR_DEVICEUSEREQUEST}
    procedure buildIndexesForDeviceUseRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_DEVICEUSESTATEMENT}
    procedure buildIndexesForDeviceUseStatement(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_DIAGNOSTICORDER}
    procedure buildIndexesForDiagnosticOrder(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
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
    {$IFDEF FHIR_ENROLLMENTREQUEST}
    procedure buildIndexesForEnrollmentRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_ENROLLMENTRESPONSE}
    procedure buildIndexesForEnrollmentResponse(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_EPISODEOFCARE}
    procedure buildIndexesForEpisodeOfCare(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
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
    {$IFDEF FHIR_GROUP}
    procedure buildIndexesForGroup(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_HEALTHCARESERVICE}
    procedure buildIndexesForHealthcareService(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_IMAGINGOBJECTSELECTION}
    procedure buildIndexesForImagingObjectSelection(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
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
    {$IFDEF FHIR_LIST}
    procedure buildIndexesForList(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_LOCATION}
    procedure buildIndexesForLocation(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
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
    {$IFDEF FHIR_MEDICATIONORDER}
    procedure buildIndexesForMedicationOrder(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_MEDICATIONSTATEMENT}
    procedure buildIndexesForMedicationStatement(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
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
    {$IFDEF FHIR_ORDER}
    procedure buildIndexesForOrder(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_ORDERRESPONSE}
    procedure buildIndexesForOrderResponse(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
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
    {$IFDEF FHIR_PRACTITIONER}
    procedure buildIndexesForPractitioner(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
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
    {$IFDEF FHIR_RISKASSESSMENT}
    procedure buildIndexesForRiskAssessment(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_SCHEDULE}
    procedure buildIndexesForSchedule(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_SEARCHPARAMETER}
    procedure buildIndexesForSearchParameter(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
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
  indexes.add('Account', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Account', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Account', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Account', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('Account', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('Account', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Account', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Account', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Account', 'balance', 'How much is in account?', SearchParamTypeQUANTITY, [], '', SearchXpathUsageNormal);
  indexes.add('Account', 'identifier', 'Account number', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Account', 'name', 'Human-readable label', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Account', 'owner', 'Who is responsible?', SearchParamTypeREFERENCE, ['Organization'], '', SearchXpathUsageNormal);
  indexes.add('Account', 'patient', 'What is account tied to?', SearchParamTypeREFERENCE, ['Practitioner', 'Organization', 'Device', 'Patient', 'HealthcareService', 'Location'], '', SearchXpathUsageNormal);
  indexes.add('Account', 'period', 'Transaction window', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Account', 'status', 'active | inactive', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Account', 'subject', 'What is account tied to?', SearchParamTypeREFERENCE, ['Practitioner', 'Organization', 'Device', 'Patient', 'HealthcareService', 'Location'], '', SearchXpathUsageNormal);
  indexes.add('Account', 'type', 'E.g. patient, expense, depreciation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_ALLERGYINTOLERANCE}
procedure TFHIRIndexBuilder.buildIndexesForAllergyIntolerance(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('AllergyIntolerance', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('AllergyIntolerance', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('AllergyIntolerance', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('AllergyIntolerance', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('AllergyIntolerance', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('AllergyIntolerance', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('AllergyIntolerance', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('AllergyIntolerance', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('AllergyIntolerance', 'category', 'food | medication | environment | other - Category of Substance', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('AllergyIntolerance', 'criticality', 'CRITL | CRITH | CRITU', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('AllergyIntolerance', 'date', 'When recorded', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('AllergyIntolerance', 'identifier', 'External ids for this item', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('AllergyIntolerance', 'last-date', 'Date(/time) of last known occurrence of a reaction', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('AllergyIntolerance', 'manifestation', 'Clinical symptoms/signs associated with the Event', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('AllergyIntolerance', 'onset', 'Date(/time) when manifestations showed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('AllergyIntolerance', 'patient', 'Who the sensitivity is for', SearchParamTypeREFERENCE, ['Patient'], '', SearchXpathUsageNormal);
  indexes.add('AllergyIntolerance', 'recorder', 'Who recorded the sensitivity', SearchParamTypeREFERENCE, ['Practitioner', 'Patient'], '', SearchXpathUsageNormal);
  indexes.add('AllergyIntolerance', 'reporter', 'Source of the information about the allergy', SearchParamTypeREFERENCE, ['Practitioner', 'Patient', 'RelatedPerson'], '', SearchXpathUsageNormal);
  indexes.add('AllergyIntolerance', 'route', 'How the subject was exposed to the substance', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('AllergyIntolerance', 'severity', 'mild | moderate | severe (of event as a whole)', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('AllergyIntolerance', 'status', 'active | unconfirmed | confirmed | inactive | resolved | refuted | entered-in-error', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('AllergyIntolerance', 'substance', 'Substance, (or class) considered to be responsible for risk', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('AllergyIntolerance', 'type', 'allergy | intolerance - Underlying mechanism (if known)', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_APPOINTMENT}
procedure TFHIRIndexBuilder.buildIndexesForAppointment(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Appointment', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Appointment', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Appointment', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Appointment', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('Appointment', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('Appointment', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Appointment', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Appointment', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Appointment', 'actor', 'Any one of the individuals participating in the appointment', SearchParamTypeREFERENCE, ['Practitioner', 'Device', 'Patient', 'HealthcareService', 'RelatedPerson', 'Location'], '', SearchXpathUsageNormal);
  indexes.add('Appointment', 'date', 'Appointment date/time.', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Appointment', 'identifier', 'An Identifier of the Appointment', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Appointment', 'location', 'This location is listed in the participants of the appointment', SearchParamTypeREFERENCE, ['Practitioner', 'Device', 'Patient', 'HealthcareService', 'RelatedPerson', 'Location'], '', SearchXpathUsageNormal);
  indexes.add('Appointment', 'part-status', 'The Participation status of the subject, or other participant on the appointment. Can be used to locate participants that have not responded to meeting requests.', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Appointment', 'patient', 'One of the individuals of the appointment is this patient', SearchParamTypeREFERENCE, ['Practitioner', 'Device', 'Patient', 'HealthcareService', 'RelatedPerson', 'Location'], '', SearchXpathUsageNormal);
  indexes.add('Appointment', 'practitioner', 'One of the individuals of the appointment is this practitioner', SearchParamTypeREFERENCE, ['Practitioner', 'Device', 'Patient', 'HealthcareService', 'RelatedPerson', 'Location'], '', SearchXpathUsageNormal);
  indexes.add('Appointment', 'status', 'The overall status of the appointment', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_APPOINTMENTRESPONSE}
procedure TFHIRIndexBuilder.buildIndexesForAppointmentResponse(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('AppointmentResponse', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('AppointmentResponse', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('AppointmentResponse', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('AppointmentResponse', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('AppointmentResponse', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('AppointmentResponse', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('AppointmentResponse', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('AppointmentResponse', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('AppointmentResponse', 'actor', 'The Person, Location/HealthcareService or Device that this appointment response replies for', SearchParamTypeREFERENCE, ['Practitioner', 'Device', 'Patient', 'HealthcareService', 'RelatedPerson', 'Location'], '', SearchXpathUsageNormal);
  indexes.add('AppointmentResponse', 'appointment', 'The appointment that the response is attached to', SearchParamTypeREFERENCE, ['Appointment'], '', SearchXpathUsageNormal);
  indexes.add('AppointmentResponse', 'identifier', 'An Identifier in this appointment response', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('AppointmentResponse', 'location', 'This Response is for this Location', SearchParamTypeREFERENCE, ['Practitioner', 'Device', 'Patient', 'HealthcareService', 'RelatedPerson', 'Location'], '', SearchXpathUsageNormal);
  indexes.add('AppointmentResponse', 'part-status', 'The participants acceptance status for this appointment', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('AppointmentResponse', 'patient', 'This Response is for this Patient', SearchParamTypeREFERENCE, ['Practitioner', 'Device', 'Patient', 'HealthcareService', 'RelatedPerson', 'Location'], '', SearchXpathUsageNormal);
  indexes.add('AppointmentResponse', 'practitioner', 'This Response is for this Practitioner', SearchParamTypeREFERENCE, ['Practitioner', 'Device', 'Patient', 'HealthcareService', 'RelatedPerson', 'Location'], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_AUDITEVENT}
procedure TFHIRIndexBuilder.buildIndexesForAuditEvent(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('AuditEvent', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('AuditEvent', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('AuditEvent', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('AuditEvent', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('AuditEvent', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('AuditEvent', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('AuditEvent', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('AuditEvent', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('AuditEvent', 'action', 'Type of action performed during the event', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('AuditEvent', 'address', 'Identifier for the network access point of the user device', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('AuditEvent', 'altid', 'Alternative User id e.g. authentication', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('AuditEvent', 'date', 'Time when the event occurred on source', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('AuditEvent', 'desc', 'Instance-specific descriptor for Object', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('AuditEvent', 'identity', 'Specific instance of object (e.g. versioned)', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('AuditEvent', 'name', 'Human-meaningful name for the user', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('AuditEvent', 'object-type', 'Type of object involved', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('AuditEvent', 'participant', 'Direct reference to resource', SearchParamTypeREFERENCE, ['Practitioner', 'Organization', 'Device', 'Patient', 'RelatedPerson'], '', SearchXpathUsageNormal);
  indexes.add('AuditEvent', 'patient', 'Direct reference to resource', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, '', SearchXpathUsageNormal);
  indexes.add('AuditEvent', 'policy', 'Policy that authorized event', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('AuditEvent', 'reference', 'Specific instance of resource (e.g. versioned)', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, '', SearchXpathUsageNormal);
  indexes.add('AuditEvent', 'site', 'Logical source location within the enterprise', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('AuditEvent', 'source', 'The identity of source detecting the event', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('AuditEvent', 'subtype', 'More specific type/id for the event', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('AuditEvent', 'type', 'Type/identifier of event', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('AuditEvent', 'user', 'Unique identifier for the user', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_BASIC}
procedure TFHIRIndexBuilder.buildIndexesForBasic(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Basic', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Basic', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Basic', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Basic', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('Basic', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('Basic', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Basic', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Basic', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Basic', 'author', 'Who created', SearchParamTypeREFERENCE, ['Practitioner', 'Patient', 'RelatedPerson'], '', SearchXpathUsageNormal);
  indexes.add('Basic', 'code', 'Kind of Resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Basic', 'created', 'When created', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Basic', 'identifier', 'Business identifier', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Basic', 'patient', 'Identifies the focus of this resource', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, '', SearchXpathUsageNormal);
  indexes.add('Basic', 'subject', 'Identifies the focus of this resource', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_BINARY}
procedure TFHIRIndexBuilder.buildIndexesForBinary(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Binary', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Binary', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Binary', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Binary', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('Binary', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('Binary', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Binary', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Binary', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Binary', 'contenttype', 'MimeType of the binary content', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_BODYSITE}
procedure TFHIRIndexBuilder.buildIndexesForBodySite(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('BodySite', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('BodySite', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('BodySite', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('BodySite', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('BodySite', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('BodySite', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('BodySite', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('BodySite', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('BodySite', 'code', 'Named anatomical location', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('BodySite', 'identifier', 'Identifier for this instance of the anatomical location', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('BodySite', 'patient', 'Patient to whom bodysite belongs', SearchParamTypeREFERENCE, ['Patient'], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_BUNDLE}
procedure TFHIRIndexBuilder.buildIndexesForBundle(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Bundle', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Bundle', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Bundle', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Bundle', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('Bundle', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('Bundle', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Bundle', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Bundle', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Bundle', 'composition', 'The first resource in the bundle, if the bundle type is "document" - this is a composition, and this parameter provides access to searches its contents', SearchParamTypeREFERENCE, [], '', SearchXpathUsageNormal);
  indexes.add('Bundle', 'message', 'The first resource in the bundle, if the bundle type is "message" - this is a message header, and this parameter provides access to search its contents', SearchParamTypeREFERENCE, [], '', SearchXpathUsageNormal);
  indexes.add('Bundle', 'type', 'document | message | transaction | transaction-response | batch | batch-response | history | searchset | collection', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_CAREPLAN}
procedure TFHIRIndexBuilder.buildIndexesForCarePlan(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('CarePlan', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('CarePlan', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('CarePlan', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('CarePlan', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('CarePlan', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('CarePlan', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('CarePlan', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('CarePlan', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('CarePlan', 'activitycode', 'Detail type of activity', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('CarePlan', 'activitydate', 'Specified date occurs within period specified by CarePlan.activity.timingSchedule', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('CarePlan', 'activityreference', 'Activity details defined in specific resource', SearchParamTypeREFERENCE, ['Appointment', 'Order', 'ReferralRequest', 'ProcessRequest', 'NutritionOrder', 'VisionPrescription', 'ProcedureRequest', 'DiagnosticOrder', 'DeviceUseRequest', 'MedicationOrder', 'CommunicationRequest', 'SupplyRequest'], '', SearchXpathUsageNormal);
  indexes.add('CarePlan', 'condition', 'Health issues this plan addresses', SearchParamTypeREFERENCE, ['Condition'], '', SearchXpathUsageNormal);
  indexes.add('CarePlan', 'date', 'Time period plan covers', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('CarePlan', 'goal', 'Desired outcome of plan', SearchParamTypeREFERENCE, ['Goal'], '', SearchXpathUsageNormal);
  indexes.add('CarePlan', 'participant', 'Who is involved', SearchParamTypeREFERENCE, ['Practitioner', 'Organization', 'Patient', 'RelatedPerson'], '', SearchXpathUsageNormal);
  indexes.add('CarePlan', 'patient', 'Who care plan is for', SearchParamTypeREFERENCE, ['Group', 'Patient'], '', SearchXpathUsageNormal);
  indexes.add('CarePlan', 'performer', 'Matches if the practitioner is listed as a performer in any of the "simple" activities.  (For performers of the detailed activities, chain through the activitydetail search parameter.)', SearchParamTypeREFERENCE, ['Practitioner', 'Organization', 'Patient', 'RelatedPerson'], '', SearchXpathUsageNormal);
  indexes.add('CarePlan', 'related', 'A combination of the type of relationship and the related plan', SearchParamTypeCOMPOSITE, [], '', SearchXpathUsageNull);
  indexes.add('CarePlan', 'relatedcode', 'includes | replaces | fulfills', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('CarePlan', 'relatedplan', 'Plan relationship exists with', SearchParamTypeREFERENCE, ['CarePlan'], '', SearchXpathUsageNormal);
  indexes.add('CarePlan', 'subject', 'Who care plan is for', SearchParamTypeREFERENCE, ['Group', 'Patient'], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_CLAIM}
procedure TFHIRIndexBuilder.buildIndexesForClaim(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Claim', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Claim', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Claim', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Claim', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('Claim', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('Claim', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Claim', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Claim', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Claim', 'identifier', 'The primary identifier of the financial resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Claim', 'patient', 'Patient', SearchParamTypeREFERENCE, ['Patient'], '', SearchXpathUsageNormal);
  indexes.add('Claim', 'priority', 'Processing priority requested', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Claim', 'provider', 'Provider responsible for the claim', SearchParamTypeREFERENCE, ['Practitioner'], '', SearchXpathUsageNormal);
  indexes.add('Claim', 'use', 'The kind of financial resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_CLAIMRESPONSE}
procedure TFHIRIndexBuilder.buildIndexesForClaimResponse(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ClaimResponse', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('ClaimResponse', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ClaimResponse', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('ClaimResponse', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('ClaimResponse', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('ClaimResponse', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ClaimResponse', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ClaimResponse', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('ClaimResponse', 'identifier', 'The identity of the insurer', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_CLINICALIMPRESSION}
procedure TFHIRIndexBuilder.buildIndexesForClinicalImpression(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ClinicalImpression', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('ClinicalImpression', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ClinicalImpression', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('ClinicalImpression', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('ClinicalImpression', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('ClinicalImpression', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ClinicalImpression', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ClinicalImpression', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('ClinicalImpression', 'action', 'Actions taken during assessment', SearchParamTypeREFERENCE, ['Appointment', 'ReferralRequest', 'NutritionOrder', 'ProcedureRequest', 'Procedure', 'DiagnosticOrder', 'MedicationOrder', 'SupplyRequest'], '', SearchXpathUsageNormal);
  indexes.add('ClinicalImpression', 'assessor', 'The clinician performing the assessment', SearchParamTypeREFERENCE, ['Practitioner'], '', SearchXpathUsageNormal);
  indexes.add('ClinicalImpression', 'date', 'When the assessment occurred', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('ClinicalImpression', 'finding', 'Specific text or code for finding', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ClinicalImpression', 'investigation', 'Record of a specific investigation', SearchParamTypeREFERENCE, ['FamilyMemberHistory', 'Observation', 'DiagnosticReport', 'QuestionnaireResponse'], '', SearchXpathUsageNormal);
  indexes.add('ClinicalImpression', 'patient', 'The patient being assessed', SearchParamTypeREFERENCE, ['Patient'], '', SearchXpathUsageNormal);
  indexes.add('ClinicalImpression', 'plan', 'Plan of action after assessment', SearchParamTypeREFERENCE, ['Appointment', 'Order', 'ReferralRequest', 'ProcessRequest', 'VisionPrescription', 'ProcedureRequest', 'DiagnosticOrder', 'DeviceUseRequest', 'SupplyRequest', 'CarePlan', 'NutritionOrder', 'MedicationOrder', 'CommunicationRequest'], '', SearchXpathUsageNormal);
  indexes.add('ClinicalImpression', 'previous', 'Reference to last assessment', SearchParamTypeREFERENCE, ['ClinicalImpression'], '', SearchXpathUsageNormal);
  indexes.add('ClinicalImpression', 'problem', 'General assessment of patient state', SearchParamTypeREFERENCE, ['Condition', 'AllergyIntolerance'], '', SearchXpathUsageNormal);
  indexes.add('ClinicalImpression', 'resolved', 'Diagnoses/conditions resolved since previous assessment', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ClinicalImpression', 'ruledout', 'Specific text of code for diagnosis', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ClinicalImpression', 'status', 'in-progress | completed | entered-in-error', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ClinicalImpression', 'trigger', 'Request or event that necessitated this assessment', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, '', SearchXpathUsageNormal);
  indexes.add('ClinicalImpression', 'trigger-code', 'Request or event that necessitated this assessment', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_COMMUNICATION}
procedure TFHIRIndexBuilder.buildIndexesForCommunication(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Communication', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Communication', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Communication', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Communication', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('Communication', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('Communication', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Communication', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Communication', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Communication', 'category', 'Message category', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Communication', 'encounter', 'Encounter leading to message', SearchParamTypeREFERENCE, ['Encounter'], '', SearchXpathUsageNormal);
  indexes.add('Communication', 'identifier', 'Unique identifier', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Communication', 'medium', 'A channel of communication', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Communication', 'patient', 'Focus of message', SearchParamTypeREFERENCE, ['Patient'], '', SearchXpathUsageNormal);
  indexes.add('Communication', 'received', 'When received', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Communication', 'recipient', 'Message recipient', SearchParamTypeREFERENCE, ['Practitioner', 'Group', 'Organization', 'Device', 'Patient', 'RelatedPerson'], '', SearchXpathUsageNormal);
  indexes.add('Communication', 'request', 'CommunicationRequest producing this message', SearchParamTypeREFERENCE, ['CommunicationRequest'], '', SearchXpathUsageNormal);
  indexes.add('Communication', 'sender', 'Message sender', SearchParamTypeREFERENCE, ['Practitioner', 'Organization', 'Device', 'Patient', 'RelatedPerson'], '', SearchXpathUsageNormal);
  indexes.add('Communication', 'sent', 'When sent', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Communication', 'status', 'in-progress | completed | suspended | rejected | failed', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Communication', 'subject', 'Focus of message', SearchParamTypeREFERENCE, ['Patient'], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_COMMUNICATIONREQUEST}
procedure TFHIRIndexBuilder.buildIndexesForCommunicationRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('CommunicationRequest', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('CommunicationRequest', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('CommunicationRequest', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('CommunicationRequest', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('CommunicationRequest', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('CommunicationRequest', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('CommunicationRequest', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('CommunicationRequest', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('CommunicationRequest', 'category', 'Message category', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('CommunicationRequest', 'encounter', 'Encounter leading to message', SearchParamTypeREFERENCE, ['Encounter'], '', SearchXpathUsageNormal);
  indexes.add('CommunicationRequest', 'identifier', 'Unique identifier', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('CommunicationRequest', 'medium', 'A channel of communication', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('CommunicationRequest', 'patient', 'Focus of message', SearchParamTypeREFERENCE, ['Patient'], '', SearchXpathUsageNormal);
  indexes.add('CommunicationRequest', 'priority', 'Message urgency', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('CommunicationRequest', 'recipient', 'Message recipient', SearchParamTypeREFERENCE, ['Practitioner', 'Organization', 'Device', 'Patient', 'RelatedPerson'], '', SearchXpathUsageNormal);
  indexes.add('CommunicationRequest', 'requested', 'When ordered or proposed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('CommunicationRequest', 'requester', 'An individual who requested a communication', SearchParamTypeREFERENCE, ['Practitioner', 'Patient', 'RelatedPerson'], '', SearchXpathUsageNormal);
  indexes.add('CommunicationRequest', 'sender', 'Message sender', SearchParamTypeREFERENCE, ['Practitioner', 'Organization', 'Device', 'Patient', 'RelatedPerson'], '', SearchXpathUsageNormal);
  indexes.add('CommunicationRequest', 'status', 'proposed | planned | requested | received | accepted | in-progress | completed | suspended | rejected | failed', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('CommunicationRequest', 'subject', 'Focus of message', SearchParamTypeREFERENCE, ['Patient'], '', SearchXpathUsageNormal);
  indexes.add('CommunicationRequest', 'time', 'When scheduled', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_COMPOSITION}
procedure TFHIRIndexBuilder.buildIndexesForComposition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Composition', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Composition', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Composition', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Composition', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('Composition', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('Composition', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Composition', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Composition', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Composition', 'attester', 'Who attested the composition', SearchParamTypeREFERENCE, ['Practitioner', 'Organization', 'Patient'], '', SearchXpathUsageNormal);
  indexes.add('Composition', 'author', 'Who and/or what authored the composition', SearchParamTypeREFERENCE, ['Practitioner', 'Device', 'Patient', 'RelatedPerson'], '', SearchXpathUsageNormal);
  indexes.add('Composition', 'class', 'Categorization of Composition', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Composition', 'confidentiality', 'As defined by affinity domain', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Composition', 'context', 'Code(s) that apply to the event being documented', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Composition', 'date', 'Composition editing time', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Composition', 'encounter', 'Context of the Composition', SearchParamTypeREFERENCE, ['Encounter'], '', SearchXpathUsageNormal);
  indexes.add('Composition', 'entry', 'A reference to data that supports this section', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, '', SearchXpathUsageNormal);
  indexes.add('Composition', 'identifier', 'Logical identifier of composition (version-independent)', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Composition', 'patient', 'Who and/or what the composition is about', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, '', SearchXpathUsageNormal);
  indexes.add('Composition', 'period', 'The period covered by the documentation', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Composition', 'section', 'Classification of section (recommended)', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Composition', 'status', 'preliminary | final | amended | entered-in-error', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Composition', 'subject', 'Who and/or what the composition is about', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, '', SearchXpathUsageNormal);
  indexes.add('Composition', 'title', 'Human Readable name/title', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Composition', 'type', 'Kind of composition (LOINC if possible)', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_CONCEPTMAP}
procedure TFHIRIndexBuilder.buildIndexesForConceptMap(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ConceptMap', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('ConceptMap', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ConceptMap', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('ConceptMap', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('ConceptMap', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('ConceptMap', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ConceptMap', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ConceptMap', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('ConceptMap', 'context', 'A use context assigned to the concept map', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ConceptMap', 'date', 'The concept map publication date', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('ConceptMap', 'dependson', 'Reference to element/field/ValueSet mapping depends on', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('ConceptMap', 'description', 'Text search in the description of the concept map', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('ConceptMap', 'identifier', 'Additional identifier for the concept map', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ConceptMap', 'name', 'Name of the concept map', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('ConceptMap', 'product', 'Reference to element/field/ValueSet mapping depends on', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('ConceptMap', 'publisher', 'Name of the publisher of the concept map', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('ConceptMap', 'source', 'Identifies the source of the concepts which are being mapped', SearchParamTypeREFERENCE, ['StructureDefinition', 'ValueSet'], '', SearchXpathUsageNormal);
  indexes.add('ConceptMap', 'sourcecode', 'Identifies element being mapped', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ConceptMap', 'sourcesystem', 'Code System (if value set crosses code systems)', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('ConceptMap', 'sourceuri', 'Identifies the source of the concepts which are being mapped', SearchParamTypeREFERENCE, ['StructureDefinition', 'ValueSet'], '', SearchXpathUsageNormal);
  indexes.add('ConceptMap', 'status', 'Status of the concept map', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ConceptMap', 'target', 'Provides context to the mappings', SearchParamTypeREFERENCE, ['StructureDefinition', 'ValueSet'], '', SearchXpathUsageNormal);
  indexes.add('ConceptMap', 'targetcode', 'Code that identifies the target element', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ConceptMap', 'targetsystem', 'System of the target (if necessary)', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('ConceptMap', 'url', 'The URL of the concept map', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('ConceptMap', 'version', 'The version identifier of the concept map', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_CONDITION}
procedure TFHIRIndexBuilder.buildIndexesForCondition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Condition', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Condition', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Condition', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Condition', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('Condition', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('Condition', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Condition', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Condition', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Condition', 'age', 'Search based on Condition onsetAge', SearchParamTypeNUMBER, [], '', SearchXpathUsageNormal);
  indexes.add('Condition', 'asserter', 'Person who asserts this condition', SearchParamTypeREFERENCE, ['Practitioner', 'Patient'], '', SearchXpathUsageNormal);
  indexes.add('Condition', 'body-site', 'Anatomical location, if relevant', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Condition', 'category', 'The category of the condition', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Condition', 'clinicalstatus', 'The clinical status of the condition', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Condition', 'code', 'Code for the condition', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Condition', 'date-recorded', 'A date, when the Condition statement was documented', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Condition', 'encounter', 'Encounter when condition first asserted', SearchParamTypeREFERENCE, ['Encounter'], '', SearchXpathUsageNormal);
  indexes.add('Condition', 'evidence', 'Manifestation/symptom', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Condition', 'identifier', 'A unique identifier of the condition record', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Condition', 'onset', 'Date related onsets (dateTime and Period)', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Condition', 'onset-info', 'Other onsets (boolean, age, range, string)', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Condition', 'patient', 'Who has the condition?', SearchParamTypeREFERENCE, ['Patient'], '', SearchXpathUsageNormal);
  indexes.add('Condition', 'severity', 'The severity of the condition', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Condition', 'stage', 'Simple summary (disease specific)', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_CONFORMANCE}
procedure TFHIRIndexBuilder.buildIndexesForConformance(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Conformance', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Conformance', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Conformance', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Conformance', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('Conformance', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('Conformance', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Conformance', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Conformance', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Conformance', 'date', 'The conformance statement publication date', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Conformance', 'description', 'Text search in the description of the conformance statement', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Conformance', 'event', 'Event code in a conformance statement', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Conformance', 'fhirversion', 'The version of FHIR', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Conformance', 'format', 'formats supported (xml | json | mime type)', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Conformance', 'mode', 'Mode - restful (server/client) or messaging (sender/receiver)', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Conformance', 'name', 'Name of the conformance statement', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Conformance', 'profile', 'A profile id invoked in a conformance statement', SearchParamTypeREFERENCE, ['StructureDefinition'], '', SearchXpathUsageNormal);
  indexes.add('Conformance', 'publisher', 'Name of the publisher of the conformance statement', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Conformance', 'resource', 'Name of a resource mentioned in a conformance statement', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Conformance', 'security', 'OAuth | SMART-on-FHIR | NTLM | Basic | Kerberos | Certificates', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Conformance', 'software', 'Part of a the name of a software application', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Conformance', 'status', 'The current status of the conformance statement', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Conformance', 'supported-profile', 'Profiles for use cases supported', SearchParamTypeREFERENCE, ['StructureDefinition'], '', SearchXpathUsageNormal);
  indexes.add('Conformance', 'url', 'The uri that identifies the conformance statement', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('Conformance', 'version', 'The version identifier of the conformance statement', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_CONTRACT}
procedure TFHIRIndexBuilder.buildIndexesForContract(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Contract', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Contract', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Contract', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Contract', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('Contract', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('Contract', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Contract', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Contract', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Contract', 'actor', 'Contract Actor Type', SearchParamTypeREFERENCE, ['Practitioner', 'Group', 'Organization', 'Device', 'Patient', 'Substance', 'Contract', 'RelatedPerson', 'Location'], '', SearchXpathUsageNormal);
  indexes.add('Contract', 'identifier', 'The identity of the contract', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Contract', 'patient', 'The identity of the target of the contract (if a patient)', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, '', SearchXpathUsageNormal);
  indexes.add('Contract', 'signer', 'Contract Signatory Party', SearchParamTypeREFERENCE, ['Practitioner', 'Organization', 'Patient', 'RelatedPerson'], '', SearchXpathUsageNormal);
  indexes.add('Contract', 'subject', 'The identity of the target of the contract', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_COVERAGE}
procedure TFHIRIndexBuilder.buildIndexesForCoverage(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Coverage', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Coverage', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Coverage', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Coverage', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('Coverage', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('Coverage', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Coverage', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Coverage', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Coverage', 'dependent', 'Dependent number', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Coverage', 'group', 'Group identifier', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Coverage', 'identifier', 'The primary identifier of the insured', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Coverage', 'issuer', 'The identity of the insurer', SearchParamTypeREFERENCE, ['Organization'], '', SearchXpathUsageNormal);
  indexes.add('Coverage', 'plan', 'A plan or policy identifier', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Coverage', 'sequence', 'Sequence number', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Coverage', 'subplan', 'Sub-plan identifier', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Coverage', 'type', 'The kind of coverage', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_DATAELEMENT}
procedure TFHIRIndexBuilder.buildIndexesForDataElement(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('DataElement', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('DataElement', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DataElement', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('DataElement', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('DataElement', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('DataElement', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DataElement', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DataElement', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('DataElement', 'code', 'A code for the data element (server may choose to do subsumption)', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DataElement', 'context', 'A use context assigned to the data element', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DataElement', 'date', 'The data element publication date', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('DataElement', 'description', 'Text search in the description of the data element.  This corresponds to the definition of the first DataElement.element.', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('DataElement', 'identifier', 'The identifier of the data element', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DataElement', 'name', 'Name of the data element', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('DataElement', 'objectClass', 'Matches on the 11179-objectClass extension value', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DataElement', 'objectClassProperty', 'Matches on the 11179-objectClassProperty extension value', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DataElement', 'publisher', 'Name of the publisher of the data element', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('DataElement', 'status', 'The current status of the data element', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DataElement', 'stringency', 'The stringency of the data element definition', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DataElement', 'url', 'The official URL for the data element', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('DataElement', 'version', 'The version identifier of the data element', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_DETECTEDISSUE}
procedure TFHIRIndexBuilder.buildIndexesForDetectedIssue(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('DetectedIssue', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('DetectedIssue', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DetectedIssue', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('DetectedIssue', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('DetectedIssue', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('DetectedIssue', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DetectedIssue', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DetectedIssue', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('DetectedIssue', 'author', 'The provider or device that identified the issue', SearchParamTypeREFERENCE, ['Practitioner', 'Device'], '', SearchXpathUsageNormal);
  indexes.add('DetectedIssue', 'category', 'Issue Category, e.g. drug-drug, duplicate therapy, etc.', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DetectedIssue', 'date', 'When identified', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('DetectedIssue', 'identifier', 'Unique id for the detected issue', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DetectedIssue', 'implicated', 'Problem resource', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, '', SearchXpathUsageNormal);
  indexes.add('DetectedIssue', 'patient', 'Associated patient', SearchParamTypeREFERENCE, ['Patient'], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_DEVICE}
procedure TFHIRIndexBuilder.buildIndexesForDevice(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Device', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Device', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Device', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Device', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('Device', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('Device', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Device', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Device', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Device', 'identifier', 'Instance id from manufacturer, owner, and others', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Device', 'location', 'A location, where the resource is found', SearchParamTypeREFERENCE, ['Location'], '', SearchXpathUsageNormal);
  indexes.add('Device', 'manufacturer', 'The manufacturer of the device', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Device', 'model', 'The model of the device', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Device', 'organization', 'The organization responsible for the device', SearchParamTypeREFERENCE, ['Organization'], '', SearchXpathUsageNormal);
  indexes.add('Device', 'patient', 'Patient information, if the resource is affixed to a person', SearchParamTypeREFERENCE, ['Patient'], '', SearchXpathUsageNormal);
  indexes.add('Device', 'type', 'The type of the device', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Device', 'udi', 'FDA mandated Unique Device Identifier', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Device', 'url', 'Network address to contact device', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_DEVICECOMPONENT}
procedure TFHIRIndexBuilder.buildIndexesForDeviceComponent(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('DeviceComponent', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('DeviceComponent', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DeviceComponent', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('DeviceComponent', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('DeviceComponent', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('DeviceComponent', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DeviceComponent', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DeviceComponent', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('DeviceComponent', 'parent', 'The parent DeviceComponent resource', SearchParamTypeREFERENCE, ['DeviceComponent'], '', SearchXpathUsageNormal);
  indexes.add('DeviceComponent', 'source', 'The device source', SearchParamTypeREFERENCE, ['Device'], '', SearchXpathUsageNormal);
  indexes.add('DeviceComponent', 'type', 'The device component type', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_DEVICEMETRIC}
procedure TFHIRIndexBuilder.buildIndexesForDeviceMetric(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('DeviceMetric', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('DeviceMetric', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DeviceMetric', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('DeviceMetric', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('DeviceMetric', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('DeviceMetric', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DeviceMetric', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DeviceMetric', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('DeviceMetric', 'category', 'The category of the metric', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DeviceMetric', 'identifier', 'The identifier of the metric', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DeviceMetric', 'parent', 'The parent DeviceMetric resource', SearchParamTypeREFERENCE, ['DeviceComponent'], '', SearchXpathUsageNormal);
  indexes.add('DeviceMetric', 'source', 'The device resource', SearchParamTypeREFERENCE, ['Device'], '', SearchXpathUsageNormal);
  indexes.add('DeviceMetric', 'type', 'The component type', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_DEVICEUSEREQUEST}
procedure TFHIRIndexBuilder.buildIndexesForDeviceUseRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('DeviceUseRequest', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('DeviceUseRequest', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DeviceUseRequest', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('DeviceUseRequest', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('DeviceUseRequest', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('DeviceUseRequest', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DeviceUseRequest', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DeviceUseRequest', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('DeviceUseRequest', 'device', 'Device requested', SearchParamTypeREFERENCE, ['Device'], '', SearchXpathUsageNormal);
  indexes.add('DeviceUseRequest', 'patient', 'Search by subject - a patient', SearchParamTypeREFERENCE, ['Patient'], '', SearchXpathUsageNormal);
  indexes.add('DeviceUseRequest', 'subject', 'Search by subject', SearchParamTypeREFERENCE, ['Patient'], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_DEVICEUSESTATEMENT}
procedure TFHIRIndexBuilder.buildIndexesForDeviceUseStatement(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('DeviceUseStatement', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('DeviceUseStatement', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DeviceUseStatement', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('DeviceUseStatement', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('DeviceUseStatement', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('DeviceUseStatement', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DeviceUseStatement', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DeviceUseStatement', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('DeviceUseStatement', 'device', 'Search by device', SearchParamTypeREFERENCE, ['Device'], '', SearchXpathUsageNormal);
  indexes.add('DeviceUseStatement', 'patient', 'Search by subject - a patient', SearchParamTypeREFERENCE, ['Patient'], '', SearchXpathUsageNormal);
  indexes.add('DeviceUseStatement', 'subject', 'Search by subject', SearchParamTypeREFERENCE, ['Patient'], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_DIAGNOSTICORDER}
procedure TFHIRIndexBuilder.buildIndexesForDiagnosticOrder(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('DiagnosticOrder', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('DiagnosticOrder', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DiagnosticOrder', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('DiagnosticOrder', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('DiagnosticOrder', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('DiagnosticOrder', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DiagnosticOrder', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DiagnosticOrder', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('DiagnosticOrder', 'actor', 'Who recorded or did this', SearchParamTypeREFERENCE, ['Practitioner', 'Device'], '', SearchXpathUsageNormal);
  indexes.add('DiagnosticOrder', 'bodysite', 'Location of requested test (if applicable)', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DiagnosticOrder', 'code', 'Code to indicate the item (test or panel) being ordered', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DiagnosticOrder', 'encounter', 'The encounter that this diagnostic order is associated with', SearchParamTypeREFERENCE, ['Encounter'], '', SearchXpathUsageNormal);
  indexes.add('DiagnosticOrder', 'event-date', 'The date at which the event happened', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('DiagnosticOrder', 'event-status', 'proposed | draft | planned | requested | received | accepted | in-progress | review | completed | cancelled | suspended | rejected | failed', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DiagnosticOrder', 'event-status-date', 'A combination of past-status and date', SearchParamTypeCOMPOSITE, [], '', SearchXpathUsageNull);
  indexes.add('DiagnosticOrder', 'identifier', 'Identifiers assigned to this order', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DiagnosticOrder', 'item-date', 'The date at which the event happened', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('DiagnosticOrder', 'item-past-status', 'proposed | draft | planned | requested | received | accepted | in-progress | review | completed | cancelled | suspended | rejected | failed', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DiagnosticOrder', 'item-status', 'proposed | draft | planned | requested | received | accepted | in-progress | review | completed | cancelled | suspended | rejected | failed', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DiagnosticOrder', 'item-status-date', 'A combination of item-past-status and item-date', SearchParamTypeCOMPOSITE, [], '', SearchXpathUsageNull);
  indexes.add('DiagnosticOrder', 'orderer', 'Who ordered the test', SearchParamTypeREFERENCE, ['Practitioner'], '', SearchXpathUsageNormal);
  indexes.add('DiagnosticOrder', 'patient', 'Who and/or what test is about', SearchParamTypeREFERENCE, ['Group', 'Device', 'Patient', 'Location'], '', SearchXpathUsageNormal);
  indexes.add('DiagnosticOrder', 'specimen', 'If the whole order relates to specific specimens', SearchParamTypeREFERENCE, ['Specimen'], '', SearchXpathUsageNormal);
  indexes.add('DiagnosticOrder', 'status', 'proposed | draft | planned | requested | received | accepted | in-progress | review | completed | cancelled | suspended | rejected | failed', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DiagnosticOrder', 'subject', 'Who and/or what test is about', SearchParamTypeREFERENCE, ['Group', 'Device', 'Patient', 'Location'], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_DIAGNOSTICREPORT}
procedure TFHIRIndexBuilder.buildIndexesForDiagnosticReport(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('DiagnosticReport', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('DiagnosticReport', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DiagnosticReport', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('DiagnosticReport', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('DiagnosticReport', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('DiagnosticReport', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DiagnosticReport', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DiagnosticReport', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('DiagnosticReport', 'category', 'Which diagnostic discipline/department created the report', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DiagnosticReport', 'code', 'The code for the report as a whole, as opposed to codes for the atomic results, which are the names on the observation resource referred to from the result', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DiagnosticReport', 'date', 'The clinically relevant time of the report', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('DiagnosticReport', 'diagnosis', 'A coded diagnosis on the report', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DiagnosticReport', 'encounter', 'The Encounter when the order was made', SearchParamTypeREFERENCE, ['Encounter'], '', SearchXpathUsageNormal);
  indexes.add('DiagnosticReport', 'identifier', 'An identifier for the report', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DiagnosticReport', 'image', 'A reference to the image source.', SearchParamTypeREFERENCE, ['Media'], '', SearchXpathUsageNormal);
  indexes.add('DiagnosticReport', 'issued', 'When the report was issued', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('DiagnosticReport', 'patient', 'The subject of the report if a patient', SearchParamTypeREFERENCE, ['Group', 'Device', 'Patient', 'Location'], '', SearchXpathUsageNormal);
  indexes.add('DiagnosticReport', 'performer', 'Who was the source of the report (organization)', SearchParamTypeREFERENCE, ['Practitioner', 'Organization'], '', SearchXpathUsageNormal);
  indexes.add('DiagnosticReport', 'request', 'Reference to the test or procedure request.', SearchParamTypeREFERENCE, ['ReferralRequest', 'ProcedureRequest', 'DiagnosticOrder'], '', SearchXpathUsageNormal);
  indexes.add('DiagnosticReport', 'result', 'Link to an atomic result (observation resource)', SearchParamTypeREFERENCE, ['Observation'], '', SearchXpathUsageNormal);
  indexes.add('DiagnosticReport', 'specimen', 'The specimen details', SearchParamTypeREFERENCE, ['Specimen'], '', SearchXpathUsageNormal);
  indexes.add('DiagnosticReport', 'status', 'The status of the report', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DiagnosticReport', 'subject', 'The subject of the report', SearchParamTypeREFERENCE, ['Group', 'Device', 'Patient', 'Location'], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_DOCUMENTMANIFEST}
procedure TFHIRIndexBuilder.buildIndexesForDocumentManifest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('DocumentManifest', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('DocumentManifest', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DocumentManifest', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('DocumentManifest', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('DocumentManifest', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('DocumentManifest', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DocumentManifest', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DocumentManifest', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('DocumentManifest', 'author', 'Who and/or what authored the manifest', SearchParamTypeREFERENCE, ['Practitioner', 'Organization', 'Device', 'Patient', 'RelatedPerson'], '', SearchXpathUsageNormal);
  indexes.add('DocumentManifest', 'content-ref', 'Contents of this set of documents', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, '', SearchXpathUsageNormal);
  indexes.add('DocumentManifest', 'created', 'When this document manifest created', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('DocumentManifest', 'description', 'Human-readable description (title)', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('DocumentManifest', 'identifier', 'Unique Identifier for the set of documents', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DocumentManifest', 'patient', 'The subject of the set of documents', SearchParamTypeREFERENCE, ['Practitioner', 'Group', 'Device', 'Patient'], '', SearchXpathUsageNormal);
  indexes.add('DocumentManifest', 'recipient', 'Intended to get notified about this set of documents', SearchParamTypeREFERENCE, ['Practitioner', 'Organization', 'Patient', 'RelatedPerson'], '', SearchXpathUsageNormal);
  indexes.add('DocumentManifest', 'related-id', 'Identifiers of things that are related', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DocumentManifest', 'related-ref', 'Related Resource', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, '', SearchXpathUsageNormal);
  indexes.add('DocumentManifest', 'source', 'The source system/application/software', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('DocumentManifest', 'status', 'current | superseded | entered-in-error', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DocumentManifest', 'subject', 'The subject of the set of documents', SearchParamTypeREFERENCE, ['Practitioner', 'Group', 'Device', 'Patient'], '', SearchXpathUsageNormal);
  indexes.add('DocumentManifest', 'type', 'Kind of document set', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_DOCUMENTREFERENCE}
procedure TFHIRIndexBuilder.buildIndexesForDocumentReference(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('DocumentReference', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('DocumentReference', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DocumentReference', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('DocumentReference', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('DocumentReference', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('DocumentReference', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DocumentReference', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DocumentReference', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('DocumentReference', 'authenticator', 'Who/what authenticated the document', SearchParamTypeREFERENCE, ['Practitioner', 'Organization'], '', SearchXpathUsageNormal);
  indexes.add('DocumentReference', 'author', 'Who and/or what authored the document', SearchParamTypeREFERENCE, ['Practitioner', 'Organization', 'Device', 'Patient', 'RelatedPerson'], '', SearchXpathUsageNormal);
  indexes.add('DocumentReference', 'class', 'Categorization of document', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DocumentReference', 'created', 'Document creation time', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('DocumentReference', 'custodian', 'Organization which maintains the document', SearchParamTypeREFERENCE, ['Organization'], '', SearchXpathUsageNormal);
  indexes.add('DocumentReference', 'description', 'Human-readable description (title)', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('DocumentReference', 'encounter', 'Context of the document  content', SearchParamTypeREFERENCE, ['Encounter'], '', SearchXpathUsageNormal);
  indexes.add('DocumentReference', 'event', 'Main Clinical Acts Documented', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DocumentReference', 'facility', 'Kind of facility where patient was seen', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DocumentReference', 'format', 'Format/content rules for the document', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DocumentReference', 'identifier', 'Master Version Specific Identifier', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DocumentReference', 'indexed', 'When this document reference created', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('DocumentReference', 'language', 'Human language of the content (BCP-47)', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DocumentReference', 'location', 'Uri where the data can be found', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('DocumentReference', 'patient', 'Who/what is the subject of the document', SearchParamTypeREFERENCE, ['Practitioner', 'Group', 'Device', 'Patient'], '', SearchXpathUsageNormal);
  indexes.add('DocumentReference', 'period', 'Time of service that is being documented', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('DocumentReference', 'related-id', 'Identifier of related objects or events', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DocumentReference', 'related-ref', 'Related Resource', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, '', SearchXpathUsageNormal);
  indexes.add('DocumentReference', 'relatesto', 'Target of the relationship', SearchParamTypeREFERENCE, ['DocumentReference'], '', SearchXpathUsageNormal);
  indexes.add('DocumentReference', 'relation', 'replaces | transforms | signs | appends', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DocumentReference', 'relationship', 'Combination of relation and relatesTo', SearchParamTypeCOMPOSITE, [], '', SearchXpathUsageNull);
  indexes.add('DocumentReference', 'securitylabel', 'Document security-tags', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DocumentReference', 'setting', 'Additional details about where the content was created (e.g. clinical specialty)', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DocumentReference', 'status', 'current | superseded | entered-in-error', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('DocumentReference', 'subject', 'Who/what is the subject of the document', SearchParamTypeREFERENCE, ['Practitioner', 'Group', 'Device', 'Patient'], '', SearchXpathUsageNormal);
  indexes.add('DocumentReference', 'type', 'Kind of document (LOINC if possible)', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_ELIGIBILITYREQUEST}
procedure TFHIRIndexBuilder.buildIndexesForEligibilityRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('EligibilityRequest', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('EligibilityRequest', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('EligibilityRequest', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('EligibilityRequest', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('EligibilityRequest', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('EligibilityRequest', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('EligibilityRequest', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('EligibilityRequest', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('EligibilityRequest', 'identifier', 'The business identifier of the Eligibility', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_ELIGIBILITYRESPONSE}
procedure TFHIRIndexBuilder.buildIndexesForEligibilityResponse(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('EligibilityResponse', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('EligibilityResponse', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('EligibilityResponse', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('EligibilityResponse', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('EligibilityResponse', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('EligibilityResponse', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('EligibilityResponse', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('EligibilityResponse', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('EligibilityResponse', 'identifier', 'The business identifier of the Explanation of Benefit', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_ENCOUNTER}
procedure TFHIRIndexBuilder.buildIndexesForEncounter(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Encounter', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Encounter', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Encounter', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Encounter', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('Encounter', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('Encounter', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Encounter', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Encounter', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Encounter', 'appointment', 'The appointment that scheduled this encounter', SearchParamTypeREFERENCE, ['Appointment'], '', SearchXpathUsageNormal);
  indexes.add('Encounter', 'condition', 'Reason the encounter takes place (resource)', SearchParamTypeREFERENCE, ['Condition', 'Procedure'], '', SearchXpathUsageNormal);
  indexes.add('Encounter', 'date', 'A date within the period the Encounter lasted', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Encounter', 'episodeofcare', 'Episode(s) of care that this encounter should be recorded against', SearchParamTypeREFERENCE, ['EpisodeOfCare'], '', SearchXpathUsageNormal);
  indexes.add('Encounter', 'identifier', 'Identifier(s) by which this encounter is known', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Encounter', 'incomingreferral', 'The ReferralRequest that initiated this encounter', SearchParamTypeREFERENCE, ['ReferralRequest'], '', SearchXpathUsageNormal);
  indexes.add('Encounter', 'indication', 'Reason the encounter takes place (resource)', SearchParamTypeREFERENCE, ['Condition', 'Procedure'], '', SearchXpathUsageNormal);
  indexes.add('Encounter', 'length', 'Length of encounter in days', SearchParamTypeNUMBER, [], '', SearchXpathUsageNormal);
  indexes.add('Encounter', 'location', 'Location the encounter takes place', SearchParamTypeREFERENCE, ['Location'], '', SearchXpathUsageNormal);
  indexes.add('Encounter', 'location-period', 'Time period during which the patient was present at the location', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Encounter', 'part-of', 'Another Encounter this encounter is part of', SearchParamTypeREFERENCE, ['Encounter'], '', SearchXpathUsageNormal);
  indexes.add('Encounter', 'participant', 'Persons involved in the encounter other than the patient', SearchParamTypeREFERENCE, ['Practitioner', 'RelatedPerson'], '', SearchXpathUsageNormal);
  indexes.add('Encounter', 'participant-type', 'Role of participant in encounter', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Encounter', 'patient', 'The patient present at the encounter', SearchParamTypeREFERENCE, ['Patient'], '', SearchXpathUsageNormal);
  indexes.add('Encounter', 'practitioner', 'Persons involved in the encounter other than the patient', SearchParamTypeREFERENCE, ['Practitioner', 'RelatedPerson'], '', SearchXpathUsageNormal);
  indexes.add('Encounter', 'procedure', 'Reason the encounter takes place (resource)', SearchParamTypeREFERENCE, ['Condition', 'Procedure'], '', SearchXpathUsageNormal);
  indexes.add('Encounter', 'reason', 'Reason the encounter takes place (code)', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Encounter', 'special-arrangement', 'Wheelchair, translator, stretcher, etc.', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Encounter', 'status', 'planned | arrived | in-progress | onleave | finished | cancelled', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Encounter', 'type', 'Specific type of encounter', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_ENROLLMENTREQUEST}
procedure TFHIRIndexBuilder.buildIndexesForEnrollmentRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('EnrollmentRequest', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('EnrollmentRequest', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('EnrollmentRequest', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('EnrollmentRequest', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('EnrollmentRequest', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('EnrollmentRequest', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('EnrollmentRequest', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('EnrollmentRequest', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('EnrollmentRequest', 'identifier', 'The business identifier of the Enrollment', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('EnrollmentRequest', 'patient', 'The party to be enrolled', SearchParamTypeREFERENCE, ['Patient'], '', SearchXpathUsageNormal);
  indexes.add('EnrollmentRequest', 'subject', 'The party to be enrolled', SearchParamTypeREFERENCE, ['Patient'], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_ENROLLMENTRESPONSE}
procedure TFHIRIndexBuilder.buildIndexesForEnrollmentResponse(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('EnrollmentResponse', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('EnrollmentResponse', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('EnrollmentResponse', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('EnrollmentResponse', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('EnrollmentResponse', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('EnrollmentResponse', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('EnrollmentResponse', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('EnrollmentResponse', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('EnrollmentResponse', 'identifier', 'The business identifier of the Explanation of Benefit', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_EPISODEOFCARE}
procedure TFHIRIndexBuilder.buildIndexesForEpisodeOfCare(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('EpisodeOfCare', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('EpisodeOfCare', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('EpisodeOfCare', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('EpisodeOfCare', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('EpisodeOfCare', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('EpisodeOfCare', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('EpisodeOfCare', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('EpisodeOfCare', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('EpisodeOfCare', 'care-manager', 'Care manager/care co-ordinator for the patient', SearchParamTypeREFERENCE, ['Practitioner'], '', SearchXpathUsageNormal);
  indexes.add('EpisodeOfCare', 'condition', 'Conditions/problems/diagnoses this episode of care is for', SearchParamTypeREFERENCE, ['Condition'], '', SearchXpathUsageNormal);
  indexes.add('EpisodeOfCare', 'date', 'The provided date search value falls within the episode of care''s period', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('EpisodeOfCare', 'identifier', 'Identifier(s) for the EpisodeOfCare', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('EpisodeOfCare', 'incomingreferral', 'Incoming Referral Request', SearchParamTypeREFERENCE, ['ReferralRequest'], '', SearchXpathUsageNormal);
  indexes.add('EpisodeOfCare', 'organization', 'The organization that has assumed the specific responsibilities of this EpisodeOfCare', SearchParamTypeREFERENCE, ['Organization'], '', SearchXpathUsageNormal);
  indexes.add('EpisodeOfCare', 'patient', 'Patient for this episode of care', SearchParamTypeREFERENCE, ['Patient'], '', SearchXpathUsageNormal);
  indexes.add('EpisodeOfCare', 'status', 'The current status of the Episode of Care as provided (does not check the status history collection)', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('EpisodeOfCare', 'team-member', 'A Practitioner or Organization allocated to the care team for this EpisodeOfCare', SearchParamTypeREFERENCE, ['Practitioner', 'Organization'], '', SearchXpathUsageNormal);
  indexes.add('EpisodeOfCare', 'type', 'Type/class  - e.g. specialist referral, disease management', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_EXPLANATIONOFBENEFIT}
procedure TFHIRIndexBuilder.buildIndexesForExplanationOfBenefit(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ExplanationOfBenefit', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('ExplanationOfBenefit', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ExplanationOfBenefit', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('ExplanationOfBenefit', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('ExplanationOfBenefit', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('ExplanationOfBenefit', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ExplanationOfBenefit', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ExplanationOfBenefit', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('ExplanationOfBenefit', 'identifier', 'The business identifier of the Explanation of Benefit', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_FAMILYMEMBERHISTORY}
procedure TFHIRIndexBuilder.buildIndexesForFamilyMemberHistory(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('FamilyMemberHistory', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('FamilyMemberHistory', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('FamilyMemberHistory', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('FamilyMemberHistory', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('FamilyMemberHistory', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('FamilyMemberHistory', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('FamilyMemberHistory', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('FamilyMemberHistory', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('FamilyMemberHistory', 'code', 'A search by a condition code', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('FamilyMemberHistory', 'condition', 'Search for a history of a particular condition within a patient''s family.', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('FamilyMemberHistory', 'date', 'When history was captured/updated', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('FamilyMemberHistory', 'gender', 'A search by a gender code of a family member', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('FamilyMemberHistory', 'identifier', 'A search by a record identifier', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('FamilyMemberHistory', 'patient', 'The identity of a subject to list family member history items for', SearchParamTypeREFERENCE, ['Patient'], '', SearchXpathUsageNormal);
  indexes.add('FamilyMemberHistory', 'relationship', 'Search for family history of members based on relationship', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_FLAG}
procedure TFHIRIndexBuilder.buildIndexesForFlag(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Flag', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Flag', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Flag', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Flag', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('Flag', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('Flag', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Flag', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Flag', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Flag', 'author', 'Flag creator', SearchParamTypeREFERENCE, ['Practitioner', 'Organization', 'Device', 'Patient'], '', SearchXpathUsageNormal);
  indexes.add('Flag', 'date', 'Time period when flag is active', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Flag', 'encounter', 'Alert relevant during encounter', SearchParamTypeREFERENCE, ['Encounter'], '', SearchXpathUsageNormal);
  indexes.add('Flag', 'patient', 'The identity of a subject to list flags for', SearchParamTypeREFERENCE, ['Practitioner', 'Group', 'Organization', 'Patient', 'Location'], '', SearchXpathUsageNormal);
  indexes.add('Flag', 'subject', 'The identity of a subject to list flags for', SearchParamTypeREFERENCE, ['Practitioner', 'Group', 'Organization', 'Patient', 'Location'], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_GOAL}
procedure TFHIRIndexBuilder.buildIndexesForGoal(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Goal', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Goal', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Goal', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Goal', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('Goal', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('Goal', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Goal', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Goal', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Goal', 'category', 'E.g. Treatment, dietary, behavioral, etc.', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Goal', 'identifier', 'External Ids for this goal', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Goal', 'patient', 'Who this goal is intended for', SearchParamTypeREFERENCE, ['Group', 'Organization', 'Patient'], '', SearchXpathUsageNormal);
  indexes.add('Goal', 'status', 'proposed | planned | accepted | rejected | in-progress | achieved | sustaining | on-hold | cancelled', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Goal', 'subject', 'Who this goal is intended for', SearchParamTypeREFERENCE, ['Group', 'Organization', 'Patient'], '', SearchXpathUsageNormal);
  indexes.add('Goal', 'targetdate', 'Reach goal on or before', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_GROUP}
procedure TFHIRIndexBuilder.buildIndexesForGroup(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Group', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Group', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Group', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Group', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('Group', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('Group', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Group', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Group', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Group', 'actual', 'Descriptive or actual', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Group', 'characteristic', 'Kind of characteristic', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Group', 'characteristic-value', 'A composite of both characteristic and value', SearchParamTypeCOMPOSITE, [], '', SearchXpathUsageNull);
  indexes.add('Group', 'code', 'The kind of resources contained', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Group', 'exclude', 'Group includes or excludes', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Group', 'identifier', 'Unique id', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Group', 'member', 'Reference to the group member', SearchParamTypeREFERENCE, ['Practitioner', 'Device', 'Medication', 'Patient', 'Substance'], '', SearchXpathUsageNormal);
  indexes.add('Group', 'type', 'The type of resources the group contains', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Group', 'value', 'Value held by characteristic', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_HEALTHCARESERVICE}
procedure TFHIRIndexBuilder.buildIndexesForHealthcareService(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('HealthcareService', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('HealthcareService', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('HealthcareService', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('HealthcareService', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('HealthcareService', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('HealthcareService', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('HealthcareService', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('HealthcareService', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('HealthcareService', 'characteristic', 'One of the HealthcareService''s characteristics', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('HealthcareService', 'identifier', 'External identifiers for this item', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('HealthcareService', 'location', 'The location of the Healthcare Service', SearchParamTypeREFERENCE, ['Location'], '', SearchXpathUsageNormal);
  indexes.add('HealthcareService', 'name', 'A portion of the Healthcare service name', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('HealthcareService', 'organization', 'The organization that provides this Healthcare Service', SearchParamTypeREFERENCE, ['Organization'], '', SearchXpathUsageNormal);
  indexes.add('HealthcareService', 'programname', 'One of the Program Names serviced by this HealthcareService', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('HealthcareService', 'servicecategory', 'Service Category of the Healthcare Service', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('HealthcareService', 'servicetype', 'The type of service provided by this healthcare service', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_IMAGINGOBJECTSELECTION}
procedure TFHIRIndexBuilder.buildIndexesForImagingObjectSelection(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ImagingObjectSelection', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('ImagingObjectSelection', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ImagingObjectSelection', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('ImagingObjectSelection', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('ImagingObjectSelection', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('ImagingObjectSelection', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ImagingObjectSelection', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ImagingObjectSelection', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('ImagingObjectSelection', 'author', 'Author of key DICOM object selection', SearchParamTypeREFERENCE, ['Practitioner', 'Organization', 'Device', 'Patient', 'RelatedPerson'], '', SearchXpathUsageNormal);
  indexes.add('ImagingObjectSelection', 'authoring-time', 'Time of key DICOM object selection authoring', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('ImagingObjectSelection', 'identifier', 'UID of key DICOM object selection', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('ImagingObjectSelection', 'patient', 'Subject of key DICOM object selection', SearchParamTypeREFERENCE, ['Patient'], '', SearchXpathUsageNormal);
  indexes.add('ImagingObjectSelection', 'selected-study', 'Study selected in key DICOM object selection', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('ImagingObjectSelection', 'title', 'Title of key DICOM object selection', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_IMAGINGSTUDY}
procedure TFHIRIndexBuilder.buildIndexesForImagingStudy(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ImagingStudy', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('ImagingStudy', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ImagingStudy', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('ImagingStudy', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('ImagingStudy', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('ImagingStudy', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ImagingStudy', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ImagingStudy', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('ImagingStudy', 'accession', 'The accession identifier for the study', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ImagingStudy', 'bodysite', 'The body site studied', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ImagingStudy', 'dicom-class', 'The type of the instance', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('ImagingStudy', 'modality', 'The modality of the series', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ImagingStudy', 'order', 'The order for the image', SearchParamTypeREFERENCE, ['DiagnosticOrder'], '', SearchXpathUsageNormal);
  indexes.add('ImagingStudy', 'patient', 'Who the study is about', SearchParamTypeREFERENCE, ['Patient'], '', SearchXpathUsageNormal);
  indexes.add('ImagingStudy', 'series', 'The identifier of the series of images', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('ImagingStudy', 'started', 'When the study was started', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('ImagingStudy', 'study', 'The study identifier for the image', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('ImagingStudy', 'uid', 'The instance unique identifier', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_IMMUNIZATION}
procedure TFHIRIndexBuilder.buildIndexesForImmunization(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Immunization', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Immunization', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Immunization', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Immunization', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('Immunization', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('Immunization', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Immunization', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Immunization', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Immunization', 'date', 'Vaccination  (non)-Administration Date', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Immunization', 'dose-sequence', 'Dose number within series', SearchParamTypeNUMBER, [], '', SearchXpathUsageNormal);
  indexes.add('Immunization', 'identifier', 'Business identifier', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Immunization', 'location', 'The service delivery location or facility in which the vaccine was / was to be administered', SearchParamTypeREFERENCE, ['Location'], '', SearchXpathUsageNormal);
  indexes.add('Immunization', 'lot-number', 'Vaccine Lot Number', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Immunization', 'manufacturer', 'Vaccine Manufacturer', SearchParamTypeREFERENCE, ['Organization'], '', SearchXpathUsageNormal);
  indexes.add('Immunization', 'notgiven', 'Administrations which were not given', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Immunization', 'patient', 'The patient for the vaccination record', SearchParamTypeREFERENCE, ['Patient'], '', SearchXpathUsageNormal);
  indexes.add('Immunization', 'performer', 'The practitioner who administered the vaccination', SearchParamTypeREFERENCE, ['Practitioner'], '', SearchXpathUsageNormal);
  indexes.add('Immunization', 'reaction', 'Additional information on reaction', SearchParamTypeREFERENCE, ['Observation'], '', SearchXpathUsageNormal);
  indexes.add('Immunization', 'reaction-date', 'When reaction started', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Immunization', 'reason', 'Why immunization occurred', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Immunization', 'reason-not-given', 'Explanation of reason vaccination was not administered', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Immunization', 'requester', 'The practitioner who ordered the vaccination', SearchParamTypeREFERENCE, ['Practitioner'], '', SearchXpathUsageNormal);
  indexes.add('Immunization', 'status', 'Immunization event status', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Immunization', 'vaccine-code', 'Vaccine Product Administered', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_IMMUNIZATIONRECOMMENDATION}
procedure TFHIRIndexBuilder.buildIndexesForImmunizationRecommendation(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ImmunizationRecommendation', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('ImmunizationRecommendation', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ImmunizationRecommendation', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('ImmunizationRecommendation', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('ImmunizationRecommendation', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('ImmunizationRecommendation', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ImmunizationRecommendation', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ImmunizationRecommendation', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('ImmunizationRecommendation', 'date', 'Date recommendation created', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('ImmunizationRecommendation', 'dose-number', 'Recommended dose number', SearchParamTypeNUMBER, [], '', SearchXpathUsageNormal);
  indexes.add('ImmunizationRecommendation', 'dose-sequence', 'Dose number within sequence', SearchParamTypeNUMBER, [], '', SearchXpathUsageNormal);
  indexes.add('ImmunizationRecommendation', 'identifier', 'Business identifier', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ImmunizationRecommendation', 'information', 'Patient observations supporting recommendation', SearchParamTypeREFERENCE, ['AllergyIntolerance', 'Observation'], '', SearchXpathUsageNormal);
  indexes.add('ImmunizationRecommendation', 'patient', 'Who this profile is for', SearchParamTypeREFERENCE, ['Patient'], '', SearchXpathUsageNormal);
  indexes.add('ImmunizationRecommendation', 'status', 'Vaccine administration status', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ImmunizationRecommendation', 'support', 'Past immunizations supporting recommendation', SearchParamTypeREFERENCE, ['Immunization'], '', SearchXpathUsageNormal);
  indexes.add('ImmunizationRecommendation', 'vaccine-type', 'Vaccine recommendation applies to', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_IMPLEMENTATIONGUIDE}
procedure TFHIRIndexBuilder.buildIndexesForImplementationGuide(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ImplementationGuide', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('ImplementationGuide', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ImplementationGuide', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('ImplementationGuide', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('ImplementationGuide', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('ImplementationGuide', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ImplementationGuide', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ImplementationGuide', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('ImplementationGuide', 'context', 'A use context assigned to the structure', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ImplementationGuide', 'date', 'The implementation guide publication date', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('ImplementationGuide', 'dependency', 'Where to find dependency', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('ImplementationGuide', 'description', 'Text search in the description of the implementation guide', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('ImplementationGuide', 'experimental', 'If for testing purposes, not real usage', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ImplementationGuide', 'name', 'Name of the implementation guide', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('ImplementationGuide', 'publisher', 'Name of the publisher of the implementation guide', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('ImplementationGuide', 'status', 'The current status of the implementation guide', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ImplementationGuide', 'url', 'Absolute URL used to reference this Implementation Guide', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('ImplementationGuide', 'version', 'The version identifier of the implementation guide', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_LIST}
procedure TFHIRIndexBuilder.buildIndexesForList(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('List', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('List', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('List', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('List', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('List', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('List', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('List', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('List', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('List', 'code', 'What the purpose of this list is', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('List', 'date', 'When the list was prepared', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('List', 'empty-reason', 'Why list is empty', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('List', 'encounter', 'Context in which list created', SearchParamTypeREFERENCE, ['Encounter'], '', SearchXpathUsageNormal);
  indexes.add('List', 'item', 'Actual entry', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, '', SearchXpathUsageNormal);
  indexes.add('List', 'notes', 'Comments about the list', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('List', 'patient', 'If all resources have the same subject', SearchParamTypeREFERENCE, ['Group', 'Device', 'Patient', 'Location'], '', SearchXpathUsageNormal);
  indexes.add('List', 'source', 'Who and/or what defined the list contents (aka Author)', SearchParamTypeREFERENCE, ['Practitioner', 'Device', 'Patient'], '', SearchXpathUsageNormal);
  indexes.add('List', 'status', 'current | retired | entered-in-error', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('List', 'subject', 'If all resources have the same subject', SearchParamTypeREFERENCE, ['Group', 'Device', 'Patient', 'Location'], '', SearchXpathUsageNormal);
  indexes.add('List', 'title', 'Descriptive name for the list', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_LOCATION}
procedure TFHIRIndexBuilder.buildIndexesForLocation(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Location', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Location', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Location', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Location', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('Location', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('Location', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Location', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Location', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Location', 'address', 'A (part of the) address of the location', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Location', 'address-city', 'A city specified in an address', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Location', 'address-country', 'A country specified in an address', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Location', 'address-postalcode', 'A postal code specified in an address', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Location', 'address-state', 'A state specified in an address', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Location', 'address-use', 'A use code specified in an address', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Location', 'identifier', 'Unique code or number identifying the location to its users', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Location', 'name', 'A (portion of the) name of the location', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Location', 'near', 'The coordinates expressed as [lat],[long] (using the WGS84 datum, see notes) to find locations near to (servers may search using a square rather than a circle for efficiency)', SearchParamTypeTOKEN, [], '', SearchXpathUsageNearby);
  indexes.add('Location', 'near-distance', 'A distance quantity to limit the near search to locations within a specific distance', SearchParamTypeTOKEN, [], '', SearchXpathUsageDistance);
  indexes.add('Location', 'organization', 'Searches for locations that are managed by the provided organization', SearchParamTypeREFERENCE, ['Organization'], '', SearchXpathUsageNormal);
  indexes.add('Location', 'partof', 'The location of which this location is a part', SearchParamTypeREFERENCE, ['Location'], '', SearchXpathUsageNormal);
  indexes.add('Location', 'status', 'Searches for locations with a specific kind of status', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Location', 'type', 'A code for the type of location', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_MEDIA}
procedure TFHIRIndexBuilder.buildIndexesForMedia(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Media', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Media', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Media', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Media', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('Media', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('Media', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Media', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Media', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Media', 'created', 'Date attachment was first created', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Media', 'identifier', 'Identifier(s) for the image', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Media', 'operator', 'The person who generated the image', SearchParamTypeREFERENCE, ['Practitioner'], '', SearchXpathUsageNormal);
  indexes.add('Media', 'patient', 'Who/What this Media is a record of', SearchParamTypeREFERENCE, ['Practitioner', 'Specimen', 'Group', 'Device', 'Patient'], '', SearchXpathUsageNormal);
  indexes.add('Media', 'subject', 'Who/What this Media is a record of', SearchParamTypeREFERENCE, ['Practitioner', 'Specimen', 'Group', 'Device', 'Patient'], '', SearchXpathUsageNormal);
  indexes.add('Media', 'subtype', 'The type of acquisition equipment/process', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Media', 'type', 'photo | video | audio', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Media', 'view', 'Imaging view, e.g. Lateral or Antero-posterior', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_MEDICATION}
procedure TFHIRIndexBuilder.buildIndexesForMedication(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Medication', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Medication', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Medication', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Medication', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('Medication', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('Medication', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Medication', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Medication', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Medication', 'code', 'Codes that identify this medication', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Medication', 'container', 'E.g. box, vial, blister-pack', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Medication', 'content', 'A product in the package', SearchParamTypeREFERENCE, ['Medication'], '', SearchXpathUsageNormal);
  indexes.add('Medication', 'form', 'powder | tablets | carton +', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Medication', 'ingredient', 'The product contained', SearchParamTypeREFERENCE, ['Medication', 'Substance'], '', SearchXpathUsageNormal);
  indexes.add('Medication', 'manufacturer', 'Manufacturer of the item', SearchParamTypeREFERENCE, ['Organization'], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_MEDICATIONADMINISTRATION}
procedure TFHIRIndexBuilder.buildIndexesForMedicationAdministration(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('MedicationAdministration', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('MedicationAdministration', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('MedicationAdministration', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('MedicationAdministration', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('MedicationAdministration', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('MedicationAdministration', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('MedicationAdministration', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('MedicationAdministration', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('MedicationAdministration', 'code', 'Return administrations of this medication code', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('MedicationAdministration', 'device', 'Return administrations with this administration device identity', SearchParamTypeREFERENCE, ['Device'], '', SearchXpathUsageNormal);
  indexes.add('MedicationAdministration', 'effectivetime', 'Date administration happened (or did not happen)', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('MedicationAdministration', 'encounter', 'Return administrations that share this encounter', SearchParamTypeREFERENCE, ['Encounter'], '', SearchXpathUsageNormal);
  indexes.add('MedicationAdministration', 'identifier', 'Return administrations with this external identifier', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('MedicationAdministration', 'medication', 'Return administrations of this medication resource', SearchParamTypeREFERENCE, ['Medication'], '', SearchXpathUsageNormal);
  indexes.add('MedicationAdministration', 'notgiven', 'Administrations that were not made', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('MedicationAdministration', 'patient', 'The identity of a patient to list administrations  for', SearchParamTypeREFERENCE, ['Patient'], '', SearchXpathUsageNormal);
  indexes.add('MedicationAdministration', 'practitioner', 'Who administered substance', SearchParamTypeREFERENCE, ['Practitioner', 'Patient', 'RelatedPerson'], '', SearchXpathUsageNormal);
  indexes.add('MedicationAdministration', 'prescription', 'The identity of a prescription to list administrations from', SearchParamTypeREFERENCE, ['MedicationOrder'], '', SearchXpathUsageNormal);
  indexes.add('MedicationAdministration', 'status', 'MedicationAdministration event status (for example one of active/paused/completed/nullified)', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_MEDICATIONDISPENSE}
procedure TFHIRIndexBuilder.buildIndexesForMedicationDispense(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('MedicationDispense', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('MedicationDispense', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('MedicationDispense', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('MedicationDispense', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('MedicationDispense', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('MedicationDispense', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('MedicationDispense', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('MedicationDispense', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('MedicationDispense', 'code', 'Return dispenses of this medicine code', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('MedicationDispense', 'destination', 'Return dispenses that should be sent to a specific destination', SearchParamTypeREFERENCE, ['Location'], '', SearchXpathUsageNormal);
  indexes.add('MedicationDispense', 'dispenser', 'Return all dispenses performed by a specific individual', SearchParamTypeREFERENCE, ['Practitioner'], '', SearchXpathUsageNormal);
  indexes.add('MedicationDispense', 'identifier', 'Return dispenses with this external identifier', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('MedicationDispense', 'medication', 'Return dispenses of this medicine resource', SearchParamTypeREFERENCE, ['Medication'], '', SearchXpathUsageNormal);
  indexes.add('MedicationDispense', 'patient', 'The identity of a patient to list dispenses  for', SearchParamTypeREFERENCE, ['Patient'], '', SearchXpathUsageNormal);
  indexes.add('MedicationDispense', 'prescription', 'The identity of a prescription to list dispenses from', SearchParamTypeREFERENCE, ['MedicationOrder'], '', SearchXpathUsageNormal);
  indexes.add('MedicationDispense', 'receiver', 'Who collected the medication', SearchParamTypeREFERENCE, ['Practitioner', 'Patient'], '', SearchXpathUsageNormal);
  indexes.add('MedicationDispense', 'responsibleparty', 'Return all dispenses with the specified responsible party', SearchParamTypeREFERENCE, ['Practitioner'], '', SearchXpathUsageNormal);
  indexes.add('MedicationDispense', 'status', 'Status of the dispense', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('MedicationDispense', 'type', 'Return all dispenses of a specific type', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('MedicationDispense', 'whenhandedover', 'Date when medication handed over to patient (outpatient setting), or supplied to ward or clinic (inpatient setting)', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('MedicationDispense', 'whenprepared', 'Date when medication prepared', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_MEDICATIONORDER}
procedure TFHIRIndexBuilder.buildIndexesForMedicationOrder(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('MedicationOrder', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('MedicationOrder', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('MedicationOrder', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('MedicationOrder', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('MedicationOrder', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('MedicationOrder', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('MedicationOrder', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('MedicationOrder', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('MedicationOrder', 'code', 'Return administrations of this medication code', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('MedicationOrder', 'datewritten', 'Return prescriptions written on this date', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('MedicationOrder', 'encounter', 'Return prescriptions with this encounter identifier', SearchParamTypeREFERENCE, ['Encounter'], '', SearchXpathUsageNormal);
  indexes.add('MedicationOrder', 'identifier', 'Return prescriptions with this external identifier', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('MedicationOrder', 'medication', 'Return administrations of this medication reference', SearchParamTypeREFERENCE, ['Medication'], '', SearchXpathUsageNormal);
  indexes.add('MedicationOrder', 'patient', 'The identity of a patient to list orders  for', SearchParamTypeREFERENCE, ['Patient'], '', SearchXpathUsageNormal);
  indexes.add('MedicationOrder', 'prescriber', 'Who ordered the medication(s)', SearchParamTypeREFERENCE, ['Practitioner'], '', SearchXpathUsageNormal);
  indexes.add('MedicationOrder', 'status', 'Status of the prescription', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_MEDICATIONSTATEMENT}
procedure TFHIRIndexBuilder.buildIndexesForMedicationStatement(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('MedicationStatement', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('MedicationStatement', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('MedicationStatement', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('MedicationStatement', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('MedicationStatement', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('MedicationStatement', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('MedicationStatement', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('MedicationStatement', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('MedicationStatement', 'code', 'Return administrations of this medication code', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('MedicationStatement', 'effectivedate', 'Date when patient was taking (or not taking) the medication', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('MedicationStatement', 'identifier', 'Return statements with this external identifier', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('MedicationStatement', 'medication', 'Return administrations of this medication reference', SearchParamTypeREFERENCE, ['Medication'], '', SearchXpathUsageNormal);
  indexes.add('MedicationStatement', 'patient', 'The identity of a patient to list statements  for', SearchParamTypeREFERENCE, ['Patient'], '', SearchXpathUsageNormal);
  indexes.add('MedicationStatement', 'source', 'Who the information in the statement came from', SearchParamTypeREFERENCE, ['Practitioner', 'Patient', 'RelatedPerson'], '', SearchXpathUsageNormal);
  indexes.add('MedicationStatement', 'status', 'Return statements that match the given status', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_MESSAGEHEADER}
procedure TFHIRIndexBuilder.buildIndexesForMessageHeader(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('MessageHeader', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('MessageHeader', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('MessageHeader', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('MessageHeader', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('MessageHeader', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('MessageHeader', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('MessageHeader', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('MessageHeader', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('MessageHeader', 'author', 'The source of the decision', SearchParamTypeREFERENCE, ['Practitioner'], '', SearchXpathUsageNormal);
  indexes.add('MessageHeader', 'code', 'ok | transient-error | fatal-error', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('MessageHeader', 'data', 'The actual content of the message', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, '', SearchXpathUsageNormal);
  indexes.add('MessageHeader', 'destination', 'Name of system', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('MessageHeader', 'destination-uri', 'Actual destination address or id', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('MessageHeader', 'enterer', 'The source of the data entry', SearchParamTypeREFERENCE, ['Practitioner'], '', SearchXpathUsageNormal);
  indexes.add('MessageHeader', 'event', 'Code for the event this message represents', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('MessageHeader', 'receiver', 'Intended "real-world" recipient for the data', SearchParamTypeREFERENCE, ['Practitioner', 'Organization'], '', SearchXpathUsageNormal);
  indexes.add('MessageHeader', 'response-id', 'Id of original message', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('MessageHeader', 'responsible', 'Final responsibility for event', SearchParamTypeREFERENCE, ['Practitioner', 'Organization'], '', SearchXpathUsageNormal);
  indexes.add('MessageHeader', 'source', 'Name of system', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('MessageHeader', 'source-uri', 'Actual message source address or id', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('MessageHeader', 'target', 'Particular delivery destination within the destination', SearchParamTypeREFERENCE, ['Device'], '', SearchXpathUsageNormal);
  indexes.add('MessageHeader', 'timestamp', 'Time that the message was sent', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_NAMINGSYSTEM}
procedure TFHIRIndexBuilder.buildIndexesForNamingSystem(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('NamingSystem', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('NamingSystem', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('NamingSystem', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('NamingSystem', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('NamingSystem', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('NamingSystem', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('NamingSystem', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('NamingSystem', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('NamingSystem', 'contact', 'Name of a individual to contact', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('NamingSystem', 'context', 'Content intends to support these contexts', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('NamingSystem', 'date', 'Publication Date(/time)', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('NamingSystem', 'id-type', 'oid | uuid | uri | other', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('NamingSystem', 'kind', 'codesystem | identifier | root', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('NamingSystem', 'name', 'Human-readable label', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('NamingSystem', 'period', 'When is identifier valid?', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('NamingSystem', 'publisher', 'Name of the publisher (Organization or individual)', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('NamingSystem', 'replaced-by', 'Use this instead', SearchParamTypeREFERENCE, ['NamingSystem'], '', SearchXpathUsageNormal);
  indexes.add('NamingSystem', 'responsible', 'Who maintains system namespace?', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('NamingSystem', 'status', 'draft | active | retired', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('NamingSystem', 'telecom', 'Contact details for individual or publisher', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('NamingSystem', 'type', 'e.g. driver,  provider,  patient, bank etc.', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('NamingSystem', 'value', 'The unique identifier', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_NUTRITIONORDER}
procedure TFHIRIndexBuilder.buildIndexesForNutritionOrder(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('NutritionOrder', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('NutritionOrder', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('NutritionOrder', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('NutritionOrder', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('NutritionOrder', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('NutritionOrder', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('NutritionOrder', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('NutritionOrder', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('NutritionOrder', 'additive', 'Type of module component to add to the feeding', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('NutritionOrder', 'datetime', 'Return nutrition orders requested on this date', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('NutritionOrder', 'encounter', 'Return nutrition orders with this encounter identifier', SearchParamTypeREFERENCE, ['Encounter'], '', SearchXpathUsageNormal);
  indexes.add('NutritionOrder', 'formula', 'Type of enteral or infant formula', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('NutritionOrder', 'identifier', 'Return nutrition orders with this external identifier', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('NutritionOrder', 'oraldiet', 'Type of diet that can be consumed orally (i.e., take via the mouth).', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('NutritionOrder', 'patient', 'The identity of the person who requires the diet, formula or nutritional supplement', SearchParamTypeREFERENCE, ['Patient'], '', SearchXpathUsageNormal);
  indexes.add('NutritionOrder', 'provider', 'The identify of the provider who placed the nutrition order', SearchParamTypeREFERENCE, ['Practitioner'], '', SearchXpathUsageNormal);
  indexes.add('NutritionOrder', 'status', 'Status of the nutrition order.', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('NutritionOrder', 'supplement', 'Type of supplement product requested', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_OBSERVATION}
procedure TFHIRIndexBuilder.buildIndexesForObservation(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Observation', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Observation', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Observation', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Observation', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('Observation', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('Observation', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Observation', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Observation', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Observation', 'category', 'The classification of the type of observation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Observation', 'code', 'The code of the observation type', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Observation', 'code-value-[x]', 'Both code and one of the value parameters', SearchParamTypeCOMPOSITE, [], '', SearchXpathUsageNull);
  indexes.add('Observation', 'component-code', 'The component code of the observation type', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Observation', 'component-code-value-[x]', 'Both component code and one of the component value parameters', SearchParamTypeCOMPOSITE, [], '', SearchXpathUsageNull);
  indexes.add('Observation', 'component-data-absent-reason', 'The reason why the expected value in the element Observation.component.value[x] is missing.', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Observation', 'component-value-concept', 'The value of the component observation, if the value is a CodeableConcept', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Observation', 'component-value-quantity', 'The value of the component observation, if the value is a Quantity, or a SampledData (just search on the bounds of the values in sampled data)', SearchParamTypeQUANTITY, [], '', SearchXpathUsageNormal);
  indexes.add('Observation', 'component-value-string', 'The value of the component observation, if the value is a string, and also searches in CodeableConcept.text', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Observation', 'data-absent-reason', 'The reason why the expected value in the element Observation.value[x] is missing.', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Observation', 'date', 'Obtained date/time. If the obtained element is a period, a date that falls in the period', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Observation', 'device', 'The Device that generated the observation data.', SearchParamTypeREFERENCE, ['Device', 'DeviceMetric'], '', SearchXpathUsageNormal);
  indexes.add('Observation', 'encounter', 'Healthcare event related to the observation', SearchParamTypeREFERENCE, ['Encounter'], '', SearchXpathUsageNormal);
  indexes.add('Observation', 'identifier', 'The unique id for a particular observation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Observation', 'patient', 'The subject that the observation is about (if patient)', SearchParamTypeREFERENCE, ['Group', 'Device', 'Patient', 'Location'], '', SearchXpathUsageNormal);
  indexes.add('Observation', 'performer', 'Who performed the observation', SearchParamTypeREFERENCE, ['Practitioner', 'Organization', 'Patient', 'RelatedPerson'], '', SearchXpathUsageNormal);
  indexes.add('Observation', 'related', 'Related Observations - search on related-type and related-target together', SearchParamTypeCOMPOSITE, [], '', SearchXpathUsageNull);
  indexes.add('Observation', 'related-target', 'Resource that is related to this one', SearchParamTypeREFERENCE, ['Observation', 'QuestionnaireResponse'], '', SearchXpathUsageNormal);
  indexes.add('Observation', 'related-type', 'has-member | derived-from | sequel-to | replaces | qualified-by | interfered-by', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Observation', 'specimen', 'Specimen used for this observation', SearchParamTypeREFERENCE, ['Specimen'], '', SearchXpathUsageNormal);
  indexes.add('Observation', 'status', 'The status of the observation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Observation', 'subject', 'The subject that the observation is about', SearchParamTypeREFERENCE, ['Group', 'Device', 'Patient', 'Location'], '', SearchXpathUsageNormal);
  indexes.add('Observation', 'value-concept', 'The value of the observation, if the value is a CodeableConcept', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Observation', 'value-date', 'The value of the observation, if the value is a date or period of time', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Observation', 'value-quantity', 'The value of the observation, if the value is a Quantity, or a SampledData (just search on the bounds of the values in sampled data)', SearchParamTypeQUANTITY, [], '', SearchXpathUsageNormal);
  indexes.add('Observation', 'value-string', 'The value of the observation, if the value is a string, and also searches in CodeableConcept.text', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_OPERATIONDEFINITION}
procedure TFHIRIndexBuilder.buildIndexesForOperationDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('OperationDefinition', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('OperationDefinition', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('OperationDefinition', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('OperationDefinition', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('OperationDefinition', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('OperationDefinition', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('OperationDefinition', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('OperationDefinition', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('OperationDefinition', 'base', 'Marks this as a profile of the base', SearchParamTypeREFERENCE, ['OperationDefinition'], '', SearchXpathUsageNormal);
  indexes.add('OperationDefinition', 'code', 'Name used to invoke the operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('OperationDefinition', 'date', 'Date for this version of the operation definition', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('OperationDefinition', 'instance', 'Invoke on an instance?', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('OperationDefinition', 'kind', 'operation | query', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('OperationDefinition', 'name', 'Informal name for this operation', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('OperationDefinition', 'profile', 'Profile on the type', SearchParamTypeREFERENCE, ['StructureDefinition'], '', SearchXpathUsageNormal);
  indexes.add('OperationDefinition', 'publisher', 'Name of the publisher (Organization or individual)', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('OperationDefinition', 'status', 'draft | active | retired', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('OperationDefinition', 'system', 'Invoke at the system level?', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('OperationDefinition', 'type', 'Invoke at resource level for these type', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('OperationDefinition', 'url', 'Logical URL to reference this operation definition', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('OperationDefinition', 'version', 'Logical id for this version of the operation definition', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_OPERATIONOUTCOME}
procedure TFHIRIndexBuilder.buildIndexesForOperationOutcome(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('OperationOutcome', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('OperationOutcome', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('OperationOutcome', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('OperationOutcome', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('OperationOutcome', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('OperationOutcome', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('OperationOutcome', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('OperationOutcome', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
end;
{$ENDIF}

{$IFDEF FHIR_ORDER}
procedure TFHIRIndexBuilder.buildIndexesForOrder(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Order', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Order', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Order', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Order', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('Order', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('Order', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Order', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Order', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Order', 'date', 'When the order was made', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Order', 'detail', 'What action is being ordered', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, '', SearchXpathUsageNormal);
  indexes.add('Order', 'identifier', 'Instance id from source, target, and/or  others', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Order', 'patient', 'Patient this order is about', SearchParamTypeREFERENCE, ['Group', 'Device', 'Patient', 'Substance'], '', SearchXpathUsageNormal);
  indexes.add('Order', 'source', 'Who initiated the order', SearchParamTypeREFERENCE, ['Practitioner', 'Organization'], '', SearchXpathUsageNormal);
  indexes.add('Order', 'subject', 'Patient this order is about', SearchParamTypeREFERENCE, ['Group', 'Device', 'Patient', 'Substance'], '', SearchXpathUsageNormal);
  indexes.add('Order', 'target', 'Who is intended to fulfill the order', SearchParamTypeREFERENCE, ['Practitioner', 'Organization', 'Device'], '', SearchXpathUsageNormal);
  indexes.add('Order', 'when', 'A formal schedule', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Order', 'when_code', 'Code specifies when request should be done. The code may simply be a priority code', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_ORDERRESPONSE}
procedure TFHIRIndexBuilder.buildIndexesForOrderResponse(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('OrderResponse', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('OrderResponse', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('OrderResponse', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('OrderResponse', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('OrderResponse', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('OrderResponse', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('OrderResponse', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('OrderResponse', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('OrderResponse', 'code', 'pending | review | rejected | error | accepted | cancelled | replaced | aborted | completed', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('OrderResponse', 'date', 'When the response was made', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('OrderResponse', 'fulfillment', 'Details of the outcome of performing the order', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, '', SearchXpathUsageNormal);
  indexes.add('OrderResponse', 'identifier', 'Identifiers assigned to this order by the orderer or by the receiver', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('OrderResponse', 'request', 'The order that this is a response to', SearchParamTypeREFERENCE, ['Order'], '', SearchXpathUsageNormal);
  indexes.add('OrderResponse', 'who', 'Who made the response', SearchParamTypeREFERENCE, ['Practitioner', 'Organization', 'Device'], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_ORGANIZATION}
procedure TFHIRIndexBuilder.buildIndexesForOrganization(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Organization', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Organization', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Organization', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Organization', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('Organization', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('Organization', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Organization', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Organization', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Organization', 'active', 'Whether the organization''s record is active', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Organization', 'address', 'A (part of the) address of the Organization', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Organization', 'address-city', 'A city specified in an address', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Organization', 'address-country', 'A country specified in an address', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Organization', 'address-postalcode', 'A postal code specified in an address', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Organization', 'address-state', 'A state specified in an address', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Organization', 'address-use', 'A use code specified in an address', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Organization', 'identifier', 'Any identifier for the organization (not the accreditation issuer''s identifier)', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Organization', 'name', 'A portion of the organization''s name', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Organization', 'partof', 'Search all organizations that are part of the given organization', SearchParamTypeREFERENCE, ['Organization'], '', SearchXpathUsageNormal);
  indexes.add('Organization', 'phonetic', 'A portion of the organization''s name using some kind of phonetic matching algorithm', SearchParamTypeSTRING, [], '', SearchXpathUsagePhonetic);
  indexes.add('Organization', 'type', 'A code for the type of organization', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_PATIENT}
procedure TFHIRIndexBuilder.buildIndexesForPatient(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Patient', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Patient', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Patient', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Patient', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('Patient', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('Patient', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Patient', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Patient', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Patient', 'active', 'Whether the patient record is active', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Patient', 'address', 'An address in any kind of address/part of the patient', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Patient', 'address-city', 'A city specified in an address', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Patient', 'address-country', 'A country specified in an address', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Patient', 'address-postalcode', 'A postalCode specified in an address', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Patient', 'address-state', 'A state specified in an address', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Patient', 'address-use', 'A use code specified in an address', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Patient', 'animal-breed', 'The breed for animal patients', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Patient', 'animal-species', 'The species for animal patients', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Patient', 'birthdate', 'The patient''s date of birth', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Patient', 'careprovider', 'Patient''s nominated care provider, could be a care manager, not the organization that manages the record', SearchParamTypeREFERENCE, ['Practitioner', 'Organization'], '', SearchXpathUsageNormal);
  indexes.add('Patient', 'deathdate', 'The date of death has been provided and satisfies this search value', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Patient', 'deceased', 'This patient has been marked as deceased, or as a death date entered', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Patient', 'email', 'A value in an email contact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Patient', 'ethnicity', 'Returns patients with an ethnicity extension matching the specified code.', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Patient', 'family', 'A portion of the family name of the patient', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Patient', 'gender', 'Gender of the patient', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Patient', 'given', 'A portion of the given name of the patient', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Patient', 'identifier', 'A patient identifier', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Patient', 'language', 'Language code (irrespective of use value)', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Patient', 'link', 'All patients linked to the given patient', SearchParamTypeREFERENCE, ['Patient'], '', SearchXpathUsageNormal);
  indexes.add('Patient', 'name', 'A portion of either family or given name of the patient', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Patient', 'organization', 'The organization at which this person is a patient', SearchParamTypeREFERENCE, ['Organization'], '', SearchXpathUsageNormal);
  indexes.add('Patient', 'phone', 'A value in a phone contact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Patient', 'phonetic', 'A portion of either family or given name using some kind of phonetic matching algorithm', SearchParamTypeSTRING, [], '', SearchXpathUsagePhonetic);
  indexes.add('Patient', 'race', 'Returns patients with a race extension matching the specified code.', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Patient', 'telecom', 'The value in any kind of telecom details of the patient', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_PAYMENTNOTICE}
procedure TFHIRIndexBuilder.buildIndexesForPaymentNotice(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('PaymentNotice', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('PaymentNotice', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('PaymentNotice', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('PaymentNotice', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('PaymentNotice', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('PaymentNotice', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('PaymentNotice', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('PaymentNotice', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('PaymentNotice', 'identifier', 'The business identifier of the Eligibility', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_PAYMENTRECONCILIATION}
procedure TFHIRIndexBuilder.buildIndexesForPaymentReconciliation(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('PaymentReconciliation', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('PaymentReconciliation', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('PaymentReconciliation', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('PaymentReconciliation', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('PaymentReconciliation', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('PaymentReconciliation', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('PaymentReconciliation', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('PaymentReconciliation', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('PaymentReconciliation', 'identifier', 'The business identifier of the Explanation of Benefit', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_PERSON}
procedure TFHIRIndexBuilder.buildIndexesForPerson(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Person', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Person', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Person', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Person', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('Person', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('Person', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Person', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Person', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Person', 'address', 'An address in any kind of address/part', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Person', 'address-city', 'A city specified in an address', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Person', 'address-country', 'A country specified in an address', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Person', 'address-postalcode', 'A postal code specified in an address', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Person', 'address-state', 'A state specified in an address', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Person', 'address-use', 'A use code specified in an address', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Person', 'birthdate', 'The person''s date of birth', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Person', 'email', 'A value in an email contact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Person', 'gender', 'The gender of the person', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Person', 'identifier', 'A person Identifier', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Person', 'link', 'Any link has this Patient, Person, RelatedPerson or Practitioner reference', SearchParamTypeREFERENCE, ['Practitioner', 'Patient', 'Person', 'RelatedPerson'], '', SearchXpathUsageNormal);
  indexes.add('Person', 'name', 'A portion of name in any name part', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Person', 'organization', 'The organization at which this person record is being managed', SearchParamTypeREFERENCE, ['Organization'], '', SearchXpathUsageNormal);
  indexes.add('Person', 'patient', 'The Person links to this Patient', SearchParamTypeREFERENCE, ['Practitioner', 'Patient', 'Person', 'RelatedPerson'], '', SearchXpathUsageNormal);
  indexes.add('Person', 'phone', 'A value in a phone contact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Person', 'phonetic', 'A portion of name using some kind of phonetic matching algorithm', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Person', 'practitioner', 'The Person links to this Practitioner', SearchParamTypeREFERENCE, ['Practitioner', 'Patient', 'Person', 'RelatedPerson'], '', SearchXpathUsageNormal);
  indexes.add('Person', 'relatedperson', 'The Person links to this RelatedPerson', SearchParamTypeREFERENCE, ['Practitioner', 'Patient', 'Person', 'RelatedPerson'], '', SearchXpathUsageNormal);
  indexes.add('Person', 'telecom', 'The value in any kind of contact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_PRACTITIONER}
procedure TFHIRIndexBuilder.buildIndexesForPractitioner(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Practitioner', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Practitioner', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Practitioner', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Practitioner', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('Practitioner', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('Practitioner', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Practitioner', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Practitioner', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Practitioner', 'address', 'An address in any kind of address/part', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Practitioner', 'address-city', 'A city specified in an address', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Practitioner', 'address-country', 'A country specified in an address', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Practitioner', 'address-postalcode', 'A postalCode specified in an address', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Practitioner', 'address-state', 'A state specified in an address', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Practitioner', 'address-use', 'A use code specified in an address', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Practitioner', 'communication', 'One of the languages that the practitioner can communicate with', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Practitioner', 'email', 'A value in an email contact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Practitioner', 'family', 'A portion of the family name', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Practitioner', 'gender', 'Gender of the practitioner', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Practitioner', 'given', 'A portion of the given name', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Practitioner', 'identifier', 'A practitioner''s Identifier', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Practitioner', 'location', 'One of the locations at which this practitioner provides care', SearchParamTypeREFERENCE, ['Location'], '', SearchXpathUsageNormal);
  indexes.add('Practitioner', 'name', 'A portion of either family or given name', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Practitioner', 'organization', 'The identity of the organization the practitioner represents / acts on behalf of', SearchParamTypeREFERENCE, ['Organization'], '', SearchXpathUsageNormal);
  indexes.add('Practitioner', 'phone', 'A value in a phone contact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Practitioner', 'phonetic', 'A portion of either family or given name using some kind of phonetic matching algorithm', SearchParamTypeSTRING, [], '', SearchXpathUsagePhonetic);
  indexes.add('Practitioner', 'role', 'The practitioner can perform this role at for the organization', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Practitioner', 'specialty', 'The practitioner has this specialty at an organization', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Practitioner', 'telecom', 'The value in any kind of contact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_PROCEDURE}
procedure TFHIRIndexBuilder.buildIndexesForProcedure(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Procedure', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Procedure', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Procedure', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Procedure', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('Procedure', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('Procedure', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Procedure', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Procedure', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Procedure', 'code', 'A code to identify a  procedure', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Procedure', 'date', 'Date/Period the procedure was performed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Procedure', 'encounter', 'The encounter associated with the procedure', SearchParamTypeREFERENCE, ['Encounter'], '', SearchXpathUsageNormal);
  indexes.add('Procedure', 'identifier', 'A unique identifier for a procedure', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Procedure', 'location', 'Where the procedure happened', SearchParamTypeREFERENCE, ['Location'], '', SearchXpathUsageNormal);
  indexes.add('Procedure', 'patient', 'Search by subject - a patient', SearchParamTypeREFERENCE, ['Group', 'Patient'], '', SearchXpathUsageNormal);
  indexes.add('Procedure', 'performer', 'The reference to the practitioner', SearchParamTypeREFERENCE, ['Practitioner', 'Organization', 'Patient', 'RelatedPerson'], '', SearchXpathUsageNormal);
  indexes.add('Procedure', 'subject', 'Search by subject', SearchParamTypeREFERENCE, ['Group', 'Patient'], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_PROCEDUREREQUEST}
procedure TFHIRIndexBuilder.buildIndexesForProcedureRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ProcedureRequest', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('ProcedureRequest', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ProcedureRequest', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('ProcedureRequest', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('ProcedureRequest', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('ProcedureRequest', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ProcedureRequest', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ProcedureRequest', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('ProcedureRequest', 'encounter', 'Encounter request created during', SearchParamTypeREFERENCE, ['Encounter'], '', SearchXpathUsageNormal);
  indexes.add('ProcedureRequest', 'identifier', 'A unique identifier of the Procedure Request', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ProcedureRequest', 'orderer', 'Who made request', SearchParamTypeREFERENCE, ['Practitioner', 'Device', 'Patient', 'RelatedPerson'], '', SearchXpathUsageNormal);
  indexes.add('ProcedureRequest', 'patient', 'Search by subject - a patient', SearchParamTypeREFERENCE, ['Group', 'Patient'], '', SearchXpathUsageNormal);
  indexes.add('ProcedureRequest', 'performer', 'Who should perform the procedure', SearchParamTypeREFERENCE, ['Practitioner', 'Organization', 'Patient', 'RelatedPerson'], '', SearchXpathUsageNormal);
  indexes.add('ProcedureRequest', 'subject', 'Search by subject', SearchParamTypeREFERENCE, ['Group', 'Patient'], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_PROCESSREQUEST}
procedure TFHIRIndexBuilder.buildIndexesForProcessRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ProcessRequest', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('ProcessRequest', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ProcessRequest', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('ProcessRequest', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('ProcessRequest', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('ProcessRequest', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ProcessRequest', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ProcessRequest', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('ProcessRequest', 'action', 'The action requested by this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ProcessRequest', 'identifier', 'The business identifier of the ProcessRequest', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ProcessRequest', 'organization', 'The organization who generated this request', SearchParamTypeREFERENCE, ['Organization'], '', SearchXpathUsageNormal);
  indexes.add('ProcessRequest', 'provider', 'The provider who regenerated this request', SearchParamTypeREFERENCE, ['Practitioner'], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_PROCESSRESPONSE}
procedure TFHIRIndexBuilder.buildIndexesForProcessResponse(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ProcessResponse', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('ProcessResponse', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ProcessResponse', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('ProcessResponse', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('ProcessResponse', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('ProcessResponse', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ProcessResponse', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ProcessResponse', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('ProcessResponse', 'identifier', 'The business identifier of the Explanation of Benefit', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ProcessResponse', 'organization', 'The organization who generated this resource', SearchParamTypeREFERENCE, ['Organization'], '', SearchXpathUsageNormal);
  indexes.add('ProcessResponse', 'request', 'The reference to the claim', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, '', SearchXpathUsageNormal);
  indexes.add('ProcessResponse', 'requestorganization', 'The Organization who is responsible the request transaction', SearchParamTypeREFERENCE, ['Organization'], '', SearchXpathUsageNormal);
  indexes.add('ProcessResponse', 'requestprovider', 'The Provider who is responsible the request transaction', SearchParamTypeREFERENCE, ['Practitioner'], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_PROVENANCE}
procedure TFHIRIndexBuilder.buildIndexesForProvenance(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Provenance', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Provenance', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Provenance', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Provenance', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('Provenance', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('Provenance', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Provenance', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Provenance', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Provenance', 'agent', 'Individual, device or organization playing role', SearchParamTypeREFERENCE, ['Practitioner', 'Organization', 'Device', 'Patient', 'RelatedPerson'], '', SearchXpathUsageNormal);
  indexes.add('Provenance', 'end', 'End time with inclusive boundary, if not ongoing', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Provenance', 'entity', 'Identity of entity', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('Provenance', 'entitytype', 'The type of resource in this entity', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Provenance', 'location', 'Where the activity occurred, if relevant', SearchParamTypeREFERENCE, ['Location'], '', SearchXpathUsageNormal);
  indexes.add('Provenance', 'patient', 'Target Reference(s) (usually version specific)', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, '', SearchXpathUsageNormal);
  indexes.add('Provenance', 'sigtype', 'Indication of the reason the entity signed the object(s)', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Provenance', 'start', 'Starting time with inclusive boundary', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Provenance', 'target', 'Target Reference(s) (usually version specific)', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, '', SearchXpathUsageNormal);
  indexes.add('Provenance', 'userid', 'Authorization-system identifier for the agent', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_QUESTIONNAIRE}
procedure TFHIRIndexBuilder.buildIndexesForQuestionnaire(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Questionnaire', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Questionnaire', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Questionnaire', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Questionnaire', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('Questionnaire', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('Questionnaire', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Questionnaire', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Questionnaire', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Questionnaire', 'code', 'A code that corresponds to the questionnaire or one of its groups', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Questionnaire', 'date', 'When the questionnaire was last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Questionnaire', 'identifier', 'An identifier for the questionnaire', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Questionnaire', 'publisher', 'The author of the questionnaire', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Questionnaire', 'status', 'The status of the questionnaire', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Questionnaire', 'title', 'All or part of the name of the questionnaire (title for the root group of the questionnaire)', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Questionnaire', 'version', 'The business version of the questionnaire', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_QUESTIONNAIRERESPONSE}
procedure TFHIRIndexBuilder.buildIndexesForQuestionnaireResponse(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('QuestionnaireResponse', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('QuestionnaireResponse', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('QuestionnaireResponse', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('QuestionnaireResponse', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('QuestionnaireResponse', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('QuestionnaireResponse', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('QuestionnaireResponse', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('QuestionnaireResponse', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('QuestionnaireResponse', 'author', 'The author of the questionnaire', SearchParamTypeREFERENCE, ['Practitioner', 'Device', 'Patient', 'RelatedPerson'], '', SearchXpathUsageNormal);
  indexes.add('QuestionnaireResponse', 'authored', 'When the questionnaire was authored', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('QuestionnaireResponse', 'encounter', 'Encounter during which questionnaire was authored', SearchParamTypeREFERENCE, ['Encounter'], '', SearchXpathUsageNormal);
  indexes.add('QuestionnaireResponse', 'patient', 'The patient that is the subject of the questionnaire', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, '', SearchXpathUsageNormal);
  indexes.add('QuestionnaireResponse', 'questionnaire', 'The questionnaire the answers are provided for', SearchParamTypeREFERENCE, ['Questionnaire'], '', SearchXpathUsageNormal);
  indexes.add('QuestionnaireResponse', 'source', 'The person who answered the questions', SearchParamTypeREFERENCE, ['Practitioner', 'Patient', 'RelatedPerson'], '', SearchXpathUsageNormal);
  indexes.add('QuestionnaireResponse', 'status', 'The status of the questionnaire response', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('QuestionnaireResponse', 'subject', 'The subject of the questionnaire', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPE_NAMES, '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_REFERRALREQUEST}
procedure TFHIRIndexBuilder.buildIndexesForReferralRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ReferralRequest', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('ReferralRequest', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ReferralRequest', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('ReferralRequest', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('ReferralRequest', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('ReferralRequest', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ReferralRequest', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ReferralRequest', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('ReferralRequest', 'date', 'Creation or activation date', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('ReferralRequest', 'patient', 'Who the referral is about', SearchParamTypeREFERENCE, ['Patient'], '', SearchXpathUsageNormal);
  indexes.add('ReferralRequest', 'priority', 'The priority assigned to the referral', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ReferralRequest', 'recipient', 'The person that the referral was sent to', SearchParamTypeREFERENCE, ['Practitioner', 'Organization'], '', SearchXpathUsageNormal);
  indexes.add('ReferralRequest', 'requester', 'Requester of referral / transfer of care', SearchParamTypeREFERENCE, ['Practitioner', 'Organization', 'Patient'], '', SearchXpathUsageNormal);
  indexes.add('ReferralRequest', 'specialty', 'The specialty that the referral is for', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ReferralRequest', 'status', 'The status of the referral', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ReferralRequest', 'type', 'The type of the referral', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_RELATEDPERSON}
procedure TFHIRIndexBuilder.buildIndexesForRelatedPerson(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('RelatedPerson', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('RelatedPerson', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('RelatedPerson', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('RelatedPerson', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('RelatedPerson', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('RelatedPerson', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('RelatedPerson', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('RelatedPerson', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('RelatedPerson', 'address', 'An address in any kind of address/part', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('RelatedPerson', 'address-city', 'A city specified in an address', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('RelatedPerson', 'address-country', 'A country specified in an address', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('RelatedPerson', 'address-postalcode', 'A postal code specified in an address', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('RelatedPerson', 'address-state', 'A state specified in an address', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('RelatedPerson', 'address-use', 'A use code specified in an address', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('RelatedPerson', 'birthdate', 'The Related Person''s date of birth', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('RelatedPerson', 'email', 'A value in an email contact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('RelatedPerson', 'gender', 'Gender of the person', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('RelatedPerson', 'identifier', 'A patient Identifier', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('RelatedPerson', 'name', 'A portion of name in any name part', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('RelatedPerson', 'patient', 'The patient this person is related to', SearchParamTypeREFERENCE, ['Patient'], '', SearchXpathUsageNormal);
  indexes.add('RelatedPerson', 'phone', 'A value in a phone contact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('RelatedPerson', 'phonetic', 'A portion of name using some kind of phonetic matching algorithm', SearchParamTypeSTRING, [], '', SearchXpathUsagePhonetic);
  indexes.add('RelatedPerson', 'telecom', 'The value in any kind of contact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_RISKASSESSMENT}
procedure TFHIRIndexBuilder.buildIndexesForRiskAssessment(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('RiskAssessment', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('RiskAssessment', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('RiskAssessment', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('RiskAssessment', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('RiskAssessment', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('RiskAssessment', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('RiskAssessment', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('RiskAssessment', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('RiskAssessment', 'condition', 'Condition assessed', SearchParamTypeREFERENCE, ['Condition'], '', SearchXpathUsageNormal);
  indexes.add('RiskAssessment', 'date', 'When was assessment made?', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('RiskAssessment', 'encounter', 'Where was assessment performed?', SearchParamTypeREFERENCE, ['Encounter'], '', SearchXpathUsageNormal);
  indexes.add('RiskAssessment', 'identifier', 'Unique identifier for the assessment', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('RiskAssessment', 'method', 'Evaluation mechanism', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('RiskAssessment', 'patient', 'Who/what does assessment apply to?', SearchParamTypeREFERENCE, ['Group', 'Patient'], '', SearchXpathUsageNormal);
  indexes.add('RiskAssessment', 'performer', 'Who did assessment?', SearchParamTypeREFERENCE, ['Practitioner', 'Device'], '', SearchXpathUsageNormal);
  indexes.add('RiskAssessment', 'subject', 'Who/what does assessment apply to?', SearchParamTypeREFERENCE, ['Group', 'Patient'], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_SCHEDULE}
procedure TFHIRIndexBuilder.buildIndexesForSchedule(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Schedule', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Schedule', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Schedule', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Schedule', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('Schedule', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('Schedule', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Schedule', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Schedule', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Schedule', 'actor', 'The individual(HealthcareService, Practitioner, Location, ...) to find a Schedule for', SearchParamTypeREFERENCE, ['Practitioner', 'Device', 'Patient', 'HealthcareService', 'RelatedPerson', 'Location'], '', SearchXpathUsageNormal);
  indexes.add('Schedule', 'date', 'Search for Schedule resources that have a period that contains this date specified', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Schedule', 'identifier', 'A Schedule Identifier', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Schedule', 'type', 'The type of appointments that can be booked into associated slot(s)', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_SEARCHPARAMETER}
procedure TFHIRIndexBuilder.buildIndexesForSearchParameter(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('SearchParameter', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('SearchParameter', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('SearchParameter', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('SearchParameter', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('SearchParameter', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('SearchParameter', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('SearchParameter', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('SearchParameter', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('SearchParameter', 'base', 'The resource type this search parameter applies to', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('SearchParameter', 'code', 'Code used in URL', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('SearchParameter', 'description', 'Documentation for  search parameter', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('SearchParameter', 'name', 'Informal name for this search parameter', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('SearchParameter', 'target', 'Types of resource (if a resource reference)', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('SearchParameter', 'type', 'number | date | string | token | reference | composite | quantity | uri', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('SearchParameter', 'url', 'Absolute URL used to reference this search parameter', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_SLOT}
procedure TFHIRIndexBuilder.buildIndexesForSlot(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Slot', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Slot', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Slot', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Slot', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('Slot', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('Slot', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Slot', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Slot', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Slot', 'fb-type', 'The free/busy status of the appointment', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Slot', 'identifier', 'A Slot Identifier', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Slot', 'schedule', 'The Schedule Resource that we are seeking a slot within', SearchParamTypeREFERENCE, ['Schedule'], '', SearchXpathUsageNormal);
  indexes.add('Slot', 'slot-type', 'The type of appointments that can be booked into the slot', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Slot', 'start', 'Appointment date/time.', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_SPECIMEN}
procedure TFHIRIndexBuilder.buildIndexesForSpecimen(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Specimen', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Specimen', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Specimen', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Specimen', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('Specimen', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('Specimen', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Specimen', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Specimen', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Specimen', 'accession', 'The accession number associated with the specimen', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Specimen', 'bodysite', 'The code for the body site from where the specimen originated', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Specimen', 'collected', 'The date the specimen was collected', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Specimen', 'collector', 'Who collected the specimen', SearchParamTypeREFERENCE, ['Practitioner'], '', SearchXpathUsageNormal);
  indexes.add('Specimen', 'container', 'The kind of specimen container', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Specimen', 'container-id', 'The unique identifier associated with the specimen container', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Specimen', 'identifier', 'The unique identifier associated with the specimen', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Specimen', 'parent', 'The parent of the specimen', SearchParamTypeREFERENCE, ['Specimen'], '', SearchXpathUsageNormal);
  indexes.add('Specimen', 'patient', 'The patient the specimen comes from', SearchParamTypeREFERENCE, ['Group', 'Device', 'Patient', 'Substance'], '', SearchXpathUsageNormal);
  indexes.add('Specimen', 'subject', 'The subject of the specimen', SearchParamTypeREFERENCE, ['Group', 'Device', 'Patient', 'Substance'], '', SearchXpathUsageNormal);
  indexes.add('Specimen', 'type', 'The specimen type', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_STRUCTUREDEFINITION}
procedure TFHIRIndexBuilder.buildIndexesForStructureDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('StructureDefinition', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('StructureDefinition', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('StructureDefinition', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('StructureDefinition', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('StructureDefinition', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('StructureDefinition', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('StructureDefinition', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('StructureDefinition', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('StructureDefinition', 'abstract', 'Whether the structure is abstract', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('StructureDefinition', 'base', 'Structure that this set of constraints applies to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('StructureDefinition', 'base-path', 'Path that identifies the base element', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('StructureDefinition', 'code', 'A code for the profile', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('StructureDefinition', 'context', 'A use context assigned to the structure', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('StructureDefinition', 'context-type', 'resource | datatype | mapping | extension', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('StructureDefinition', 'date', 'The profile publication date', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('StructureDefinition', 'description', 'Text search in the description of the profile', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('StructureDefinition', 'display', 'Use this name when displaying the value', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('StructureDefinition', 'experimental', 'If for testing purposes, not real usage', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('StructureDefinition', 'ext-context', 'Where the extension can be used in instances', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('StructureDefinition', 'identifier', 'The identifier of the profile', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('StructureDefinition', 'kind', 'datatype | resource | logical', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('StructureDefinition', 'name', 'Name of the profile', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('StructureDefinition', 'path', 'A path that is constrained in the profile', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('StructureDefinition', 'publisher', 'Name of the publisher of the profile', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('StructureDefinition', 'status', 'The current status of the profile', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('StructureDefinition', 'type', 'Any datatype or resource, including abstract ones', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('StructureDefinition', 'url', 'Absolute URL used to reference this StructureDefinition', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('StructureDefinition', 'valueset', 'A vocabulary binding reference', SearchParamTypeREFERENCE, ['ValueSet'], '', SearchXpathUsageNormal);
  indexes.add('StructureDefinition', 'version', 'The version identifier of the profile', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_SUBSCRIPTION}
procedure TFHIRIndexBuilder.buildIndexesForSubscription(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Subscription', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Subscription', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Subscription', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Subscription', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('Subscription', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('Subscription', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Subscription', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Subscription', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Subscription', 'contact', 'Contact details for source (e.g. troubleshooting)', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Subscription', 'criteria', 'Rule for server push criteria', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Subscription', 'payload', 'Mimetype to send, or blank for no payload', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('Subscription', 'status', 'requested | active | error | off', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Subscription', 'tag', 'A tag to add to matching resources', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Subscription', 'type', 'rest-hook | websocket | email | sms | message', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Subscription', 'url', 'Where the channel points to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_SUBSTANCE}
procedure TFHIRIndexBuilder.buildIndexesForSubstance(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Substance', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Substance', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Substance', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Substance', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('Substance', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('Substance', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Substance', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Substance', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('Substance', 'category', 'The category of the substance', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Substance', 'code', 'The code of the substance', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Substance', 'container-identifier', 'Identifier of the package/container', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Substance', 'expiry', 'Expiry date of package or container of substance', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('Substance', 'identifier', 'Unique identifier for the substance', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('Substance', 'quantity', 'Amount of substance in the package', SearchParamTypeQUANTITY, [], '', SearchXpathUsageNormal);
  indexes.add('Substance', 'substance', 'A component of the substance', SearchParamTypeREFERENCE, ['Substance'], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_SUPPLYDELIVERY}
procedure TFHIRIndexBuilder.buildIndexesForSupplyDelivery(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('SupplyDelivery', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('SupplyDelivery', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('SupplyDelivery', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('SupplyDelivery', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('SupplyDelivery', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('SupplyDelivery', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('SupplyDelivery', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('SupplyDelivery', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('SupplyDelivery', 'identifier', 'External identifier', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('SupplyDelivery', 'patient', 'Patient for whom the item is supplied', SearchParamTypeREFERENCE, ['Patient'], '', SearchXpathUsageNormal);
  indexes.add('SupplyDelivery', 'receiver', 'Who collected the Supply', SearchParamTypeREFERENCE, ['Practitioner'], '', SearchXpathUsageNormal);
  indexes.add('SupplyDelivery', 'status', 'in-progress | completed | abandoned', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('SupplyDelivery', 'supplier', 'Dispenser', SearchParamTypeREFERENCE, ['Practitioner'], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_SUPPLYREQUEST}
procedure TFHIRIndexBuilder.buildIndexesForSupplyRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('SupplyRequest', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('SupplyRequest', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('SupplyRequest', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('SupplyRequest', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('SupplyRequest', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('SupplyRequest', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('SupplyRequest', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('SupplyRequest', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('SupplyRequest', 'date', 'When the request was made', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('SupplyRequest', 'identifier', 'Unique identifier', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('SupplyRequest', 'kind', 'The kind of supply (central, non-stock, etc.)', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('SupplyRequest', 'patient', 'Patient for whom the item is supplied', SearchParamTypeREFERENCE, ['Patient'], '', SearchXpathUsageNormal);
  indexes.add('SupplyRequest', 'source', 'Who initiated this order', SearchParamTypeREFERENCE, ['Practitioner', 'Organization', 'Patient'], '', SearchXpathUsageNormal);
  indexes.add('SupplyRequest', 'status', 'requested | completed | failed | cancelled', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('SupplyRequest', 'supplier', 'Who is intended to fulfill the request', SearchParamTypeREFERENCE, ['Organization'], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_TESTSCRIPT}
procedure TFHIRIndexBuilder.buildIndexesForTestScript(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('TestScript', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('TestScript', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('TestScript', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('TestScript', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('TestScript', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('TestScript', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('TestScript', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('TestScript', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('TestScript', 'description', 'Natural language description of the TestScript', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('TestScript', 'identifier', 'External identifier', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('TestScript', 'name', 'Informal name for this TestScript', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('TestScript', 'testscript-capability', 'TestScript required and validated capability', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('TestScript', 'testscript-setup-capability', 'TestScript setup required and validated capability', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('TestScript', 'testscript-test-capability', 'TestScript test required and validated capability', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('TestScript', 'url', 'Absolute URL used to reference this TestScript', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_VALUESET}
procedure TFHIRIndexBuilder.buildIndexesForValueSet(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ValueSet', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('ValueSet', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ValueSet', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('ValueSet', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('ValueSet', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('ValueSet', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ValueSet', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ValueSet', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('ValueSet', 'code', 'A code defined in the value set', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ValueSet', 'context', 'A use context assigned to the value set', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ValueSet', 'date', 'The value set publication date', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('ValueSet', 'description', 'Text search in the description of the value set', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('ValueSet', 'expansion', 'Uniquely identifies this expansion', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('ValueSet', 'identifier', 'The identifier for the value set', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ValueSet', 'name', 'The name of the value set', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('ValueSet', 'publisher', 'Name of the publisher of the value set', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add('ValueSet', 'reference', 'A code system included or excluded in the value set or an imported value set', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('ValueSet', 'status', 'The status of the value set', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('ValueSet', 'system', 'The system for any codes defined by this value set', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('ValueSet', 'url', 'The logical URL for the value set', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('ValueSet', 'version', 'The version identifier of the value set', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;
{$ENDIF}

{$IFDEF FHIR_VISIONPRESCRIPTION}
procedure TFHIRIndexBuilder.buildIndexesForVisionPrescription(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('VisionPrescription', '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('VisionPrescription', '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('VisionPrescription', '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('VisionPrescription', '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add('VisionPrescription', '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNull);
  indexes.add('VisionPrescription', '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('VisionPrescription', '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('VisionPrescription', '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNull);
  indexes.add('VisionPrescription', 'datewritten', 'Return prescriptions written on this date', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add('VisionPrescription', 'encounter', 'Return prescriptions with this encounter identifier', SearchParamTypeREFERENCE, ['Encounter'], '', SearchXpathUsageNormal);
  indexes.add('VisionPrescription', 'identifier', 'Return prescriptions with this external identifier', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add('VisionPrescription', 'patient', 'The identity of a patient to list dispenses for', SearchParamTypeREFERENCE, ['Patient'], '', SearchXpathUsageNormal);
  indexes.add('VisionPrescription', 'prescriber', 'Who authorizes the vision product', SearchParamTypeREFERENCE, ['Practitioner'], '', SearchXpathUsageNormal);
end;
{$ENDIF}

procedure TFHIRIndexBuilder.registerIndexes(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  {$IFDEF FHIR_ACCOUNT}
  buildIndexesForAccount(Indexes, compartments);
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
  {$IFDEF FHIR_CAREPLAN}
  buildIndexesForCarePlan(Indexes, compartments);
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
  {$IFDEF FHIR_COMMUNICATION}
  buildIndexesForCommunication(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_COMMUNICATIONREQUEST}
  buildIndexesForCommunicationRequest(Indexes, compartments);
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
  {$IFDEF FHIR_CONFORMANCE}
  buildIndexesForConformance(Indexes, compartments);
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
  {$IFDEF FHIR_DEVICEUSEREQUEST}
  buildIndexesForDeviceUseRequest(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_DEVICEUSESTATEMENT}
  buildIndexesForDeviceUseStatement(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_DIAGNOSTICORDER}
  buildIndexesForDiagnosticOrder(Indexes, compartments);
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
  {$IFDEF FHIR_ENROLLMENTREQUEST}
  buildIndexesForEnrollmentRequest(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_ENROLLMENTRESPONSE}
  buildIndexesForEnrollmentResponse(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_EPISODEOFCARE}
  buildIndexesForEpisodeOfCare(Indexes, compartments);
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
  {$IFDEF FHIR_GROUP}
  buildIndexesForGroup(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_HEALTHCARESERVICE}
  buildIndexesForHealthcareService(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_IMAGINGOBJECTSELECTION}
  buildIndexesForImagingObjectSelection(Indexes, compartments);
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
  {$IFDEF FHIR_LIST}
  buildIndexesForList(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_LOCATION}
  buildIndexesForLocation(Indexes, compartments);
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
  {$IFDEF FHIR_MEDICATIONORDER}
  buildIndexesForMedicationOrder(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_MEDICATIONSTATEMENT}
  buildIndexesForMedicationStatement(Indexes, compartments);
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
  {$IFDEF FHIR_ORDER}
  buildIndexesForOrder(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_ORDERRESPONSE}
  buildIndexesForOrderResponse(Indexes, compartments);
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
  {$IFDEF FHIR_PRACTITIONER}
  buildIndexesForPractitioner(Indexes, compartments);
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
  {$IFDEF FHIR_RISKASSESSMENT}
  buildIndexesForRiskAssessment(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_SCHEDULE}
  buildIndexesForSchedule(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_SEARCHPARAMETER}
  buildIndexesForSearchParameter(Indexes, compartments);
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

