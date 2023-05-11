unit fhir2_indexinfo;

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
  fsl_base, fsl_utilities, fsl_stream,
  fhir_common,
  fhir2_resources, fhir2_types, fhir2_constants, fhir_indexing;

Type

  TFHIRIndexBuilderR2 = class (TFHIRIndexBuilder)
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
    {$IFDEF FHIR_PARAMETERS}
    procedure buildIndexesForParameters(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
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
    procedure registerIndexes(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList); override;
  end;
  TFHIRIndexBuilderX = TFHIRIndexBuilderR2;

implementation

{$IFDEF FHIR_ACCOUNT}
procedure TFHIRIndexBuilderR2.buildIndexesForAccount(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Account', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Account', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('Account', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('Account', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('Account', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('Account', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Account', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Account', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Account', 'balance', 'How much is in account?', sptQUANTITY, [], '', sxpNormal);
  indexes.add('Account', 'identifier', 'Account number', sptTOKEN, [], '', sxpNormal);
  indexes.add('Account', 'name', 'Human-readable label', sptSTRING, [], '', sxpNormal);
  indexes.add('Account', 'owner', 'Who is responsible?', sptREFERENCE, ['Organization'], '', sxpNormal);
  indexes.add('Account', 'patient', 'What is account tied to?', sptREFERENCE, ['Practitioner', 'Organization', 'Device', 'Patient', 'HealthcareService', 'Location'], '', sxpNormal);
  indexes.add('Account', 'period', 'Transaction window', sptDATE, [], '', sxpNormal);
  indexes.add('Account', 'status', 'active | inactive', sptTOKEN, [], '', sxpNormal);
  indexes.add('Account', 'subject', 'What is account tied to?', sptREFERENCE, ['Practitioner', 'Organization', 'Device', 'Patient', 'HealthcareService', 'Location'], '', sxpNormal);
  indexes.add('Account', 'type', 'E.g. patient, expense, depreciation', sptTOKEN, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_ALLERGYINTOLERANCE}
procedure TFHIRIndexBuilderR2.buildIndexesForAllergyIntolerance(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('AllergyIntolerance', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('AllergyIntolerance', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('AllergyIntolerance', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('AllergyIntolerance', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('AllergyIntolerance', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('AllergyIntolerance', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('AllergyIntolerance', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('AllergyIntolerance', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('AllergyIntolerance', 'category', 'food | medication | environment | other - Category of Substance', sptTOKEN, [], '', sxpNormal);
  indexes.add('AllergyIntolerance', 'criticality', 'CRITL | CRITH | CRITU', sptTOKEN, [], '', sxpNormal);
  indexes.add('AllergyIntolerance', 'date', 'When recorded', sptDATE, [], '', sxpNormal);
  indexes.add('AllergyIntolerance', 'identifier', 'External ids for this item', sptTOKEN, [], '', sxpNormal);
  indexes.add('AllergyIntolerance', 'last-date', 'Date(/time) of last known occurrence of a reaction', sptDATE, [], '', sxpNormal);
  indexes.add('AllergyIntolerance', 'manifestation', 'Clinical symptoms/signs associated with the Event', sptTOKEN, [], '', sxpNormal);
  indexes.add('AllergyIntolerance', 'onset', 'Date(/time) when manifestations showed', sptDATE, [], '', sxpNormal);
  indexes.add('AllergyIntolerance', 'patient', 'Who the sensitivity is for', sptREFERENCE, ['Patient'], '', sxpNormal);
  indexes.add('AllergyIntolerance', 'recorder', 'Who recorded the sensitivity', sptREFERENCE, ['Practitioner', 'Patient'], '', sxpNormal);
  indexes.add('AllergyIntolerance', 'reporter', 'Source of the information about the allergy', sptREFERENCE, ['Practitioner', 'Patient', 'RelatedPerson'], '', sxpNormal);
  indexes.add('AllergyIntolerance', 'route', 'How the subject was exposed to the substance', sptTOKEN, [], '', sxpNormal);
  indexes.add('AllergyIntolerance', 'severity', 'mild | moderate | severe (of event as a whole)', sptTOKEN, [], '', sxpNormal);
  indexes.add('AllergyIntolerance', 'status', 'active | unconfirmed | confirmed | inactive | resolved | refuted | entered-in-error', sptTOKEN, [], '', sxpNormal);
  indexes.add('AllergyIntolerance', 'substance', 'Substance, (or class) considered to be responsible for risk', sptTOKEN, [], '', sxpNormal);
  indexes.add('AllergyIntolerance', 'type', 'allergy | intolerance - Underlying mechanism (if known)', sptTOKEN, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_APPOINTMENT}
procedure TFHIRIndexBuilderR2.buildIndexesForAppointment(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Appointment', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Appointment', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('Appointment', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('Appointment', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('Appointment', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('Appointment', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Appointment', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Appointment', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Appointment', 'actor', 'Any one of the individuals participating in the appointment', sptREFERENCE, ['Practitioner', 'Device', 'Patient', 'HealthcareService', 'RelatedPerson', 'Location'], '', sxpNormal);
  indexes.add('Appointment', 'date', 'Appointment date/time.', sptDATE, [], '', sxpNormal);
  indexes.add('Appointment', 'identifier', 'An Identifier of the Appointment', sptTOKEN, [], '', sxpNormal);
  indexes.add('Appointment', 'location', 'This location is listed in the participants of the appointment', sptREFERENCE, ['Practitioner', 'Device', 'Patient', 'HealthcareService', 'RelatedPerson', 'Location'], '', sxpNormal);
  indexes.add('Appointment', 'part-status', 'The Participation status of the subject, or other participant on the appointment. Can be used to locate participants that have not responded to meeting requests.', sptTOKEN, [], '', sxpNormal);
  indexes.add('Appointment', 'patient', 'One of the individuals of the appointment is this patient', sptREFERENCE, ['Practitioner', 'Device', 'Patient', 'HealthcareService', 'RelatedPerson', 'Location'], '', sxpNormal);
  indexes.add('Appointment', 'practitioner', 'One of the individuals of the appointment is this practitioner', sptREFERENCE, ['Practitioner', 'Device', 'Patient', 'HealthcareService', 'RelatedPerson', 'Location'], '', sxpNormal);
  indexes.add('Appointment', 'status', 'The overall status of the appointment', sptTOKEN, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_APPOINTMENTRESPONSE}
procedure TFHIRIndexBuilderR2.buildIndexesForAppointmentResponse(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('AppointmentResponse', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('AppointmentResponse', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('AppointmentResponse', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('AppointmentResponse', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('AppointmentResponse', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('AppointmentResponse', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('AppointmentResponse', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('AppointmentResponse', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('AppointmentResponse', 'actor', 'The Person, Location/HealthcareService or Device that this appointment response replies for', sptREFERENCE, ['Practitioner', 'Device', 'Patient', 'HealthcareService', 'RelatedPerson', 'Location'], '', sxpNormal);
  indexes.add('AppointmentResponse', 'appointment', 'The appointment that the response is attached to', sptREFERENCE, ['Appointment'], '', sxpNormal);
  indexes.add('AppointmentResponse', 'identifier', 'An Identifier in this appointment response', sptTOKEN, [], '', sxpNormal);
  indexes.add('AppointmentResponse', 'location', 'This Response is for this Location', sptREFERENCE, ['Practitioner', 'Device', 'Patient', 'HealthcareService', 'RelatedPerson', 'Location'], '', sxpNormal);
  indexes.add('AppointmentResponse', 'part-status', 'The participants acceptance status for this appointment', sptTOKEN, [], '', sxpNormal);
  indexes.add('AppointmentResponse', 'patient', 'This Response is for this Patient', sptREFERENCE, ['Practitioner', 'Device', 'Patient', 'HealthcareService', 'RelatedPerson', 'Location'], '', sxpNormal);
  indexes.add('AppointmentResponse', 'practitioner', 'This Response is for this Practitioner', sptREFERENCE, ['Practitioner', 'Device', 'Patient', 'HealthcareService', 'RelatedPerson', 'Location'], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_AUDITEVENT}
procedure TFHIRIndexBuilderR2.buildIndexesForAuditEvent(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('AuditEvent', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('AuditEvent', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('AuditEvent', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('AuditEvent', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('AuditEvent', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('AuditEvent', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('AuditEvent', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('AuditEvent', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('AuditEvent', 'action', 'Type of action performed during the event', sptTOKEN, [], '', sxpNormal);
  indexes.add('AuditEvent', 'address', 'Identifier for the network access point of the user device', sptTOKEN, [], '', sxpNormal);
  indexes.add('AuditEvent', 'altid', 'Alternative User id e.g. authentication', sptTOKEN, [], '', sxpNormal);
  indexes.add('AuditEvent', 'date', 'Time when the event occurred on source', sptDATE, [], '', sxpNormal);
  indexes.add('AuditEvent', 'desc', 'Instance-specific descriptor for Object', sptSTRING, [], '', sxpNormal);
  indexes.add('AuditEvent', 'identity', 'Specific instance of object (e.g. versioned)', sptTOKEN, [], '', sxpNormal);
  indexes.add('AuditEvent', 'name', 'Human-meaningful name for the user', sptSTRING, [], '', sxpNormal);
  indexes.add('AuditEvent', 'object-type', 'Type of object involved', sptTOKEN, [], '', sxpNormal);
  indexes.add('AuditEvent', 'participant', 'Direct reference to resource', sptREFERENCE, ['Practitioner', 'Organization', 'Device', 'Patient', 'RelatedPerson'], '', sxpNormal);
  indexes.add('AuditEvent', 'patient', 'Direct reference to resource', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, '', sxpNormal);
  indexes.add('AuditEvent', 'policy', 'Policy that authorized event', sptURI, [], '', sxpNormal);
  indexes.add('AuditEvent', 'reference', 'Specific instance of resource (e.g. versioned)', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, '', sxpNormal);
  indexes.add('AuditEvent', 'site', 'Logical source location within the enterprise', sptTOKEN, [], '', sxpNormal);
  indexes.add('AuditEvent', 'source', 'The identity of source detecting the event', sptTOKEN, [], '', sxpNormal);
  indexes.add('AuditEvent', 'subtype', 'More specific type/id for the event', sptTOKEN, [], '', sxpNormal);
  indexes.add('AuditEvent', 'type', 'Type/identifier of event', sptTOKEN, [], '', sxpNormal);
  indexes.add('AuditEvent', 'user', 'Unique identifier for the user', sptTOKEN, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_BASIC}
procedure TFHIRIndexBuilderR2.buildIndexesForBasic(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Basic', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Basic', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('Basic', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('Basic', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('Basic', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('Basic', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Basic', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Basic', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Basic', 'author', 'Who created', sptREFERENCE, ['Practitioner', 'Patient', 'RelatedPerson'], '', sxpNormal);
  indexes.add('Basic', 'code', 'Kind of Resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Basic', 'created', 'When created', sptDATE, [], '', sxpNormal);
  indexes.add('Basic', 'identifier', 'Business identifier', sptTOKEN, [], '', sxpNormal);
  indexes.add('Basic', 'patient', 'Identifies the focus of this resource', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, '', sxpNormal);
  indexes.add('Basic', 'subject', 'Identifies the focus of this resource', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_BINARY}
procedure TFHIRIndexBuilderR2.buildIndexesForBinary(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Binary', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Binary', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('Binary', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('Binary', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('Binary', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('Binary', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Binary', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Binary', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Binary', 'contenttype', 'MimeType of the binary content', sptTOKEN, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_BODYSITE}
procedure TFHIRIndexBuilderR2.buildIndexesForBodySite(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('BodySite', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('BodySite', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('BodySite', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('BodySite', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('BodySite', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('BodySite', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('BodySite', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('BodySite', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('BodySite', 'code', 'Named anatomical location', sptTOKEN, [], '', sxpNormal);
  indexes.add('BodySite', 'identifier', 'Identifier for this instance of the anatomical location', sptTOKEN, [], '', sxpNormal);
  indexes.add('BodySite', 'patient', 'Patient to whom bodysite belongs', sptREFERENCE, ['Patient'], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_BUNDLE}
procedure TFHIRIndexBuilderR2.buildIndexesForBundle(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Bundle', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Bundle', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('Bundle', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('Bundle', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('Bundle', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('Bundle', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Bundle', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Bundle', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Bundle', 'composition', 'The first resource in the bundle, if the bundle type is "document" - this is a composition, and this parameter provides access to searches its contents', sptREFERENCE, ['Composition'], '', sxpNormal);
  indexes.add('Bundle', 'message', 'The first resource in the bundle, if the bundle type is "message" - this is a message header, and this parameter provides access to search its contents', sptREFERENCE, ['MessageHeader'], '', sxpNormal);
  indexes.add('Bundle', 'type', 'document | message | transaction | transaction-response | batch | batch-response | history | searchset | collection', sptTOKEN, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_CAREPLAN}
procedure TFHIRIndexBuilderR2.buildIndexesForCarePlan(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('CarePlan', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('CarePlan', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('CarePlan', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('CarePlan', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('CarePlan', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('CarePlan', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('CarePlan', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('CarePlan', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('CarePlan', 'activitycode', 'Detail type of activity', sptTOKEN, [], '', sxpNormal);
  indexes.add('CarePlan', 'activitydate', 'Specified date occurs within period specified by CarePlan.activity.timingSchedule', sptDATE, [], '', sxpNormal);
  indexes.add('CarePlan', 'activityreference', 'Activity details defined in specific resource', sptREFERENCE, ['Appointment', 'Order', 'ReferralRequest', 'ProcessRequest', 'NutritionOrder', 'VisionPrescription', 'ProcedureRequest', 'DiagnosticOrder', 'DeviceUseRequest', 'MedicationOrder', 'CommunicationRequest', 'SupplyRequest'], '', sxpNormal);
  indexes.add('CarePlan', 'condition', 'Health issues this plan addresses', sptREFERENCE, ['Condition'], '', sxpNormal);
  indexes.add('CarePlan', 'date', 'Time period plan covers', sptDATE, [], '', sxpNormal);
  indexes.add('CarePlan', 'goal', 'Desired outcome of plan', sptREFERENCE, ['Goal'], '', sxpNormal);
  indexes.add('CarePlan', 'participant', 'Who is involved', sptREFERENCE, ['Practitioner', 'Organization', 'Patient', 'RelatedPerson'], '', sxpNormal);
  indexes.add('CarePlan', 'patient', 'Who care plan is for', sptREFERENCE, ['Group', 'Patient'], '', sxpNormal);
  indexes.add('CarePlan', 'performer', 'Matches if the practitioner is listed as a performer in any of the "simple" activities.  (For performers of the detailed activities, chain through the activitydetail search parameter.)', sptREFERENCE, ['Practitioner', 'Organization', 'Patient', 'RelatedPerson'], '', sxpNormal);
  indexes.add('CarePlan', 'related', 'A combination of the type of relationship and the related plan', sptCOMPOSITE, [], '', sxpNull);
  indexes.add('CarePlan', 'relatedcode', 'includes | replaces | fulfills', sptTOKEN, [], '', sxpNormal);
  indexes.add('CarePlan', 'relatedplan', 'Plan relationship exists with', sptREFERENCE, ['CarePlan'], '', sxpNormal);
  indexes.add('CarePlan', 'subject', 'Who care plan is for', sptREFERENCE, ['Group', 'Patient'], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_CLAIM}
procedure TFHIRIndexBuilderR2.buildIndexesForClaim(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Claim', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Claim', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('Claim', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('Claim', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('Claim', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('Claim', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Claim', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Claim', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Claim', 'identifier', 'The primary identifier of the financial resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Claim', 'patient', 'Patient', sptREFERENCE, ['Patient'], '', sxpNormal);
  indexes.add('Claim', 'priority', 'Processing priority requested', sptTOKEN, [], '', sxpNormal);
  indexes.add('Claim', 'provider', 'Provider responsible for the claim', sptREFERENCE, ['Practitioner'], '', sxpNormal);
  indexes.add('Claim', 'use', 'The kind of financial resource', sptTOKEN, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_CLAIMRESPONSE}
procedure TFHIRIndexBuilderR2.buildIndexesForClaimResponse(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ClaimResponse', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('ClaimResponse', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('ClaimResponse', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('ClaimResponse', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('ClaimResponse', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('ClaimResponse', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('ClaimResponse', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('ClaimResponse', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('ClaimResponse', 'identifier', 'The identity of the insurer', sptTOKEN, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_CLINICALIMPRESSION}
procedure TFHIRIndexBuilderR2.buildIndexesForClinicalImpression(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ClinicalImpression', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('ClinicalImpression', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('ClinicalImpression', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('ClinicalImpression', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('ClinicalImpression', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('ClinicalImpression', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('ClinicalImpression', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('ClinicalImpression', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('ClinicalImpression', 'action', 'Actions taken during assessment', sptREFERENCE, ['Appointment', 'ReferralRequest', 'NutritionOrder', 'ProcedureRequest', 'Procedure', 'DiagnosticOrder', 'MedicationOrder', 'SupplyRequest'], '', sxpNormal);
  indexes.add('ClinicalImpression', 'assessor', 'The clinician performing the assessment', sptREFERENCE, ['Practitioner'], '', sxpNormal);
  indexes.add('ClinicalImpression', 'date', 'When the assessment occurred', sptDATE, [], '', sxpNormal);
  indexes.add('ClinicalImpression', 'finding', 'Specific text or code for finding', sptTOKEN, [], '', sxpNormal);
  indexes.add('ClinicalImpression', 'investigation', 'Record of a specific investigation', sptREFERENCE, ['FamilyMemberHistory', 'Observation', 'DiagnosticReport', 'QuestionnaireResponse'], '', sxpNormal);
  indexes.add('ClinicalImpression', 'patient', 'The patient being assessed', sptREFERENCE, ['Patient'], '', sxpNormal);
  indexes.add('ClinicalImpression', 'plan', 'Plan of action after assessment', sptREFERENCE, ['Appointment', 'Order', 'ReferralRequest', 'ProcessRequest', 'VisionPrescription', 'ProcedureRequest', 'DiagnosticOrder', 'DeviceUseRequest', 'SupplyRequest', 'CarePlan', 'NutritionOrder', 'MedicationOrder', 'CommunicationRequest'], '', sxpNormal);
  indexes.add('ClinicalImpression', 'previous', 'Reference to last assessment', sptREFERENCE, ['ClinicalImpression'], '', sxpNormal);
  indexes.add('ClinicalImpression', 'problem', 'General assessment of patient state', sptREFERENCE, ['Condition', 'AllergyIntolerance'], '', sxpNormal);
  indexes.add('ClinicalImpression', 'resolved', 'Diagnoses/conditions resolved since previous assessment', sptTOKEN, [], '', sxpNormal);
  indexes.add('ClinicalImpression', 'ruledout', 'Specific text of code for diagnosis', sptTOKEN, [], '', sxpNormal);
  indexes.add('ClinicalImpression', 'status', 'in-progress | completed | entered-in-error', sptTOKEN, [], '', sxpNormal);
  indexes.add('ClinicalImpression', 'trigger', 'Request or event that necessitated this assessment', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, '', sxpNormal);
  indexes.add('ClinicalImpression', 'trigger-code', 'Request or event that necessitated this assessment', sptTOKEN, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_COMMUNICATION}
procedure TFHIRIndexBuilderR2.buildIndexesForCommunication(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Communication', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Communication', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('Communication', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('Communication', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('Communication', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('Communication', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Communication', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Communication', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Communication', 'category', 'Message category', sptTOKEN, [], '', sxpNormal);
  indexes.add('Communication', 'encounter', 'Encounter leading to message', sptREFERENCE, ['Encounter'], '', sxpNormal);
  indexes.add('Communication', 'identifier', 'Unique identifier', sptTOKEN, [], '', sxpNormal);
  indexes.add('Communication', 'medium', 'A channel of communication', sptTOKEN, [], '', sxpNormal);
  indexes.add('Communication', 'patient', 'Focus of message', sptREFERENCE, ['Patient'], '', sxpNormal);
  indexes.add('Communication', 'received', 'When received', sptDATE, [], '', sxpNormal);
  indexes.add('Communication', 'recipient', 'Message recipient', sptREFERENCE, ['Practitioner', 'Group', 'Organization', 'Device', 'Patient', 'RelatedPerson'], '', sxpNormal);
  indexes.add('Communication', 'request', 'CommunicationRequest producing this message', sptREFERENCE, ['CommunicationRequest'], '', sxpNormal);
  indexes.add('Communication', 'sender', 'Message sender', sptREFERENCE, ['Practitioner', 'Organization', 'Device', 'Patient', 'RelatedPerson'], '', sxpNormal);
  indexes.add('Communication', 'sent', 'When sent', sptDATE, [], '', sxpNormal);
  indexes.add('Communication', 'status', 'in-progress | completed | suspended | rejected | failed', sptTOKEN, [], '', sxpNormal);
  indexes.add('Communication', 'subject', 'Focus of message', sptREFERENCE, ['Patient'], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_COMMUNICATIONREQUEST}
procedure TFHIRIndexBuilderR2.buildIndexesForCommunicationRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('CommunicationRequest', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('CommunicationRequest', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('CommunicationRequest', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('CommunicationRequest', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('CommunicationRequest', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('CommunicationRequest', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('CommunicationRequest', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('CommunicationRequest', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('CommunicationRequest', 'category', 'Message category', sptTOKEN, [], '', sxpNormal);
  indexes.add('CommunicationRequest', 'encounter', 'Encounter leading to message', sptREFERENCE, ['Encounter'], '', sxpNormal);
  indexes.add('CommunicationRequest', 'identifier', 'Unique identifier', sptTOKEN, [], '', sxpNormal);
  indexes.add('CommunicationRequest', 'medium', 'A channel of communication', sptTOKEN, [], '', sxpNormal);
  indexes.add('CommunicationRequest', 'patient', 'Focus of message', sptREFERENCE, ['Patient'], '', sxpNormal);
  indexes.add('CommunicationRequest', 'priority', 'Message urgency', sptTOKEN, [], '', sxpNormal);
  indexes.add('CommunicationRequest', 'recipient', 'Message recipient', sptREFERENCE, ['Practitioner', 'Organization', 'Device', 'Patient', 'RelatedPerson'], '', sxpNormal);
  indexes.add('CommunicationRequest', 'requested', 'When ordered or proposed', sptDATE, [], '', sxpNormal);
  indexes.add('CommunicationRequest', 'requester', 'An individual who requested a communication', sptREFERENCE, ['Practitioner', 'Patient', 'RelatedPerson'], '', sxpNormal);
  indexes.add('CommunicationRequest', 'sender', 'Message sender', sptREFERENCE, ['Practitioner', 'Organization', 'Device', 'Patient', 'RelatedPerson'], '', sxpNormal);
  indexes.add('CommunicationRequest', 'status', 'proposed | planned | requested | received | accepted | in-progress | completed | suspended | rejected | failed', sptTOKEN, [], '', sxpNormal);
  indexes.add('CommunicationRequest', 'subject', 'Focus of message', sptREFERENCE, ['Patient'], '', sxpNormal);
  indexes.add('CommunicationRequest', 'time', 'When scheduled', sptDATE, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_COMPOSITION}
procedure TFHIRIndexBuilderR2.buildIndexesForComposition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Composition', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Composition', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('Composition', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('Composition', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('Composition', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('Composition', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Composition', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Composition', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Composition', 'attester', 'Who attested the composition', sptREFERENCE, ['Practitioner', 'Organization', 'Patient'], '', sxpNormal);
  indexes.add('Composition', 'author', 'Who and/or what authored the composition', sptREFERENCE, ['Practitioner', 'Device', 'Patient', 'RelatedPerson'], '', sxpNormal);
  indexes.add('Composition', 'class', 'Categorization of Composition', sptTOKEN, [], '', sxpNormal);
  indexes.add('Composition', 'confidentiality', 'As defined by affinity domain', sptTOKEN, [], '', sxpNormal);
  indexes.add('Composition', 'context', 'Code(s) that apply to the event being documented', sptTOKEN, [], '', sxpNormal);
  indexes.add('Composition', 'date', 'Composition editing time', sptDATE, [], '', sxpNormal);
  indexes.add('Composition', 'encounter', 'Context of the Composition', sptREFERENCE, ['Encounter'], '', sxpNormal);
  indexes.add('Composition', 'entry', 'A reference to data that supports this section', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, '', sxpNormal);
  indexes.add('Composition', 'identifier', 'Logical identifier of composition (version-independent)', sptTOKEN, [], '', sxpNormal);
  indexes.add('Composition', 'patient', 'Who and/or what the composition is about', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, '', sxpNormal);
  indexes.add('Composition', 'period', 'The period covered by the documentation', sptDATE, [], '', sxpNormal);
  indexes.add('Composition', 'section', 'Classification of section (recommended)', sptTOKEN, [], '', sxpNormal);
  indexes.add('Composition', 'status', 'preliminary | final | amended | entered-in-error', sptTOKEN, [], '', sxpNormal);
  indexes.add('Composition', 'subject', 'Who and/or what the composition is about', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, '', sxpNormal);
  indexes.add('Composition', 'title', 'Human Readable name/title', sptSTRING, [], '', sxpNormal);
  indexes.add('Composition', 'type', 'Kind of composition (LOINC if possible)', sptTOKEN, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_CONCEPTMAP}
procedure TFHIRIndexBuilderR2.buildIndexesForConceptMap(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ConceptMap', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('ConceptMap', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('ConceptMap', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('ConceptMap', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('ConceptMap', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('ConceptMap', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('ConceptMap', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('ConceptMap', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('ConceptMap', 'context', 'A use context assigned to the concept map', sptTOKEN, [], '', sxpNormal);
  indexes.add('ConceptMap', 'date', 'The concept map publication date', sptDATE, [], '', sxpNormal);
  indexes.add('ConceptMap', 'dependson', 'Reference to element/field/ValueSet mapping depends on', sptURI, [], '', sxpNormal);
  indexes.add('ConceptMap', 'description', 'Text search in the description of the concept map', sptSTRING, [], '', sxpNormal);
  indexes.add('ConceptMap', 'identifier', 'Additional identifier for the concept map', sptTOKEN, [], '', sxpNormal);
  indexes.add('ConceptMap', 'name', 'Name of the concept map', sptSTRING, [], '', sxpNormal);
  indexes.add('ConceptMap', 'product', 'Reference to element/field/ValueSet mapping depends on', sptURI, [], '', sxpNormal);
  indexes.add('ConceptMap', 'publisher', 'Name of the publisher of the concept map', sptSTRING, [], '', sxpNormal);
  indexes.add('ConceptMap', 'source', 'Identifies the source of the concepts which are being mapped', sptREFERENCE, ['StructureDefinition', 'ValueSet'], '', sxpNormal);
  indexes.add('ConceptMap', 'sourcecode', 'Identifies element being mapped', sptTOKEN, [], '', sxpNormal);
  indexes.add('ConceptMap', 'sourcesystem', 'Code System (if value set crosses code systems)', sptURI, [], '', sxpNormal);
  indexes.add('ConceptMap', 'sourceuri', 'Identifies the source of the concepts which are being mapped', sptREFERENCE, ['StructureDefinition', 'ValueSet'], '', sxpNormal);
  indexes.add('ConceptMap', 'status', 'Status of the concept map', sptTOKEN, [], '', sxpNormal);
  indexes.add('ConceptMap', 'target', 'Provides context to the mappings', sptREFERENCE, ['StructureDefinition', 'ValueSet'], '', sxpNormal);
  indexes.add('ConceptMap', 'targetcode', 'Code that identifies the target element', sptTOKEN, [], '', sxpNormal);
  indexes.add('ConceptMap', 'targetsystem', 'System of the target (if necessary)', sptURI, [], '', sxpNormal);
  indexes.add('ConceptMap', 'url', 'The URL of the concept map', sptURI, [], '', sxpNormal);
  indexes.add('ConceptMap', 'version', 'The version identifier of the concept map', sptTOKEN, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_CONDITION}
procedure TFHIRIndexBuilderR2.buildIndexesForCondition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Condition', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Condition', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('Condition', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('Condition', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('Condition', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('Condition', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Condition', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Condition', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Condition', 'age', 'Search based on Condition onsetAge', sptNUMBER, [], '', sxpNormal);
  indexes.add('Condition', 'asserter', 'Person who asserts this condition', sptREFERENCE, ['Practitioner', 'Patient'], '', sxpNormal);
  indexes.add('Condition', 'body-site', 'Anatomical location, if relevant', sptTOKEN, [], '', sxpNormal);
  indexes.add('Condition', 'category', 'The category of the condition', sptTOKEN, [], '', sxpNormal);
  indexes.add('Condition', 'clinicalstatus', 'The clinical status of the condition', sptTOKEN, [], '', sxpNormal);
  indexes.add('Condition', 'code', 'Code for the condition', sptTOKEN, [], '', sxpNormal);
  indexes.add('Condition', 'date-recorded', 'A date, when the Condition statement was documented', sptDATE, [], '', sxpNormal);
  indexes.add('Condition', 'encounter', 'Encounter when condition first asserted', sptREFERENCE, ['Encounter'], '', sxpNormal);
  indexes.add('Condition', 'evidence', 'Manifestation/symptom', sptTOKEN, [], '', sxpNormal);
  indexes.add('Condition', 'identifier', 'A unique identifier of the condition record', sptTOKEN, [], '', sxpNormal);
  indexes.add('Condition', 'onset', 'Date related onsets (dateTime and Period)', sptDATE, [], '', sxpNormal);
  indexes.add('Condition', 'onset-info', 'Other onsets (boolean, age, range, string)', sptSTRING, [], '', sxpNormal);
  indexes.add('Condition', 'patient', 'Who has the condition?', sptREFERENCE, ['Patient'], '', sxpNormal);
  indexes.add('Condition', 'severity', 'The severity of the condition', sptTOKEN, [], '', sxpNormal);
  indexes.add('Condition', 'stage', 'Simple summary (disease specific)', sptTOKEN, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_CONFORMANCE}
procedure TFHIRIndexBuilderR2.buildIndexesForConformance(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Conformance', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Conformance', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('Conformance', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('Conformance', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('Conformance', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('Conformance', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Conformance', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Conformance', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Conformance', 'date', 'The conformance statement publication date', sptDATE, [], '', sxpNormal);
  indexes.add('Conformance', 'description', 'Text search in the description of the conformance statement', sptSTRING, [], '', sxpNormal);
  indexes.add('Conformance', 'event', 'Event code in a conformance statement', sptTOKEN, [], '', sxpNormal);
  indexes.add('Conformance', 'fhirversion', 'The version of FHIR', sptTOKEN, [], '', sxpNormal);
  indexes.add('Conformance', 'format', 'formats supported (xml | json | mime type)', sptTOKEN, [], '', sxpNormal);
  indexes.add('Conformance', 'mode', 'Mode - restful (server/client) or messaging (sender/receiver)', sptTOKEN, [], '', sxpNormal);
  indexes.add('Conformance', 'name', 'Name of the conformance statement', sptSTRING, [], '', sxpNormal);
  indexes.add('Conformance', 'profile', 'A profile id invoked in a conformance statement', sptREFERENCE, ['StructureDefinition'], '', sxpNormal);
  indexes.add('Conformance', 'publisher', 'Name of the publisher of the conformance statement', sptSTRING, [], '', sxpNormal);
  indexes.add('Conformance', 'resource', 'Name of a resource mentioned in a conformance statement', sptTOKEN, [], '', sxpNormal);
  indexes.add('Conformance', 'security', 'OAuth | SMART-on-FHIR | NTLM | Basic | Kerberos | Certificates', sptTOKEN, [], '', sxpNormal);
  indexes.add('Conformance', 'software', 'Part of a the name of a software application', sptSTRING, [], '', sxpNormal);
  indexes.add('Conformance', 'status', 'The current status of the conformance statement', sptTOKEN, [], '', sxpNormal);
  indexes.add('Conformance', 'supported-profile', 'Profiles for use cases supported', sptREFERENCE, ['StructureDefinition'], '', sxpNormal);
  indexes.add('Conformance', 'url', 'The uri that identifies the conformance statement', sptURI, [], '', sxpNormal);
  indexes.add('Conformance', 'version', 'The version identifier of the conformance statement', sptTOKEN, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_CONTRACT}
procedure TFHIRIndexBuilderR2.buildIndexesForContract(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Contract', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Contract', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('Contract', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('Contract', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('Contract', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('Contract', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Contract', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Contract', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Contract', 'actor', 'Contract Actor Type', sptREFERENCE, ['Practitioner', 'Group', 'Organization', 'Device', 'Patient', 'Substance', 'Contract', 'RelatedPerson', 'Location'], '', sxpNormal);
  indexes.add('Contract', 'identifier', 'The identity of the contract', sptTOKEN, [], '', sxpNormal);
  indexes.add('Contract', 'patient', 'The identity of the target of the contract (if a patient)', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, '', sxpNormal);
  indexes.add('Contract', 'signer', 'Contract Signatory Party', sptREFERENCE, ['Practitioner', 'Organization', 'Patient', 'RelatedPerson'], '', sxpNormal);
  indexes.add('Contract', 'subject', 'The identity of the target of the contract', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_COVERAGE}
procedure TFHIRIndexBuilderR2.buildIndexesForCoverage(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Coverage', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Coverage', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('Coverage', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('Coverage', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('Coverage', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('Coverage', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Coverage', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Coverage', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Coverage', 'dependent', 'Dependent number', sptTOKEN, [], '', sxpNormal);
  indexes.add('Coverage', 'group', 'Group identifier', sptTOKEN, [], '', sxpNormal);
  indexes.add('Coverage', 'identifier', 'The primary identifier of the insured', sptTOKEN, [], '', sxpNormal);
  indexes.add('Coverage', 'issuer', 'The identity of the insurer', sptREFERENCE, ['Organization'], '', sxpNormal);
  indexes.add('Coverage', 'plan', 'A plan or policy identifier', sptTOKEN, [], '', sxpNormal);
  indexes.add('Coverage', 'sequence', 'Sequence number', sptTOKEN, [], '', sxpNormal);
  indexes.add('Coverage', 'subplan', 'Sub-plan identifier', sptTOKEN, [], '', sxpNormal);
  indexes.add('Coverage', 'type', 'The kind of coverage', sptTOKEN, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_DATAELEMENT}
procedure TFHIRIndexBuilderR2.buildIndexesForDataElement(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('DataElement', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('DataElement', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('DataElement', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('DataElement', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('DataElement', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('DataElement', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('DataElement', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('DataElement', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('DataElement', 'code', 'A code for the data element (server may choose to do subsumption)', sptTOKEN, [], '', sxpNormal);
  indexes.add('DataElement', 'context', 'A use context assigned to the data element', sptTOKEN, [], '', sxpNormal);
  indexes.add('DataElement', 'date', 'The data element publication date', sptDATE, [], '', sxpNormal);
  indexes.add('DataElement', 'description', 'Text search in the description of the data element.  This corresponds to the definition of the first DataElement.element.', sptSTRING, [], '', sxpNormal);
  indexes.add('DataElement', 'identifier', 'The identifier of the data element', sptTOKEN, [], '', sxpNormal);
  indexes.add('DataElement', 'name', 'Name of the data element', sptSTRING, [], '', sxpNormal);
  indexes.add('DataElement', 'objectClass', 'Matches on the 11179-objectClass extension value', sptTOKEN, [], '', sxpNormal);
  indexes.add('DataElement', 'objectClassProperty', 'Matches on the 11179-objectClassProperty extension value', sptTOKEN, [], '', sxpNormal);
  indexes.add('DataElement', 'publisher', 'Name of the publisher of the data element', sptSTRING, [], '', sxpNormal);
  indexes.add('DataElement', 'status', 'The current status of the data element', sptTOKEN, [], '', sxpNormal);
  indexes.add('DataElement', 'stringency', 'The stringency of the data element definition', sptTOKEN, [], '', sxpNormal);
  indexes.add('DataElement', 'url', 'The official URL for the data element', sptURI, [], '', sxpNormal);
  indexes.add('DataElement', 'version', 'The version identifier of the data element', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_DETECTEDISSUE}
procedure TFHIRIndexBuilderR2.buildIndexesForDetectedIssue(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('DetectedIssue', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('DetectedIssue', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('DetectedIssue', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('DetectedIssue', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('DetectedIssue', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('DetectedIssue', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('DetectedIssue', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('DetectedIssue', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('DetectedIssue', 'author', 'The provider or device that identified the issue', sptREFERENCE, ['Practitioner', 'Device'], '', sxpNormal);
  indexes.add('DetectedIssue', 'category', 'Issue Category, e.g. drug-drug, duplicate therapy, etc.', sptTOKEN, [], '', sxpNormal);
  indexes.add('DetectedIssue', 'date', 'When identified', sptDATE, [], '', sxpNormal);
  indexes.add('DetectedIssue', 'identifier', 'Unique id for the detected issue', sptTOKEN, [], '', sxpNormal);
  indexes.add('DetectedIssue', 'implicated', 'Problem resource', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, '', sxpNormal);
  indexes.add('DetectedIssue', 'patient', 'Associated patient', sptREFERENCE, ['Patient'], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_DEVICE}
procedure TFHIRIndexBuilderR2.buildIndexesForDevice(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Device', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Device', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('Device', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('Device', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('Device', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('Device', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Device', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Device', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Device', 'identifier', 'Instance id from manufacturer, owner, and others', sptTOKEN, [], '', sxpNormal);
  indexes.add('Device', 'location', 'A location, where the resource is found', sptREFERENCE, ['Location'], '', sxpNormal);
  indexes.add('Device', 'manufacturer', 'The manufacturer of the device', sptSTRING, [], '', sxpNormal);
  indexes.add('Device', 'model', 'The model of the device', sptSTRING, [], '', sxpNormal);
  indexes.add('Device', 'organization', 'The organization responsible for the device', sptREFERENCE, ['Organization'], '', sxpNormal);
  indexes.add('Device', 'patient', 'Patient information, if the resource is affixed to a person', sptREFERENCE, ['Patient'], '', sxpNormal);
  indexes.add('Device', 'type', 'The type of the device', sptTOKEN, [], '', sxpNormal);
  indexes.add('Device', 'udi', 'FDA mandated Unique Device Identifier', sptSTRING, [], '', sxpNormal);
  indexes.add('Device', 'url', 'Network address to contact device', sptURI, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_DEVICECOMPONENT}
procedure TFHIRIndexBuilderR2.buildIndexesForDeviceComponent(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('DeviceComponent', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('DeviceComponent', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('DeviceComponent', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('DeviceComponent', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('DeviceComponent', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('DeviceComponent', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('DeviceComponent', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('DeviceComponent', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('DeviceComponent', 'parent', 'The parent DeviceComponent resource', sptREFERENCE, ['DeviceComponent'], '', sxpNormal);
  indexes.add('DeviceComponent', 'source', 'The device source', sptREFERENCE, ['Device'], '', sxpNormal);
  indexes.add('DeviceComponent', 'type', 'The device component type', sptTOKEN, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_DEVICEMETRIC}
procedure TFHIRIndexBuilderR2.buildIndexesForDeviceMetric(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('DeviceMetric', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('DeviceMetric', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('DeviceMetric', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('DeviceMetric', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('DeviceMetric', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('DeviceMetric', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('DeviceMetric', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('DeviceMetric', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('DeviceMetric', 'category', 'The category of the metric', sptTOKEN, [], '', sxpNormal);
  indexes.add('DeviceMetric', 'identifier', 'The identifier of the metric', sptTOKEN, [], '', sxpNormal);
  indexes.add('DeviceMetric', 'parent', 'The parent DeviceMetric resource', sptREFERENCE, ['DeviceComponent'], '', sxpNormal);
  indexes.add('DeviceMetric', 'source', 'The device resource', sptREFERENCE, ['Device'], '', sxpNormal);
  indexes.add('DeviceMetric', 'type', 'The component type', sptTOKEN, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_DEVICEUSEREQUEST}
procedure TFHIRIndexBuilderR2.buildIndexesForDeviceUseRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('DeviceUseRequest', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('DeviceUseRequest', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('DeviceUseRequest', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('DeviceUseRequest', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('DeviceUseRequest', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('DeviceUseRequest', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('DeviceUseRequest', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('DeviceUseRequest', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('DeviceUseRequest', 'device', 'Device requested', sptREFERENCE, ['Device'], '', sxpNormal);
  indexes.add('DeviceUseRequest', 'patient', 'Search by subject - a patient', sptREFERENCE, ['Patient'], '', sxpNormal);
  indexes.add('DeviceUseRequest', 'subject', 'Search by subject', sptREFERENCE, ['Patient'], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_DEVICEUSESTATEMENT}
procedure TFHIRIndexBuilderR2.buildIndexesForDeviceUseStatement(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('DeviceUseStatement', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('DeviceUseStatement', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('DeviceUseStatement', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('DeviceUseStatement', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('DeviceUseStatement', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('DeviceUseStatement', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('DeviceUseStatement', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('DeviceUseStatement', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('DeviceUseStatement', 'device', 'Search by device', sptREFERENCE, ['Device'], '', sxpNormal);
  indexes.add('DeviceUseStatement', 'patient', 'Search by subject - a patient', sptREFERENCE, ['Patient'], '', sxpNormal);
  indexes.add('DeviceUseStatement', 'subject', 'Search by subject', sptREFERENCE, ['Patient'], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_DIAGNOSTICORDER}
procedure TFHIRIndexBuilderR2.buildIndexesForDiagnosticOrder(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('DiagnosticOrder', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('DiagnosticOrder', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('DiagnosticOrder', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('DiagnosticOrder', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('DiagnosticOrder', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('DiagnosticOrder', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('DiagnosticOrder', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('DiagnosticOrder', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('DiagnosticOrder', 'actor', 'Who recorded or did this', sptREFERENCE, ['Practitioner', 'Device'], '', sxpNormal);
  indexes.add('DiagnosticOrder', 'bodysite', 'Location of requested test (if applicable)', sptTOKEN, [], '', sxpNormal);
  indexes.add('DiagnosticOrder', 'code', 'Code to indicate the item (test or panel) being ordered', sptTOKEN, [], '', sxpNormal);
  indexes.add('DiagnosticOrder', 'encounter', 'The encounter that this diagnostic order is associated with', sptREFERENCE, ['Encounter'], '', sxpNormal);
  indexes.add('DiagnosticOrder', 'event-date', 'The date at which the event happened', sptDATE, [], '', sxpNormal);
  indexes.add('DiagnosticOrder', 'event-status', 'proposed | draft | planned | requested | received | accepted | in-progress | review | completed | cancelled | suspended | rejected | failed', sptTOKEN, [], '', sxpNormal);
  indexes.add('DiagnosticOrder', 'event-status-date', 'A combination of past-status and date', sptCOMPOSITE, [], '', sxpNull);
  indexes.add('DiagnosticOrder', 'identifier', 'Identifiers assigned to this order', sptTOKEN, [], '', sxpNormal);
  indexes.add('DiagnosticOrder', 'item-date', 'The date at which the event happened', sptDATE, [], '', sxpNormal);
  indexes.add('DiagnosticOrder', 'item-past-status', 'proposed | draft | planned | requested | received | accepted | in-progress | review | completed | cancelled | suspended | rejected | failed', sptTOKEN, [], '', sxpNormal);
  indexes.add('DiagnosticOrder', 'item-status', 'proposed | draft | planned | requested | received | accepted | in-progress | review | completed | cancelled | suspended | rejected | failed', sptTOKEN, [], '', sxpNormal);
  indexes.add('DiagnosticOrder', 'item-status-date', 'A combination of item-past-status and item-date', sptCOMPOSITE, [], '', sxpNull);
  indexes.add('DiagnosticOrder', 'orderer', 'Who ordered the test', sptREFERENCE, ['Practitioner'], '', sxpNormal);
  indexes.add('DiagnosticOrder', 'patient', 'Who and/or what test is about', sptREFERENCE, ['Group', 'Device', 'Patient', 'Location'], '', sxpNormal);
  indexes.add('DiagnosticOrder', 'specimen', 'If the whole order relates to specific specimens', sptREFERENCE, ['Specimen'], '', sxpNormal);
  indexes.add('DiagnosticOrder', 'status', 'proposed | draft | planned | requested | received | accepted | in-progress | review | completed | cancelled | suspended | rejected | failed', sptTOKEN, [], '', sxpNormal);
  indexes.add('DiagnosticOrder', 'subject', 'Who and/or what test is about', sptREFERENCE, ['Group', 'Device', 'Patient', 'Location'], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_DIAGNOSTICREPORT}
procedure TFHIRIndexBuilderR2.buildIndexesForDiagnosticReport(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('DiagnosticReport', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('DiagnosticReport', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('DiagnosticReport', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('DiagnosticReport', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('DiagnosticReport', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('DiagnosticReport', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('DiagnosticReport', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('DiagnosticReport', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('DiagnosticReport', 'category', 'Which diagnostic discipline/department created the report', sptTOKEN, [], '', sxpNormal);
  indexes.add('DiagnosticReport', 'code', 'The code for the report as a whole, as opposed to codes for the atomic results, which are the names on the observation resource referred to from the result', sptTOKEN, [], '', sxpNormal);
  indexes.add('DiagnosticReport', 'date', 'The clinically relevant time of the report', sptDATE, [], '', sxpNormal);
  indexes.add('DiagnosticReport', 'diagnosis', 'A coded diagnosis on the report', sptTOKEN, [], '', sxpNormal);
  indexes.add('DiagnosticReport', 'encounter', 'The Encounter when the order was made', sptREFERENCE, ['Encounter'], '', sxpNormal);
  indexes.add('DiagnosticReport', 'identifier', 'An identifier for the report', sptTOKEN, [], '', sxpNormal);
  indexes.add('DiagnosticReport', 'image', 'A reference to the image source.', sptREFERENCE, ['Media'], '', sxpNormal);
  indexes.add('DiagnosticReport', 'issued', 'When the report was issued', sptDATE, [], '', sxpNormal);
  indexes.add('DiagnosticReport', 'patient', 'The subject of the report if a patient', sptREFERENCE, ['Group', 'Device', 'Patient', 'Location'], '', sxpNormal);
  indexes.add('DiagnosticReport', 'performer', 'Who was the source of the report (organization)', sptREFERENCE, ['Practitioner', 'Organization'], '', sxpNormal);
  indexes.add('DiagnosticReport', 'request', 'Reference to the test or procedure request.', sptREFERENCE, ['ReferralRequest', 'ProcedureRequest', 'DiagnosticOrder'], '', sxpNormal);
  indexes.add('DiagnosticReport', 'result', 'Link to an atomic result (observation resource)', sptREFERENCE, ['Observation'], '', sxpNormal);
  indexes.add('DiagnosticReport', 'specimen', 'The specimen details', sptREFERENCE, ['Specimen'], '', sxpNormal);
  indexes.add('DiagnosticReport', 'status', 'The status of the report', sptTOKEN, [], '', sxpNormal);
  indexes.add('DiagnosticReport', 'subject', 'The subject of the report', sptREFERENCE, ['Group', 'Device', 'Patient', 'Location'], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_DOCUMENTMANIFEST}
procedure TFHIRIndexBuilderR2.buildIndexesForDocumentManifest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('DocumentManifest', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('DocumentManifest', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('DocumentManifest', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('DocumentManifest', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('DocumentManifest', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('DocumentManifest', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('DocumentManifest', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('DocumentManifest', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('DocumentManifest', 'author', 'Who and/or what authored the manifest', sptREFERENCE, ['Practitioner', 'Organization', 'Device', 'Patient', 'RelatedPerson'], '', sxpNormal);
  indexes.add('DocumentManifest', 'content-ref', 'Contents of this set of documents', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, '', sxpNormal);
  indexes.add('DocumentManifest', 'created', 'When this document manifest created', sptDATE, [], '', sxpNormal);
  indexes.add('DocumentManifest', 'description', 'Human-readable description (title)', sptSTRING, [], '', sxpNormal);
  indexes.add('DocumentManifest', 'identifier', 'Unique Identifier for the set of documents', sptTOKEN, [], '', sxpNormal);
  indexes.add('DocumentManifest', 'patient', 'The subject of the set of documents', sptREFERENCE, ['Practitioner', 'Group', 'Device', 'Patient'], '', sxpNormal);
  indexes.add('DocumentManifest', 'recipient', 'Intended to get notified about this set of documents', sptREFERENCE, ['Practitioner', 'Organization', 'Patient', 'RelatedPerson'], '', sxpNormal);
  indexes.add('DocumentManifest', 'related-id', 'Identifiers of things that are related', sptTOKEN, [], '', sxpNormal);
  indexes.add('DocumentManifest', 'related-ref', 'Related Resource', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, '', sxpNormal);
  indexes.add('DocumentManifest', 'source', 'The source system/application/software', sptURI, [], '', sxpNormal);
  indexes.add('DocumentManifest', 'status', 'current | superseded | entered-in-error', sptTOKEN, [], '', sxpNormal);
  indexes.add('DocumentManifest', 'subject', 'The subject of the set of documents', sptREFERENCE, ['Practitioner', 'Group', 'Device', 'Patient'], '', sxpNormal);
  indexes.add('DocumentManifest', 'type', 'Kind of document set', sptTOKEN, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_DOCUMENTREFERENCE}
procedure TFHIRIndexBuilderR2.buildIndexesForDocumentReference(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('DocumentReference', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('DocumentReference', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('DocumentReference', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('DocumentReference', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('DocumentReference', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('DocumentReference', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('DocumentReference', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('DocumentReference', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('DocumentReference', 'authenticator', 'Who/what authenticated the document', sptREFERENCE, ['Practitioner', 'Organization'], '', sxpNormal);
  indexes.add('DocumentReference', 'author', 'Who and/or what authored the document', sptREFERENCE, ['Practitioner', 'Organization', 'Device', 'Patient', 'RelatedPerson'], '', sxpNormal);
  indexes.add('DocumentReference', 'class', 'Categorization of document', sptTOKEN, [], '', sxpNormal);
  indexes.add('DocumentReference', 'created', 'Document creation time', sptDATE, [], '', sxpNormal);
  indexes.add('DocumentReference', 'custodian', 'Organization which maintains the document', sptREFERENCE, ['Organization'], '', sxpNormal);
  indexes.add('DocumentReference', 'description', 'Human-readable description (title)', sptSTRING, [], '', sxpNormal);
  indexes.add('DocumentReference', 'encounter', 'Context of the document  content', sptREFERENCE, ['Encounter'], '', sxpNormal);
  indexes.add('DocumentReference', 'event', 'Main Clinical Acts Documented', sptTOKEN, [], '', sxpNormal);
  indexes.add('DocumentReference', 'facility', 'Kind of facility where patient was seen', sptTOKEN, [], '', sxpNormal);
  indexes.add('DocumentReference', 'format', 'Format/content rules for the document', sptTOKEN, [], '', sxpNormal);
  indexes.add('DocumentReference', 'identifier', 'Master Version Specific Identifier', sptTOKEN, [], '', sxpNormal);
  indexes.add('DocumentReference', 'indexed', 'When this document reference created', sptDATE, [], '', sxpNormal);
  indexes.add('DocumentReference', 'language', 'Human language of the content (BCP-47)', sptTOKEN, [], '', sxpNormal);
  indexes.add('DocumentReference', 'location', 'Uri where the data can be found', sptURI, [], '', sxpNormal);
  indexes.add('DocumentReference', 'patient', 'Who/what is the subject of the document', sptREFERENCE, ['Practitioner', 'Group', 'Device', 'Patient'], '', sxpNormal);
  indexes.add('DocumentReference', 'period', 'Time of service that is being documented', sptDATE, [], '', sxpNormal);
  indexes.add('DocumentReference', 'related-id', 'Identifier of related objects or events', sptTOKEN, [], '', sxpNormal);
  indexes.add('DocumentReference', 'related-ref', 'Related Resource', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, '', sxpNormal);
  indexes.add('DocumentReference', 'relatesto', 'Target of the relationship', sptREFERENCE, ['DocumentReference'], '', sxpNormal);
  indexes.add('DocumentReference', 'relation', 'replaces | transforms | signs | appends', sptTOKEN, [], '', sxpNormal);
  indexes.add('DocumentReference', 'relationship', 'Combination of relation and relatesTo', sptCOMPOSITE, [], '', sxpNull);
  indexes.add('DocumentReference', 'securitylabel', 'Document security-tags', sptTOKEN, [], '', sxpNormal);
  indexes.add('DocumentReference', 'setting', 'Additional details about where the content was created (e.g. clinical specialty)', sptTOKEN, [], '', sxpNormal);
  indexes.add('DocumentReference', 'status', 'current | superseded | entered-in-error', sptTOKEN, [], '', sxpNormal);
  indexes.add('DocumentReference', 'subject', 'Who/what is the subject of the document', sptREFERENCE, ['Practitioner', 'Group', 'Device', 'Patient'], '', sxpNormal);
  indexes.add('DocumentReference', 'type', 'Kind of document (LOINC if possible)', sptTOKEN, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_ELIGIBILITYREQUEST}
procedure TFHIRIndexBuilderR2.buildIndexesForEligibilityRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('EligibilityRequest', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('EligibilityRequest', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('EligibilityRequest', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('EligibilityRequest', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('EligibilityRequest', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('EligibilityRequest', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('EligibilityRequest', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('EligibilityRequest', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('EligibilityRequest', 'identifier', 'The business identifier of the Eligibility', sptTOKEN, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_ELIGIBILITYRESPONSE}
procedure TFHIRIndexBuilderR2.buildIndexesForEligibilityResponse(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('EligibilityResponse', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('EligibilityResponse', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('EligibilityResponse', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('EligibilityResponse', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('EligibilityResponse', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('EligibilityResponse', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('EligibilityResponse', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('EligibilityResponse', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('EligibilityResponse', 'identifier', 'The business identifier of the Explanation of Benefit', sptTOKEN, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_ENCOUNTER}
procedure TFHIRIndexBuilderR2.buildIndexesForEncounter(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Encounter', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Encounter', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('Encounter', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('Encounter', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('Encounter', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('Encounter', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Encounter', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Encounter', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Encounter', 'appointment', 'The appointment that scheduled this encounter', sptREFERENCE, ['Appointment'], '', sxpNormal);
  indexes.add('Encounter', 'condition', 'Reason the encounter takes place (resource)', sptREFERENCE, ['Condition', 'Procedure'], '', sxpNormal);
  indexes.add('Encounter', 'date', 'A date within the period the Encounter lasted', sptDATE, [], '', sxpNormal);
  indexes.add('Encounter', 'episodeofcare', 'Episode(s) of care that this encounter should be recorded against', sptREFERENCE, ['EpisodeOfCare'], '', sxpNormal);
  indexes.add('Encounter', 'identifier', 'Identifier(s) by which this encounter is known', sptTOKEN, [], '', sxpNormal);
  indexes.add('Encounter', 'incomingreferral', 'The ReferralRequest that initiated this encounter', sptREFERENCE, ['ReferralRequest'], '', sxpNormal);
  indexes.add('Encounter', 'indication', 'Reason the encounter takes place (resource)', sptREFERENCE, ['Condition', 'Procedure'], '', sxpNormal);
  indexes.add('Encounter', 'length', 'Length of encounter in days', sptNUMBER, [], '', sxpNormal);
  indexes.add('Encounter', 'location', 'Location the encounter takes place', sptREFERENCE, ['Location'], '', sxpNormal);
  indexes.add('Encounter', 'location-period', 'Time period during which the patient was present at the location', sptDATE, [], '', sxpNormal);
  indexes.add('Encounter', 'part-of', 'Another Encounter this encounter is part of', sptREFERENCE, ['Encounter'], '', sxpNormal);
  indexes.add('Encounter', 'participant', 'Persons involved in the encounter other than the patient', sptREFERENCE, ['Practitioner', 'RelatedPerson'], '', sxpNormal);
  indexes.add('Encounter', 'participant-type', 'Role of participant in encounter', sptTOKEN, [], '', sxpNormal);
  indexes.add('Encounter', 'patient', 'The patient present at the encounter', sptREFERENCE, ['Patient'], '', sxpNormal);
  indexes.add('Encounter', 'practitioner', 'Persons involved in the encounter other than the patient', sptREFERENCE, ['Practitioner', 'RelatedPerson'], '', sxpNormal);
  indexes.add('Encounter', 'procedure', 'Reason the encounter takes place (resource)', sptREFERENCE, ['Condition', 'Procedure'], '', sxpNormal);
  indexes.add('Encounter', 'reason', 'Reason the encounter takes place (code)', sptTOKEN, [], '', sxpNormal);
  indexes.add('Encounter', 'special-arrangement', 'Wheelchair, translator, stretcher, etc.', sptTOKEN, [], '', sxpNormal);
  indexes.add('Encounter', 'status', 'planned | arrived | in-progress | onleave | finished | cancelled', sptTOKEN, [], '', sxpNormal);
  indexes.add('Encounter', 'type', 'Specific type of encounter', sptTOKEN, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_ENROLLMENTREQUEST}
procedure TFHIRIndexBuilderR2.buildIndexesForEnrollmentRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('EnrollmentRequest', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('EnrollmentRequest', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('EnrollmentRequest', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('EnrollmentRequest', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('EnrollmentRequest', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('EnrollmentRequest', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('EnrollmentRequest', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('EnrollmentRequest', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('EnrollmentRequest', 'identifier', 'The business identifier of the Enrollment', sptTOKEN, [], '', sxpNormal);
  indexes.add('EnrollmentRequest', 'patient', 'The party to be enrolled', sptREFERENCE, ['Patient'], '', sxpNormal);
  indexes.add('EnrollmentRequest', 'subject', 'The party to be enrolled', sptREFERENCE, ['Patient'], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_ENROLLMENTRESPONSE}
procedure TFHIRIndexBuilderR2.buildIndexesForEnrollmentResponse(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('EnrollmentResponse', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('EnrollmentResponse', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('EnrollmentResponse', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('EnrollmentResponse', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('EnrollmentResponse', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('EnrollmentResponse', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('EnrollmentResponse', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('EnrollmentResponse', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('EnrollmentResponse', 'identifier', 'The business identifier of the Explanation of Benefit', sptTOKEN, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_EPISODEOFCARE}
procedure TFHIRIndexBuilderR2.buildIndexesForEpisodeOfCare(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('EpisodeOfCare', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('EpisodeOfCare', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('EpisodeOfCare', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('EpisodeOfCare', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('EpisodeOfCare', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('EpisodeOfCare', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('EpisodeOfCare', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('EpisodeOfCare', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('EpisodeOfCare', 'care-manager', 'Care manager/care co-ordinator for the patient', sptREFERENCE, ['Practitioner'], '', sxpNormal);
  indexes.add('EpisodeOfCare', 'condition', 'Conditions/problems/diagnoses this episode of care is for', sptREFERENCE, ['Condition'], '', sxpNormal);
  indexes.add('EpisodeOfCare', 'date', 'The provided date search value falls within the episode of care''s period', sptDATE, [], '', sxpNormal);
  indexes.add('EpisodeOfCare', 'identifier', 'Identifier(s) for the EpisodeOfCare', sptTOKEN, [], '', sxpNormal);
  indexes.add('EpisodeOfCare', 'incomingreferral', 'Incoming Referral Request', sptREFERENCE, ['ReferralRequest'], '', sxpNormal);
  indexes.add('EpisodeOfCare', 'organization', 'The organization that has assumed the specific responsibilities of this EpisodeOfCare', sptREFERENCE, ['Organization'], '', sxpNormal);
  indexes.add('EpisodeOfCare', 'patient', 'Patient for this episode of care', sptREFERENCE, ['Patient'], '', sxpNormal);
  indexes.add('EpisodeOfCare', 'status', 'The current status of the Episode of Care as provided (does not check the status history collection)', sptTOKEN, [], '', sxpNormal);
  indexes.add('EpisodeOfCare', 'team-member', 'A Practitioner or Organization allocated to the care team for this EpisodeOfCare', sptREFERENCE, ['Practitioner', 'Organization'], '', sxpNormal);
  indexes.add('EpisodeOfCare', 'type', 'Type/class  - e.g. specialist referral, disease management', sptTOKEN, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_EXPLANATIONOFBENEFIT}
procedure TFHIRIndexBuilderR2.buildIndexesForExplanationOfBenefit(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ExplanationOfBenefit', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('ExplanationOfBenefit', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('ExplanationOfBenefit', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('ExplanationOfBenefit', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('ExplanationOfBenefit', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('ExplanationOfBenefit', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('ExplanationOfBenefit', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('ExplanationOfBenefit', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('ExplanationOfBenefit', 'identifier', 'The business identifier of the Explanation of Benefit', sptTOKEN, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_FAMILYMEMBERHISTORY}
procedure TFHIRIndexBuilderR2.buildIndexesForFamilyMemberHistory(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('FamilyMemberHistory', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('FamilyMemberHistory', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('FamilyMemberHistory', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('FamilyMemberHistory', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('FamilyMemberHistory', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('FamilyMemberHistory', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('FamilyMemberHistory', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('FamilyMemberHistory', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('FamilyMemberHistory', 'code', 'A search by a condition code', sptTOKEN, [], '', sxpNormal);
  indexes.add('FamilyMemberHistory', 'condition', 'Search for a history of a particular condition within a patient''s family.', sptTOKEN, [], '', sxpNormal);
  indexes.add('FamilyMemberHistory', 'date', 'When history was captured/updated', sptDATE, [], '', sxpNormal);
  indexes.add('FamilyMemberHistory', 'gender', 'A search by a gender code of a family member', sptTOKEN, [], '', sxpNormal);
  indexes.add('FamilyMemberHistory', 'identifier', 'A search by a record identifier', sptTOKEN, [], '', sxpNormal);
  indexes.add('FamilyMemberHistory', 'patient', 'The identity of a subject to list family member history items for', sptREFERENCE, ['Patient'], '', sxpNormal);
  indexes.add('FamilyMemberHistory', 'relationship', 'Search for family history of members based on relationship', sptTOKEN, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_FLAG}
procedure TFHIRIndexBuilderR2.buildIndexesForFlag(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Flag', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Flag', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('Flag', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('Flag', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('Flag', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('Flag', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Flag', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Flag', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Flag', 'author', 'Flag creator', sptREFERENCE, ['Practitioner', 'Organization', 'Device', 'Patient'], '', sxpNormal);
  indexes.add('Flag', 'date', 'Time period when flag is active', sptDATE, [], '', sxpNormal);
  indexes.add('Flag', 'encounter', 'Alert relevant during encounter', sptREFERENCE, ['Encounter'], '', sxpNormal);
  indexes.add('Flag', 'patient', 'The identity of a subject to list flags for', sptREFERENCE, ['Practitioner', 'Group', 'Organization', 'Patient', 'Location'], '', sxpNormal);
  indexes.add('Flag', 'subject', 'The identity of a subject to list flags for', sptREFERENCE, ['Practitioner', 'Group', 'Organization', 'Patient', 'Location'], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_GOAL}
procedure TFHIRIndexBuilderR2.buildIndexesForGoal(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Goal', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Goal', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('Goal', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('Goal', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('Goal', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('Goal', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Goal', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Goal', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Goal', 'category', 'E.g. Treatment, dietary, behavioral, etc.', sptTOKEN, [], '', sxpNormal);
  indexes.add('Goal', 'identifier', 'External Ids for this goal', sptTOKEN, [], '', sxpNormal);
  indexes.add('Goal', 'patient', 'Who this goal is intended for', sptREFERENCE, ['Group', 'Organization', 'Patient'], '', sxpNormal);
  indexes.add('Goal', 'status', 'proposed | planned | accepted | rejected | in-progress | achieved | sustaining | on-hold | cancelled', sptTOKEN, [], '', sxpNormal);
  indexes.add('Goal', 'subject', 'Who this goal is intended for', sptREFERENCE, ['Group', 'Organization', 'Patient'], '', sxpNormal);
  indexes.add('Goal', 'targetdate', 'Reach goal on or before', sptDATE, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_GROUP}
procedure TFHIRIndexBuilderR2.buildIndexesForGroup(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Group', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Group', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('Group', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('Group', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('Group', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('Group', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Group', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Group', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Group', 'actual', 'Descriptive or actual', sptTOKEN, [], '', sxpNormal);
  indexes.add('Group', 'characteristic', 'Kind of characteristic', sptTOKEN, [], '', sxpNormal);
  indexes.add('Group', 'characteristic-value', 'A composite of both characteristic and value', sptCOMPOSITE, [], '', sxpNull);
  indexes.add('Group', 'code', 'The kind of resources contained', sptTOKEN, [], '', sxpNormal);
  indexes.add('Group', 'exclude', 'Group includes or excludes', sptTOKEN, [], '', sxpNormal);
  indexes.add('Group', 'identifier', 'Unique id', sptTOKEN, [], '', sxpNormal);
  indexes.add('Group', 'member', 'Reference to the group member', sptREFERENCE, ['Practitioner', 'Device', 'Medication', 'Patient', 'Substance'], '', sxpNormal);
  indexes.add('Group', 'type', 'The type of resources the group contains', sptTOKEN, [], '', sxpNormal);
  indexes.add('Group', 'value', 'Value held by characteristic', sptTOKEN, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_HEALTHCARESERVICE}
procedure TFHIRIndexBuilderR2.buildIndexesForHealthcareService(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('HealthcareService', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('HealthcareService', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('HealthcareService', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('HealthcareService', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('HealthcareService', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('HealthcareService', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('HealthcareService', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('HealthcareService', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('HealthcareService', 'characteristic', 'One of the HealthcareService''s characteristics', sptTOKEN, [], '', sxpNormal);
  indexes.add('HealthcareService', 'identifier', 'External identifiers for this item', sptTOKEN, [], '', sxpNormal);
  indexes.add('HealthcareService', 'location', 'The location of the Healthcare Service', sptREFERENCE, ['Location'], '', sxpNormal);
  indexes.add('HealthcareService', 'name', 'A portion of the Healthcare service name', sptSTRING, [], '', sxpNormal);
  indexes.add('HealthcareService', 'organization', 'The organization that provides this Healthcare Service', sptREFERENCE, ['Organization'], '', sxpNormal);
  indexes.add('HealthcareService', 'programname', 'One of the Program Names serviced by this HealthcareService', sptSTRING, [], '', sxpNormal);
  indexes.add('HealthcareService', 'servicecategory', 'Service Category of the Healthcare Service', sptTOKEN, [], '', sxpNormal);
  indexes.add('HealthcareService', 'servicetype', 'The type of service provided by this healthcare service', sptTOKEN, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_IMAGINGOBJECTSELECTION}
procedure TFHIRIndexBuilderR2.buildIndexesForImagingObjectSelection(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ImagingObjectSelection', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('ImagingObjectSelection', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('ImagingObjectSelection', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('ImagingObjectSelection', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('ImagingObjectSelection', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('ImagingObjectSelection', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('ImagingObjectSelection', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('ImagingObjectSelection', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('ImagingObjectSelection', 'author', 'Author of key DICOM object selection', sptREFERENCE, ['Practitioner', 'Organization', 'Device', 'Patient', 'RelatedPerson'], '', sxpNormal);
  indexes.add('ImagingObjectSelection', 'authoring-time', 'Time of key DICOM object selection authoring', sptDATE, [], '', sxpNormal);
  indexes.add('ImagingObjectSelection', 'identifier', 'UID of key DICOM object selection', sptURI, [], '', sxpNormal);
  indexes.add('ImagingObjectSelection', 'patient', 'Subject of key DICOM object selection', sptREFERENCE, ['Patient'], '', sxpNormal);
  indexes.add('ImagingObjectSelection', 'selected-study', 'Study selected in key DICOM object selection', sptURI, [], '', sxpNormal);
  indexes.add('ImagingObjectSelection', 'title', 'Title of key DICOM object selection', sptTOKEN, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_IMAGINGSTUDY}
procedure TFHIRIndexBuilderR2.buildIndexesForImagingStudy(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ImagingStudy', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('ImagingStudy', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('ImagingStudy', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('ImagingStudy', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('ImagingStudy', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('ImagingStudy', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('ImagingStudy', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('ImagingStudy', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('ImagingStudy', 'accession', 'The accession identifier for the study', sptTOKEN, [], '', sxpNormal);
  indexes.add('ImagingStudy', 'bodysite', 'The body site studied', sptTOKEN, [], '', sxpNormal);
  indexes.add('ImagingStudy', 'dicom-class', 'The type of the instance', sptURI, [], '', sxpNormal);
  indexes.add('ImagingStudy', 'modality', 'The modality of the series', sptTOKEN, [], '', sxpNormal);
  indexes.add('ImagingStudy', 'order', 'The order for the image', sptREFERENCE, ['DiagnosticOrder'], '', sxpNormal);
  indexes.add('ImagingStudy', 'patient', 'Who the study is about', sptREFERENCE, ['Patient'], '', sxpNormal);
  indexes.add('ImagingStudy', 'series', 'The identifier of the series of images', sptURI, [], '', sxpNormal);
  indexes.add('ImagingStudy', 'started', 'When the study was started', sptDATE, [], '', sxpNormal);
  indexes.add('ImagingStudy', 'study', 'The study identifier for the image', sptURI, [], '', sxpNormal);
  indexes.add('ImagingStudy', 'uid', 'The instance unique identifier', sptURI, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_IMMUNIZATION}
procedure TFHIRIndexBuilderR2.buildIndexesForImmunization(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Immunization', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Immunization', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('Immunization', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('Immunization', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('Immunization', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('Immunization', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Immunization', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Immunization', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Immunization', 'date', 'Vaccination  (non)-Administration Date', sptDATE, [], '', sxpNormal);
  indexes.add('Immunization', 'dose-sequence', 'Dose number within series', sptNUMBER, [], '', sxpNormal);
  indexes.add('Immunization', 'identifier', 'Business identifier', sptTOKEN, [], '', sxpNormal);
  indexes.add('Immunization', 'location', 'The service delivery location or facility in which the vaccine was / was to be administered', sptREFERENCE, ['Location'], '', sxpNormal);
  indexes.add('Immunization', 'lot-number', 'Vaccine Lot Number', sptSTRING, [], '', sxpNormal);
  indexes.add('Immunization', 'manufacturer', 'Vaccine Manufacturer', sptREFERENCE, ['Organization'], '', sxpNormal);
  indexes.add('Immunization', 'notgiven', 'Administrations which were not given', sptTOKEN, [], '', sxpNormal);
  indexes.add('Immunization', 'patient', 'The patient for the vaccination record', sptREFERENCE, ['Patient'], '', sxpNormal);
  indexes.add('Immunization', 'performer', 'The practitioner who administered the vaccination', sptREFERENCE, ['Practitioner'], '', sxpNormal);
  indexes.add('Immunization', 'reaction', 'Additional information on reaction', sptREFERENCE, ['Observation'], '', sxpNormal);
  indexes.add('Immunization', 'reaction-date', 'When reaction started', sptDATE, [], '', sxpNormal);
  indexes.add('Immunization', 'reason', 'Why immunization occurred', sptTOKEN, [], '', sxpNormal);
  indexes.add('Immunization', 'reason-not-given', 'Explanation of reason vaccination was not administered', sptTOKEN, [], '', sxpNormal);
  indexes.add('Immunization', 'requester', 'The practitioner who ordered the vaccination', sptREFERENCE, ['Practitioner'], '', sxpNormal);
  indexes.add('Immunization', 'status', 'Immunization event status', sptTOKEN, [], '', sxpNormal);
  indexes.add('Immunization', 'vaccine-code', 'Vaccine Product Administered', sptTOKEN, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_IMMUNIZATIONRECOMMENDATION}
procedure TFHIRIndexBuilderR2.buildIndexesForImmunizationRecommendation(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ImmunizationRecommendation', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('ImmunizationRecommendation', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('ImmunizationRecommendation', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('ImmunizationRecommendation', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('ImmunizationRecommendation', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('ImmunizationRecommendation', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('ImmunizationRecommendation', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('ImmunizationRecommendation', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('ImmunizationRecommendation', 'date', 'Date recommendation created', sptDATE, [], '', sxpNormal);
  indexes.add('ImmunizationRecommendation', 'dose-number', 'Recommended dose number', sptNUMBER, [], '', sxpNormal);
  indexes.add('ImmunizationRecommendation', 'dose-sequence', 'Dose number within sequence', sptNUMBER, [], '', sxpNormal);
  indexes.add('ImmunizationRecommendation', 'identifier', 'Business identifier', sptTOKEN, [], '', sxpNormal);
  indexes.add('ImmunizationRecommendation', 'information', 'Patient observations supporting recommendation', sptREFERENCE, ['AllergyIntolerance', 'Observation'], '', sxpNormal);
  indexes.add('ImmunizationRecommendation', 'patient', 'Who this profile is for', sptREFERENCE, ['Patient'], '', sxpNormal);
  indexes.add('ImmunizationRecommendation', 'status', 'Vaccine administration status', sptTOKEN, [], '', sxpNormal);
  indexes.add('ImmunizationRecommendation', 'support', 'Past immunizations supporting recommendation', sptREFERENCE, ['Immunization'], '', sxpNormal);
  indexes.add('ImmunizationRecommendation', 'vaccine-type', 'Vaccine recommendation applies to', sptTOKEN, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_IMPLEMENTATIONGUIDE}
procedure TFHIRIndexBuilderR2.buildIndexesForImplementationGuide(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ImplementationGuide', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('ImplementationGuide', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('ImplementationGuide', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('ImplementationGuide', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('ImplementationGuide', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('ImplementationGuide', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('ImplementationGuide', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('ImplementationGuide', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('ImplementationGuide', 'context', 'A use context assigned to the structure', sptTOKEN, [], '', sxpNormal);
  indexes.add('ImplementationGuide', 'date', 'The implementation guide publication date', sptDATE, [], '', sxpNormal);
  indexes.add('ImplementationGuide', 'dependency', 'Where to find dependency', sptURI, [], '', sxpNormal);
  indexes.add('ImplementationGuide', 'description', 'Text search in the description of the implementation guide', sptSTRING, [], '', sxpNormal);
  indexes.add('ImplementationGuide', 'experimental', 'If for testing purposes, not real usage', sptTOKEN, [], '', sxpNormal);
  indexes.add('ImplementationGuide', 'name', 'Name of the implementation guide', sptSTRING, [], '', sxpNormal);
  indexes.add('ImplementationGuide', 'publisher', 'Name of the publisher of the implementation guide', sptSTRING, [], '', sxpNormal);
  indexes.add('ImplementationGuide', 'status', 'The current status of the implementation guide', sptTOKEN, [], '', sxpNormal);
  indexes.add('ImplementationGuide', 'url', 'Absolute URL used to reference this Implementation Guide', sptURI, [], '', sxpNormal);
  indexes.add('ImplementationGuide', 'version', 'The version identifier of the implementation guide', sptTOKEN, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_LIST}
procedure TFHIRIndexBuilderR2.buildIndexesForList(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('List', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('List', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('List', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('List', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('List', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('List', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('List', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('List', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('List', 'code', 'What the purpose of this list is', sptTOKEN, [], '', sxpNormal);
  indexes.add('List', 'date', 'When the list was prepared', sptDATE, [], '', sxpNormal);
  indexes.add('List', 'empty-reason', 'Why list is empty', sptTOKEN, [], '', sxpNormal);
  indexes.add('List', 'encounter', 'Context in which list created', sptREFERENCE, ['Encounter'], '', sxpNormal);
  indexes.add('List', 'item', 'Actual entry', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, '', sxpNormal);
  indexes.add('List', 'notes', 'Comments about the list', sptSTRING, [], '', sxpNormal);
  indexes.add('List', 'patient', 'If all resources have the same subject', sptREFERENCE, ['Group', 'Device', 'Patient', 'Location'], '', sxpNormal);
  indexes.add('List', 'source', 'Who and/or what defined the list contents (aka Author)', sptREFERENCE, ['Practitioner', 'Device', 'Patient'], '', sxpNormal);
  indexes.add('List', 'status', 'current | retired | entered-in-error', sptTOKEN, [], '', sxpNormal);
  indexes.add('List', 'subject', 'If all resources have the same subject', sptREFERENCE, ['Group', 'Device', 'Patient', 'Location'], '', sxpNormal);
  indexes.add('List', 'title', 'Descriptive name for the list', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_LOCATION}
procedure TFHIRIndexBuilderR2.buildIndexesForLocation(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Location', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Location', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('Location', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('Location', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('Location', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('Location', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Location', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Location', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Location', 'address', 'A (part of the) address of the location', sptSTRING, [], '', sxpNormal);
  indexes.add('Location', 'address-city', 'A city specified in an address', sptSTRING, [], '', sxpNormal);
  indexes.add('Location', 'address-country', 'A country specified in an address', sptSTRING, [], '', sxpNormal);
  indexes.add('Location', 'address-postalcode', 'A postal code specified in an address', sptSTRING, [], '', sxpNormal);
  indexes.add('Location', 'address-state', 'A state specified in an address', sptSTRING, [], '', sxpNormal);
  indexes.add('Location', 'address-use', 'A use code specified in an address', sptTOKEN, [], '', sxpNormal);
  indexes.add('Location', 'identifier', 'Unique code or number identifying the location to its users', sptTOKEN, [], '', sxpNormal);
  indexes.add('Location', 'name', 'A (portion of the) name of the location', sptSTRING, [], '', sxpNormal);
  indexes.add('Location', 'near', 'The coordinates expressed as [lat],[long] (using the WGS84 datum, see notes) to find locations near to (servers may search using a square rather than a circle for efficiency)', sptTOKEN, [], '', sxpNearby);
  indexes.add('Location', 'near-distance', 'A distance quantity to limit the near search to locations within a specific distance', sptTOKEN, [], '', sxpDistance);
  indexes.add('Location', 'organization', 'Searches for locations that are managed by the provided organization', sptREFERENCE, ['Organization'], '', sxpNormal);
  indexes.add('Location', 'partof', 'The location of which this location is a part', sptREFERENCE, ['Location'], '', sxpNormal);
  indexes.add('Location', 'status', 'Searches for locations with a specific kind of status', sptTOKEN, [], '', sxpNormal);
  indexes.add('Location', 'type', 'A code for the type of location', sptTOKEN, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_MEDIA}
procedure TFHIRIndexBuilderR2.buildIndexesForMedia(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Media', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Media', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('Media', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('Media', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('Media', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('Media', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Media', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Media', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Media', 'created', 'Date attachment was first created', sptDATE, [], '', sxpNormal);
  indexes.add('Media', 'identifier', 'Identifier(s) for the image', sptTOKEN, [], '', sxpNormal);
  indexes.add('Media', 'operator', 'The person who generated the image', sptREFERENCE, ['Practitioner'], '', sxpNormal);
  indexes.add('Media', 'patient', 'Who/What this Media is a record of', sptREFERENCE, ['Practitioner', 'Specimen', 'Group', 'Device', 'Patient'], '', sxpNormal);
  indexes.add('Media', 'subject', 'Who/What this Media is a record of', sptREFERENCE, ['Practitioner', 'Specimen', 'Group', 'Device', 'Patient'], '', sxpNormal);
  indexes.add('Media', 'subtype', 'The type of acquisition equipment/process', sptTOKEN, [], '', sxpNormal);
  indexes.add('Media', 'type', 'photo | video | audio', sptTOKEN, [], '', sxpNormal);
  indexes.add('Media', 'view', 'Imaging view, e.g. Lateral or Antero-posterior', sptTOKEN, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_MEDICATION}
procedure TFHIRIndexBuilderR2.buildIndexesForMedication(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Medication', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Medication', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('Medication', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('Medication', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('Medication', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('Medication', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Medication', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Medication', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Medication', 'code', 'Codes that identify this medication', sptTOKEN, [], '', sxpNormal);
  indexes.add('Medication', 'container', 'E.g. box, vial, blister-pack', sptTOKEN, [], '', sxpNormal);
  indexes.add('Medication', 'content', 'A product in the package', sptREFERENCE, ['Medication'], '', sxpNormal);
  indexes.add('Medication', 'form', 'powder | tablets | carton +', sptTOKEN, [], '', sxpNormal);
  indexes.add('Medication', 'ingredient', 'The product contained', sptREFERENCE, ['Medication', 'Substance'], '', sxpNormal);
  indexes.add('Medication', 'manufacturer', 'Manufacturer of the item', sptREFERENCE, ['Organization'], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_MEDICATIONADMINISTRATION}
procedure TFHIRIndexBuilderR2.buildIndexesForMedicationAdministration(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('MedicationAdministration', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('MedicationAdministration', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('MedicationAdministration', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('MedicationAdministration', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('MedicationAdministration', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('MedicationAdministration', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('MedicationAdministration', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('MedicationAdministration', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('MedicationAdministration', 'code', 'Return administrations of this medication code', sptTOKEN, [], '', sxpNormal);
  indexes.add('MedicationAdministration', 'device', 'Return administrations with this administration device identity', sptREFERENCE, ['Device'], '', sxpNormal);
  indexes.add('MedicationAdministration', 'effectivetime', 'Date administration happened (or did not happen)', sptDATE, [], '', sxpNormal);
  indexes.add('MedicationAdministration', 'encounter', 'Return administrations that share this encounter', sptREFERENCE, ['Encounter'], '', sxpNormal);
  indexes.add('MedicationAdministration', 'identifier', 'Return administrations with this external identifier', sptTOKEN, [], '', sxpNormal);
  indexes.add('MedicationAdministration', 'medication', 'Return administrations of this medication resource', sptREFERENCE, ['Medication'], '', sxpNormal);
  indexes.add('MedicationAdministration', 'notgiven', 'Administrations that were not made', sptTOKEN, [], '', sxpNormal);
  indexes.add('MedicationAdministration', 'patient', 'The identity of a patient to list administrations  for', sptREFERENCE, ['Patient'], '', sxpNormal);
  indexes.add('MedicationAdministration', 'practitioner', 'Who administered substance', sptREFERENCE, ['Practitioner', 'Patient', 'RelatedPerson'], '', sxpNormal);
  indexes.add('MedicationAdministration', 'prescription', 'The identity of a prescription to list administrations from', sptREFERENCE, ['MedicationOrder'], '', sxpNormal);
  indexes.add('MedicationAdministration', 'status', 'MedicationAdministration event status (for example one of active/paused/completed/nullified)', sptTOKEN, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_MEDICATIONDISPENSE}
procedure TFHIRIndexBuilderR2.buildIndexesForMedicationDispense(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('MedicationDispense', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('MedicationDispense', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('MedicationDispense', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('MedicationDispense', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('MedicationDispense', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('MedicationDispense', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('MedicationDispense', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('MedicationDispense', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('MedicationDispense', 'code', 'Return dispenses of this medicine code', sptTOKEN, [], '', sxpNormal);
  indexes.add('MedicationDispense', 'destination', 'Return dispenses that should be sent to a specific destination', sptREFERENCE, ['Location'], '', sxpNormal);
  indexes.add('MedicationDispense', 'dispenser', 'Return all dispenses performed by a specific individual', sptREFERENCE, ['Practitioner'], '', sxpNormal);
  indexes.add('MedicationDispense', 'identifier', 'Return dispenses with this external identifier', sptTOKEN, [], '', sxpNormal);
  indexes.add('MedicationDispense', 'medication', 'Return dispenses of this medicine resource', sptREFERENCE, ['Medication'], '', sxpNormal);
  indexes.add('MedicationDispense', 'patient', 'The identity of a patient to list dispenses  for', sptREFERENCE, ['Patient'], '', sxpNormal);
  indexes.add('MedicationDispense', 'prescription', 'The identity of a prescription to list dispenses from', sptREFERENCE, ['MedicationOrder'], '', sxpNormal);
  indexes.add('MedicationDispense', 'receiver', 'Who collected the medication', sptREFERENCE, ['Practitioner', 'Patient'], '', sxpNormal);
  indexes.add('MedicationDispense', 'responsibleparty', 'Return all dispenses with the specified responsible party', sptREFERENCE, ['Practitioner'], '', sxpNormal);
  indexes.add('MedicationDispense', 'status', 'Status of the dispense', sptTOKEN, [], '', sxpNormal);
  indexes.add('MedicationDispense', 'type', 'Return all dispenses of a specific type', sptTOKEN, [], '', sxpNormal);
  indexes.add('MedicationDispense', 'whenhandedover', 'Date when medication handed over to patient (outpatient setting), or supplied to ward or clinic (inpatient setting)', sptDATE, [], '', sxpNormal);
  indexes.add('MedicationDispense', 'whenprepared', 'Date when medication prepared', sptDATE, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_MEDICATIONORDER}
procedure TFHIRIndexBuilderR2.buildIndexesForMedicationOrder(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('MedicationOrder', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('MedicationOrder', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('MedicationOrder', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('MedicationOrder', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('MedicationOrder', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('MedicationOrder', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('MedicationOrder', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('MedicationOrder', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('MedicationOrder', 'code', 'Return administrations of this medication code', sptTOKEN, [], '', sxpNormal);
  indexes.add('MedicationOrder', 'datewritten', 'Return prescriptions written on this date', sptDATE, [], '', sxpNormal);
  indexes.add('MedicationOrder', 'encounter', 'Return prescriptions with this encounter identifier', sptREFERENCE, ['Encounter'], '', sxpNormal);
  indexes.add('MedicationOrder', 'identifier', 'Return prescriptions with this external identifier', sptTOKEN, [], '', sxpNormal);
  indexes.add('MedicationOrder', 'medication', 'Return administrations of this medication reference', sptREFERENCE, ['Medication'], '', sxpNormal);
  indexes.add('MedicationOrder', 'patient', 'The identity of a patient to list orders  for', sptREFERENCE, ['Patient'], '', sxpNormal);
  indexes.add('MedicationOrder', 'prescriber', 'Who ordered the medication(s)', sptREFERENCE, ['Practitioner'], '', sxpNormal);
  indexes.add('MedicationOrder', 'status', 'Status of the prescription', sptTOKEN, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_MEDICATIONSTATEMENT}
procedure TFHIRIndexBuilderR2.buildIndexesForMedicationStatement(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('MedicationStatement', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('MedicationStatement', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('MedicationStatement', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('MedicationStatement', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('MedicationStatement', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('MedicationStatement', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('MedicationStatement', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('MedicationStatement', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('MedicationStatement', 'code', 'Return administrations of this medication code', sptTOKEN, [], '', sxpNormal);
  indexes.add('MedicationStatement', 'effectivedate', 'Date when patient was taking (or not taking) the medication', sptDATE, [], '', sxpNormal);
  indexes.add('MedicationStatement', 'identifier', 'Return statements with this external identifier', sptTOKEN, [], '', sxpNormal);
  indexes.add('MedicationStatement', 'medication', 'Return administrations of this medication reference', sptREFERENCE, ['Medication'], '', sxpNormal);
  indexes.add('MedicationStatement', 'patient', 'The identity of a patient to list statements  for', sptREFERENCE, ['Patient'], '', sxpNormal);
  indexes.add('MedicationStatement', 'source', 'Who the information in the statement came from', sptREFERENCE, ['Practitioner', 'Patient', 'RelatedPerson'], '', sxpNormal);
  indexes.add('MedicationStatement', 'status', 'Return statements that match the given status', sptTOKEN, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_MESSAGEHEADER}
procedure TFHIRIndexBuilderR2.buildIndexesForMessageHeader(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('MessageHeader', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('MessageHeader', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('MessageHeader', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('MessageHeader', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('MessageHeader', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('MessageHeader', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('MessageHeader', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('MessageHeader', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('MessageHeader', 'author', 'The source of the decision', sptREFERENCE, ['Practitioner'], '', sxpNormal);
  indexes.add('MessageHeader', 'code', 'ok | transient-error | fatal-error', sptTOKEN, [], '', sxpNormal);
  indexes.add('MessageHeader', 'data', 'The actual content of the message', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, '', sxpNormal);
  indexes.add('MessageHeader', 'destination', 'Name of system', sptSTRING, [], '', sxpNormal);
  indexes.add('MessageHeader', 'destination-uri', 'Actual destination address or id', sptURI, [], '', sxpNormal);
  indexes.add('MessageHeader', 'enterer', 'The source of the data entry', sptREFERENCE, ['Practitioner'], '', sxpNormal);
  indexes.add('MessageHeader', 'event', 'Code for the event this message represents', sptTOKEN, [], '', sxpNormal);
  indexes.add('MessageHeader', 'receiver', 'Intended "real-world" recipient for the data', sptREFERENCE, ['Practitioner', 'Organization'], '', sxpNormal);
  indexes.add('MessageHeader', 'response-id', 'Id of original message', sptTOKEN, [], '', sxpNormal);
  indexes.add('MessageHeader', 'responsible', 'Final responsibility for event', sptREFERENCE, ['Practitioner', 'Organization'], '', sxpNormal);
  indexes.add('MessageHeader', 'source', 'Name of system', sptSTRING, [], '', sxpNormal);
  indexes.add('MessageHeader', 'source-uri', 'Actual message source address or id', sptURI, [], '', sxpNormal);
  indexes.add('MessageHeader', 'target', 'Particular delivery destination within the destination', sptREFERENCE, ['Device'], '', sxpNormal);
  indexes.add('MessageHeader', 'timestamp', 'Time that the message was sent', sptDATE, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_NAMINGSYSTEM}
procedure TFHIRIndexBuilderR2.buildIndexesForNamingSystem(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('NamingSystem', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('NamingSystem', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('NamingSystem', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('NamingSystem', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('NamingSystem', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('NamingSystem', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('NamingSystem', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('NamingSystem', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('NamingSystem', 'contact', 'Name of a individual to contact', sptSTRING, [], '', sxpNormal);
  indexes.add('NamingSystem', 'context', 'Content intends to support these contexts', sptTOKEN, [], '', sxpNormal);
  indexes.add('NamingSystem', 'date', 'Publication Date(/time)', sptDATE, [], '', sxpNormal);
  indexes.add('NamingSystem', 'id-type', 'oid | uuid | uri | other', sptTOKEN, [], '', sxpNormal);
  indexes.add('NamingSystem', 'kind', 'codesystem | identifier | root', sptTOKEN, [], '', sxpNormal);
  indexes.add('NamingSystem', 'name', 'Human-readable label', sptSTRING, [], '', sxpNormal);
  indexes.add('NamingSystem', 'period', 'When is identifier valid?', sptDATE, [], '', sxpNormal);
  indexes.add('NamingSystem', 'publisher', 'Name of the publisher (Organization or individual)', sptSTRING, [], '', sxpNormal);
  indexes.add('NamingSystem', 'replaced-by', 'Use this instead', sptREFERENCE, ['NamingSystem'], '', sxpNormal);
  indexes.add('NamingSystem', 'responsible', 'Who maintains system namespace?', sptSTRING, [], '', sxpNormal);
  indexes.add('NamingSystem', 'status', 'draft | active | retired', sptTOKEN, [], '', sxpNormal);
  indexes.add('NamingSystem', 'telecom', 'Contact details for individual or publisher', sptTOKEN, [], '', sxpNormal);
  indexes.add('NamingSystem', 'type', 'e.g. driver,  provider,  patient, bank etc.', sptTOKEN, [], '', sxpNormal);
  indexes.add('NamingSystem', 'value', 'The unique identifier', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_NUTRITIONORDER}
procedure TFHIRIndexBuilderR2.buildIndexesForNutritionOrder(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('NutritionOrder', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('NutritionOrder', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('NutritionOrder', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('NutritionOrder', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('NutritionOrder', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('NutritionOrder', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('NutritionOrder', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('NutritionOrder', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('NutritionOrder', 'additive', 'Type of module component to add to the feeding', sptTOKEN, [], '', sxpNormal);
  indexes.add('NutritionOrder', 'datetime', 'Return nutrition orders requested on this date', sptDATE, [], '', sxpNormal);
  indexes.add('NutritionOrder', 'encounter', 'Return nutrition orders with this encounter identifier', sptREFERENCE, ['Encounter'], '', sxpNormal);
  indexes.add('NutritionOrder', 'formula', 'Type of enteral or infant formula', sptTOKEN, [], '', sxpNormal);
  indexes.add('NutritionOrder', 'identifier', 'Return nutrition orders with this external identifier', sptTOKEN, [], '', sxpNormal);
  indexes.add('NutritionOrder', 'oraldiet', 'Type of diet that can be consumed orally (i.e., take via the mouth).', sptTOKEN, [], '', sxpNormal);
  indexes.add('NutritionOrder', 'patient', 'The identity of the person who requires the diet, formula or nutritional supplement', sptREFERENCE, ['Patient'], '', sxpNormal);
  indexes.add('NutritionOrder', 'provider', 'The identify of the provider who placed the nutrition order', sptREFERENCE, ['Practitioner'], '', sxpNormal);
  indexes.add('NutritionOrder', 'status', 'Status of the nutrition order.', sptTOKEN, [], '', sxpNormal);
  indexes.add('NutritionOrder', 'supplement', 'Type of supplement product requested', sptTOKEN, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_OBSERVATION}
procedure TFHIRIndexBuilderR2.buildIndexesForObservation(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Observation', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Observation', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('Observation', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('Observation', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('Observation', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('Observation', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Observation', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Observation', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Observation', 'category', 'The classification of the type of observation', sptTOKEN, [], '', sxpNormal);
  indexes.add('Observation', 'code', 'The code of the observation type', sptTOKEN, [], '', sxpNormal);
  indexes.add('Observation', 'code-value-[x]', 'Both code and one of the value parameters', sptCOMPOSITE, [], '', sxpNull);
  indexes.add('Observation', 'component-code', 'The component code of the observation type', sptTOKEN, [], '', sxpNormal);
  indexes.add('Observation', 'component-code-value-[x]', 'Both component code and one of the component value parameters', sptCOMPOSITE, [], '', sxpNull);
  indexes.add('Observation', 'component-data-absent-reason', 'The reason why the expected value in the element Observation.component.value[x] is missing.', sptTOKEN, [], '', sxpNormal);
  indexes.add('Observation', 'component-value-concept', 'The value of the component observation, if the value is a CodeableConcept', sptTOKEN, [], '', sxpNormal);
  indexes.add('Observation', 'component-value-quantity', 'The value of the component observation, if the value is a Quantity, or a SampledData (just search on the bounds of the values in sampled data)', sptQUANTITY, [], '', sxpNormal);
  indexes.add('Observation', 'component-value-string', 'The value of the component observation, if the value is a string, and also searches in CodeableConcept.text', sptSTRING, [], '', sxpNormal);
  indexes.add('Observation', 'data-absent-reason', 'The reason why the expected value in the element Observation.value[x] is missing.', sptTOKEN, [], '', sxpNormal);
  indexes.add('Observation', 'date', 'Obtained date/time. If the obtained element is a period, a date that falls in the period', sptDATE, [], '', sxpNormal);
  indexes.add('Observation', 'device', 'The Device that generated the observation data.', sptREFERENCE, ['Device', 'DeviceMetric'], '', sxpNormal);
  indexes.add('Observation', 'dna-variant', 'search for extension http://hl7.org/fhir/StructureDefinition/observation-geneticsDNASequenceVariantName', sptTOKEN, [], 'Observation.extension(''http://hl7.org/fhir/StructureDefinition/observation-geneticsDNASequenceVariantName'').value', sxpNormal);
  indexes.add('Observation', 'encounter', 'Healthcare event related to the observation', sptREFERENCE, ['Encounter'], '', sxpNormal);
  indexes.add('Observation', 'gene-dnavariant', 'search for extension http://hl7.org/fhir/StructureDefinition/observation-geneticsDNAVariantId', sptTOKEN, [], 'Observation.extension(''http://hl7.org/fhir/StructureDefinition/observation-geneticsDNAVariantId'').value', sxpNormal);
  indexes.add('Observation', 'gene-identifier', 'search for extension http://hl7.org/fhir/StructureDefinition/observation-geneticsGene', sptTOKEN, [], 'Observation.extension(''http://hl7.org/fhir/StructureDefinition/observation-geneticsGene'').value', sxpNormal);
  indexes.add('Observation', 'identifier', 'The unique id for a particular observation', sptTOKEN, [], '', sxpNormal);
  indexes.add('Observation', 'patient', 'The subject that the observation is about (if patient)', sptREFERENCE, ['Group', 'Device', 'Patient', 'Location'], '', sxpNormal);
  indexes.add('Observation', 'performer', 'Who performed the observation', sptREFERENCE, ['Practitioner', 'Organization', 'Patient', 'RelatedPerson'], '', sxpNormal);
  indexes.add('Observation', 'related', 'Related Observations - search on related-type and related-target together', sptCOMPOSITE, [], '', sxpNull);
  indexes.add('Observation', 'related-target', 'Resource that is related to this one', sptREFERENCE, ['Observation', 'QuestionnaireResponse'], '', sxpNormal);
  indexes.add('Observation', 'related-type', 'has-member | derived-from | sequel-to | replaces | qualified-by | interfered-by', sptTOKEN, [], '', sxpNormal);
  indexes.add('Observation', 'specimen', 'Specimen used for this observation', sptREFERENCE, ['Specimen'], '', sxpNormal);
  indexes.add('Observation', 'status', 'The status of the observation', sptTOKEN, [], '', sxpNormal);
  indexes.add('Observation', 'subject', 'The subject that the observation is about', sptREFERENCE, ['Group', 'Device', 'Patient', 'Location'], '', sxpNormal);
  indexes.add('Observation', 'value-concept', 'The value of the observation, if the value is a CodeableConcept', sptTOKEN, [], '', sxpNormal);
  indexes.add('Observation', 'value-date', 'The value of the observation, if the value is a date or period of time', sptDATE, [], '', sxpNormal);
  indexes.add('Observation', 'value-quantity', 'The value of the observation, if the value is a Quantity, or a SampledData (just search on the bounds of the values in sampled data)', sptQUANTITY, [], '', sxpNormal);
  indexes.add('Observation', 'value-string', 'The value of the observation, if the value is a string, and also searches in CodeableConcept.text', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_OPERATIONDEFINITION}
procedure TFHIRIndexBuilderR2.buildIndexesForOperationDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('OperationDefinition', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('OperationDefinition', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('OperationDefinition', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('OperationDefinition', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('OperationDefinition', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('OperationDefinition', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('OperationDefinition', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('OperationDefinition', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('OperationDefinition', 'base', 'Marks this as a profile of the base', sptREFERENCE, ['OperationDefinition'], '', sxpNormal);
  indexes.add('OperationDefinition', 'code', 'Name used to invoke the operation', sptTOKEN, [], '', sxpNormal);
  indexes.add('OperationDefinition', 'date', 'Date for this version of the operation definition', sptDATE, [], '', sxpNormal);
  indexes.add('OperationDefinition', 'instance', 'Invoke on an instance?', sptTOKEN, [], '', sxpNormal);
  indexes.add('OperationDefinition', 'kind', 'operation | query', sptTOKEN, [], '', sxpNormal);
  indexes.add('OperationDefinition', 'name', 'Informal name for this operation', sptSTRING, [], '', sxpNormal);
  indexes.add('OperationDefinition', 'profile', 'Profile on the type', sptREFERENCE, ['StructureDefinition'], '', sxpNormal);
  indexes.add('OperationDefinition', 'publisher', 'Name of the publisher (Organization or individual)', sptSTRING, [], '', sxpNormal);
  indexes.add('OperationDefinition', 'status', 'draft | active | retired', sptTOKEN, [], '', sxpNormal);
  indexes.add('OperationDefinition', 'system', 'Invoke at the system level?', sptTOKEN, [], '', sxpNormal);
  indexes.add('OperationDefinition', 'type', 'Invoke at resource level for these type', sptTOKEN, [], '', sxpNormal);
  indexes.add('OperationDefinition', 'url', 'Logical URL to reference this operation definition', sptURI, [], '', sxpNormal);
  indexes.add('OperationDefinition', 'version', 'Logical id for this version of the operation definition', sptTOKEN, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_OPERATIONOUTCOME}
procedure TFHIRIndexBuilderR2.buildIndexesForOperationOutcome(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('OperationOutcome', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('OperationOutcome', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('OperationOutcome', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('OperationOutcome', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('OperationOutcome', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('OperationOutcome', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('OperationOutcome', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('OperationOutcome', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
end;
{$ENDIF}

{$IFDEF FHIR_ORDER}
procedure TFHIRIndexBuilderR2.buildIndexesForOrder(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Order', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Order', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('Order', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('Order', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('Order', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('Order', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Order', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Order', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Order', 'date', 'When the order was made', sptDATE, [], '', sxpNormal);
  indexes.add('Order', 'detail', 'What action is being ordered', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, '', sxpNormal);
  indexes.add('Order', 'identifier', 'Instance id from source, target, and/or  others', sptTOKEN, [], '', sxpNormal);
  indexes.add('Order', 'patient', 'Patient this order is about', sptREFERENCE, ['Group', 'Device', 'Patient', 'Substance'], '', sxpNormal);
  indexes.add('Order', 'source', 'Who initiated the order', sptREFERENCE, ['Practitioner', 'Organization'], '', sxpNormal);
  indexes.add('Order', 'subject', 'Patient this order is about', sptREFERENCE, ['Group', 'Device', 'Patient', 'Substance'], '', sxpNormal);
  indexes.add('Order', 'target', 'Who is intended to fulfill the order', sptREFERENCE, ['Practitioner', 'Organization', 'Device'], '', sxpNormal);
  indexes.add('Order', 'when', 'A formal schedule', sptDATE, [], '', sxpNormal);
  indexes.add('Order', 'when_code', 'Code specifies when request should be done. The code may simply be a priority code', sptTOKEN, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_ORDERRESPONSE}
procedure TFHIRIndexBuilderR2.buildIndexesForOrderResponse(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('OrderResponse', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('OrderResponse', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('OrderResponse', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('OrderResponse', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('OrderResponse', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('OrderResponse', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('OrderResponse', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('OrderResponse', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('OrderResponse', 'code', 'pending | review | rejected | error | accepted | cancelled | replaced | aborted | completed', sptTOKEN, [], '', sxpNormal);
  indexes.add('OrderResponse', 'date', 'When the response was made', sptDATE, [], '', sxpNormal);
  indexes.add('OrderResponse', 'fulfillment', 'Details of the outcome of performing the order', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, '', sxpNormal);
  indexes.add('OrderResponse', 'identifier', 'Identifiers assigned to this order by the orderer or by the receiver', sptTOKEN, [], '', sxpNormal);
  indexes.add('OrderResponse', 'request', 'The order that this is a response to', sptREFERENCE, ['Order'], '', sxpNormal);
  indexes.add('OrderResponse', 'who', 'Who made the response', sptREFERENCE, ['Practitioner', 'Organization', 'Device'], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_ORGANIZATION}
procedure TFHIRIndexBuilderR2.buildIndexesForOrganization(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Organization', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Organization', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('Organization', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('Organization', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('Organization', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('Organization', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Organization', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Organization', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Organization', 'active', 'Whether the organization''s record is active', sptTOKEN, [], '', sxpNormal);
  indexes.add('Organization', 'address', 'A (part of the) address of the Organization', sptSTRING, [], '', sxpNormal);
  indexes.add('Organization', 'address-city', 'A city specified in an address', sptSTRING, [], '', sxpNormal);
  indexes.add('Organization', 'address-country', 'A country specified in an address', sptSTRING, [], '', sxpNormal);
  indexes.add('Organization', 'address-postalcode', 'A postal code specified in an address', sptSTRING, [], '', sxpNormal);
  indexes.add('Organization', 'address-state', 'A state specified in an address', sptSTRING, [], '', sxpNormal);
  indexes.add('Organization', 'address-use', 'A use code specified in an address', sptTOKEN, [], '', sxpNormal);
  indexes.add('Organization', 'identifier', 'Any identifier for the organization (not the accreditation issuer''s identifier)', sptTOKEN, [], '', sxpNormal);
  indexes.add('Organization', 'name', 'A portion of the organization''s name', sptSTRING, [], '', sxpNormal);
  indexes.add('Organization', 'partof', 'Search all organizations that are part of the given organization', sptREFERENCE, ['Organization'], '', sxpNormal);
  indexes.add('Organization', 'phonetic', 'A portion of the organization''s name using some kind of phonetic matching algorithm', sptSTRING, [], '', sxpPhonetic);
  indexes.add('Organization', 'type', 'A code for the type of organization', sptTOKEN, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_PARAMETERS}
procedure TFHIRIndexBuilderR2.buildIndexesForParameters(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Parameters', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Parameters', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('Parameters', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('Parameters', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('Parameters', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('Parameters', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Parameters', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_PATIENT}
procedure TFHIRIndexBuilderR2.buildIndexesForPatient(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Patient', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Patient', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('Patient', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('Patient', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('Patient', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('Patient', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Patient', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Patient', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Patient', 'active', 'Whether the patient record is active', sptTOKEN, [], '', sxpNormal);
  indexes.add('Patient', 'address', 'An address in any kind of address/part of the patient', sptSTRING, [], '', sxpNormal);
  indexes.add('Patient', 'address-city', 'A city specified in an address', sptSTRING, [], '', sxpNormal);
  indexes.add('Patient', 'address-country', 'A country specified in an address', sptSTRING, [], '', sxpNormal);
  indexes.add('Patient', 'address-postalcode', 'A postalCode specified in an address', sptSTRING, [], '', sxpNormal);
  indexes.add('Patient', 'address-state', 'A state specified in an address', sptSTRING, [], '', sxpNormal);
  indexes.add('Patient', 'address-use', 'A use code specified in an address', sptTOKEN, [], '', sxpNormal);
  indexes.add('Patient', 'animal-breed', 'The breed for animal patients', sptTOKEN, [], '', sxpNormal);
  indexes.add('Patient', 'animal-species', 'The species for animal patients', sptTOKEN, [], '', sxpNormal);
  indexes.add('Patient', 'birthdate', 'The patient''s date of birth', sptDATE, [], '', sxpNormal);
  indexes.add('Patient', 'careprovider', 'Patient''s nominated care provider, could be a care manager, not the organization that manages the record', sptREFERENCE, ['Practitioner', 'Organization'], '', sxpNormal);
  indexes.add('Patient', 'deathdate', 'The date of death has been provided and satisfies this search value', sptDATE, [], '', sxpNormal);
  indexes.add('Patient', 'deceased', 'This patient has been marked as deceased, or as a death date entered', sptTOKEN, [], '', sxpNormal);
  indexes.add('Patient', 'email', 'A value in an email contact', sptTOKEN, [], '', sxpNormal);
  indexes.add('Patient', 'ethnicity', 'Returns patients with an ethnicity extension matching the specified code.', sptTOKEN, [], '', sxpNormal);
  indexes.add('Patient', 'family', 'A portion of the family name of the patient', sptSTRING, [], '', sxpNormal);
  indexes.add('Patient', 'gender', 'Gender of the patient', sptTOKEN, [], '', sxpNormal);
  indexes.add('Patient', 'given', 'A portion of the given name of the patient', sptSTRING, [], '', sxpNormal);
  indexes.add('Patient', 'identifier', 'A patient identifier', sptTOKEN, [], '', sxpNormal);
  indexes.add('Patient', 'language', 'Language code (irrespective of use value)', sptTOKEN, [], '', sxpNormal);
  indexes.add('Patient', 'link', 'All patients linked to the given patient', sptREFERENCE, ['Patient'], '', sxpNormal);
  indexes.add('Patient', 'name', 'A portion of either family or given name of the patient', sptSTRING, [], '', sxpNormal);
  indexes.add('Patient', 'organization', 'The organization at which this person is a patient', sptREFERENCE, ['Organization'], '', sxpNormal);
  indexes.add('Patient', 'phone', 'A value in a phone contact', sptTOKEN, [], '', sxpNormal);
  indexes.add('Patient', 'phonetic', 'A portion of either family or given name using some kind of phonetic matching algorithm', sptSTRING, [], '', sxpPhonetic);
  indexes.add('Patient', 'race', 'Returns patients with a race extension matching the specified code.', sptTOKEN, [], '', sxpNormal);
  indexes.add('Patient', 'telecom', 'The value in any kind of telecom details of the patient', sptTOKEN, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_PAYMENTNOTICE}
procedure TFHIRIndexBuilderR2.buildIndexesForPaymentNotice(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('PaymentNotice', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('PaymentNotice', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('PaymentNotice', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('PaymentNotice', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('PaymentNotice', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('PaymentNotice', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('PaymentNotice', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('PaymentNotice', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('PaymentNotice', 'identifier', 'The business identifier of the Eligibility', sptTOKEN, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_PAYMENTRECONCILIATION}
procedure TFHIRIndexBuilderR2.buildIndexesForPaymentReconciliation(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('PaymentReconciliation', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('PaymentReconciliation', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('PaymentReconciliation', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('PaymentReconciliation', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('PaymentReconciliation', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('PaymentReconciliation', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('PaymentReconciliation', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('PaymentReconciliation', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('PaymentReconciliation', 'identifier', 'The business identifier of the Explanation of Benefit', sptTOKEN, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_PERSON}
procedure TFHIRIndexBuilderR2.buildIndexesForPerson(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Person', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Person', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('Person', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('Person', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('Person', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('Person', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Person', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Person', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Person', 'address', 'An address in any kind of address/part', sptSTRING, [], '', sxpNormal);
  indexes.add('Person', 'address-city', 'A city specified in an address', sptSTRING, [], '', sxpNormal);
  indexes.add('Person', 'address-country', 'A country specified in an address', sptSTRING, [], '', sxpNormal);
  indexes.add('Person', 'address-postalcode', 'A postal code specified in an address', sptSTRING, [], '', sxpNormal);
  indexes.add('Person', 'address-state', 'A state specified in an address', sptSTRING, [], '', sxpNormal);
  indexes.add('Person', 'address-use', 'A use code specified in an address', sptTOKEN, [], '', sxpNormal);
  indexes.add('Person', 'birthdate', 'The person''s date of birth', sptDATE, [], '', sxpNormal);
  indexes.add('Person', 'email', 'A value in an email contact', sptTOKEN, [], '', sxpNormal);
  indexes.add('Person', 'gender', 'The gender of the person', sptTOKEN, [], '', sxpNormal);
  indexes.add('Person', 'identifier', 'A person Identifier', sptTOKEN, [], '', sxpNormal);
  indexes.add('Person', 'link', 'Any link has this Patient, Person, RelatedPerson or Practitioner reference', sptREFERENCE, ['Practitioner', 'Patient', 'Person', 'RelatedPerson'], '', sxpNormal);
  indexes.add('Person', 'name', 'A portion of name in any name part', sptSTRING, [], '', sxpNormal);
  indexes.add('Person', 'organization', 'The organization at which this person record is being managed', sptREFERENCE, ['Organization'], '', sxpNormal);
  indexes.add('Person', 'patient', 'The Person links to this Patient', sptREFERENCE, ['Practitioner', 'Patient', 'Person', 'RelatedPerson'], '', sxpNormal);
  indexes.add('Person', 'phone', 'A value in a phone contact', sptTOKEN, [], '', sxpNormal);
  indexes.add('Person', 'phonetic', 'A portion of name using some kind of phonetic matching algorithm', sptSTRING, [], '', sxpNormal);
  indexes.add('Person', 'practitioner', 'The Person links to this Practitioner', sptREFERENCE, ['Practitioner', 'Patient', 'Person', 'RelatedPerson'], '', sxpNormal);
  indexes.add('Person', 'relatedperson', 'The Person links to this RelatedPerson', sptREFERENCE, ['Practitioner', 'Patient', 'Person', 'RelatedPerson'], '', sxpNormal);
  indexes.add('Person', 'telecom', 'The value in any kind of contact', sptTOKEN, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_PRACTITIONER}
procedure TFHIRIndexBuilderR2.buildIndexesForPractitioner(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Practitioner', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Practitioner', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('Practitioner', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('Practitioner', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('Practitioner', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('Practitioner', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Practitioner', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Practitioner', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Practitioner', 'address', 'An address in any kind of address/part', sptSTRING, [], '', sxpNormal);
  indexes.add('Practitioner', 'address-city', 'A city specified in an address', sptSTRING, [], '', sxpNormal);
  indexes.add('Practitioner', 'address-country', 'A country specified in an address', sptSTRING, [], '', sxpNormal);
  indexes.add('Practitioner', 'address-postalcode', 'A postalCode specified in an address', sptSTRING, [], '', sxpNormal);
  indexes.add('Practitioner', 'address-state', 'A state specified in an address', sptSTRING, [], '', sxpNormal);
  indexes.add('Practitioner', 'address-use', 'A use code specified in an address', sptTOKEN, [], '', sxpNormal);
  indexes.add('Practitioner', 'communication', 'One of the languages that the practitioner can communicate with', sptTOKEN, [], '', sxpNormal);
  indexes.add('Practitioner', 'email', 'A value in an email contact', sptTOKEN, [], '', sxpNormal);
  indexes.add('Practitioner', 'family', 'A portion of the family name', sptSTRING, [], '', sxpNormal);
  indexes.add('Practitioner', 'gender', 'Gender of the practitioner', sptTOKEN, [], '', sxpNormal);
  indexes.add('Practitioner', 'given', 'A portion of the given name', sptSTRING, [], '', sxpNormal);
  indexes.add('Practitioner', 'identifier', 'A practitioner''s Identifier', sptTOKEN, [], '', sxpNormal);
  indexes.add('Practitioner', 'location', 'One of the locations at which this practitioner provides care', sptREFERENCE, ['Location'], '', sxpNormal);
  indexes.add('Practitioner', 'name', 'A portion of either family or given name', sptSTRING, [], '', sxpNormal);
  indexes.add('Practitioner', 'organization', 'The identity of the organization the practitioner represents / acts on behalf of', sptREFERENCE, ['Organization'], '', sxpNormal);
  indexes.add('Practitioner', 'phone', 'A value in a phone contact', sptTOKEN, [], '', sxpNormal);
  indexes.add('Practitioner', 'phonetic', 'A portion of either family or given name using some kind of phonetic matching algorithm', sptSTRING, [], '', sxpPhonetic);
  indexes.add('Practitioner', 'role', 'The practitioner can perform this role at for the organization', sptTOKEN, [], '', sxpNormal);
  indexes.add('Practitioner', 'specialty', 'The practitioner has this specialty at an organization', sptTOKEN, [], '', sxpNormal);
  indexes.add('Practitioner', 'telecom', 'The value in any kind of contact', sptTOKEN, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_PROCEDURE}
procedure TFHIRIndexBuilderR2.buildIndexesForProcedure(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Procedure', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Procedure', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('Procedure', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('Procedure', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('Procedure', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('Procedure', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Procedure', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Procedure', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Procedure', 'code', 'A code to identify a  procedure', sptTOKEN, [], '', sxpNormal);
  indexes.add('Procedure', 'date', 'Date/Period the procedure was performed', sptDATE, [], '', sxpNormal);
  indexes.add('Procedure', 'encounter', 'The encounter associated with the procedure', sptREFERENCE, ['Encounter'], '', sxpNormal);
  indexes.add('Procedure', 'identifier', 'A unique identifier for a procedure', sptTOKEN, [], '', sxpNormal);
  indexes.add('Procedure', 'location', 'Where the procedure happened', sptREFERENCE, ['Location'], '', sxpNormal);
  indexes.add('Procedure', 'patient', 'Search by subject - a patient', sptREFERENCE, ['Group', 'Patient'], '', sxpNormal);
  indexes.add('Procedure', 'performer', 'The reference to the practitioner', sptREFERENCE, ['Practitioner', 'Organization', 'Patient', 'RelatedPerson'], '', sxpNormal);
  indexes.add('Procedure', 'subject', 'Search by subject', sptREFERENCE, ['Group', 'Patient'], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_PROCEDUREREQUEST}
procedure TFHIRIndexBuilderR2.buildIndexesForProcedureRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ProcedureRequest', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('ProcedureRequest', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('ProcedureRequest', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('ProcedureRequest', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('ProcedureRequest', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('ProcedureRequest', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('ProcedureRequest', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('ProcedureRequest', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('ProcedureRequest', 'encounter', 'Encounter request created during', sptREFERENCE, ['Encounter'], '', sxpNormal);
  indexes.add('ProcedureRequest', 'identifier', 'A unique identifier of the Procedure Request', sptTOKEN, [], '', sxpNormal);
  indexes.add('ProcedureRequest', 'orderer', 'Who made request', sptREFERENCE, ['Practitioner', 'Device', 'Patient', 'RelatedPerson'], '', sxpNormal);
  indexes.add('ProcedureRequest', 'patient', 'Search by subject - a patient', sptREFERENCE, ['Group', 'Patient'], '', sxpNormal);
  indexes.add('ProcedureRequest', 'performer', 'Who should perform the procedure', sptREFERENCE, ['Practitioner', 'Organization', 'Patient', 'RelatedPerson'], '', sxpNormal);
  indexes.add('ProcedureRequest', 'subject', 'Search by subject', sptREFERENCE, ['Group', 'Patient'], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_PROCESSREQUEST}
procedure TFHIRIndexBuilderR2.buildIndexesForProcessRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ProcessRequest', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('ProcessRequest', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('ProcessRequest', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('ProcessRequest', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('ProcessRequest', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('ProcessRequest', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('ProcessRequest', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('ProcessRequest', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('ProcessRequest', 'action', 'The action requested by this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('ProcessRequest', 'identifier', 'The business identifier of the ProcessRequest', sptTOKEN, [], '', sxpNormal);
  indexes.add('ProcessRequest', 'organization', 'The organization who generated this request', sptREFERENCE, ['Organization'], '', sxpNormal);
  indexes.add('ProcessRequest', 'provider', 'The provider who regenerated this request', sptREFERENCE, ['Practitioner'], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_PROCESSRESPONSE}
procedure TFHIRIndexBuilderR2.buildIndexesForProcessResponse(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ProcessResponse', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('ProcessResponse', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('ProcessResponse', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('ProcessResponse', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('ProcessResponse', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('ProcessResponse', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('ProcessResponse', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('ProcessResponse', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('ProcessResponse', 'identifier', 'The business identifier of the Explanation of Benefit', sptTOKEN, [], '', sxpNormal);
  indexes.add('ProcessResponse', 'organization', 'The organization who generated this resource', sptREFERENCE, ['Organization'], '', sxpNormal);
  indexes.add('ProcessResponse', 'request', 'The reference to the claim', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, '', sxpNormal);
  indexes.add('ProcessResponse', 'requestorganization', 'The Organization who is responsible the request transaction', sptREFERENCE, ['Organization'], '', sxpNormal);
  indexes.add('ProcessResponse', 'requestprovider', 'The Provider who is responsible the request transaction', sptREFERENCE, ['Practitioner'], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_PROVENANCE}
procedure TFHIRIndexBuilderR2.buildIndexesForProvenance(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Provenance', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Provenance', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('Provenance', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('Provenance', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('Provenance', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('Provenance', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Provenance', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Provenance', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Provenance', 'agent', 'Individual, device or organization playing role', sptREFERENCE, ['Practitioner', 'Organization', 'Device', 'Patient', 'RelatedPerson'], '', sxpNormal);
  indexes.add('Provenance', 'end', 'End time with inclusive boundary, if not ongoing', sptDATE, [], '', sxpNormal);
  indexes.add('Provenance', 'entity', 'Identity of entity', sptURI, [], '', sxpNormal);
  indexes.add('Provenance', 'entitytype', 'The type of resource in this entity', sptTOKEN, [], '', sxpNormal);
  indexes.add('Provenance', 'location', 'Where the activity occurred, if relevant', sptREFERENCE, ['Location'], '', sxpNormal);
  indexes.add('Provenance', 'patient', 'Target Reference(s) (usually version specific)', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, '', sxpNormal);
  indexes.add('Provenance', 'sigtype', 'Indication of the reason the entity signed the object(s)', sptTOKEN, [], '', sxpNormal);
  indexes.add('Provenance', 'start', 'Starting time with inclusive boundary', sptDATE, [], '', sxpNormal);
  indexes.add('Provenance', 'target', 'Target Reference(s) (usually version specific)', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, '', sxpNormal);
  indexes.add('Provenance', 'userid', 'Authorization-system identifier for the agent', sptTOKEN, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_QUESTIONNAIRE}
procedure TFHIRIndexBuilderR2.buildIndexesForQuestionnaire(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Questionnaire', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Questionnaire', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('Questionnaire', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('Questionnaire', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('Questionnaire', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('Questionnaire', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Questionnaire', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Questionnaire', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Questionnaire', 'code', 'A code that corresponds to the questionnaire or one of its groups', sptTOKEN, [], '', sxpNormal);
  indexes.add('Questionnaire', 'date', 'When the questionnaire was last changed', sptDATE, [], '', sxpNormal);
  indexes.add('Questionnaire', 'identifier', 'An identifier for the questionnaire', sptTOKEN, [], '', sxpNormal);
  indexes.add('Questionnaire', 'publisher', 'The author of the questionnaire', sptSTRING, [], '', sxpNormal);
  indexes.add('Questionnaire', 'status', 'The status of the questionnaire', sptTOKEN, [], '', sxpNormal);
  indexes.add('Questionnaire', 'title', 'All or part of the name of the questionnaire (title for the root group of the questionnaire)', sptSTRING, [], '', sxpNormal);
  indexes.add('Questionnaire', 'version', 'The business version of the questionnaire', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_QUESTIONNAIRERESPONSE}
procedure TFHIRIndexBuilderR2.buildIndexesForQuestionnaireResponse(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('QuestionnaireResponse', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('QuestionnaireResponse', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('QuestionnaireResponse', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('QuestionnaireResponse', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('QuestionnaireResponse', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('QuestionnaireResponse', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('QuestionnaireResponse', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('QuestionnaireResponse', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('QuestionnaireResponse', 'author', 'The author of the questionnaire', sptREFERENCE, ['Practitioner', 'Device', 'Patient', 'RelatedPerson'], '', sxpNormal);
  indexes.add('QuestionnaireResponse', 'authored', 'When the questionnaire was authored', sptDATE, [], '', sxpNormal);
  indexes.add('QuestionnaireResponse', 'encounter', 'Encounter during which questionnaire was authored', sptREFERENCE, ['Encounter'], '', sxpNormal);
  indexes.add('QuestionnaireResponse', 'patient', 'The patient that is the subject of the questionnaire', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, '', sxpNormal);
  indexes.add('QuestionnaireResponse', 'questionnaire', 'The questionnaire the answers are provided for', sptREFERENCE, ['Questionnaire'], '', sxpNormal);
  indexes.add('QuestionnaireResponse', 'source', 'The person who answered the questions', sptREFERENCE, ['Practitioner', 'Patient', 'RelatedPerson'], '', sxpNormal);
  indexes.add('QuestionnaireResponse', 'status', 'The status of the questionnaire response', sptTOKEN, [], '', sxpNormal);
  indexes.add('QuestionnaireResponse', 'subject', 'The subject of the questionnaire', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_REFERRALREQUEST}
procedure TFHIRIndexBuilderR2.buildIndexesForReferralRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ReferralRequest', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('ReferralRequest', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('ReferralRequest', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('ReferralRequest', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('ReferralRequest', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('ReferralRequest', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('ReferralRequest', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('ReferralRequest', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('ReferralRequest', 'date', 'Creation or activation date', sptDATE, [], '', sxpNormal);
  indexes.add('ReferralRequest', 'patient', 'Who the referral is about', sptREFERENCE, ['Patient'], '', sxpNormal);
  indexes.add('ReferralRequest', 'priority', 'The priority assigned to the referral', sptTOKEN, [], '', sxpNormal);
  indexes.add('ReferralRequest', 'recipient', 'The person that the referral was sent to', sptREFERENCE, ['Practitioner', 'Organization'], '', sxpNormal);
  indexes.add('ReferralRequest', 'requester', 'Requester of referral / transfer of care', sptREFERENCE, ['Practitioner', 'Organization', 'Patient'], '', sxpNormal);
  indexes.add('ReferralRequest', 'specialty', 'The specialty that the referral is for', sptTOKEN, [], '', sxpNormal);
  indexes.add('ReferralRequest', 'status', 'The status of the referral', sptTOKEN, [], '', sxpNormal);
  indexes.add('ReferralRequest', 'type', 'The type of the referral', sptTOKEN, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_RELATEDPERSON}
procedure TFHIRIndexBuilderR2.buildIndexesForRelatedPerson(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('RelatedPerson', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('RelatedPerson', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('RelatedPerson', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('RelatedPerson', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('RelatedPerson', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('RelatedPerson', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('RelatedPerson', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('RelatedPerson', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('RelatedPerson', 'address', 'An address in any kind of address/part', sptSTRING, [], '', sxpNormal);
  indexes.add('RelatedPerson', 'address-city', 'A city specified in an address', sptSTRING, [], '', sxpNormal);
  indexes.add('RelatedPerson', 'address-country', 'A country specified in an address', sptSTRING, [], '', sxpNormal);
  indexes.add('RelatedPerson', 'address-postalcode', 'A postal code specified in an address', sptSTRING, [], '', sxpNormal);
  indexes.add('RelatedPerson', 'address-state', 'A state specified in an address', sptSTRING, [], '', sxpNormal);
  indexes.add('RelatedPerson', 'address-use', 'A use code specified in an address', sptTOKEN, [], '', sxpNormal);
  indexes.add('RelatedPerson', 'birthdate', 'The Related Person''s date of birth', sptDATE, [], '', sxpNormal);
  indexes.add('RelatedPerson', 'email', 'A value in an email contact', sptTOKEN, [], '', sxpNormal);
  indexes.add('RelatedPerson', 'gender', 'Gender of the person', sptTOKEN, [], '', sxpNormal);
  indexes.add('RelatedPerson', 'identifier', 'A patient Identifier', sptTOKEN, [], '', sxpNormal);
  indexes.add('RelatedPerson', 'name', 'A portion of name in any name part', sptSTRING, [], '', sxpNormal);
  indexes.add('RelatedPerson', 'patient', 'The patient this person is related to', sptREFERENCE, ['Patient'], '', sxpNormal);
  indexes.add('RelatedPerson', 'phone', 'A value in a phone contact', sptTOKEN, [], '', sxpNormal);
  indexes.add('RelatedPerson', 'phonetic', 'A portion of name using some kind of phonetic matching algorithm', sptSTRING, [], '', sxpPhonetic);
  indexes.add('RelatedPerson', 'telecom', 'The value in any kind of contact', sptTOKEN, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_RISKASSESSMENT}
procedure TFHIRIndexBuilderR2.buildIndexesForRiskAssessment(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('RiskAssessment', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('RiskAssessment', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('RiskAssessment', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('RiskAssessment', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('RiskAssessment', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('RiskAssessment', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('RiskAssessment', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('RiskAssessment', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('RiskAssessment', 'condition', 'Condition assessed', sptREFERENCE, ['Condition'], '', sxpNormal);
  indexes.add('RiskAssessment', 'date', 'When was assessment made?', sptDATE, [], '', sxpNormal);
  indexes.add('RiskAssessment', 'encounter', 'Where was assessment performed?', sptREFERENCE, ['Encounter'], '', sxpNormal);
  indexes.add('RiskAssessment', 'identifier', 'Unique identifier for the assessment', sptTOKEN, [], '', sxpNormal);
  indexes.add('RiskAssessment', 'method', 'Evaluation mechanism', sptTOKEN, [], '', sxpNormal);
  indexes.add('RiskAssessment', 'patient', 'Who/what does assessment apply to?', sptREFERENCE, ['Group', 'Patient'], '', sxpNormal);
  indexes.add('RiskAssessment', 'performer', 'Who did assessment?', sptREFERENCE, ['Practitioner', 'Device'], '', sxpNormal);
  indexes.add('RiskAssessment', 'subject', 'Who/what does assessment apply to?', sptREFERENCE, ['Group', 'Patient'], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_SCHEDULE}
procedure TFHIRIndexBuilderR2.buildIndexesForSchedule(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Schedule', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Schedule', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('Schedule', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('Schedule', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('Schedule', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('Schedule', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Schedule', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Schedule', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Schedule', 'actor', 'The individual(HealthcareService, Practitioner, Location, ...) to find a Schedule for', sptREFERENCE, ['Practitioner', 'Device', 'Patient', 'HealthcareService', 'RelatedPerson', 'Location'], '', sxpNormal);
  indexes.add('Schedule', 'date', 'Search for Schedule resources that have a period that contains this date specified', sptDATE, [], '', sxpNormal);
  indexes.add('Schedule', 'identifier', 'A Schedule Identifier', sptTOKEN, [], '', sxpNormal);
  indexes.add('Schedule', 'type', 'The type of appointments that can be booked into associated slot(s)', sptTOKEN, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_SEARCHPARAMETER}
procedure TFHIRIndexBuilderR2.buildIndexesForSearchParameter(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('SearchParameter', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('SearchParameter', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('SearchParameter', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('SearchParameter', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('SearchParameter', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('SearchParameter', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('SearchParameter', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('SearchParameter', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('SearchParameter', 'base', 'The resource type this search parameter applies to', sptTOKEN, [], '', sxpNormal);
  indexes.add('SearchParameter', 'code', 'Code used in URL', sptTOKEN, [], '', sxpNormal);
  indexes.add('SearchParameter', 'description', 'Documentation for  search parameter', sptSTRING, [], '', sxpNormal);
  indexes.add('SearchParameter', 'name', 'Informal name for this search parameter', sptSTRING, [], '', sxpNormal);
  indexes.add('SearchParameter', 'target', 'Types of resource (if a resource reference)', sptTOKEN, [], '', sxpNormal);
  indexes.add('SearchParameter', 'type', 'number | date | string | token | reference | composite | quantity | uri', sptTOKEN, [], '', sxpNormal);
  indexes.add('SearchParameter', 'url', 'Absolute URL used to reference this search parameter', sptURI, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_SLOT}
procedure TFHIRIndexBuilderR2.buildIndexesForSlot(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Slot', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Slot', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('Slot', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('Slot', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('Slot', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('Slot', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Slot', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Slot', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Slot', 'fb-type', 'The free/busy status of the appointment', sptTOKEN, [], '', sxpNormal);
  indexes.add('Slot', 'identifier', 'A Slot Identifier', sptTOKEN, [], '', sxpNormal);
  indexes.add('Slot', 'schedule', 'The Schedule Resource that we are seeking a slot within', sptREFERENCE, ['Schedule'], '', sxpNormal);
  indexes.add('Slot', 'slot-type', 'The type of appointments that can be booked into the slot', sptTOKEN, [], '', sxpNormal);
  indexes.add('Slot', 'start', 'Appointment date/time.', sptDATE, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_SPECIMEN}
procedure TFHIRIndexBuilderR2.buildIndexesForSpecimen(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Specimen', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Specimen', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('Specimen', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('Specimen', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('Specimen', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('Specimen', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Specimen', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Specimen', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Specimen', 'accession', 'The accession number associated with the specimen', sptTOKEN, [], '', sxpNormal);
  indexes.add('Specimen', 'bodysite', 'The code for the body site from where the specimen originated', sptTOKEN, [], '', sxpNormal);
  indexes.add('Specimen', 'collected', 'The date the specimen was collected', sptDATE, [], '', sxpNormal);
  indexes.add('Specimen', 'collector', 'Who collected the specimen', sptREFERENCE, ['Practitioner'], '', sxpNormal);
  indexes.add('Specimen', 'container', 'The kind of specimen container', sptTOKEN, [], '', sxpNormal);
  indexes.add('Specimen', 'container-id', 'The unique identifier associated with the specimen container', sptTOKEN, [], '', sxpNormal);
  indexes.add('Specimen', 'identifier', 'The unique identifier associated with the specimen', sptTOKEN, [], '', sxpNormal);
  indexes.add('Specimen', 'parent', 'The parent of the specimen', sptREFERENCE, ['Specimen'], '', sxpNormal);
  indexes.add('Specimen', 'patient', 'The patient the specimen comes from', sptREFERENCE, ['Group', 'Device', 'Patient', 'Substance'], '', sxpNormal);
  indexes.add('Specimen', 'subject', 'The subject of the specimen', sptREFERENCE, ['Group', 'Device', 'Patient', 'Substance'], '', sxpNormal);
  indexes.add('Specimen', 'type', 'The specimen type', sptTOKEN, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_STRUCTUREDEFINITION}
procedure TFHIRIndexBuilderR2.buildIndexesForStructureDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('StructureDefinition', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('StructureDefinition', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('StructureDefinition', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('StructureDefinition', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('StructureDefinition', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('StructureDefinition', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('StructureDefinition', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('StructureDefinition', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('StructureDefinition', 'abstract', 'Whether the structure is abstract', sptTOKEN, [], '', sxpNormal);
  indexes.add('StructureDefinition', 'base', 'Structure that this set of constraints applies to', sptURI, [], '', sxpNormal);
  indexes.add('StructureDefinition', 'base-path', 'Path that identifies the base element', sptTOKEN, [], '', sxpNormal);
  indexes.add('StructureDefinition', 'code', 'A code for the profile', sptTOKEN, [], '', sxpNormal);
  indexes.add('StructureDefinition', 'context', 'A use context assigned to the structure', sptTOKEN, [], '', sxpNormal);
  indexes.add('StructureDefinition', 'context-type', 'resource | datatype | mapping | extension', sptTOKEN, [], '', sxpNormal);
  indexes.add('StructureDefinition', 'date', 'The profile publication date', sptDATE, [], '', sxpNormal);
  indexes.add('StructureDefinition', 'description', 'Text search in the description of the profile', sptSTRING, [], '', sxpNormal);
  indexes.add('StructureDefinition', 'display', 'Use this name when displaying the value', sptSTRING, [], '', sxpNormal);
  indexes.add('StructureDefinition', 'experimental', 'If for testing purposes, not real usage', sptTOKEN, [], '', sxpNormal);
  indexes.add('StructureDefinition', 'ext-context', 'Where the extension can be used in instances', sptSTRING, [], '', sxpNormal);
  indexes.add('StructureDefinition', 'identifier', 'The identifier of the profile', sptTOKEN, [], '', sxpNormal);
  indexes.add('StructureDefinition', 'kind', 'datatype | resource | logical', sptTOKEN, [], '', sxpNormal);
  indexes.add('StructureDefinition', 'name', 'Name of the profile', sptSTRING, [], '', sxpNormal);
  indexes.add('StructureDefinition', 'path', 'A path that is constrained in the profile', sptTOKEN, [], '', sxpNormal);
  indexes.add('StructureDefinition', 'publisher', 'Name of the publisher of the profile', sptSTRING, [], '', sxpNormal);
  indexes.add('StructureDefinition', 'status', 'The current status of the profile', sptTOKEN, [], '', sxpNormal);
  indexes.add('StructureDefinition', 'type', 'Any datatype or resource, including abstract ones', sptTOKEN, [], '', sxpNormal);
  indexes.add('StructureDefinition', 'url', 'Absolute URL used to reference this StructureDefinition', sptURI, [], '', sxpNormal);
  indexes.add('StructureDefinition', 'valueset', 'A vocabulary binding reference', sptREFERENCE, ['ValueSet'], '', sxpNormal);
  indexes.add('StructureDefinition', 'version', 'The version identifier of the profile', sptTOKEN, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_SUBSCRIPTION}
procedure TFHIRIndexBuilderR2.buildIndexesForSubscription(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Subscription', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Subscription', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('Subscription', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('Subscription', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('Subscription', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('Subscription', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Subscription', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Subscription', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Subscription', 'contact', 'Contact details for source (e.g. troubleshooting)', sptTOKEN, [], '', sxpNormal);
  indexes.add('Subscription', 'criteria', 'Rule for server push criteria', sptSTRING, [], '', sxpNormal);
  indexes.add('Subscription', 'payload', 'Mimetype to send, or blank for no payload', sptSTRING, [], '', sxpNormal);
  indexes.add('Subscription', 'status', 'requested | active | error | off', sptTOKEN, [], '', sxpNormal);
  indexes.add('Subscription', 'tag', 'A tag to add to matching resources', sptTOKEN, [], '', sxpNormal);
  indexes.add('Subscription', 'type', 'rest-hook | websocket | email | sms | message', sptTOKEN, [], '', sxpNormal);
  indexes.add('Subscription', 'url', 'Where the channel points to', sptURI, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_SUBSTANCE}
procedure TFHIRIndexBuilderR2.buildIndexesForSubstance(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Substance', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Substance', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('Substance', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('Substance', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('Substance', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('Substance', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Substance', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('Substance', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('Substance', 'category', 'The category of the substance', sptTOKEN, [], '', sxpNormal);
  indexes.add('Substance', 'code', 'The code of the substance', sptTOKEN, [], '', sxpNormal);
  indexes.add('Substance', 'container-identifier', 'Identifier of the package/container', sptTOKEN, [], '', sxpNormal);
  indexes.add('Substance', 'expiry', 'Expiry date of package or container of substance', sptDATE, [], '', sxpNormal);
  indexes.add('Substance', 'identifier', 'Unique identifier for the substance', sptTOKEN, [], '', sxpNormal);
  indexes.add('Substance', 'quantity', 'Amount of substance in the package', sptQUANTITY, [], '', sxpNormal);
  indexes.add('Substance', 'substance', 'A component of the substance', sptREFERENCE, ['Substance'], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_SUPPLYDELIVERY}
procedure TFHIRIndexBuilderR2.buildIndexesForSupplyDelivery(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('SupplyDelivery', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('SupplyDelivery', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('SupplyDelivery', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('SupplyDelivery', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('SupplyDelivery', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('SupplyDelivery', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('SupplyDelivery', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('SupplyDelivery', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('SupplyDelivery', 'identifier', 'External identifier', sptTOKEN, [], '', sxpNormal);
  indexes.add('SupplyDelivery', 'patient', 'Patient for whom the item is supplied', sptREFERENCE, ['Patient'], '', sxpNormal);
  indexes.add('SupplyDelivery', 'receiver', 'Who collected the Supply', sptREFERENCE, ['Practitioner'], '', sxpNormal);
  indexes.add('SupplyDelivery', 'status', 'in-progress | completed | abandoned', sptTOKEN, [], '', sxpNormal);
  indexes.add('SupplyDelivery', 'supplier', 'Dispenser', sptREFERENCE, ['Practitioner'], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_SUPPLYREQUEST}
procedure TFHIRIndexBuilderR2.buildIndexesForSupplyRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('SupplyRequest', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('SupplyRequest', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('SupplyRequest', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('SupplyRequest', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('SupplyRequest', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('SupplyRequest', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('SupplyRequest', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('SupplyRequest', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('SupplyRequest', 'date', 'When the request was made', sptDATE, [], '', sxpNormal);
  indexes.add('SupplyRequest', 'identifier', 'Unique identifier', sptTOKEN, [], '', sxpNormal);
  indexes.add('SupplyRequest', 'kind', 'The kind of supply (central, non-stock, etc.)', sptTOKEN, [], '', sxpNormal);
  indexes.add('SupplyRequest', 'patient', 'Patient for whom the item is supplied', sptREFERENCE, ['Patient'], '', sxpNormal);
  indexes.add('SupplyRequest', 'source', 'Who initiated this order', sptREFERENCE, ['Practitioner', 'Organization', 'Patient'], '', sxpNormal);
  indexes.add('SupplyRequest', 'status', 'requested | completed | failed | cancelled', sptTOKEN, [], '', sxpNormal);
  indexes.add('SupplyRequest', 'supplier', 'Who is intended to fulfill the request', sptREFERENCE, ['Organization'], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_TESTSCRIPT}
procedure TFHIRIndexBuilderR2.buildIndexesForTestScript(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('TestScript', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('TestScript', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('TestScript', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('TestScript', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('TestScript', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('TestScript', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('TestScript', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('TestScript', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('TestScript', 'description', 'Natural language description of the TestScript', sptSTRING, [], '', sxpNormal);
  indexes.add('TestScript', 'identifier', 'External identifier', sptTOKEN, [], '', sxpNormal);
  indexes.add('TestScript', 'name', 'Informal name for this TestScript', sptSTRING, [], '', sxpNormal);
  indexes.add('TestScript', 'testscript-capability', 'TestScript required and validated capability', sptSTRING, [], '', sxpNormal);
  indexes.add('TestScript', 'testscript-setup-capability', 'TestScript setup required and validated capability', sptSTRING, [], '', sxpNormal);
  indexes.add('TestScript', 'testscript-test-capability', 'TestScript test required and validated capability', sptSTRING, [], '', sxpNormal);
  indexes.add('TestScript', 'url', 'Absolute URL used to reference this TestScript', sptURI, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_VALUESET}
procedure TFHIRIndexBuilderR2.buildIndexesForValueSet(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ValueSet', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('ValueSet', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('ValueSet', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('ValueSet', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('ValueSet', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('ValueSet', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('ValueSet', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('ValueSet', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('ValueSet', 'code', 'A code defined in the value set', sptTOKEN, [], '', sxpNormal);
  indexes.add('ValueSet', 'context', 'A use context assigned to the value set', sptTOKEN, [], '', sxpNormal);
  indexes.add('ValueSet', 'date', 'The value set publication date', sptDATE, [], '', sxpNormal);
  indexes.add('ValueSet', 'description', 'Text search in the description of the value set', sptSTRING, [], '', sxpNormal);
  indexes.add('ValueSet', 'expansion', 'Uniquely identifies this expansion', sptURI, [], '', sxpNormal);
  indexes.add('ValueSet', 'identifier', 'The identifier for the value set', sptTOKEN, [], '', sxpNormal);
  indexes.add('ValueSet', 'name', 'The name of the value set', sptSTRING, [], '', sxpNormal);
  indexes.add('ValueSet', 'publisher', 'Name of the publisher of the value set', sptSTRING, [], '', sxpNormal);
  indexes.add('ValueSet', 'reference', 'A code system included or excluded in the value set or an imported value set', sptURI, [], '', sxpNormal);
  indexes.add('ValueSet', 'status', 'The status of the value set', sptTOKEN, [], '', sxpNormal);
  indexes.add('ValueSet', 'system', 'The system for any codes defined by this value set', sptURI, [], '', sxpNormal);
  indexes.add('ValueSet', 'url', 'The logical URL for the value set', sptURI, [], '', sxpNormal);
  indexes.add('ValueSet', 'version', 'The version identifier of the value set', sptTOKEN, [], '', sxpNormal);
end;
{$ENDIF}

{$IFDEF FHIR_VISIONPRESCRIPTION}
procedure TFHIRIndexBuilderR2.buildIndexesForVisionPrescription(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('VisionPrescription', '_content', 'Search on the entire content of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('VisionPrescription', '_id', 'Logical id of this artifact', sptTOKEN, [], '', sxpNormal);
  indexes.add('VisionPrescription', '_lastUpdated', 'When the resource version last changed', sptDATE, [], '', sxpNormal);
  indexes.add('VisionPrescription', '_profile', 'Profiles this resource claims to conform to', sptURI, [], '', sxpNormal);
  indexes.add('VisionPrescription', '_query', 'A custom search profile that describes a specific defined query operation', sptTOKEN, [], '', sxpNull);
  indexes.add('VisionPrescription', '_security', 'Security Labels applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('VisionPrescription', '_tag', 'Tags applied to this resource', sptTOKEN, [], '', sxpNormal);
  indexes.add('VisionPrescription', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNull);
  indexes.add('VisionPrescription', 'datewritten', 'Return prescriptions written on this date', sptDATE, [], '', sxpNormal);
  indexes.add('VisionPrescription', 'encounter', 'Return prescriptions with this encounter identifier', sptREFERENCE, ['Encounter'], '', sxpNormal);
  indexes.add('VisionPrescription', 'identifier', 'Return prescriptions with this external identifier', sptTOKEN, [], '', sxpNormal);
  indexes.add('VisionPrescription', 'patient', 'The identity of a patient to list dispenses for', sptREFERENCE, ['Patient'], '', sxpNormal);
  indexes.add('VisionPrescription', 'prescriber', 'Who authorizes the vision product', sptREFERENCE, ['Practitioner'], '', sxpNormal);
end;
{$ENDIF}

procedure TFHIRIndexBuilderR2.registerIndexes(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
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
  {$IFDEF FHIR_PARAMETERS}
  buildIndexesForParameters(Indexes, compartments);
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

