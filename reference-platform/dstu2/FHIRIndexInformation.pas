unit FHIRIndexInformation;

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

// FHIR v1.0.2 generated 2015-12-22T12:09:07+11:00

uses
  SysUtils, Classes, StringSupport, DecimalSupport, AdvBuffers, DateAndTime, FHIRIndexManagers, FHIRResources, FHIRTypes, FHIRConstants, FHIRSupport;

Type

  TFHIRIndexBuilder = class (TAdvObject)
  private
    procedure buildIndexesForAccount(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForAllergyIntolerance(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForAppointment(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForAppointmentResponse(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForAuditEvent(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForBasic(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForBinary(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForBodySite(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForBundle(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForCarePlan(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForClaim(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForClaimResponse(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForClinicalImpression(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForCommunication(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForCommunicationRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForComposition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForConceptMap(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForCondition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForConformance(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForContract(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForCoverage(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForDataElement(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForDetectedIssue(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForDevice(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForDeviceComponent(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForDeviceMetric(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForDeviceUseRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForDeviceUseStatement(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForDiagnosticOrder(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForDiagnosticReport(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForDocumentManifest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForDocumentReference(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForEligibilityRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForEligibilityResponse(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForEncounter(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForEnrollmentRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForEnrollmentResponse(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForEpisodeOfCare(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForExplanationOfBenefit(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForFamilyMemberHistory(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForFlag(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForGoal(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForGroup(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForHealthcareService(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForImagingObjectSelection(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForImagingStudy(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForImmunization(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForImmunizationRecommendation(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForImplementationGuide(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForList(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForLocation(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForMedia(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForMedication(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForMedicationAdministration(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForMedicationDispense(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForMedicationOrder(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForMedicationStatement(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForMessageHeader(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForNamingSystem(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForNutritionOrder(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForObservation(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForOperationDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForOperationOutcome(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForOrder(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForOrderResponse(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForOrganization(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForPatient(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForPaymentNotice(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForPaymentReconciliation(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForPerson(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForPractitioner(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForProcedure(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForProcedureRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForProcessRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForProcessResponse(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForProvenance(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForQuestionnaire(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForQuestionnaireResponse(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForReferralRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForRelatedPerson(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForRiskAssessment(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForSchedule(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForSearchParameter(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForSlot(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForSpecimen(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForStructureDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForSubscription(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForSubstance(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForSupplyDelivery(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForSupplyRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForTestScript(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForValueSet(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    procedure buildIndexesForVisionPrescription(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
  public
    procedure registerIndexes(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
 end;

implementation

procedure TFHIRIndexBuilder.buildIndexesForAccount(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtAccount, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtAccount, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtAccount, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtAccount, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtAccount, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtAccount, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtAccount, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtAccount, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtAccount, 'balance', 'How much is in account?', SearchParamTypeQUANTITY, [], '', SearchXpathUsageNormal);
  indexes.add(frtAccount, 'identifier', 'Account number', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtAccount, 'name', 'Human-readable label', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtAccount, 'owner', 'Who is responsible?', SearchParamTypeREFERENCE, [frtOrganization], '', SearchXpathUsageNormal);
  indexes.add(frtAccount, 'patient', 'What is account tied to?', SearchParamTypeREFERENCE, [frtPatient], '', SearchXpathUsageNormal);
  indexes.add(frtAccount, 'period', 'Transaction window', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtAccount, 'status', 'active | inactive', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtAccount, 'subject', 'What is account tied to?', SearchParamTypeREFERENCE, [frtDevice, frtPatient, frtLocation, frtHealthcareService, frtOrganization, frtPractitioner], '', SearchXpathUsageNormal);
  indexes.add(frtAccount, 'type', 'E.g. patient, expense, depreciation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForAllergyIntolerance(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtAllergyIntolerance, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtAllergyIntolerance, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtAllergyIntolerance, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtAllergyIntolerance, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtAllergyIntolerance, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtAllergyIntolerance, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtAllergyIntolerance, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtAllergyIntolerance, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtAllergyIntolerance, 'category', 'food | medication | environment | other - Category of Substance', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtAllergyIntolerance, 'criticality', 'CRITL | CRITH | CRITU', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtAllergyIntolerance, 'date', 'When recorded', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtAllergyIntolerance, 'identifier', 'External ids for this item', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtAllergyIntolerance, 'last-date', 'Date(/time) of last known occurrence of a reaction', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtAllergyIntolerance, 'manifestation', 'Clinical symptoms/signs associated with the Event', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtAllergyIntolerance, 'onset', 'Date(/time) when manifestations showed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtAllergyIntolerance, 'patient', 'Who the sensitivity is for', SearchParamTypeREFERENCE, [frtPatient], '', SearchXpathUsageNormal);
  indexes.add(frtAllergyIntolerance, 'recorder', 'Who recorded the sensitivity', SearchParamTypeREFERENCE, [frtPatient, frtPractitioner], '', SearchXpathUsageNormal);
  indexes.add(frtAllergyIntolerance, 'reporter', 'Source of the information about the allergy', SearchParamTypeREFERENCE, [frtPatient, frtPractitioner, frtRelatedPerson], '', SearchXpathUsageNormal);
  indexes.add(frtAllergyIntolerance, 'route', 'How the subject was exposed to the substance', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtAllergyIntolerance, 'severity', 'mild | moderate | severe (of event as a whole)', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtAllergyIntolerance, 'status', 'active | unconfirmed | confirmed | inactive | resolved | refuted | entered-in-error', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtAllergyIntolerance, 'substance', 'Substance, (or class) considered to be responsible for risk', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtAllergyIntolerance, 'type', 'allergy | intolerance - Underlying mechanism (if known)', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForAppointment(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtAppointment, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtAppointment, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtAppointment, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtAppointment, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtAppointment, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtAppointment, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtAppointment, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtAppointment, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtAppointment, 'actor', 'Any one of the individuals participating in the appointment', SearchParamTypeREFERENCE, [frtDevice, frtPatient, frtHealthcareService, frtLocation, frtPractitioner, frtRelatedPerson], '', SearchXpathUsageNormal);
  indexes.add(frtAppointment, 'date', 'Appointment date/time.', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtAppointment, 'identifier', 'An Identifier of the Appointment', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtAppointment, 'location', 'This location is listed in the participants of the appointment', SearchParamTypeREFERENCE, [frtLocation], '', SearchXpathUsageNormal);
  indexes.add(frtAppointment, 'part-status', 'The Participation status of the subject, or other participant on the appointment. Can be used to locate participants that have not responded to meeting requests.', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtAppointment, 'patient', 'One of the individuals of the appointment is this patient', SearchParamTypeREFERENCE, [frtPatient], '', SearchXpathUsageNormal);
  indexes.add(frtAppointment, 'practitioner', 'One of the individuals of the appointment is this practitioner', SearchParamTypeREFERENCE, [frtPractitioner], '', SearchXpathUsageNormal);
  indexes.add(frtAppointment, 'status', 'The overall status of the appointment', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForAppointmentResponse(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtAppointmentResponse, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtAppointmentResponse, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtAppointmentResponse, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtAppointmentResponse, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtAppointmentResponse, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtAppointmentResponse, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtAppointmentResponse, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtAppointmentResponse, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtAppointmentResponse, 'actor', 'The Person, Location/HealthcareService or Device that this appointment response replies for', SearchParamTypeREFERENCE, [frtDevice, frtPatient, frtHealthcareService, frtLocation, frtPractitioner, frtRelatedPerson], '', SearchXpathUsageNormal);
  indexes.add(frtAppointmentResponse, 'appointment', 'The appointment that the response is attached to', SearchParamTypeREFERENCE, [frtAppointment], '', SearchXpathUsageNormal);
  indexes.add(frtAppointmentResponse, 'identifier', 'An Identifier in this appointment response', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtAppointmentResponse, 'location', 'This Response is for this Location', SearchParamTypeREFERENCE, [frtLocation], '', SearchXpathUsageNormal);
  indexes.add(frtAppointmentResponse, 'part-status', 'The participants acceptance status for this appointment', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtAppointmentResponse, 'patient', 'This Response is for this Patient', SearchParamTypeREFERENCE, [frtPatient], '', SearchXpathUsageNormal);
  indexes.add(frtAppointmentResponse, 'practitioner', 'This Response is for this Practitioner', SearchParamTypeREFERENCE, [frtPractitioner], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForAuditEvent(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtAuditEvent, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtAuditEvent, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtAuditEvent, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtAuditEvent, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtAuditEvent, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtAuditEvent, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtAuditEvent, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtAuditEvent, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtAuditEvent, 'action', 'Type of action performed during the event', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtAuditEvent, 'address', 'Identifier for the network access point of the user device', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtAuditEvent, 'altid', 'Alternative User id e.g. authentication', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtAuditEvent, 'date', 'Time when the event occurred on source', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtAuditEvent, 'desc', 'Instance-specific descriptor for Object', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtAuditEvent, 'identity', 'Specific instance of object (e.g. versioned)', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtAuditEvent, 'name', 'Human-meaningful name for the user', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtAuditEvent, 'object-type', 'Type of object involved', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtAuditEvent, 'participant', 'Direct reference to resource', SearchParamTypeREFERENCE, [frtDevice, frtOrganization, frtPatient, frtPractitioner, frtRelatedPerson], '', SearchXpathUsageNormal);
  indexes.add(frtAuditEvent, 'patient', 'Direct reference to resource', SearchParamTypeREFERENCE, [frtPatient], '', SearchXpathUsageNormal);
  indexes.add(frtAuditEvent, 'policy', 'Policy that authorized event', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtAuditEvent, 'reference', 'Specific instance of resource (e.g. versioned)', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPES, '', SearchXpathUsageNormal);
  indexes.add(frtAuditEvent, 'site', 'Logical source location within the enterprise', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtAuditEvent, 'source', 'The identity of source detecting the event', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtAuditEvent, 'subtype', 'More specific type/id for the event', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtAuditEvent, 'type', 'Type/identifier of event', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtAuditEvent, 'user', 'Unique identifier for the user', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForBasic(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtBasic, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtBasic, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtBasic, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtBasic, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtBasic, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtBasic, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtBasic, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtBasic, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtBasic, 'author', 'Who created', SearchParamTypeREFERENCE, [frtPatient, frtPractitioner, frtRelatedPerson], '', SearchXpathUsageNormal);
  indexes.add(frtBasic, 'code', 'Kind of Resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtBasic, 'created', 'When created', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtBasic, 'description', 'Text search against the description', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtBasic, 'identifier', 'Logical identifier for the module (e.g. CMS-143)', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtBasic, 'keyword', 'Keywords associated with the module', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtBasic, 'minScore', 'The minimum relevance score of any match that will be returned', SearchParamTypeNUMBER, [], '', SearchXpathUsageNormal);
  indexes.add(frtBasic, 'patient', 'Identifies the focus of this resource', SearchParamTypeREFERENCE, [frtPatient], '', SearchXpathUsageNormal);
  indexes.add(frtBasic, 'status', 'Status of the module', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtBasic, 'subject', 'Identifies the focus of this resource', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPES, '', SearchXpathUsageNormal);
  indexes.add(frtBasic, 'title', 'Text search against the title', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtBasic, 'topic', 'Topics associated with the module', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtBasic, 'version', 'Version of the module (e.g. 1.0.0)', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForBinary(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtBinary, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtBinary, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtBinary, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtBinary, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtBinary, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtBinary, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtBinary, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtBinary, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtBinary, 'contenttype', 'MimeType of the binary content', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForBodySite(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtBodySite, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtBodySite, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtBodySite, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtBodySite, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtBodySite, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtBodySite, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtBodySite, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtBodySite, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtBodySite, 'code', 'Named anatomical location', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtBodySite, 'identifier', 'Identifier for this instance of the anatomical location', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtBodySite, 'patient', 'Patient to whom bodysite belongs', SearchParamTypeREFERENCE, [frtPatient], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForBundle(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtBundle, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtBundle, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtBundle, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtBundle, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtBundle, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtBundle, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtBundle, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtBundle, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtBundle, 'composition', 'The first resource in the bundle, if the bundle type is "document" - this is a composition, and this parameter provides access to searches its contents', SearchParamTypeREFERENCE, [frtComposition], '', SearchXpathUsageNormal);
  indexes.add(frtBundle, 'message', 'The first resource in the bundle, if the bundle type is "message" - this is a message header, and this parameter provides access to search its contents', SearchParamTypeREFERENCE, [frtMessageHeader], '', SearchXpathUsageNormal);
  indexes.add(frtBundle, 'type', 'document | message | transaction | transaction-response | batch | batch-response | history | searchset | collection', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForCarePlan(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtCarePlan, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtCarePlan, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtCarePlan, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtCarePlan, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtCarePlan, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtCarePlan, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtCarePlan, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtCarePlan, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtCarePlan, 'activitycode', 'Detail type of activity', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtCarePlan, 'activitydate', 'Specified date occurs within period specified by CarePlan.activity.timingSchedule', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtCarePlan, 'activityreference', 'Activity details defined in specific resource', SearchParamTypeREFERENCE, [frtReferralRequest, frtAppointment, frtProcedureRequest, frtCommunicationRequest, frtOrder, frtSupplyRequest, frtVisionPrescription, frtMedicationOrder, frtDeviceUseRequest, frtProcessRequest, frtDiagnosticOrder, frtNutritionOrder], '', SearchXpathUsageNormal);
  indexes.add(frtCarePlan, 'condition', 'Health issues this plan addresses', SearchParamTypeREFERENCE, [frtCondition], '', SearchXpathUsageNormal);
  indexes.add(frtCarePlan, 'date', 'Time period plan covers', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtCarePlan, 'goal', 'Desired outcome of plan', SearchParamTypeREFERENCE, [frtGoal], '', SearchXpathUsageNormal);
  indexes.add(frtCarePlan, 'participant', 'Who is involved', SearchParamTypeREFERENCE, [frtPatient, frtOrganization, frtPractitioner, frtRelatedPerson], '', SearchXpathUsageNormal);
  indexes.add(frtCarePlan, 'patient', 'Who care plan is for', SearchParamTypeREFERENCE, [frtPatient], '', SearchXpathUsageNormal);
  indexes.add(frtCarePlan, 'performer', 'Matches if the practitioner is listed as a performer in any of the "simple" activities.  (For performers of the detailed activities, chain through the activitydetail search parameter.)', SearchParamTypeREFERENCE, [frtOrganization, frtPatient, frtPractitioner, frtRelatedPerson], '', SearchXpathUsageNormal);
  indexes.add(frtCarePlan, 'related', 'A combination of the type of relationship and the related plan', SearchParamTypeCOMPOSITE, [], '', SearchXpathUsageNormal);
  indexes.add(frtCarePlan, 'relatedcode', 'includes | replaces | fulfills', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtCarePlan, 'relatedplan', 'Plan relationship exists with', SearchParamTypeREFERENCE, [frtCarePlan], '', SearchXpathUsageNormal);
  indexes.add(frtCarePlan, 'subject', 'Who care plan is for', SearchParamTypeREFERENCE, [frtPatient, frtGroup], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForClaim(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtClaim, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtClaim, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtClaim, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtClaim, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtClaim, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtClaim, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtClaim, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtClaim, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtClaim, 'identifier', 'The primary identifier of the financial resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtClaim, 'patient', 'Patient', SearchParamTypeREFERENCE, [frtPatient], '', SearchXpathUsageNormal);
  indexes.add(frtClaim, 'priority', 'Processing priority requested', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtClaim, 'provider', 'Provider responsible for the claim', SearchParamTypeREFERENCE, [frtPractitioner], '', SearchXpathUsageNormal);
  indexes.add(frtClaim, 'use', 'The kind of financial resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForClaimResponse(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtClaimResponse, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtClaimResponse, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtClaimResponse, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtClaimResponse, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtClaimResponse, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtClaimResponse, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtClaimResponse, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtClaimResponse, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtClaimResponse, 'identifier', 'The identity of the insurer', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForClinicalImpression(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtClinicalImpression, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtClinicalImpression, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtClinicalImpression, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtClinicalImpression, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtClinicalImpression, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtClinicalImpression, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtClinicalImpression, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtClinicalImpression, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtClinicalImpression, 'action', 'Actions taken during assessment', SearchParamTypeREFERENCE, [frtReferralRequest, frtProcedureRequest, frtAppointment, frtSupplyRequest, frtProcedure, frtMedicationOrder, frtDiagnosticOrder, frtNutritionOrder], '', SearchXpathUsageNormal);
  indexes.add(frtClinicalImpression, 'assessor', 'The clinician performing the assessment', SearchParamTypeREFERENCE, [frtPractitioner], '', SearchXpathUsageNormal);
  indexes.add(frtClinicalImpression, 'date', 'When the assessment occurred', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtClinicalImpression, 'finding', 'Specific text or code for finding', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtClinicalImpression, 'investigation', 'Record of a specific investigation', SearchParamTypeREFERENCE, [frtFamilyMemberHistory, frtObservation, frtQuestionnaireResponse, frtDiagnosticReport], '', SearchXpathUsageNormal);
  indexes.add(frtClinicalImpression, 'patient', 'The patient being assessed', SearchParamTypeREFERENCE, [frtPatient], '', SearchXpathUsageNormal);
  indexes.add(frtClinicalImpression, 'plan', 'Plan of action after assessment', SearchParamTypeREFERENCE, [frtCarePlan, frtReferralRequest, frtProcedureRequest, frtCommunicationRequest, frtOrder, frtVisionPrescription, frtProcessRequest, frtDeviceUseRequest, frtAppointment, frtSupplyRequest, frtMedicationOrder, frtDiagnosticOrder, frtNutritionOrder], '', SearchXpathUsageNormal);
  indexes.add(frtClinicalImpression, 'previous', 'Reference to last assessment', SearchParamTypeREFERENCE, [frtClinicalImpression], '', SearchXpathUsageNormal);
  indexes.add(frtClinicalImpression, 'problem', 'General assessment of patient state', SearchParamTypeREFERENCE, [frtCondition, frtAllergyIntolerance], '', SearchXpathUsageNormal);
  indexes.add(frtClinicalImpression, 'resolved', 'Diagnoses/conditions resolved since previous assessment', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtClinicalImpression, 'ruledout', 'Specific text of code for diagnosis', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtClinicalImpression, 'status', 'in-progress | completed | entered-in-error', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtClinicalImpression, 'trigger', 'Request or event that necessitated this assessment', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPES, '', SearchXpathUsageNormal);
  indexes.add(frtClinicalImpression, 'trigger-code', 'Request or event that necessitated this assessment', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForCommunication(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtCommunication, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtCommunication, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtCommunication, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtCommunication, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtCommunication, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtCommunication, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtCommunication, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtCommunication, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtCommunication, 'category', 'Message category', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtCommunication, 'encounter', 'Encounter leading to message', SearchParamTypeREFERENCE, [frtEncounter], '', SearchXpathUsageNormal);
  indexes.add(frtCommunication, 'identifier', 'Unique identifier', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtCommunication, 'medium', 'A channel of communication', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtCommunication, 'patient', 'Focus of message', SearchParamTypeREFERENCE, [frtPatient], '', SearchXpathUsageNormal);
  indexes.add(frtCommunication, 'received', 'When received', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtCommunication, 'recipient', 'Message recipient', SearchParamTypeREFERENCE, [frtDevice, frtOrganization, frtPatient, frtPractitioner, frtGroup, frtRelatedPerson], '', SearchXpathUsageNormal);
  indexes.add(frtCommunication, 'request', 'CommunicationRequest producing this message', SearchParamTypeREFERENCE, [frtCommunicationRequest], '', SearchXpathUsageNormal);
  indexes.add(frtCommunication, 'sender', 'Message sender', SearchParamTypeREFERENCE, [frtDevice, frtOrganization, frtPatient, frtPractitioner, frtRelatedPerson], '', SearchXpathUsageNormal);
  indexes.add(frtCommunication, 'sent', 'When sent', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtCommunication, 'status', 'in-progress | completed | suspended | rejected | failed', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtCommunication, 'subject', 'Focus of message', SearchParamTypeREFERENCE, [frtPatient], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForCommunicationRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtCommunicationRequest, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtCommunicationRequest, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtCommunicationRequest, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtCommunicationRequest, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtCommunicationRequest, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtCommunicationRequest, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtCommunicationRequest, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtCommunicationRequest, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtCommunicationRequest, 'category', 'Message category', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtCommunicationRequest, 'encounter', 'Encounter leading to message', SearchParamTypeREFERENCE, [frtEncounter], '', SearchXpathUsageNormal);
  indexes.add(frtCommunicationRequest, 'identifier', 'Unique identifier', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtCommunicationRequest, 'medium', 'A channel of communication', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtCommunicationRequest, 'patient', 'Focus of message', SearchParamTypeREFERENCE, [frtPatient], '', SearchXpathUsageNormal);
  indexes.add(frtCommunicationRequest, 'priority', 'Message urgency', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtCommunicationRequest, 'recipient', 'Message recipient', SearchParamTypeREFERENCE, [frtDevice, frtOrganization, frtPatient, frtPractitioner, frtRelatedPerson], '', SearchXpathUsageNormal);
  indexes.add(frtCommunicationRequest, 'requested', 'When ordered or proposed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtCommunicationRequest, 'requester', 'An individual who requested a communication', SearchParamTypeREFERENCE, [frtPatient, frtPractitioner, frtRelatedPerson], '', SearchXpathUsageNormal);
  indexes.add(frtCommunicationRequest, 'sender', 'Message sender', SearchParamTypeREFERENCE, [frtDevice, frtOrganization, frtPatient, frtPractitioner, frtRelatedPerson], '', SearchXpathUsageNormal);
  indexes.add(frtCommunicationRequest, 'status', 'proposed | planned | requested | received | accepted | in-progress | completed | suspended | rejected | failed', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtCommunicationRequest, 'subject', 'Focus of message', SearchParamTypeREFERENCE, [frtPatient], '', SearchXpathUsageNormal);
  indexes.add(frtCommunicationRequest, 'time', 'When scheduled', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForComposition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtComposition, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtComposition, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtComposition, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtComposition, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtComposition, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtComposition, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtComposition, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtComposition, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtComposition, 'attester', 'Who attested the composition', SearchParamTypeREFERENCE, [frtPatient, frtOrganization, frtPractitioner], '', SearchXpathUsageNormal);
  indexes.add(frtComposition, 'author', 'Who and/or what authored the composition', SearchParamTypeREFERENCE, [frtDevice, frtPatient, frtPractitioner, frtRelatedPerson], '', SearchXpathUsageNormal);
  indexes.add(frtComposition, 'class', 'Categorization of Composition', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtComposition, 'confidentiality', 'As defined by affinity domain', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtComposition, 'context', 'Code(s) that apply to the event being documented', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtComposition, 'date', 'Composition editing time', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtComposition, 'encounter', 'Context of the Composition', SearchParamTypeREFERENCE, [frtEncounter], '', SearchXpathUsageNormal);
  indexes.add(frtComposition, 'entry', 'A reference to data that supports this section', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPES, '', SearchXpathUsageNormal);
  indexes.add(frtComposition, 'identifier', 'Logical identifier of composition (version-independent)', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtComposition, 'patient', 'Who and/or what the composition is about', SearchParamTypeREFERENCE, [frtPatient], '', SearchXpathUsageNormal);
  indexes.add(frtComposition, 'period', 'The period covered by the documentation', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtComposition, 'section', 'Classification of section (recommended)', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtComposition, 'status', 'preliminary | final | amended | entered-in-error', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtComposition, 'subject', 'Who and/or what the composition is about', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPES, '', SearchXpathUsageNormal);
  indexes.add(frtComposition, 'title', 'Human Readable name/title', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtComposition, 'type', 'Kind of composition (LOINC if possible)', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForConceptMap(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtConceptMap, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtConceptMap, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtConceptMap, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtConceptMap, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtConceptMap, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtConceptMap, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtConceptMap, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtConceptMap, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtConceptMap, 'context', 'A use context assigned to the concept map', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtConceptMap, 'date', 'The concept map publication date', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtConceptMap, 'dependson', 'Reference to element/field/ValueSet mapping depends on', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtConceptMap, 'description', 'Text search in the description of the concept map', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtConceptMap, 'identifier', 'Additional identifier for the concept map', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtConceptMap, 'name', 'Name of the concept map', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtConceptMap, 'product', 'Reference to element/field/ValueSet mapping depends on', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtConceptMap, 'publisher', 'Name of the publisher of the concept map', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtConceptMap, 'source', 'Identifies the source of the concepts which are being mapped', SearchParamTypeREFERENCE, [frtValueSet, frtStructureDefinition], '', SearchXpathUsageNormal);
  indexes.add(frtConceptMap, 'sourcecode', 'Identifies element being mapped', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtConceptMap, 'sourcesystem', 'Code System (if value set crosses code systems)', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtConceptMap, 'sourceuri', 'Identifies the source of the concepts which are being mapped', SearchParamTypeREFERENCE, [frtValueSet, frtStructureDefinition], '', SearchXpathUsageNormal);
  indexes.add(frtConceptMap, 'status', 'Status of the concept map', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtConceptMap, 'target', 'Provides context to the mappings', SearchParamTypeREFERENCE, [frtValueSet, frtStructureDefinition], '', SearchXpathUsageNormal);
  indexes.add(frtConceptMap, 'targetcode', 'Code that identifies the target element', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtConceptMap, 'targetsystem', 'System of the target (if necessary)', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtConceptMap, 'url', 'The URL of the concept map', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtConceptMap, 'version', 'The version identifier of the concept map', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForCondition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtCondition, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtCondition, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtCondition, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtCondition, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtCondition, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtCondition, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtCondition, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtCondition, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtCondition, 'age', 'Search based on Condition onsetAge', SearchParamTypeNUMBER, [], '', SearchXpathUsageNormal);
  indexes.add(frtCondition, 'asserter', 'Person who asserts this condition', SearchParamTypeREFERENCE, [frtPatient, frtPractitioner], '', SearchXpathUsageNormal);
  indexes.add(frtCondition, 'body-site', 'Anatomical location, if relevant', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtCondition, 'category', 'The category of the condition', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtCondition, 'clinicalstatus', 'The clinical status of the condition', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtCondition, 'code', 'Code for the condition', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtCondition, 'date-recorded', 'A date, when the Condition statement was documented', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtCondition, 'encounter', 'Encounter when condition first asserted', SearchParamTypeREFERENCE, [frtEncounter], '', SearchXpathUsageNormal);
  indexes.add(frtCondition, 'evidence', 'Manifestation/symptom', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtCondition, 'identifier', 'A unique identifier of the condition record', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtCondition, 'onset', 'Date related onsets (dateTime and Period)', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtCondition, 'onset-info', 'Other onsets (boolean, age, range, string)', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtCondition, 'patient', 'Who has the condition?', SearchParamTypeREFERENCE, [frtPatient], '', SearchXpathUsageNormal);
  indexes.add(frtCondition, 'severity', 'The severity of the condition', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtCondition, 'stage', 'Simple summary (disease specific)', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForConformance(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtConformance, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtConformance, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtConformance, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtConformance, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtConformance, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtConformance, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtConformance, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtConformance, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtConformance, 'date', 'The conformance statement publication date', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtConformance, 'description', 'Text search in the description of the conformance statement', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtConformance, 'event', 'Event code in a conformance statement', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtConformance, 'fhirversion', 'The version of FHIR', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtConformance, 'format', 'formats supported (xml | json | mime type)', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtConformance, 'mode', 'Mode - restful (server/client) or messaging (sender/receiver)', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtConformance, 'name', 'Name of the conformance statement', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtConformance, 'profile', 'A profile id invoked in a conformance statement', SearchParamTypeREFERENCE, [frtStructureDefinition], '', SearchXpathUsageNormal);
  indexes.add(frtConformance, 'publisher', 'Name of the publisher of the conformance statement', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtConformance, 'resource', 'Name of a resource mentioned in a conformance statement', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtConformance, 'security', 'OAuth | SMART-on-FHIR | NTLM | Basic | Kerberos | Certificates', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtConformance, 'software', 'Part of a the name of a software application', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtConformance, 'status', 'The current status of the conformance statement', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtConformance, 'supported-profile', 'Profiles for use cases supported', SearchParamTypeREFERENCE, [frtStructureDefinition], '', SearchXpathUsageNormal);
  indexes.add(frtConformance, 'url', 'The uri that identifies the conformance statement', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtConformance, 'version', 'The version identifier of the conformance statement', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForContract(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtContract, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtContract, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtContract, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtContract, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtContract, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtContract, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtContract, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtContract, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtContract, 'actor', 'Contract Actor Type', SearchParamTypeREFERENCE, [frtDevice, frtLocation, frtOrganization, frtPatient, frtContract, frtPractitioner, frtSubstance, frtGroup, frtRelatedPerson], '', SearchXpathUsageNormal);
  indexes.add(frtContract, 'identifier', 'The identity of the contract', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtContract, 'patient', 'The identity of the target of the contract (if a patient)', SearchParamTypeREFERENCE, [frtPatient], '', SearchXpathUsageNormal);
  indexes.add(frtContract, 'signer', 'Contract Signatory Party', SearchParamTypeREFERENCE, [frtOrganization, frtPatient, frtPractitioner, frtRelatedPerson], '', SearchXpathUsageNormal);
  indexes.add(frtContract, 'subject', 'The identity of the target of the contract', SearchParamTypeREFERENCE, [frtPatient], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForCoverage(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtCoverage, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtCoverage, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtCoverage, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtCoverage, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtCoverage, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtCoverage, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtCoverage, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtCoverage, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtCoverage, 'dependent', 'Dependent number', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtCoverage, 'group', 'Group identifier', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtCoverage, 'identifier', 'The primary identifier of the insured', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtCoverage, 'issuer', 'The identity of the insurer', SearchParamTypeREFERENCE, [frtOrganization], '', SearchXpathUsageNormal);
  indexes.add(frtCoverage, 'plan', 'A plan or policy identifier', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtCoverage, 'sequence', 'Sequence number', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtCoverage, 'subplan', 'Sub-plan identifier', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtCoverage, 'type', 'The kind of coverage', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForDataElement(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtDataElement, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtDataElement, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDataElement, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtDataElement, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtDataElement, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDataElement, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDataElement, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDataElement, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtDataElement, 'code', 'A code for the data element (server may choose to do subsumption)', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDataElement, 'context', 'A use context assigned to the data element', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDataElement, 'date', 'The data element publication date', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtDataElement, 'description', 'Text search in the description of the data element.  This corresponds to the definition of the first DataElement.element.', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtDataElement, 'identifier', 'The identifier of the data element', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDataElement, 'name', 'Name of the data element', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtDataElement, 'objectClass', 'Matches on the 11179-objectClass extension value', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDataElement, 'objectClassProperty', 'Matches on the 11179-objectClassProperty extension value', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDataElement, 'publisher', 'Name of the publisher of the data element', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtDataElement, 'status', 'The current status of the data element', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDataElement, 'stringency', 'The stringency of the data element definition', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDataElement, 'url', 'The official URL for the data element', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtDataElement, 'version', 'The version identifier of the data element', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForDetectedIssue(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtDetectedIssue, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtDetectedIssue, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDetectedIssue, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtDetectedIssue, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtDetectedIssue, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDetectedIssue, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDetectedIssue, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDetectedIssue, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtDetectedIssue, 'author', 'The provider or device that identified the issue', SearchParamTypeREFERENCE, [frtDevice, frtPractitioner], '', SearchXpathUsageNormal);
  indexes.add(frtDetectedIssue, 'category', 'Issue Category, e.g. drug-drug, duplicate therapy, etc.', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDetectedIssue, 'date', 'When identified', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtDetectedIssue, 'identifier', 'Unique id for the detected issue', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDetectedIssue, 'implicated', 'Problem resource', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPES, '', SearchXpathUsageNormal);
  indexes.add(frtDetectedIssue, 'patient', 'Associated patient', SearchParamTypeREFERENCE, [frtPatient], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForDevice(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtDevice, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtDevice, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDevice, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtDevice, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtDevice, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDevice, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDevice, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDevice, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtDevice, 'identifier', 'Instance id from manufacturer, owner, and others', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDevice, 'location', 'A location, where the resource is found', SearchParamTypeREFERENCE, [frtLocation], '', SearchXpathUsageNormal);
  indexes.add(frtDevice, 'manufacturer', 'The manufacturer of the device', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtDevice, 'model', 'The model of the device', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtDevice, 'organization', 'The organization responsible for the device', SearchParamTypeREFERENCE, [frtOrganization], '', SearchXpathUsageNormal);
  indexes.add(frtDevice, 'patient', 'Patient information, if the resource is affixed to a person', SearchParamTypeREFERENCE, [frtPatient], '', SearchXpathUsageNormal);
  indexes.add(frtDevice, 'type', 'The type of the device', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDevice, 'udi', 'FDA mandated Unique Device Identifier', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtDevice, 'url', 'Network address to contact device', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForDeviceComponent(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtDeviceComponent, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtDeviceComponent, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDeviceComponent, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtDeviceComponent, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtDeviceComponent, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDeviceComponent, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDeviceComponent, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDeviceComponent, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtDeviceComponent, 'parent', 'The parent DeviceComponent resource', SearchParamTypeREFERENCE, [frtDeviceComponent], '', SearchXpathUsageNormal);
  indexes.add(frtDeviceComponent, 'source', 'The device source', SearchParamTypeREFERENCE, [frtDevice], '', SearchXpathUsageNormal);
  indexes.add(frtDeviceComponent, 'type', 'The device component type', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForDeviceMetric(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtDeviceMetric, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtDeviceMetric, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDeviceMetric, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtDeviceMetric, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtDeviceMetric, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDeviceMetric, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDeviceMetric, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDeviceMetric, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtDeviceMetric, 'category', 'The category of the metric', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDeviceMetric, 'identifier', 'The identifier of the metric', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDeviceMetric, 'parent', 'The parent DeviceMetric resource', SearchParamTypeREFERENCE, [frtDeviceComponent], '', SearchXpathUsageNormal);
  indexes.add(frtDeviceMetric, 'source', 'The device resource', SearchParamTypeREFERENCE, [frtDevice], '', SearchXpathUsageNormal);
  indexes.add(frtDeviceMetric, 'type', 'The component type', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForDeviceUseRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtDeviceUseRequest, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtDeviceUseRequest, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDeviceUseRequest, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtDeviceUseRequest, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtDeviceUseRequest, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDeviceUseRequest, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDeviceUseRequest, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDeviceUseRequest, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtDeviceUseRequest, 'device', 'Device requested', SearchParamTypeREFERENCE, [frtDevice], '', SearchXpathUsageNormal);
  indexes.add(frtDeviceUseRequest, 'patient', 'Search by subject - a patient', SearchParamTypeREFERENCE, [frtPatient], '', SearchXpathUsageNormal);
  indexes.add(frtDeviceUseRequest, 'subject', 'Search by subject', SearchParamTypeREFERENCE, [frtPatient], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForDeviceUseStatement(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtDeviceUseStatement, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtDeviceUseStatement, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDeviceUseStatement, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtDeviceUseStatement, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtDeviceUseStatement, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDeviceUseStatement, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDeviceUseStatement, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDeviceUseStatement, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtDeviceUseStatement, 'device', 'Search by device', SearchParamTypeREFERENCE, [frtDevice], '', SearchXpathUsageNormal);
  indexes.add(frtDeviceUseStatement, 'patient', 'Search by subject - a patient', SearchParamTypeREFERENCE, [frtPatient], '', SearchXpathUsageNormal);
  indexes.add(frtDeviceUseStatement, 'subject', 'Search by subject', SearchParamTypeREFERENCE, [frtPatient], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForDiagnosticOrder(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtDiagnosticOrder, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtDiagnosticOrder, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDiagnosticOrder, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtDiagnosticOrder, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtDiagnosticOrder, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDiagnosticOrder, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDiagnosticOrder, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDiagnosticOrder, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtDiagnosticOrder, 'actor', 'Who recorded or did this', SearchParamTypeREFERENCE, [frtDevice, frtPractitioner], '', SearchXpathUsageNormal);
  indexes.add(frtDiagnosticOrder, 'bodysite', 'Location of requested test (if applicable)', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDiagnosticOrder, 'code', 'Code to indicate the item (test or panel) being ordered', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDiagnosticOrder, 'encounter', 'The encounter that this diagnostic order is associated with', SearchParamTypeREFERENCE, [frtEncounter], '', SearchXpathUsageNormal);
  indexes.add(frtDiagnosticOrder, 'event-date', 'The date at which the event happened', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtDiagnosticOrder, 'event-status', 'proposed | draft | planned | requested | received | accepted | in-progress | review | completed | cancelled | suspended | rejected | failed', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDiagnosticOrder, 'event-status-date', 'A combination of past-status and date', SearchParamTypeCOMPOSITE, [], '', SearchXpathUsageNormal);
  indexes.add(frtDiagnosticOrder, 'identifier', 'Identifiers assigned to this order', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDiagnosticOrder, 'item-date', 'The date at which the event happened', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtDiagnosticOrder, 'item-past-status', 'proposed | draft | planned | requested | received | accepted | in-progress | review | completed | cancelled | suspended | rejected | failed', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDiagnosticOrder, 'item-status', 'proposed | draft | planned | requested | received | accepted | in-progress | review | completed | cancelled | suspended | rejected | failed', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDiagnosticOrder, 'item-status-date', 'A combination of item-past-status and item-date', SearchParamTypeCOMPOSITE, [], '', SearchXpathUsageNormal);
  indexes.add(frtDiagnosticOrder, 'orderer', 'Who ordered the test', SearchParamTypeREFERENCE, [frtPractitioner], '', SearchXpathUsageNormal);
  indexes.add(frtDiagnosticOrder, 'patient', 'Who and/or what test is about', SearchParamTypeREFERENCE, [frtPatient], '', SearchXpathUsageNormal);
  indexes.add(frtDiagnosticOrder, 'specimen', 'If the whole order relates to specific specimens', SearchParamTypeREFERENCE, [frtSpecimen], '', SearchXpathUsageNormal);
  indexes.add(frtDiagnosticOrder, 'status', 'proposed | draft | planned | requested | received | accepted | in-progress | review | completed | cancelled | suspended | rejected | failed', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDiagnosticOrder, 'subject', 'Who and/or what test is about', SearchParamTypeREFERENCE, [frtDevice, frtPatient, frtLocation, frtGroup], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForDiagnosticReport(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtDiagnosticReport, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtDiagnosticReport, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDiagnosticReport, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtDiagnosticReport, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtDiagnosticReport, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDiagnosticReport, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDiagnosticReport, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDiagnosticReport, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtDiagnosticReport, 'category', 'Which diagnostic discipline/department created the report', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDiagnosticReport, 'code', 'The code for the report as a whole, as opposed to codes for the atomic results, which are the names on the observation resource referred to from the result', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDiagnosticReport, 'date', 'The clinically relevant time of the report', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtDiagnosticReport, 'diagnosis', 'A coded diagnosis on the report', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDiagnosticReport, 'encounter', 'The Encounter when the order was made', SearchParamTypeREFERENCE, [frtEncounter], '', SearchXpathUsageNormal);
  indexes.add(frtDiagnosticReport, 'identifier', 'An identifier for the report', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDiagnosticReport, 'image', 'A reference to the image source.', SearchParamTypeREFERENCE, [frtMedia], '', SearchXpathUsageNormal);
  indexes.add(frtDiagnosticReport, 'issued', 'When the report was issued', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtDiagnosticReport, 'patient', 'The subject of the report if a patient', SearchParamTypeREFERENCE, [frtPatient], '', SearchXpathUsageNormal);
  indexes.add(frtDiagnosticReport, 'performer', 'Who was the source of the report (organization)', SearchParamTypeREFERENCE, [frtOrganization, frtPractitioner], '', SearchXpathUsageNormal);
  indexes.add(frtDiagnosticReport, 'request', 'Reference to the test or procedure request.', SearchParamTypeREFERENCE, [frtReferralRequest, frtProcedureRequest, frtDiagnosticOrder], '', SearchXpathUsageNormal);
  indexes.add(frtDiagnosticReport, 'result', 'Link to an atomic result (observation resource)', SearchParamTypeREFERENCE, [frtObservation], '', SearchXpathUsageNormal);
  indexes.add(frtDiagnosticReport, 'specimen', 'The specimen details', SearchParamTypeREFERENCE, [frtSpecimen], '', SearchXpathUsageNormal);
  indexes.add(frtDiagnosticReport, 'status', 'The status of the report', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDiagnosticReport, 'subject', 'The subject of the report', SearchParamTypeREFERENCE, [frtDevice, frtPatient, frtLocation, frtGroup], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForDocumentManifest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtDocumentManifest, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtDocumentManifest, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDocumentManifest, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtDocumentManifest, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtDocumentManifest, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDocumentManifest, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDocumentManifest, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDocumentManifest, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtDocumentManifest, 'author', 'Who and/or what authored the manifest', SearchParamTypeREFERENCE, [frtDevice, frtOrganization, frtPatient, frtPractitioner, frtRelatedPerson], '', SearchXpathUsageNormal);
  indexes.add(frtDocumentManifest, 'content-ref', 'Contents of this set of documents', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPES, '', SearchXpathUsageNormal);
  indexes.add(frtDocumentManifest, 'created', 'When this document manifest created', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtDocumentManifest, 'description', 'Human-readable description (title)', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtDocumentManifest, 'identifier', 'Unique Identifier for the set of documents', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDocumentManifest, 'patient', 'The subject of the set of documents', SearchParamTypeREFERENCE, [frtPatient], '', SearchXpathUsageNormal);
  indexes.add(frtDocumentManifest, 'recipient', 'Intended to get notified about this set of documents', SearchParamTypeREFERENCE, [frtPatient, frtOrganization, frtPractitioner, frtRelatedPerson], '', SearchXpathUsageNormal);
  indexes.add(frtDocumentManifest, 'related-id', 'Identifiers of things that are related', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDocumentManifest, 'related-ref', 'Related Resource', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPES, '', SearchXpathUsageNormal);
  indexes.add(frtDocumentManifest, 'source', 'The source system/application/software', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtDocumentManifest, 'status', 'current | superseded | entered-in-error', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDocumentManifest, 'subject', 'The subject of the set of documents', SearchParamTypeREFERENCE, [frtDevice, frtPatient, frtPractitioner, frtGroup], '', SearchXpathUsageNormal);
  indexes.add(frtDocumentManifest, 'type', 'Kind of document set', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForDocumentReference(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtDocumentReference, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtDocumentReference, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDocumentReference, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtDocumentReference, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtDocumentReference, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDocumentReference, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDocumentReference, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDocumentReference, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtDocumentReference, 'authenticator', 'Who/what authenticated the document', SearchParamTypeREFERENCE, [frtOrganization, frtPractitioner], '', SearchXpathUsageNormal);
  indexes.add(frtDocumentReference, 'author', 'Who and/or what authored the document', SearchParamTypeREFERENCE, [frtDevice, frtOrganization, frtPatient, frtPractitioner, frtRelatedPerson], '', SearchXpathUsageNormal);
  indexes.add(frtDocumentReference, 'class', 'Categorization of document', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDocumentReference, 'created', 'Document creation time', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtDocumentReference, 'custodian', 'Organization which maintains the document', SearchParamTypeREFERENCE, [frtOrganization], '', SearchXpathUsageNormal);
  indexes.add(frtDocumentReference, 'description', 'Human-readable description (title)', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtDocumentReference, 'encounter', 'Context of the document  content', SearchParamTypeREFERENCE, [frtEncounter], '', SearchXpathUsageNormal);
  indexes.add(frtDocumentReference, 'event', 'Main Clinical Acts Documented', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDocumentReference, 'facility', 'Kind of facility where patient was seen', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDocumentReference, 'format', 'Format/content rules for the document', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDocumentReference, 'identifier', 'Master Version Specific Identifier', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDocumentReference, 'indexed', 'When this document reference created', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtDocumentReference, 'language', 'Human language of the content (BCP-47)', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDocumentReference, 'location', 'Uri where the data can be found', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtDocumentReference, 'patient', 'Who/what is the subject of the document', SearchParamTypeREFERENCE, [frtPatient], '', SearchXpathUsageNormal);
  indexes.add(frtDocumentReference, 'period', 'Time of service that is being documented', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtDocumentReference, 'related-id', 'Identifier of related objects or events', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDocumentReference, 'related-ref', 'Related Resource', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPES, '', SearchXpathUsageNormal);
  indexes.add(frtDocumentReference, 'relatesto', 'Target of the relationship', SearchParamTypeREFERENCE, [frtDocumentReference], '', SearchXpathUsageNormal);
  indexes.add(frtDocumentReference, 'relation', 'replaces | transforms | signs | appends', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDocumentReference, 'relationship', 'Combination of relation and relatesTo', SearchParamTypeCOMPOSITE, [], '', SearchXpathUsageNormal);
  indexes.add(frtDocumentReference, 'securitylabel', 'Document security-tags', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDocumentReference, 'setting', 'Additional details about where the content was created (e.g. clinical specialty)', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDocumentReference, 'status', 'current | superseded | entered-in-error', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtDocumentReference, 'subject', 'Who/what is the subject of the document', SearchParamTypeREFERENCE, [frtDevice, frtPatient, frtPractitioner, frtGroup], '', SearchXpathUsageNormal);
  indexes.add(frtDocumentReference, 'type', 'Kind of document (LOINC if possible)', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForEligibilityRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtEligibilityRequest, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtEligibilityRequest, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtEligibilityRequest, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtEligibilityRequest, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtEligibilityRequest, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtEligibilityRequest, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtEligibilityRequest, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtEligibilityRequest, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtEligibilityRequest, 'identifier', 'The business identifier of the Eligibility', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForEligibilityResponse(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtEligibilityResponse, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtEligibilityResponse, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtEligibilityResponse, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtEligibilityResponse, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtEligibilityResponse, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtEligibilityResponse, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtEligibilityResponse, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtEligibilityResponse, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtEligibilityResponse, 'identifier', 'The business identifier of the Explanation of Benefit', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForEncounter(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtEncounter, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtEncounter, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtEncounter, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtEncounter, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtEncounter, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtEncounter, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtEncounter, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtEncounter, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtEncounter, 'appointment', 'The appointment that scheduled this encounter', SearchParamTypeREFERENCE, [frtAppointment], '', SearchXpathUsageNormal);
  indexes.add(frtEncounter, 'condition', 'Reason the encounter takes place (resource)', SearchParamTypeREFERENCE, [frtCondition], '', SearchXpathUsageNormal);
  indexes.add(frtEncounter, 'date', 'A date within the period the Encounter lasted', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtEncounter, 'episodeofcare', 'Episode(s) of care that this encounter should be recorded against', SearchParamTypeREFERENCE, [frtEpisodeOfCare], '', SearchXpathUsageNormal);
  indexes.add(frtEncounter, 'identifier', 'Identifier(s) by which this encounter is known', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtEncounter, 'incomingreferral', 'The ReferralRequest that initiated this encounter', SearchParamTypeREFERENCE, [frtReferralRequest], '', SearchXpathUsageNormal);
  indexes.add(frtEncounter, 'indication', 'Reason the encounter takes place (resource)', SearchParamTypeREFERENCE, [frtCondition, frtProcedure], '', SearchXpathUsageNormal);
  indexes.add(frtEncounter, 'length', 'Length of encounter in days', SearchParamTypeNUMBER, [], '', SearchXpathUsageNormal);
  indexes.add(frtEncounter, 'location', 'Location the encounter takes place', SearchParamTypeREFERENCE, [frtLocation], '', SearchXpathUsageNormal);
  indexes.add(frtEncounter, 'location-period', 'Time period during which the patient was present at the location', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtEncounter, 'part-of', 'Another Encounter this encounter is part of', SearchParamTypeREFERENCE, [frtEncounter], '', SearchXpathUsageNormal);
  indexes.add(frtEncounter, 'participant', 'Persons involved in the encounter other than the patient', SearchParamTypeREFERENCE, [frtPractitioner, frtRelatedPerson], '', SearchXpathUsageNormal);
  indexes.add(frtEncounter, 'participant-type', 'Role of participant in encounter', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtEncounter, 'patient', 'The patient present at the encounter', SearchParamTypeREFERENCE, [frtPatient], '', SearchXpathUsageNormal);
  indexes.add(frtEncounter, 'practitioner', 'Persons involved in the encounter other than the patient', SearchParamTypeREFERENCE, [frtPractitioner], '', SearchXpathUsageNormal);
  indexes.add(frtEncounter, 'procedure', 'Reason the encounter takes place (resource)', SearchParamTypeREFERENCE, [frtProcedure], '', SearchXpathUsageNormal);
  indexes.add(frtEncounter, 'reason', 'Reason the encounter takes place (code)', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtEncounter, 'special-arrangement', 'Wheelchair, translator, stretcher, etc.', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtEncounter, 'status', 'planned | arrived | in-progress | onleave | finished | cancelled', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtEncounter, 'type', 'Specific type of encounter', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForEnrollmentRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtEnrollmentRequest, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtEnrollmentRequest, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtEnrollmentRequest, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtEnrollmentRequest, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtEnrollmentRequest, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtEnrollmentRequest, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtEnrollmentRequest, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtEnrollmentRequest, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtEnrollmentRequest, 'identifier', 'The business identifier of the Enrollment', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtEnrollmentRequest, 'patient', 'The party to be enrolled', SearchParamTypeREFERENCE, [frtPatient], '', SearchXpathUsageNormal);
  indexes.add(frtEnrollmentRequest, 'subject', 'The party to be enrolled', SearchParamTypeREFERENCE, [frtPatient], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForEnrollmentResponse(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtEnrollmentResponse, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtEnrollmentResponse, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtEnrollmentResponse, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtEnrollmentResponse, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtEnrollmentResponse, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtEnrollmentResponse, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtEnrollmentResponse, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtEnrollmentResponse, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtEnrollmentResponse, 'identifier', 'The business identifier of the Explanation of Benefit', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForEpisodeOfCare(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtEpisodeOfCare, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtEpisodeOfCare, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtEpisodeOfCare, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtEpisodeOfCare, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtEpisodeOfCare, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtEpisodeOfCare, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtEpisodeOfCare, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtEpisodeOfCare, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtEpisodeOfCare, 'care-manager', 'Care manager/care co-ordinator for the patient', SearchParamTypeREFERENCE, [frtPractitioner], '', SearchXpathUsageNormal);
  indexes.add(frtEpisodeOfCare, 'condition', 'Conditions/problems/diagnoses this episode of care is for', SearchParamTypeREFERENCE, [frtCondition], '', SearchXpathUsageNormal);
  indexes.add(frtEpisodeOfCare, 'date', 'The provided date search value falls within the episode of care''s period', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtEpisodeOfCare, 'identifier', 'Identifier(s) for the EpisodeOfCare', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtEpisodeOfCare, 'incomingreferral', 'Incoming Referral Request', SearchParamTypeREFERENCE, [frtReferralRequest], '', SearchXpathUsageNormal);
  indexes.add(frtEpisodeOfCare, 'organization', 'The organization that has assumed the specific responsibilities of this EpisodeOfCare', SearchParamTypeREFERENCE, [frtOrganization], '', SearchXpathUsageNormal);
  indexes.add(frtEpisodeOfCare, 'patient', 'Patient for this episode of care', SearchParamTypeREFERENCE, [frtPatient], '', SearchXpathUsageNormal);
  indexes.add(frtEpisodeOfCare, 'status', 'The current status of the Episode of Care as provided (does not check the status history collection)', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtEpisodeOfCare, 'team-member', 'A Practitioner or Organization allocated to the care team for this EpisodeOfCare', SearchParamTypeREFERENCE, [frtOrganization, frtPractitioner], '', SearchXpathUsageNormal);
  indexes.add(frtEpisodeOfCare, 'type', 'Type/class  - e.g. specialist referral, disease management', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForExplanationOfBenefit(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtExplanationOfBenefit, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtExplanationOfBenefit, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtExplanationOfBenefit, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtExplanationOfBenefit, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtExplanationOfBenefit, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtExplanationOfBenefit, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtExplanationOfBenefit, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtExplanationOfBenefit, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtExplanationOfBenefit, 'identifier', 'The business identifier of the Explanation of Benefit', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForFamilyMemberHistory(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtFamilyMemberHistory, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtFamilyMemberHistory, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtFamilyMemberHistory, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtFamilyMemberHistory, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtFamilyMemberHistory, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtFamilyMemberHistory, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtFamilyMemberHistory, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtFamilyMemberHistory, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtFamilyMemberHistory, 'code', 'A search by a condition code', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtFamilyMemberHistory, 'condition', 'Search for a history of a particular condition within a patient''s family.', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtFamilyMemberHistory, 'date', 'When history was captured/updated', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtFamilyMemberHistory, 'gender', 'A search by a gender code of a family member', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtFamilyMemberHistory, 'identifier', 'A search by a record identifier', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtFamilyMemberHistory, 'patient', 'The identity of a subject to list family member history items for', SearchParamTypeREFERENCE, [frtPatient], '', SearchXpathUsageNormal);
  indexes.add(frtFamilyMemberHistory, 'relationship', 'Search for family history of members based on relationship', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForFlag(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtFlag, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtFlag, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtFlag, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtFlag, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtFlag, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtFlag, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtFlag, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtFlag, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtFlag, 'author', 'Flag creator', SearchParamTypeREFERENCE, [frtDevice, frtOrganization, frtPatient, frtPractitioner], '', SearchXpathUsageNormal);
  indexes.add(frtFlag, 'date', 'Time period when flag is active', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtFlag, 'encounter', 'Alert relevant during encounter', SearchParamTypeREFERENCE, [frtEncounter], '', SearchXpathUsageNormal);
  indexes.add(frtFlag, 'patient', 'The identity of a subject to list flags for', SearchParamTypeREFERENCE, [frtPatient], '', SearchXpathUsageNormal);
  indexes.add(frtFlag, 'subject', 'The identity of a subject to list flags for', SearchParamTypeREFERENCE, [frtPatient, frtLocation, frtOrganization, frtPractitioner, frtGroup], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForGoal(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtGoal, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtGoal, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtGoal, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtGoal, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtGoal, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtGoal, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtGoal, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtGoal, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtGoal, 'category', 'E.g. Treatment, dietary, behavioral, etc.', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtGoal, 'identifier', 'External Ids for this goal', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtGoal, 'patient', 'Who this goal is intended for', SearchParamTypeREFERENCE, [frtPatient], '', SearchXpathUsageNormal);
  indexes.add(frtGoal, 'status', 'proposed | planned | accepted | rejected | in-progress | achieved | sustaining | on-hold | cancelled', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtGoal, 'subject', 'Who this goal is intended for', SearchParamTypeREFERENCE, [frtPatient, frtOrganization, frtGroup], '', SearchXpathUsageNormal);
  indexes.add(frtGoal, 'targetdate', 'Reach goal on or before', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForGroup(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtGroup, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtGroup, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtGroup, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtGroup, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtGroup, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtGroup, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtGroup, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtGroup, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtGroup, 'actual', 'Descriptive or actual', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtGroup, 'characteristic', 'Kind of characteristic', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtGroup, 'characteristic-value', 'A composite of both characteristic and value', SearchParamTypeCOMPOSITE, [], '', SearchXpathUsageNormal);
  indexes.add(frtGroup, 'code', 'The kind of resources contained', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtGroup, 'exclude', 'Group includes or excludes', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtGroup, 'identifier', 'Unique id', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtGroup, 'member', 'Reference to the group member', SearchParamTypeREFERENCE, [frtMedication, frtDevice, frtPatient, frtPractitioner, frtSubstance], '', SearchXpathUsageNormal);
  indexes.add(frtGroup, 'type', 'The type of resources the group contains', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtGroup, 'value', 'Value held by characteristic', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForHealthcareService(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtHealthcareService, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtHealthcareService, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtHealthcareService, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtHealthcareService, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtHealthcareService, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtHealthcareService, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtHealthcareService, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtHealthcareService, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtHealthcareService, 'characteristic', 'One of the HealthcareService''s characteristics', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtHealthcareService, 'identifier', 'External identifiers for this item', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtHealthcareService, 'location', 'The location of the Healthcare Service', SearchParamTypeREFERENCE, [frtLocation], '', SearchXpathUsageNormal);
  indexes.add(frtHealthcareService, 'name', 'A portion of the Healthcare service name', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtHealthcareService, 'organization', 'The organization that provides this Healthcare Service', SearchParamTypeREFERENCE, [frtOrganization], '', SearchXpathUsageNormal);
  indexes.add(frtHealthcareService, 'programname', 'One of the Program Names serviced by this HealthcareService', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtHealthcareService, 'servicecategory', 'Service Category of the Healthcare Service', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtHealthcareService, 'servicetype', 'The type of service provided by this healthcare service', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForImagingObjectSelection(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtImagingObjectSelection, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtImagingObjectSelection, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtImagingObjectSelection, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtImagingObjectSelection, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtImagingObjectSelection, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtImagingObjectSelection, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtImagingObjectSelection, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtImagingObjectSelection, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtImagingObjectSelection, 'author', 'Author of key DICOM object selection', SearchParamTypeREFERENCE, [frtDevice, frtOrganization, frtPatient, frtPractitioner, frtRelatedPerson], '', SearchXpathUsageNormal);
  indexes.add(frtImagingObjectSelection, 'authoring-time', 'Time of key DICOM object selection authoring', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtImagingObjectSelection, 'identifier', 'UID of key DICOM object selection', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtImagingObjectSelection, 'patient', 'Subject of key DICOM object selection', SearchParamTypeREFERENCE, [frtPatient], '', SearchXpathUsageNormal);
  indexes.add(frtImagingObjectSelection, 'selected-study', 'Study selected in key DICOM object selection', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtImagingObjectSelection, 'title', 'Title of key DICOM object selection', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForImagingStudy(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtImagingStudy, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtImagingStudy, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtImagingStudy, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtImagingStudy, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtImagingStudy, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtImagingStudy, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtImagingStudy, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtImagingStudy, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtImagingStudy, 'accession', 'The accession identifier for the study', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtImagingStudy, 'bodysite', 'The body site studied', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtImagingStudy, 'dicom-class', 'The type of the instance', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtImagingStudy, 'modality', 'The modality of the series', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtImagingStudy, 'order', 'The order for the image', SearchParamTypeREFERENCE, [frtDiagnosticOrder], '', SearchXpathUsageNormal);
  indexes.add(frtImagingStudy, 'patient', 'Who the study is about', SearchParamTypeREFERENCE, [frtPatient], '', SearchXpathUsageNormal);
  indexes.add(frtImagingStudy, 'series', 'The identifier of the series of images', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtImagingStudy, 'started', 'When the study was started', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtImagingStudy, 'study', 'The study identifier for the image', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtImagingStudy, 'uid', 'The instance unique identifier', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForImmunization(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtImmunization, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtImmunization, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtImmunization, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtImmunization, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtImmunization, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtImmunization, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtImmunization, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtImmunization, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtImmunization, 'date', 'Vaccination  (non)-Administration Date', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtImmunization, 'dose-sequence', 'Dose number within series', SearchParamTypeNUMBER, [], '', SearchXpathUsageNormal);
  indexes.add(frtImmunization, 'identifier', 'Business identifier', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtImmunization, 'location', 'The service delivery location or facility in which the vaccine was / was to be administered', SearchParamTypeREFERENCE, [frtLocation], '', SearchXpathUsageNormal);
  indexes.add(frtImmunization, 'lot-number', 'Vaccine Lot Number', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtImmunization, 'manufacturer', 'Vaccine Manufacturer', SearchParamTypeREFERENCE, [frtOrganization], '', SearchXpathUsageNormal);
  indexes.add(frtImmunization, 'notgiven', 'Administrations which were not given', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtImmunization, 'patient', 'The patient for the vaccination record', SearchParamTypeREFERENCE, [frtPatient], '', SearchXpathUsageNormal);
  indexes.add(frtImmunization, 'performer', 'The practitioner who administered the vaccination', SearchParamTypeREFERENCE, [frtPractitioner], '', SearchXpathUsageNormal);
  indexes.add(frtImmunization, 'reaction', 'Additional information on reaction', SearchParamTypeREFERENCE, [frtObservation], '', SearchXpathUsageNormal);
  indexes.add(frtImmunization, 'reaction-date', 'When reaction started', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtImmunization, 'reason', 'Why immunization occurred', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtImmunization, 'reason-not-given', 'Explanation of reason vaccination was not administered', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtImmunization, 'requester', 'The practitioner who ordered the vaccination', SearchParamTypeREFERENCE, [frtPractitioner], '', SearchXpathUsageNormal);
  indexes.add(frtImmunization, 'status', 'Immunization event status', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtImmunization, 'vaccine-code', 'Vaccine Product Administered', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForImmunizationRecommendation(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtImmunizationRecommendation, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtImmunizationRecommendation, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtImmunizationRecommendation, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtImmunizationRecommendation, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtImmunizationRecommendation, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtImmunizationRecommendation, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtImmunizationRecommendation, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtImmunizationRecommendation, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtImmunizationRecommendation, 'date', 'Date recommendation created', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtImmunizationRecommendation, 'dose-number', 'Recommended dose number', SearchParamTypeNUMBER, [], '', SearchXpathUsageNormal);
  indexes.add(frtImmunizationRecommendation, 'dose-sequence', 'Dose number within sequence', SearchParamTypeNUMBER, [], '', SearchXpathUsageNormal);
  indexes.add(frtImmunizationRecommendation, 'identifier', 'Business identifier', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtImmunizationRecommendation, 'information', 'Patient observations supporting recommendation', SearchParamTypeREFERENCE, [frtObservation, frtAllergyIntolerance], '', SearchXpathUsageNormal);
  indexes.add(frtImmunizationRecommendation, 'patient', 'Who this profile is for', SearchParamTypeREFERENCE, [frtPatient], '', SearchXpathUsageNormal);
  indexes.add(frtImmunizationRecommendation, 'status', 'Vaccine administration status', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtImmunizationRecommendation, 'support', 'Past immunizations supporting recommendation', SearchParamTypeREFERENCE, [frtImmunization], '', SearchXpathUsageNormal);
  indexes.add(frtImmunizationRecommendation, 'vaccine-type', 'Vaccine recommendation applies to', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForImplementationGuide(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtImplementationGuide, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtImplementationGuide, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtImplementationGuide, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtImplementationGuide, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtImplementationGuide, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtImplementationGuide, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtImplementationGuide, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtImplementationGuide, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtImplementationGuide, 'context', 'A use context assigned to the structure', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtImplementationGuide, 'date', 'The implementation guide publication date', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtImplementationGuide, 'dependency', 'Where to find dependency', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtImplementationGuide, 'description', 'Text search in the description of the implementation guide', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtImplementationGuide, 'experimental', 'If for testing purposes, not real usage', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtImplementationGuide, 'name', 'Name of the implementation guide', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtImplementationGuide, 'publisher', 'Name of the publisher of the implementation guide', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtImplementationGuide, 'status', 'The current status of the implementation guide', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtImplementationGuide, 'url', 'Absolute URL used to reference this Implementation Guide', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtImplementationGuide, 'version', 'The version identifier of the implementation guide', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForList(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtList, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtList, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtList, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtList, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtList, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtList, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtList, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtList, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtList, 'code', 'What the purpose of this list is', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtList, 'date', 'When the list was prepared', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtList, 'empty-reason', 'Why list is empty', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtList, 'encounter', 'Context in which list created', SearchParamTypeREFERENCE, [frtEncounter], '', SearchXpathUsageNormal);
  indexes.add(frtList, 'item', 'Actual entry', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPES, '', SearchXpathUsageNormal);
  indexes.add(frtList, 'notes', 'Comments about the list', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtList, 'patient', 'If all resources have the same subject', SearchParamTypeREFERENCE, [frtPatient], '', SearchXpathUsageNormal);
  indexes.add(frtList, 'source', 'Who and/or what defined the list contents (aka Author)', SearchParamTypeREFERENCE, [frtDevice, frtPatient, frtPractitioner], '', SearchXpathUsageNormal);
  indexes.add(frtList, 'status', 'current | retired | entered-in-error', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtList, 'subject', 'If all resources have the same subject', SearchParamTypeREFERENCE, [frtDevice, frtPatient, frtLocation, frtGroup], '', SearchXpathUsageNormal);
  indexes.add(frtList, 'title', 'Descriptive name for the list', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForLocation(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtLocation, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtLocation, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtLocation, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtLocation, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtLocation, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtLocation, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtLocation, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtLocation, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtLocation, 'address', 'A (part of the) address of the location', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtLocation, 'address-city', 'A city specified in an address', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtLocation, 'address-country', 'A country specified in an address', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtLocation, 'address-postalcode', 'A postal code specified in an address', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtLocation, 'address-state', 'A state specified in an address', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtLocation, 'address-use', 'A use code specified in an address', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtLocation, 'identifier', 'Unique code or number identifying the location to its users', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtLocation, 'name', 'A (portion of the) name of the location', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtLocation, 'near', 'The coordinates expressed as [lat],[long] (using the WGS84 datum, see notes) to find locations near to (servers may search using a square rather than a circle for efficiency)', SearchParamTypeTOKEN, [], '', SearchXpathUsageNearby);
  indexes.add(frtLocation, 'near-distance', 'A distance quantity to limit the near search to locations within a specific distance', SearchParamTypeTOKEN, [], '', SearchXpathUsageDistance);
  indexes.add(frtLocation, 'organization', 'Searches for locations that are managed by the provided organization', SearchParamTypeREFERENCE, [frtOrganization], '', SearchXpathUsageNormal);
  indexes.add(frtLocation, 'partof', 'The location of which this location is a part', SearchParamTypeREFERENCE, [frtLocation], '', SearchXpathUsageNormal);
  indexes.add(frtLocation, 'status', 'Searches for locations with a specific kind of status', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtLocation, 'type', 'A code for the type of location', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForMedia(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtMedia, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedia, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedia, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedia, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedia, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedia, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedia, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedia, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedia, 'created', 'Date attachment was first created', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedia, 'identifier', 'Identifier(s) for the image', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedia, 'operator', 'The person who generated the image', SearchParamTypeREFERENCE, [frtPractitioner], '', SearchXpathUsageNormal);
  indexes.add(frtMedia, 'patient', 'Who/What this Media is a record of', SearchParamTypeREFERENCE, [frtPatient], '', SearchXpathUsageNormal);
  indexes.add(frtMedia, 'subject', 'Who/What this Media is a record of', SearchParamTypeREFERENCE, [frtSpecimen, frtDevice, frtPatient, frtPractitioner, frtGroup], '', SearchXpathUsageNormal);
  indexes.add(frtMedia, 'subtype', 'The type of acquisition equipment/process', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedia, 'type', 'photo | video | audio', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedia, 'view', 'Imaging view, e.g. Lateral or Antero-posterior', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForMedication(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtMedication, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedication, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedication, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedication, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedication, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedication, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedication, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedication, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedication, 'code', 'Codes that identify this medication', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedication, 'container', 'E.g. box, vial, blister-pack', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedication, 'content', 'A product in the package', SearchParamTypeREFERENCE, [frtMedication], '', SearchXpathUsageNormal);
  indexes.add(frtMedication, 'form', 'powder | tablets | carton +', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedication, 'ingredient', 'The product contained', SearchParamTypeREFERENCE, [frtMedication, frtSubstance], '', SearchXpathUsageNormal);
  indexes.add(frtMedication, 'manufacturer', 'Manufacturer of the item', SearchParamTypeREFERENCE, [frtOrganization], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForMedicationAdministration(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtMedicationAdministration, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationAdministration, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationAdministration, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationAdministration, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationAdministration, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationAdministration, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationAdministration, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationAdministration, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationAdministration, 'code', 'Return administrations of this medication code', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationAdministration, 'device', 'Return administrations with this administration device identity', SearchParamTypeREFERENCE, [frtDevice], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationAdministration, 'effectivetime', 'Date administration happened (or did not happen)', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationAdministration, 'encounter', 'Return administrations that share this encounter', SearchParamTypeREFERENCE, [frtEncounter], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationAdministration, 'identifier', 'Return administrations with this external identifier', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationAdministration, 'medication', 'Return administrations of this medication resource', SearchParamTypeREFERENCE, [frtMedication], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationAdministration, 'notgiven', 'Administrations that were not made', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationAdministration, 'patient', 'The identity of a patient to list administrations  for', SearchParamTypeREFERENCE, [frtPatient], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationAdministration, 'practitioner', 'Who administered substance', SearchParamTypeREFERENCE, [frtPatient, frtPractitioner, frtRelatedPerson], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationAdministration, 'prescription', 'The identity of a prescription to list administrations from', SearchParamTypeREFERENCE, [frtMedicationOrder], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationAdministration, 'status', 'MedicationAdministration event status (for example one of active/paused/completed/nullified)', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForMedicationDispense(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtMedicationDispense, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationDispense, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationDispense, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationDispense, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationDispense, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationDispense, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationDispense, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationDispense, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationDispense, 'code', 'Return dispenses of this medicine code', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationDispense, 'destination', 'Return dispenses that should be sent to a specific destination', SearchParamTypeREFERENCE, [frtLocation], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationDispense, 'dispenser', 'Return all dispenses performed by a specific individual', SearchParamTypeREFERENCE, [frtPractitioner], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationDispense, 'identifier', 'Return dispenses with this external identifier', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationDispense, 'medication', 'Return dispenses of this medicine resource', SearchParamTypeREFERENCE, [frtMedication], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationDispense, 'patient', 'The identity of a patient to list dispenses  for', SearchParamTypeREFERENCE, [frtPatient], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationDispense, 'prescription', 'The identity of a prescription to list dispenses from', SearchParamTypeREFERENCE, [frtMedicationOrder], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationDispense, 'receiver', 'Who collected the medication', SearchParamTypeREFERENCE, [frtPatient, frtPractitioner], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationDispense, 'responsibleparty', 'Return all dispenses with the specified responsible party', SearchParamTypeREFERENCE, [frtPractitioner], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationDispense, 'status', 'Status of the dispense', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationDispense, 'type', 'Return all dispenses of a specific type', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationDispense, 'whenhandedover', 'Date when medication handed over to patient (outpatient setting), or supplied to ward or clinic (inpatient setting)', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationDispense, 'whenprepared', 'Date when medication prepared', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForMedicationOrder(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtMedicationOrder, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationOrder, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationOrder, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationOrder, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationOrder, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationOrder, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationOrder, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationOrder, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationOrder, 'code', 'Return administrations of this medication code', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationOrder, 'datewritten', 'Return prescriptions written on this date', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationOrder, 'encounter', 'Return prescriptions with this encounter identifier', SearchParamTypeREFERENCE, [frtEncounter], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationOrder, 'identifier', 'Return prescriptions with this external identifier', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationOrder, 'medication', 'Return administrations of this medication reference', SearchParamTypeREFERENCE, [frtMedication], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationOrder, 'patient', 'The identity of a patient to list orders  for', SearchParamTypeREFERENCE, [frtPatient], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationOrder, 'prescriber', 'Who ordered the medication(s)', SearchParamTypeREFERENCE, [frtPractitioner], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationOrder, 'status', 'Status of the prescription', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForMedicationStatement(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtMedicationStatement, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationStatement, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationStatement, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationStatement, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationStatement, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationStatement, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationStatement, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationStatement, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationStatement, 'code', 'Return administrations of this medication code', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationStatement, 'effectivedate', 'Date when patient was taking (or not taking) the medication', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationStatement, 'identifier', 'Return statements with this external identifier', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationStatement, 'medication', 'Return administrations of this medication reference', SearchParamTypeREFERENCE, [frtMedication], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationStatement, 'patient', 'The identity of a patient to list statements  for', SearchParamTypeREFERENCE, [frtPatient], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationStatement, 'source', 'Who the information in the statement came from', SearchParamTypeREFERENCE, [frtPatient, frtPractitioner, frtRelatedPerson], '', SearchXpathUsageNormal);
  indexes.add(frtMedicationStatement, 'status', 'Return statements that match the given status', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForMessageHeader(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtMessageHeader, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtMessageHeader, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtMessageHeader, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtMessageHeader, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtMessageHeader, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtMessageHeader, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtMessageHeader, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtMessageHeader, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtMessageHeader, 'author', 'The source of the decision', SearchParamTypeREFERENCE, [frtPractitioner], '', SearchXpathUsageNormal);
  indexes.add(frtMessageHeader, 'code', 'ok | transient-error | fatal-error', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtMessageHeader, 'data', 'The actual content of the message', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPES, '', SearchXpathUsageNormal);
  indexes.add(frtMessageHeader, 'destination', 'Name of system', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtMessageHeader, 'destination-uri', 'Actual destination address or id', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtMessageHeader, 'enterer', 'The source of the data entry', SearchParamTypeREFERENCE, [frtPractitioner], '', SearchXpathUsageNormal);
  indexes.add(frtMessageHeader, 'event', 'Code for the event this message represents', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtMessageHeader, 'receiver', 'Intended "real-world" recipient for the data', SearchParamTypeREFERENCE, [frtOrganization, frtPractitioner], '', SearchXpathUsageNormal);
  indexes.add(frtMessageHeader, 'response-id', 'Id of original message', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtMessageHeader, 'responsible', 'Final responsibility for event', SearchParamTypeREFERENCE, [frtOrganization, frtPractitioner], '', SearchXpathUsageNormal);
  indexes.add(frtMessageHeader, 'source', 'Name of system', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtMessageHeader, 'source-uri', 'Actual message source address or id', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtMessageHeader, 'target', 'Particular delivery destination within the destination', SearchParamTypeREFERENCE, [frtDevice], '', SearchXpathUsageNormal);
  indexes.add(frtMessageHeader, 'timestamp', 'Time that the message was sent', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForNamingSystem(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtNamingSystem, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtNamingSystem, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtNamingSystem, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtNamingSystem, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtNamingSystem, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtNamingSystem, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtNamingSystem, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtNamingSystem, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtNamingSystem, 'contact', 'Name of a individual to contact', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtNamingSystem, 'context', 'Content intends to support these contexts', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtNamingSystem, 'date', 'Publication Date(/time)', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtNamingSystem, 'id-type', 'oid | uuid | uri | other', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtNamingSystem, 'kind', 'codesystem | identifier | root', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtNamingSystem, 'name', 'Human-readable label', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtNamingSystem, 'period', 'When is identifier valid?', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtNamingSystem, 'publisher', 'Name of the publisher (Organization or individual)', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtNamingSystem, 'replaced-by', 'Use this instead', SearchParamTypeREFERENCE, [frtNamingSystem], '', SearchXpathUsageNormal);
  indexes.add(frtNamingSystem, 'responsible', 'Who maintains system namespace?', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtNamingSystem, 'status', 'draft | active | retired', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtNamingSystem, 'telecom', 'Contact details for individual or publisher', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtNamingSystem, 'type', 'e.g. driver,  provider,  patient, bank etc.', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtNamingSystem, 'value', 'The unique identifier', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForNutritionOrder(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtNutritionOrder, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtNutritionOrder, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtNutritionOrder, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtNutritionOrder, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtNutritionOrder, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtNutritionOrder, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtNutritionOrder, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtNutritionOrder, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtNutritionOrder, 'additive', 'Type of module component to add to the feeding', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtNutritionOrder, 'datetime', 'Return nutrition orders requested on this date', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtNutritionOrder, 'encounter', 'Return nutrition orders with this encounter identifier', SearchParamTypeREFERENCE, [frtEncounter], '', SearchXpathUsageNormal);
  indexes.add(frtNutritionOrder, 'formula', 'Type of enteral or infant formula', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtNutritionOrder, 'identifier', 'Return nutrition orders with this external identifier', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtNutritionOrder, 'oraldiet', 'Type of diet that can be consumed orally (i.e., take via the mouth).', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtNutritionOrder, 'patient', 'The identity of the person who requires the diet, formula or nutritional supplement', SearchParamTypeREFERENCE, [frtPatient], '', SearchXpathUsageNormal);
  indexes.add(frtNutritionOrder, 'provider', 'The identify of the provider who placed the nutrition order', SearchParamTypeREFERENCE, [frtPractitioner], '', SearchXpathUsageNormal);
  indexes.add(frtNutritionOrder, 'status', 'Status of the nutrition order.', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtNutritionOrder, 'supplement', 'Type of supplement product requested', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForObservation(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtObservation, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtObservation, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtObservation, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtObservation, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtObservation, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtObservation, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtObservation, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtObservation, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtObservation, 'category', 'The classification of the type of observation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtObservation, 'code', 'The code of the observation type', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtObservation, 'code-value-[x]', 'Both code and one of the value parameters', SearchParamTypeCOMPOSITE, [], '', SearchXpathUsageNormal);
  indexes.add(frtObservation, 'component-code', 'The component code of the observation type', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtObservation, 'component-code-value-[x]', 'Both component code and one of the component value parameters', SearchParamTypeCOMPOSITE, [], '', SearchXpathUsageNormal);
  indexes.add(frtObservation, 'component-data-absent-reason', 'The reason why the expected value in the element Observation.component.value[x] is missing.', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtObservation, 'component-value-concept', 'The value of the component observation, if the value is a CodeableConcept', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtObservation, 'component-value-quantity', 'The value of the component observation, if the value is a Quantity, or a SampledData (just search on the bounds of the values in sampled data)', SearchParamTypeQUANTITY, [], '', SearchXpathUsageNormal);
  indexes.add(frtObservation, 'component-value-string', 'The value of the component observation, if the value is a string, and also searches in CodeableConcept.text', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtObservation, 'data-absent-reason', 'The reason why the expected value in the element Observation.value[x] is missing.', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtObservation, 'date', 'Obtained date/time. If the obtained element is a period, a date that falls in the period', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtObservation, 'device', 'The Device that generated the observation data.', SearchParamTypeREFERENCE, [frtDevice, frtDeviceMetric], '', SearchXpathUsageNormal);
  indexes.add(frtObservation, 'encounter', 'Healthcare event related to the observation', SearchParamTypeREFERENCE, [frtEncounter], '', SearchXpathUsageNormal);
  indexes.add(frtObservation, 'identifier', 'The unique id for a particular observation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtObservation, 'patient', 'The subject that the observation is about (if patient)', SearchParamTypeREFERENCE, [frtPatient], '', SearchXpathUsageNormal);
  indexes.add(frtObservation, 'performer', 'Who performed the observation', SearchParamTypeREFERENCE, [frtOrganization, frtPatient, frtPractitioner, frtRelatedPerson], '', SearchXpathUsageNormal);
  indexes.add(frtObservation, 'related', 'Related Observations - search on related-type and related-target together', SearchParamTypeCOMPOSITE, [], '', SearchXpathUsageNormal);
  indexes.add(frtObservation, 'related-target', 'Resource that is related to this one', SearchParamTypeREFERENCE, [frtObservation, frtQuestionnaireResponse], '', SearchXpathUsageNormal);
  indexes.add(frtObservation, 'related-type', 'has-member | derived-from | sequel-to | replaces | qualified-by | interfered-by', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtObservation, 'specimen', 'Specimen used for this observation', SearchParamTypeREFERENCE, [frtSpecimen], '', SearchXpathUsageNormal);
  indexes.add(frtObservation, 'status', 'The status of the observation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtObservation, 'subject', 'The subject that the observation is about', SearchParamTypeREFERENCE, [frtDevice, frtPatient, frtLocation, frtGroup], '', SearchXpathUsageNormal);
  indexes.add(frtObservation, 'value-concept', 'The value of the observation, if the value is a CodeableConcept', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtObservation, 'value-date', 'The value of the observation, if the value is a date or period of time', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtObservation, 'value-quantity', 'The value of the observation, if the value is a Quantity, or a SampledData (just search on the bounds of the values in sampled data)', SearchParamTypeQUANTITY, [], '', SearchXpathUsageNormal);
  indexes.add(frtObservation, 'value-string', 'The value of the observation, if the value is a string, and also searches in CodeableConcept.text', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForOperationDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtOperationDefinition, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtOperationDefinition, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtOperationDefinition, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtOperationDefinition, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtOperationDefinition, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtOperationDefinition, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtOperationDefinition, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtOperationDefinition, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtOperationDefinition, 'base', 'Marks this as a profile of the base', SearchParamTypeREFERENCE, [frtOperationDefinition], '', SearchXpathUsageNormal);
  indexes.add(frtOperationDefinition, 'code', 'Name used to invoke the operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtOperationDefinition, 'date', 'Date for this version of the operation definition', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtOperationDefinition, 'instance', 'Invoke on an instance?', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtOperationDefinition, 'kind', 'operation | query', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtOperationDefinition, 'name', 'Informal name for this operation', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtOperationDefinition, 'profile', 'Profile on the type', SearchParamTypeREFERENCE, [frtStructureDefinition], '', SearchXpathUsageNormal);
  indexes.add(frtOperationDefinition, 'publisher', 'Name of the publisher (Organization or individual)', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtOperationDefinition, 'status', 'draft | active | retired', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtOperationDefinition, 'system', 'Invoke at the system level?', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtOperationDefinition, 'type', 'Invoke at resource level for these type', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtOperationDefinition, 'url', 'Logical URL to reference this operation definition', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtOperationDefinition, 'version', 'Logical id for this version of the operation definition', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForOperationOutcome(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtOperationOutcome, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtOperationOutcome, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtOperationOutcome, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtOperationOutcome, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtOperationOutcome, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtOperationOutcome, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtOperationOutcome, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtOperationOutcome, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForOrder(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtOrder, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtOrder, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtOrder, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtOrder, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtOrder, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtOrder, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtOrder, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtOrder, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtOrder, 'date', 'When the order was made', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtOrder, 'detail', 'What action is being ordered', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPES, '', SearchXpathUsageNormal);
  indexes.add(frtOrder, 'identifier', 'Instance id from source, target, and/or  others', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtOrder, 'patient', 'Patient this order is about', SearchParamTypeREFERENCE, [frtPatient], '', SearchXpathUsageNormal);
  indexes.add(frtOrder, 'source', 'Who initiated the order', SearchParamTypeREFERENCE, [frtOrganization, frtPractitioner], '', SearchXpathUsageNormal);
  indexes.add(frtOrder, 'subject', 'Patient this order is about', SearchParamTypeREFERENCE, [frtDevice, frtPatient, frtSubstance, frtGroup], '', SearchXpathUsageNormal);
  indexes.add(frtOrder, 'target', 'Who is intended to fulfill the order', SearchParamTypeREFERENCE, [frtDevice, frtOrganization, frtPractitioner], '', SearchXpathUsageNormal);
  indexes.add(frtOrder, 'when', 'A formal schedule', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtOrder, 'when_code', 'Code specifies when request should be done. The code may simply be a priority code', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForOrderResponse(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtOrderResponse, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtOrderResponse, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtOrderResponse, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtOrderResponse, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtOrderResponse, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtOrderResponse, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtOrderResponse, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtOrderResponse, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtOrderResponse, 'code', 'pending | review | rejected | error | accepted | cancelled | replaced | aborted | completed', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtOrderResponse, 'date', 'When the response was made', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtOrderResponse, 'fulfillment', 'Details of the outcome of performing the order', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPES, '', SearchXpathUsageNormal);
  indexes.add(frtOrderResponse, 'identifier', 'Identifiers assigned to this order by the orderer or by the receiver', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtOrderResponse, 'request', 'The order that this is a response to', SearchParamTypeREFERENCE, [frtOrder], '', SearchXpathUsageNormal);
  indexes.add(frtOrderResponse, 'who', 'Who made the response', SearchParamTypeREFERENCE, [frtDevice, frtOrganization, frtPractitioner], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForOrganization(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtOrganization, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtOrganization, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtOrganization, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtOrganization, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtOrganization, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtOrganization, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtOrganization, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtOrganization, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtOrganization, 'active', 'Whether the organization''s record is active', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtOrganization, 'address', 'A (part of the) address of the Organization', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtOrganization, 'address-city', 'A city specified in an address', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtOrganization, 'address-country', 'A country specified in an address', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtOrganization, 'address-postalcode', 'A postal code specified in an address', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtOrganization, 'address-state', 'A state specified in an address', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtOrganization, 'address-use', 'A use code specified in an address', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtOrganization, 'identifier', 'Any identifier for the organization (not the accreditation issuer''s identifier)', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtOrganization, 'name', 'A portion of the organization''s name', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtOrganization, 'partof', 'Search all organizations that are part of the given organization', SearchParamTypeREFERENCE, [frtOrganization], '', SearchXpathUsageNormal);
  indexes.add(frtOrganization, 'phonetic', 'A portion of the organization''s name using some kind of phonetic matching algorithm', SearchParamTypeSTRING, [], '', SearchXpathUsagePhonetic);
  indexes.add(frtOrganization, 'type', 'A code for the type of organization', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForPatient(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtPatient, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtPatient, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtPatient, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtPatient, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtPatient, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtPatient, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtPatient, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtPatient, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtPatient, 'active', 'Whether the patient record is active', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtPatient, 'address', 'An address in any kind of address/part of the patient', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtPatient, 'address-city', 'A city specified in an address', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtPatient, 'address-country', 'A country specified in an address', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtPatient, 'address-postalcode', 'A postalCode specified in an address', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtPatient, 'address-state', 'A state specified in an address', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtPatient, 'address-use', 'A use code specified in an address', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtPatient, 'animal-breed', 'The breed for animal patients', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtPatient, 'animal-species', 'The species for animal patients', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtPatient, 'birthdate', 'The patient''s date of birth', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtPatient, 'careprovider', 'Patient''s nominated care provider, could be a care manager, not the organization that manages the record', SearchParamTypeREFERENCE, [frtOrganization, frtPractitioner], '', SearchXpathUsageNormal);
  indexes.add(frtPatient, 'deathdate', 'The date of death has been provided and satisfies this search value', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtPatient, 'deceased', 'This patient has been marked as deceased, or as a death date entered', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtPatient, 'email', 'A value in an email contact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtPatient, 'ethnicity', 'Returns patients with an ethnicity extension matching the specified code.', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtPatient, 'family', 'A portion of the family name of the patient', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtPatient, 'gender', 'Gender of the patient', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtPatient, 'given', 'A portion of the given name of the patient', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtPatient, 'identifier', 'A patient identifier', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtPatient, 'language', 'Language code (irrespective of use value)', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtPatient, 'link', 'All patients linked to the given patient', SearchParamTypeREFERENCE, [frtPatient], '', SearchXpathUsageNormal);
  indexes.add(frtPatient, 'name', 'A portion of either family or given name of the patient', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtPatient, 'organization', 'The organization at which this person is a patient', SearchParamTypeREFERENCE, [frtOrganization], '', SearchXpathUsageNormal);
  indexes.add(frtPatient, 'phone', 'A value in a phone contact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtPatient, 'phonetic', 'A portion of either family or given name using some kind of phonetic matching algorithm', SearchParamTypeSTRING, [], '', SearchXpathUsagePhonetic);
  indexes.add(frtPatient, 'race', 'Returns patients with a race extension matching the specified code.', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtPatient, 'telecom', 'The value in any kind of telecom details of the patient', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForPaymentNotice(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtPaymentNotice, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtPaymentNotice, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtPaymentNotice, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtPaymentNotice, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtPaymentNotice, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtPaymentNotice, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtPaymentNotice, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtPaymentNotice, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtPaymentNotice, 'identifier', 'The business identifier of the Eligibility', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForPaymentReconciliation(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtPaymentReconciliation, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtPaymentReconciliation, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtPaymentReconciliation, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtPaymentReconciliation, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtPaymentReconciliation, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtPaymentReconciliation, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtPaymentReconciliation, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtPaymentReconciliation, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtPaymentReconciliation, 'identifier', 'The business identifier of the Explanation of Benefit', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForPerson(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtPerson, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtPerson, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtPerson, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtPerson, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtPerson, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtPerson, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtPerson, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtPerson, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtPerson, 'address', 'An address in any kind of address/part', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtPerson, 'address-city', 'A city specified in an address', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtPerson, 'address-country', 'A country specified in an address', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtPerson, 'address-postalcode', 'A postal code specified in an address', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtPerson, 'address-state', 'A state specified in an address', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtPerson, 'address-use', 'A use code specified in an address', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtPerson, 'birthdate', 'The person''s date of birth', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtPerson, 'email', 'A value in an email contact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtPerson, 'gender', 'The gender of the person', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtPerson, 'identifier', 'A person Identifier', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtPerson, 'link', 'Any link has this Patient, Person, RelatedPerson or Practitioner reference', SearchParamTypeREFERENCE, [frtPatient, frtPractitioner, frtPerson, frtRelatedPerson], '', SearchXpathUsageNormal);
  indexes.add(frtPerson, 'name', 'A portion of name in any name part', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtPerson, 'organization', 'The organization at which this person record is being managed', SearchParamTypeREFERENCE, [frtOrganization], '', SearchXpathUsageNormal);
  indexes.add(frtPerson, 'patient', 'The Person links to this Patient', SearchParamTypeREFERENCE, [frtPatient], '', SearchXpathUsageNormal);
  indexes.add(frtPerson, 'phone', 'A value in a phone contact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtPerson, 'phonetic', 'A portion of name using some kind of phonetic matching algorithm', SearchParamTypeSTRING, [], '', SearchXpathUsagePhonetic);
  indexes.add(frtPerson, 'practitioner', 'The Person links to this Practitioner', SearchParamTypeREFERENCE, [frtPractitioner], '', SearchXpathUsageNormal);
  indexes.add(frtPerson, 'relatedperson', 'The Person links to this RelatedPerson', SearchParamTypeREFERENCE, [frtRelatedPerson], '', SearchXpathUsageNormal);
  indexes.add(frtPerson, 'telecom', 'The value in any kind of contact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForPractitioner(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtPractitioner, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtPractitioner, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtPractitioner, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtPractitioner, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtPractitioner, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtPractitioner, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtPractitioner, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtPractitioner, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtPractitioner, 'address', 'An address in any kind of address/part', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtPractitioner, 'address-city', 'A city specified in an address', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtPractitioner, 'address-country', 'A country specified in an address', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtPractitioner, 'address-postalcode', 'A postalCode specified in an address', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtPractitioner, 'address-state', 'A state specified in an address', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtPractitioner, 'address-use', 'A use code specified in an address', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtPractitioner, 'communication', 'One of the languages that the practitioner can communicate with', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtPractitioner, 'email', 'A value in an email contact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtPractitioner, 'family', 'A portion of the family name', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtPractitioner, 'gender', 'Gender of the practitioner', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtPractitioner, 'given', 'A portion of the given name', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtPractitioner, 'identifier', 'A practitioner''s Identifier', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtPractitioner, 'location', 'One of the locations at which this practitioner provides care', SearchParamTypeREFERENCE, [frtLocation], '', SearchXpathUsageNormal);
  indexes.add(frtPractitioner, 'name', 'A portion of either family or given name', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtPractitioner, 'organization', 'The identity of the organization the practitioner represents / acts on behalf of', SearchParamTypeREFERENCE, [frtOrganization], '', SearchXpathUsageNormal);
  indexes.add(frtPractitioner, 'phone', 'A value in a phone contact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtPractitioner, 'phonetic', 'A portion of either family or given name using some kind of phonetic matching algorithm', SearchParamTypeSTRING, [], '', SearchXpathUsagePhonetic);
  indexes.add(frtPractitioner, 'role', 'The practitioner can perform this role at for the organization', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtPractitioner, 'specialty', 'The practitioner has this specialty at an organization', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtPractitioner, 'telecom', 'The value in any kind of contact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForProcedure(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtProcedure, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtProcedure, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtProcedure, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtProcedure, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtProcedure, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtProcedure, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtProcedure, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtProcedure, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtProcedure, 'code', 'A code to identify a  procedure', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtProcedure, 'date', 'Date/Period the procedure was performed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtProcedure, 'encounter', 'The encounter associated with the procedure', SearchParamTypeREFERENCE, [frtEncounter], '', SearchXpathUsageNormal);
  indexes.add(frtProcedure, 'identifier', 'A unique identifier for a procedure', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtProcedure, 'location', 'Where the procedure happened', SearchParamTypeREFERENCE, [frtLocation], '', SearchXpathUsageNormal);
  indexes.add(frtProcedure, 'patient', 'Search by subject - a patient', SearchParamTypeREFERENCE, [frtPatient], '', SearchXpathUsageNormal);
  indexes.add(frtProcedure, 'performer', 'The reference to the practitioner', SearchParamTypeREFERENCE, [frtOrganization, frtPatient, frtPractitioner, frtRelatedPerson], '', SearchXpathUsageNormal);
  indexes.add(frtProcedure, 'subject', 'Search by subject', SearchParamTypeREFERENCE, [frtPatient, frtGroup], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForProcedureRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtProcedureRequest, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtProcedureRequest, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtProcedureRequest, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtProcedureRequest, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtProcedureRequest, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtProcedureRequest, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtProcedureRequest, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtProcedureRequest, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtProcedureRequest, 'encounter', 'Encounter request created during', SearchParamTypeREFERENCE, [frtEncounter], '', SearchXpathUsageNormal);
  indexes.add(frtProcedureRequest, 'identifier', 'A unique identifier of the Procedure Request', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtProcedureRequest, 'orderer', 'Who made request', SearchParamTypeREFERENCE, [frtDevice, frtPatient, frtPractitioner, frtRelatedPerson], '', SearchXpathUsageNormal);
  indexes.add(frtProcedureRequest, 'patient', 'Search by subject - a patient', SearchParamTypeREFERENCE, [frtPatient], '', SearchXpathUsageNormal);
  indexes.add(frtProcedureRequest, 'performer', 'Who should perform the procedure', SearchParamTypeREFERENCE, [frtOrganization, frtPatient, frtPractitioner, frtRelatedPerson], '', SearchXpathUsageNormal);
  indexes.add(frtProcedureRequest, 'subject', 'Search by subject', SearchParamTypeREFERENCE, [frtPatient, frtGroup], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForProcessRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtProcessRequest, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtProcessRequest, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtProcessRequest, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtProcessRequest, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtProcessRequest, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtProcessRequest, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtProcessRequest, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtProcessRequest, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtProcessRequest, 'action', 'The action requested by this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtProcessRequest, 'identifier', 'The business identifier of the ProcessRequest', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtProcessRequest, 'organization', 'The organization who generated this request', SearchParamTypeREFERENCE, [frtOrganization], '', SearchXpathUsageNormal);
  indexes.add(frtProcessRequest, 'provider', 'The provider who regenerated this request', SearchParamTypeREFERENCE, [frtPractitioner], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForProcessResponse(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtProcessResponse, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtProcessResponse, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtProcessResponse, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtProcessResponse, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtProcessResponse, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtProcessResponse, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtProcessResponse, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtProcessResponse, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtProcessResponse, 'identifier', 'The business identifier of the Explanation of Benefit', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtProcessResponse, 'organization', 'The organization who generated this resource', SearchParamTypeREFERENCE, [frtOrganization], '', SearchXpathUsageNormal);
  indexes.add(frtProcessResponse, 'request', 'The reference to the claim', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPES, '', SearchXpathUsageNormal);
  indexes.add(frtProcessResponse, 'requestorganization', 'The Organization who is responsible the request transaction', SearchParamTypeREFERENCE, [frtOrganization], '', SearchXpathUsageNormal);
  indexes.add(frtProcessResponse, 'requestprovider', 'The Provider who is responsible the request transaction', SearchParamTypeREFERENCE, [frtPractitioner], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForProvenance(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtProvenance, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtProvenance, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtProvenance, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtProvenance, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtProvenance, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtProvenance, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtProvenance, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtProvenance, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtProvenance, 'agent', 'Individual, device or organization playing role', SearchParamTypeREFERENCE, [frtDevice, frtPatient, frtOrganization, frtPractitioner, frtRelatedPerson], '', SearchXpathUsageNormal);
  indexes.add(frtProvenance, 'end', 'End time with inclusive boundary, if not ongoing', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtProvenance, 'entity', 'Identity of entity', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtProvenance, 'entitytype', 'The type of resource in this entity', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtProvenance, 'location', 'Where the activity occurred, if relevant', SearchParamTypeREFERENCE, [frtLocation], '', SearchXpathUsageNormal);
  indexes.add(frtProvenance, 'patient', 'Target Reference(s) (usually version specific)', SearchParamTypeREFERENCE, [frtPatient], '', SearchXpathUsageNormal);
  indexes.add(frtProvenance, 'sigtype', 'Indication of the reason the entity signed the object(s)', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtProvenance, 'start', 'Starting time with inclusive boundary', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtProvenance, 'target', 'Target Reference(s) (usually version specific)', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPES, '', SearchXpathUsageNormal);
  indexes.add(frtProvenance, 'userid', 'Authorization-system identifier for the agent', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForQuestionnaire(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtQuestionnaire, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtQuestionnaire, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtQuestionnaire, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtQuestionnaire, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtQuestionnaire, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtQuestionnaire, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtQuestionnaire, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtQuestionnaire, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtQuestionnaire, 'code', 'A code that corresponds to the questionnaire or one of its groups', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtQuestionnaire, 'date', 'When the questionnaire was last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtQuestionnaire, 'identifier', 'An identifier for the questionnaire', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtQuestionnaire, 'publisher', 'The author of the questionnaire', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtQuestionnaire, 'status', 'The status of the questionnaire', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtQuestionnaire, 'title', 'All or part of the name of the questionnaire (title for the root group of the questionnaire)', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtQuestionnaire, 'version', 'The business version of the questionnaire', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForQuestionnaireResponse(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtQuestionnaireResponse, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtQuestionnaireResponse, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtQuestionnaireResponse, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtQuestionnaireResponse, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtQuestionnaireResponse, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtQuestionnaireResponse, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtQuestionnaireResponse, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtQuestionnaireResponse, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtQuestionnaireResponse, 'author', 'The author of the questionnaire', SearchParamTypeREFERENCE, [frtDevice, frtPatient, frtPractitioner, frtRelatedPerson], '', SearchXpathUsageNormal);
  indexes.add(frtQuestionnaireResponse, 'authored', 'When the questionnaire was authored', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtQuestionnaireResponse, 'encounter', 'Encounter during which questionnaire was authored', SearchParamTypeREFERENCE, [frtEncounter], '', SearchXpathUsageNormal);
  indexes.add(frtQuestionnaireResponse, 'patient', 'The patient that is the subject of the questionnaire', SearchParamTypeREFERENCE, [frtPatient], '', SearchXpathUsageNormal);
  indexes.add(frtQuestionnaireResponse, 'questionnaire', 'The questionnaire the answers are provided for', SearchParamTypeREFERENCE, [frtQuestionnaire], '', SearchXpathUsageNormal);
  indexes.add(frtQuestionnaireResponse, 'source', 'The person who answered the questions', SearchParamTypeREFERENCE, [frtPatient, frtPractitioner, frtRelatedPerson], '', SearchXpathUsageNormal);
  indexes.add(frtQuestionnaireResponse, 'status', 'The status of the questionnaire response', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtQuestionnaireResponse, 'subject', 'The subject of the questionnaire', SearchParamTypeREFERENCE, ALL_RESOURCE_TYPES, '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForReferralRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtReferralRequest, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtReferralRequest, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtReferralRequest, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtReferralRequest, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtReferralRequest, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtReferralRequest, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtReferralRequest, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtReferralRequest, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtReferralRequest, 'date', 'Creation or activation date', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtReferralRequest, 'patient', 'Who the referral is about', SearchParamTypeREFERENCE, [frtPatient], '', SearchXpathUsageNormal);
  indexes.add(frtReferralRequest, 'priority', 'The priority assigned to the referral', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtReferralRequest, 'recipient', 'The person that the referral was sent to', SearchParamTypeREFERENCE, [frtOrganization, frtPractitioner], '', SearchXpathUsageNormal);
  indexes.add(frtReferralRequest, 'requester', 'Requester of referral / transfer of care', SearchParamTypeREFERENCE, [frtOrganization, frtPatient, frtPractitioner], '', SearchXpathUsageNormal);
  indexes.add(frtReferralRequest, 'specialty', 'The specialty that the referral is for', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtReferralRequest, 'status', 'The status of the referral', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtReferralRequest, 'type', 'The type of the referral', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForRelatedPerson(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtRelatedPerson, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtRelatedPerson, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtRelatedPerson, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtRelatedPerson, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtRelatedPerson, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtRelatedPerson, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtRelatedPerson, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtRelatedPerson, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtRelatedPerson, 'address', 'An address in any kind of address/part', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtRelatedPerson, 'address-city', 'A city specified in an address', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtRelatedPerson, 'address-country', 'A country specified in an address', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtRelatedPerson, 'address-postalcode', 'A postal code specified in an address', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtRelatedPerson, 'address-state', 'A state specified in an address', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtRelatedPerson, 'address-use', 'A use code specified in an address', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtRelatedPerson, 'birthdate', 'The Related Person''s date of birth', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtRelatedPerson, 'email', 'A value in an email contact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtRelatedPerson, 'gender', 'Gender of the person', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtRelatedPerson, 'identifier', 'A patient Identifier', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtRelatedPerson, 'name', 'A portion of name in any name part', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtRelatedPerson, 'patient', 'The patient this person is related to', SearchParamTypeREFERENCE, [frtPatient], '', SearchXpathUsageNormal);
  indexes.add(frtRelatedPerson, 'phone', 'A value in a phone contact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtRelatedPerson, 'phonetic', 'A portion of name using some kind of phonetic matching algorithm', SearchParamTypeSTRING, [], '', SearchXpathUsagePhonetic);
  indexes.add(frtRelatedPerson, 'telecom', 'The value in any kind of contact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForRiskAssessment(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtRiskAssessment, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtRiskAssessment, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtRiskAssessment, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtRiskAssessment, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtRiskAssessment, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtRiskAssessment, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtRiskAssessment, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtRiskAssessment, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtRiskAssessment, 'condition', 'Condition assessed', SearchParamTypeREFERENCE, [frtCondition], '', SearchXpathUsageNormal);
  indexes.add(frtRiskAssessment, 'date', 'When was assessment made?', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtRiskAssessment, 'encounter', 'Where was assessment performed?', SearchParamTypeREFERENCE, [frtEncounter], '', SearchXpathUsageNormal);
  indexes.add(frtRiskAssessment, 'identifier', 'Unique identifier for the assessment', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtRiskAssessment, 'method', 'Evaluation mechanism', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtRiskAssessment, 'patient', 'Who/what does assessment apply to?', SearchParamTypeREFERENCE, [frtPatient], '', SearchXpathUsageNormal);
  indexes.add(frtRiskAssessment, 'performer', 'Who did assessment?', SearchParamTypeREFERENCE, [frtDevice, frtPractitioner], '', SearchXpathUsageNormal);
  indexes.add(frtRiskAssessment, 'subject', 'Who/what does assessment apply to?', SearchParamTypeREFERENCE, [frtPatient, frtGroup], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForSchedule(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtSchedule, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtSchedule, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtSchedule, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtSchedule, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtSchedule, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtSchedule, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtSchedule, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtSchedule, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtSchedule, 'actor', 'The individual(HealthcareService, Practitioner, Location, ...) to find a Schedule for', SearchParamTypeREFERENCE, [frtDevice, frtPatient, frtHealthcareService, frtLocation, frtPractitioner, frtRelatedPerson], '', SearchXpathUsageNormal);
  indexes.add(frtSchedule, 'date', 'Search for Schedule resources that have a period that contains this date specified', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtSchedule, 'identifier', 'A Schedule Identifier', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtSchedule, 'type', 'The type of appointments that can be booked into associated slot(s)', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForSearchParameter(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtSearchParameter, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtSearchParameter, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtSearchParameter, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtSearchParameter, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtSearchParameter, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtSearchParameter, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtSearchParameter, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtSearchParameter, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtSearchParameter, 'base', 'The resource type this search parameter applies to', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtSearchParameter, 'code', 'Code used in URL', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtSearchParameter, 'description', 'Documentation for  search parameter', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtSearchParameter, 'name', 'Informal name for this search parameter', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtSearchParameter, 'target', 'Types of resource (if a resource reference)', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtSearchParameter, 'type', 'number | date | string | token | reference | composite | quantity | uri', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtSearchParameter, 'url', 'Absolute URL used to reference this search parameter', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForSlot(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtSlot, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtSlot, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtSlot, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtSlot, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtSlot, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtSlot, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtSlot, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtSlot, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtSlot, 'fb-type', 'The free/busy status of the appointment', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtSlot, 'identifier', 'A Slot Identifier', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtSlot, 'schedule', 'The Schedule Resource that we are seeking a slot within', SearchParamTypeREFERENCE, [frtSchedule], '', SearchXpathUsageNormal);
  indexes.add(frtSlot, 'slot-type', 'The type of appointments that can be booked into the slot', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtSlot, 'start', 'Appointment date/time.', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForSpecimen(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtSpecimen, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtSpecimen, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtSpecimen, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtSpecimen, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtSpecimen, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtSpecimen, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtSpecimen, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtSpecimen, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtSpecimen, 'accession', 'The accession number associated with the specimen', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtSpecimen, 'bodysite', 'The code for the body site from where the specimen originated', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtSpecimen, 'collected', 'The date the specimen was collected', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtSpecimen, 'collector', 'Who collected the specimen', SearchParamTypeREFERENCE, [frtPractitioner], '', SearchXpathUsageNormal);
  indexes.add(frtSpecimen, 'container', 'The kind of specimen container', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtSpecimen, 'container-id', 'The unique identifier associated with the specimen container', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtSpecimen, 'identifier', 'The unique identifier associated with the specimen', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtSpecimen, 'parent', 'The parent of the specimen', SearchParamTypeREFERENCE, [frtSpecimen], '', SearchXpathUsageNormal);
  indexes.add(frtSpecimen, 'patient', 'The patient the specimen comes from', SearchParamTypeREFERENCE, [frtPatient], '', SearchXpathUsageNormal);
  indexes.add(frtSpecimen, 'subject', 'The subject of the specimen', SearchParamTypeREFERENCE, [frtDevice, frtPatient, frtSubstance, frtGroup], '', SearchXpathUsageNormal);
  indexes.add(frtSpecimen, 'type', 'The specimen type', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForStructureDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtStructureDefinition, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtStructureDefinition, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtStructureDefinition, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtStructureDefinition, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtStructureDefinition, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtStructureDefinition, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtStructureDefinition, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtStructureDefinition, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtStructureDefinition, 'abstract', 'Whether the structure is abstract', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtStructureDefinition, 'base', 'Structure that this set of constraints applies to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtStructureDefinition, 'base-path', 'Path that identifies the base element', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtStructureDefinition, 'code', 'A code for the profile', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtStructureDefinition, 'context', 'A use context assigned to the structure', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtStructureDefinition, 'context-type', 'resource | datatype | mapping | extension', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtStructureDefinition, 'date', 'The profile publication date', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtStructureDefinition, 'description', 'Text search in the description of the profile', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtStructureDefinition, 'display', 'Use this name when displaying the value', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtStructureDefinition, 'experimental', 'If for testing purposes, not real usage', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtStructureDefinition, 'ext-context', 'Where the extension can be used in instances', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtStructureDefinition, 'identifier', 'The identifier of the profile', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtStructureDefinition, 'kind', 'datatype | resource | logical', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtStructureDefinition, 'name', 'Name of the profile', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtStructureDefinition, 'path', 'A path that is constrained in the profile', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtStructureDefinition, 'publisher', 'Name of the publisher of the profile', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtStructureDefinition, 'status', 'The current status of the profile', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtStructureDefinition, 'type', 'Any datatype or resource, including abstract ones', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtStructureDefinition, 'url', 'Absolute URL used to reference this StructureDefinition', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtStructureDefinition, 'valueset', 'A vocabulary binding reference', SearchParamTypeREFERENCE, [frtValueSet], '', SearchXpathUsageNormal);
  indexes.add(frtStructureDefinition, 'version', 'The version identifier of the profile', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForSubscription(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtSubscription, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtSubscription, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtSubscription, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtSubscription, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtSubscription, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtSubscription, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtSubscription, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtSubscription, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtSubscription, 'contact', 'Contact details for source (e.g. troubleshooting)', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtSubscription, 'criteria', 'Rule for server push criteria', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtSubscription, 'payload', 'Mimetype to send, or blank for no payload', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtSubscription, 'status', 'requested | active | error | off', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtSubscription, 'tag', 'A tag to add to matching resources', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtSubscription, 'type', 'rest-hook | websocket | email | sms | message', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtSubscription, 'url', 'Where the channel points to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForSubstance(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtSubstance, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtSubstance, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtSubstance, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtSubstance, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtSubstance, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtSubstance, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtSubstance, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtSubstance, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtSubstance, 'category', 'The category of the substance', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtSubstance, 'code', 'The code of the substance', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtSubstance, 'container-identifier', 'Identifier of the package/container', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtSubstance, 'expiry', 'Expiry date of package or container of substance', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtSubstance, 'identifier', 'Unique identifier for the substance', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtSubstance, 'quantity', 'Amount of substance in the package', SearchParamTypeQUANTITY, [], '', SearchXpathUsageNormal);
  indexes.add(frtSubstance, 'substance', 'A component of the substance', SearchParamTypeREFERENCE, [frtSubstance], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForSupplyDelivery(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtSupplyDelivery, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtSupplyDelivery, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtSupplyDelivery, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtSupplyDelivery, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtSupplyDelivery, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtSupplyDelivery, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtSupplyDelivery, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtSupplyDelivery, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtSupplyDelivery, 'identifier', 'External identifier', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtSupplyDelivery, 'patient', 'Patient for whom the item is supplied', SearchParamTypeREFERENCE, [frtPatient], '', SearchXpathUsageNormal);
  indexes.add(frtSupplyDelivery, 'receiver', 'Who collected the Supply', SearchParamTypeREFERENCE, [frtPractitioner], '', SearchXpathUsageNormal);
  indexes.add(frtSupplyDelivery, 'status', 'in-progress | completed | abandoned', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtSupplyDelivery, 'supplier', 'Dispenser', SearchParamTypeREFERENCE, [frtPractitioner], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForSupplyRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtSupplyRequest, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtSupplyRequest, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtSupplyRequest, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtSupplyRequest, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtSupplyRequest, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtSupplyRequest, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtSupplyRequest, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtSupplyRequest, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtSupplyRequest, 'date', 'When the request was made', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtSupplyRequest, 'identifier', 'Unique identifier', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtSupplyRequest, 'kind', 'The kind of supply (central, non-stock, etc.)', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtSupplyRequest, 'patient', 'Patient for whom the item is supplied', SearchParamTypeREFERENCE, [frtPatient], '', SearchXpathUsageNormal);
  indexes.add(frtSupplyRequest, 'source', 'Who initiated this order', SearchParamTypeREFERENCE, [frtOrganization, frtPatient, frtPractitioner], '', SearchXpathUsageNormal);
  indexes.add(frtSupplyRequest, 'status', 'requested | completed | failed | cancelled', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtSupplyRequest, 'supplier', 'Who is intended to fulfill the request', SearchParamTypeREFERENCE, [frtOrganization], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForTestScript(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtTestScript, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtTestScript, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtTestScript, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtTestScript, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtTestScript, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtTestScript, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtTestScript, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtTestScript, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtTestScript, 'description', 'Natural language description of the TestScript', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtTestScript, 'identifier', 'External identifier', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtTestScript, 'name', 'Informal name for this TestScript', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtTestScript, 'testscript-capability', 'TestScript required and validated capability', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtTestScript, 'testscript-setup-capability', 'TestScript setup required and validated capability', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtTestScript, 'testscript-test-capability', 'TestScript test required and validated capability', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtTestScript, 'url', 'Absolute URL used to reference this TestScript', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForValueSet(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtValueSet, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtValueSet, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtValueSet, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtValueSet, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtValueSet, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtValueSet, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtValueSet, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtValueSet, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtValueSet, 'code', 'A code defined in the value set', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtValueSet, 'context', 'A use context assigned to the value set', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtValueSet, 'date', 'The value set publication date', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtValueSet, 'description', 'Text search in the description of the value set', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtValueSet, 'expansion', 'Uniquely identifies this expansion', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtValueSet, 'identifier', 'The identifier for the value set', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtValueSet, 'name', 'The name of the value set', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtValueSet, 'publisher', 'Name of the publisher of the value set', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtValueSet, 'reference', 'A code system included or excluded in the value set or an imported value set', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtValueSet, 'status', 'The status of the value set', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtValueSet, 'system', 'The system for any codes defined by this value set', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtValueSet, 'url', 'The logical URL for the value set', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtValueSet, 'version', 'The version identifier of the value set', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.buildIndexesForVisionPrescription(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add(frtVisionPrescription, '_content', 'Search on the entire content of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtVisionPrescription, '_id', 'Logical id of this artifact', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtVisionPrescription, '_lastUpdated', 'When the resource version last changed', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtVisionPrescription, '_profile', 'Profiles this resource claims to conform to', SearchParamTypeURI, [], '', SearchXpathUsageNormal);
  indexes.add(frtVisionPrescription, '_query', 'A custom search profile that describes a specific defined query operation', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtVisionPrescription, '_security', 'Security Labels applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtVisionPrescription, '_tag', 'Tags applied to this resource', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtVisionPrescription, '_text', 'Search on the narrative of the resource', SearchParamTypeSTRING, [], '', SearchXpathUsageNormal);
  indexes.add(frtVisionPrescription, 'datewritten', 'Return prescriptions written on this date', SearchParamTypeDATE, [], '', SearchXpathUsageNormal);
  indexes.add(frtVisionPrescription, 'encounter', 'Return prescriptions with this encounter identifier', SearchParamTypeREFERENCE, [frtEncounter], '', SearchXpathUsageNormal);
  indexes.add(frtVisionPrescription, 'identifier', 'Return prescriptions with this external identifier', SearchParamTypeTOKEN, [], '', SearchXpathUsageNormal);
  indexes.add(frtVisionPrescription, 'patient', 'The identity of a patient to list dispenses for', SearchParamTypeREFERENCE, [frtPatient], '', SearchXpathUsageNormal);
  indexes.add(frtVisionPrescription, 'prescriber', 'Who authorizes the vision product', SearchParamTypeREFERENCE, [frtPractitioner], '', SearchXpathUsageNormal);
end;

procedure TFHIRIndexBuilder.registerIndexes(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  buildIndexesForAccount(Indexes, compartments);
  buildIndexesForAllergyIntolerance(Indexes, compartments);
  buildIndexesForAppointment(Indexes, compartments);
  buildIndexesForAppointmentResponse(Indexes, compartments);
  buildIndexesForAuditEvent(Indexes, compartments);
  buildIndexesForBasic(Indexes, compartments);
  buildIndexesForBinary(Indexes, compartments);
  buildIndexesForBodySite(Indexes, compartments);
  buildIndexesForBundle(Indexes, compartments);
  buildIndexesForCarePlan(Indexes, compartments);
  buildIndexesForClaim(Indexes, compartments);
  buildIndexesForClaimResponse(Indexes, compartments);
  buildIndexesForClinicalImpression(Indexes, compartments);
  buildIndexesForCommunication(Indexes, compartments);
  buildIndexesForCommunicationRequest(Indexes, compartments);
  buildIndexesForComposition(Indexes, compartments);
  buildIndexesForConceptMap(Indexes, compartments);
  buildIndexesForCondition(Indexes, compartments);
  buildIndexesForConformance(Indexes, compartments);
  buildIndexesForContract(Indexes, compartments);
  buildIndexesForCoverage(Indexes, compartments);
  buildIndexesForDataElement(Indexes, compartments);
  buildIndexesForDetectedIssue(Indexes, compartments);
  buildIndexesForDevice(Indexes, compartments);
  buildIndexesForDeviceComponent(Indexes, compartments);
  buildIndexesForDeviceMetric(Indexes, compartments);
  buildIndexesForDeviceUseRequest(Indexes, compartments);
  buildIndexesForDeviceUseStatement(Indexes, compartments);
  buildIndexesForDiagnosticOrder(Indexes, compartments);
  buildIndexesForDiagnosticReport(Indexes, compartments);
  buildIndexesForDocumentManifest(Indexes, compartments);
  buildIndexesForDocumentReference(Indexes, compartments);
  buildIndexesForEligibilityRequest(Indexes, compartments);
  buildIndexesForEligibilityResponse(Indexes, compartments);
  buildIndexesForEncounter(Indexes, compartments);
  buildIndexesForEnrollmentRequest(Indexes, compartments);
  buildIndexesForEnrollmentResponse(Indexes, compartments);
  buildIndexesForEpisodeOfCare(Indexes, compartments);
  buildIndexesForExplanationOfBenefit(Indexes, compartments);
  buildIndexesForFamilyMemberHistory(Indexes, compartments);
  buildIndexesForFlag(Indexes, compartments);
  buildIndexesForGoal(Indexes, compartments);
  buildIndexesForGroup(Indexes, compartments);
  buildIndexesForHealthcareService(Indexes, compartments);
  buildIndexesForImagingObjectSelection(Indexes, compartments);
  buildIndexesForImagingStudy(Indexes, compartments);
  buildIndexesForImmunization(Indexes, compartments);
  buildIndexesForImmunizationRecommendation(Indexes, compartments);
  buildIndexesForImplementationGuide(Indexes, compartments);
  buildIndexesForList(Indexes, compartments);
  buildIndexesForLocation(Indexes, compartments);
  buildIndexesForMedia(Indexes, compartments);
  buildIndexesForMedication(Indexes, compartments);
  buildIndexesForMedicationAdministration(Indexes, compartments);
  buildIndexesForMedicationDispense(Indexes, compartments);
  buildIndexesForMedicationOrder(Indexes, compartments);
  buildIndexesForMedicationStatement(Indexes, compartments);
  buildIndexesForMessageHeader(Indexes, compartments);
  buildIndexesForNamingSystem(Indexes, compartments);
  buildIndexesForNutritionOrder(Indexes, compartments);
  buildIndexesForObservation(Indexes, compartments);
  buildIndexesForOperationDefinition(Indexes, compartments);
  buildIndexesForOperationOutcome(Indexes, compartments);
  buildIndexesForOrder(Indexes, compartments);
  buildIndexesForOrderResponse(Indexes, compartments);
  buildIndexesForOrganization(Indexes, compartments);
  buildIndexesForPatient(Indexes, compartments);
  buildIndexesForPaymentNotice(Indexes, compartments);
  buildIndexesForPaymentReconciliation(Indexes, compartments);
  buildIndexesForPerson(Indexes, compartments);
  buildIndexesForPractitioner(Indexes, compartments);
  buildIndexesForProcedure(Indexes, compartments);
  buildIndexesForProcedureRequest(Indexes, compartments);
  buildIndexesForProcessRequest(Indexes, compartments);
  buildIndexesForProcessResponse(Indexes, compartments);
  buildIndexesForProvenance(Indexes, compartments);
  buildIndexesForQuestionnaire(Indexes, compartments);
  buildIndexesForQuestionnaireResponse(Indexes, compartments);
  buildIndexesForReferralRequest(Indexes, compartments);
  buildIndexesForRelatedPerson(Indexes, compartments);
  buildIndexesForRiskAssessment(Indexes, compartments);
  buildIndexesForSchedule(Indexes, compartments);
  buildIndexesForSearchParameter(Indexes, compartments);
  buildIndexesForSlot(Indexes, compartments);
  buildIndexesForSpecimen(Indexes, compartments);
  buildIndexesForStructureDefinition(Indexes, compartments);
  buildIndexesForSubscription(Indexes, compartments);
  buildIndexesForSubstance(Indexes, compartments);
  buildIndexesForSupplyDelivery(Indexes, compartments);
  buildIndexesForSupplyRequest(Indexes, compartments);
  buildIndexesForTestScript(Indexes, compartments);
  buildIndexesForValueSet(Indexes, compartments);
  buildIndexesForVisionPrescription(Indexes, compartments);
end;

end.

