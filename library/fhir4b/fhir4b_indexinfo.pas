unit fhir4b_indexinfo;

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
  fsl_base, fsl_utilities, fsl_stream,
  fhir_common,
  fhir4b_enums, fhir4b_types, fhir4b_resources, fhir4b_constants, fhir_indexing;

Type

  TFHIRIndexBuilderR5 = class (TFHIRIndexBuilder)
  private
    {$IFDEF FHIR_ACCOUNT}
    procedure buildIndexesForAccount(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_ACTIVITYDEFINITION}
    procedure buildIndexesForActivityDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_ADMINISTRABLEPRODUCTDEFINITION}
    procedure buildIndexesForAdministrableProductDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
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
    {$IFDEF FHIR_BIOLOGICALLYDERIVEDPRODUCT}
    procedure buildIndexesForBiologicallyDerivedProduct(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_BODYSTRUCTURE}
    procedure buildIndexesForBodyStructure(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
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
    {$IFDEF FHIR_CATALOGENTRY}
    procedure buildIndexesForCatalogEntry(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_CHARGEITEM}
    procedure buildIndexesForChargeItem(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_CHARGEITEMDEFINITION}
    procedure buildIndexesForChargeItemDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_CITATION}
    procedure buildIndexesForCitation(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
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
    {$IFDEF FHIR_CLINICALUSEDEFINITION}
    procedure buildIndexesForClinicalUseDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
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
    {$IFDEF FHIR_COVERAGEELIGIBILITYREQUEST}
    procedure buildIndexesForCoverageEligibilityRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_COVERAGEELIGIBILITYRESPONSE}
    procedure buildIndexesForCoverageEligibilityResponse(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_DETECTEDISSUE}
    procedure buildIndexesForDetectedIssue(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_DEVICE}
    procedure buildIndexesForDevice(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_DEVICEDEFINITION}
    procedure buildIndexesForDeviceDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
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
    {$IFDEF FHIR_EVENTDEFINITION}
    procedure buildIndexesForEventDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_EVIDENCE}
    procedure buildIndexesForEvidence(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_EVIDENCEREPORT}
    procedure buildIndexesForEvidenceReport(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_EVIDENCEVARIABLE}
    procedure buildIndexesForEvidenceVariable(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_EXAMPLESCENARIO}
    procedure buildIndexesForExampleScenario(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
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
    {$IFDEF FHIR_IMAGINGSTUDY}
    procedure buildIndexesForImagingStudy(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_IMMUNIZATION}
    procedure buildIndexesForImmunization(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_IMMUNIZATIONEVALUATION}
    procedure buildIndexesForImmunizationEvaluation(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_IMMUNIZATIONRECOMMENDATION}
    procedure buildIndexesForImmunizationRecommendation(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_IMPLEMENTATIONGUIDE}
    procedure buildIndexesForImplementationGuide(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_INGREDIENT}
    procedure buildIndexesForIngredient(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_INSURANCEPLAN}
    procedure buildIndexesForInsurancePlan(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_INVOICE}
    procedure buildIndexesForInvoice(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
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
    {$IFDEF FHIR_MANUFACTUREDITEMDEFINITION}
    procedure buildIndexesForManufacturedItemDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
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
    {$IFDEF FHIR_MEDICATIONKNOWLEDGE}
    procedure buildIndexesForMedicationKnowledge(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_MEDICATIONREQUEST}
    procedure buildIndexesForMedicationRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_MEDICATIONSTATEMENT}
    procedure buildIndexesForMedicationStatement(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_MEDICINALPRODUCTDEFINITION}
    procedure buildIndexesForMedicinalProductDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_MESSAGEDEFINITION}
    procedure buildIndexesForMessageDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_MESSAGEHEADER}
    procedure buildIndexesForMessageHeader(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_MOLECULARSEQUENCE}
    procedure buildIndexesForMolecularSequence(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_NAMINGSYSTEM}
    procedure buildIndexesForNamingSystem(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_NUTRITIONORDER}
    procedure buildIndexesForNutritionOrder(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_NUTRITIONPRODUCT}
    procedure buildIndexesForNutritionProduct(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_OBSERVATION}
    procedure buildIndexesForObservation(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_OBSERVATIONDEFINITION}
    procedure buildIndexesForObservationDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
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
    {$IFDEF FHIR_ORGANIZATIONAFFILIATION}
    procedure buildIndexesForOrganizationAffiliation(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_PACKAGEDPRODUCTDEFINITION}
    procedure buildIndexesForPackagedProductDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
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
    {$IFDEF FHIR_PROVENANCE}
    procedure buildIndexesForProvenance(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_QUESTIONNAIRE}
    procedure buildIndexesForQuestionnaire(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_QUESTIONNAIRERESPONSE}
    procedure buildIndexesForQuestionnaireResponse(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_REGULATEDAUTHORIZATION}
    procedure buildIndexesForRegulatedAuthorization(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_RELATEDPERSON}
    procedure buildIndexesForRelatedPerson(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_REQUESTGROUP}
    procedure buildIndexesForRequestGroup(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_RESEARCHDEFINITION}
    procedure buildIndexesForResearchDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_RESEARCHELEMENTDEFINITION}
    procedure buildIndexesForResearchElementDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
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
    {$IFDEF FHIR_SERVICEREQUEST}
    procedure buildIndexesForServiceRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_SLOT}
    procedure buildIndexesForSlot(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_SPECIMEN}
    procedure buildIndexesForSpecimen(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_SPECIMENDEFINITION}
    procedure buildIndexesForSpecimenDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
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
    {$IFDEF FHIR_SUBSCRIPTIONSTATUS}
    procedure buildIndexesForSubscriptionStatus(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_SUBSCRIPTIONTOPIC}
    procedure buildIndexesForSubscriptionTopic(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_SUBSTANCE}
    procedure buildIndexesForSubstance(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_SUBSTANCEDEFINITION}
    procedure buildIndexesForSubstanceDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
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
    {$IFDEF FHIR_TERMINOLOGYCAPABILITIES}
    procedure buildIndexesForTerminologyCapabilities(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
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
    {$IFDEF FHIR_VERIFICATIONRESULT}
    procedure buildIndexesForVerificationResult(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_VISIONPRESCRIPTION}
    procedure buildIndexesForVisionPrescription(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}

  public
    procedure registerIndexes(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList); override;
 end;
 TFHIRIndexBuilderX = TFHIRIndexBuilderR5;

implementation

{$IFDEF FHIR_ACCOUNT}
procedure TFHIRIndexBuilderR5.buildIndexesForAccount(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Account', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('Account', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Device', 'Account', ['subject']);
  compartments.register('Patient', 'Account', ['subject']);
  compartments.register('Practitioner', 'Account', ['subject']);
end;
{$ENDIF FHIR_ACCOUNT}

{$IFDEF FHIR_ACTIVITYDEFINITION}
procedure TFHIRIndexBuilderR5.buildIndexesForActivityDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ActivityDefinition', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('ActivityDefinition', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_ACTIVITYDEFINITION}

{$IFDEF FHIR_ADMINISTRABLEPRODUCTDEFINITION}
procedure TFHIRIndexBuilderR5.buildIndexesForAdministrableProductDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('AdministrableProductDefinition', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('AdministrableProductDefinition', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_ADMINISTRABLEPRODUCTDEFINITION}

{$IFDEF FHIR_ADVERSEEVENT}
procedure TFHIRIndexBuilderR5.buildIndexesForAdverseEvent(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('AdverseEvent', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('AdverseEvent', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Patient', 'AdverseEvent', ['subject']);
  compartments.register('Practitioner', 'AdverseEvent', ['recorder']);
  compartments.register('RelatedPerson', 'AdverseEvent', ['recorder']);
end;
{$ENDIF FHIR_ADVERSEEVENT}

{$IFDEF FHIR_ALLERGYINTOLERANCE}
procedure TFHIRIndexBuilderR5.buildIndexesForAllergyIntolerance(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('AllergyIntolerance', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('AllergyIntolerance', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Patient', 'AllergyIntolerance', ['patient', 'recorder', 'asserter']);
  compartments.register('Practitioner', 'AllergyIntolerance', ['recorder', 'asserter']);
  compartments.register('RelatedPerson', 'AllergyIntolerance', ['asserter']);
end;
{$ENDIF FHIR_ALLERGYINTOLERANCE}

{$IFDEF FHIR_APPOINTMENT}
procedure TFHIRIndexBuilderR5.buildIndexesForAppointment(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Appointment', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('Appointment', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Device', 'Appointment', ['actor']);
  compartments.register('Patient', 'Appointment', ['actor']);
  compartments.register('Practitioner', 'Appointment', ['actor']);
  compartments.register('RelatedPerson', 'Appointment', ['actor']);
end;
{$ENDIF FHIR_APPOINTMENT}

{$IFDEF FHIR_APPOINTMENTRESPONSE}
procedure TFHIRIndexBuilderR5.buildIndexesForAppointmentResponse(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('AppointmentResponse', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('AppointmentResponse', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Device', 'AppointmentResponse', ['actor']);
  compartments.register('Patient', 'AppointmentResponse', ['actor']);
  compartments.register('Practitioner', 'AppointmentResponse', ['actor']);
  compartments.register('RelatedPerson', 'AppointmentResponse', ['actor']);
end;
{$ENDIF FHIR_APPOINTMENTRESPONSE}

{$IFDEF FHIR_AUDITEVENT}
procedure TFHIRIndexBuilderR5.buildIndexesForAuditEvent(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('AuditEvent', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('AuditEvent', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Device', 'AuditEvent', ['agent']);
  compartments.register('Patient', 'AuditEvent', ['patient']);
  compartments.register('Practitioner', 'AuditEvent', ['agent']);
end;
{$ENDIF FHIR_AUDITEVENT}

{$IFDEF FHIR_BASIC}
procedure TFHIRIndexBuilderR5.buildIndexesForBasic(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Basic', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('Basic', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Patient', 'Basic', ['patient', 'author']);
  compartments.register('Practitioner', 'Basic', ['author']);
  compartments.register('RelatedPerson', 'Basic', ['author']);
end;
{$ENDIF FHIR_BASIC}

{$IFDEF FHIR_BINARY}
procedure TFHIRIndexBuilderR5.buildIndexesForBinary(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Binary', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('Binary', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_BINARY}

{$IFDEF FHIR_BIOLOGICALLYDERIVEDPRODUCT}
procedure TFHIRIndexBuilderR5.buildIndexesForBiologicallyDerivedProduct(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('BiologicallyDerivedProduct', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('BiologicallyDerivedProduct', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_BIOLOGICALLYDERIVEDPRODUCT}

{$IFDEF FHIR_BODYSTRUCTURE}
procedure TFHIRIndexBuilderR5.buildIndexesForBodyStructure(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('BodyStructure', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('BodyStructure', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Patient', 'BodyStructure', ['patient']);
end;
{$ENDIF FHIR_BODYSTRUCTURE}

{$IFDEF FHIR_BUNDLE}
procedure TFHIRIndexBuilderR5.buildIndexesForBundle(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Bundle', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('Bundle', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_BUNDLE}

{$IFDEF FHIR_CAPABILITYSTATEMENT}
procedure TFHIRIndexBuilderR5.buildIndexesForCapabilityStatement(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('CapabilityStatement', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('CapabilityStatement', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_CAPABILITYSTATEMENT}

{$IFDEF FHIR_CAREPLAN}
procedure TFHIRIndexBuilderR5.buildIndexesForCarePlan(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('CarePlan', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('CarePlan', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Encounter', 'CarePlan', ['encounter']);
  compartments.register('Patient', 'CarePlan', ['patient', 'performer']);
  compartments.register('Practitioner', 'CarePlan', ['performer']);
  compartments.register('RelatedPerson', 'CarePlan', ['performer']);
end;
{$ENDIF FHIR_CAREPLAN}

{$IFDEF FHIR_CARETEAM}
procedure TFHIRIndexBuilderR5.buildIndexesForCareTeam(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('CareTeam', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('CareTeam', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Encounter', 'CareTeam', ['encounter']);
  compartments.register('Patient', 'CareTeam', ['patient', 'participant']);
  compartments.register('Practitioner', 'CareTeam', ['participant']);
  compartments.register('RelatedPerson', 'CareTeam', ['participant']);
end;
{$ENDIF FHIR_CARETEAM}

{$IFDEF FHIR_CATALOGENTRY}
procedure TFHIRIndexBuilderR5.buildIndexesForCatalogEntry(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('CatalogEntry', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('CatalogEntry', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_CATALOGENTRY}

{$IFDEF FHIR_CHARGEITEM}
procedure TFHIRIndexBuilderR5.buildIndexesForChargeItem(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ChargeItem', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('ChargeItem', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Device', 'ChargeItem', ['enterer', 'performer-actor']);
  compartments.register('Encounter', 'ChargeItem', ['context']);
  compartments.register('Patient', 'ChargeItem', ['subject']);
  compartments.register('Practitioner', 'ChargeItem', ['enterer', 'performer-actor']);
  compartments.register('RelatedPerson', 'ChargeItem', ['enterer', 'performer-actor']);
end;
{$ENDIF FHIR_CHARGEITEM}

{$IFDEF FHIR_CHARGEITEMDEFINITION}
procedure TFHIRIndexBuilderR5.buildIndexesForChargeItemDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ChargeItemDefinition', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('ChargeItemDefinition', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_CHARGEITEMDEFINITION}

{$IFDEF FHIR_CITATION}
procedure TFHIRIndexBuilderR5.buildIndexesForCitation(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Citation', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('Citation', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_CITATION}

{$IFDEF FHIR_CLAIM}
procedure TFHIRIndexBuilderR5.buildIndexesForClaim(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Claim', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('Claim', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Device', 'Claim', ['procedure-udi', 'item-udi', 'detail-udi', 'subdetail-udi']);
  compartments.register('Encounter', 'Claim', ['encounter']);
  compartments.register('Patient', 'Claim', ['patient', 'payee']);
  compartments.register('Practitioner', 'Claim', ['enterer', 'provider', 'payee', 'care-team']);
  compartments.register('RelatedPerson', 'Claim', ['payee']);
end;
{$ENDIF FHIR_CLAIM}

{$IFDEF FHIR_CLAIMRESPONSE}
procedure TFHIRIndexBuilderR5.buildIndexesForClaimResponse(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ClaimResponse', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('ClaimResponse', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Patient', 'ClaimResponse', ['patient']);
  compartments.register('Practitioner', 'ClaimResponse', ['requestor']);
end;
{$ENDIF FHIR_CLAIMRESPONSE}

{$IFDEF FHIR_CLINICALIMPRESSION}
procedure TFHIRIndexBuilderR5.buildIndexesForClinicalImpression(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ClinicalImpression', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('ClinicalImpression', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Encounter', 'ClinicalImpression', ['encounter']);
  compartments.register('Patient', 'ClinicalImpression', ['subject']);
  compartments.register('Practitioner', 'ClinicalImpression', ['assessor']);
end;
{$ENDIF FHIR_CLINICALIMPRESSION}

{$IFDEF FHIR_CLINICALUSEDEFINITION}
procedure TFHIRIndexBuilderR5.buildIndexesForClinicalUseDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ClinicalUseDefinition', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('ClinicalUseDefinition', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_CLINICALUSEDEFINITION}

{$IFDEF FHIR_CODESYSTEM}
procedure TFHIRIndexBuilderR5.buildIndexesForCodeSystem(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('CodeSystem', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('CodeSystem', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_CODESYSTEM}

{$IFDEF FHIR_COMMUNICATION}
procedure TFHIRIndexBuilderR5.buildIndexesForCommunication(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Communication', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('Communication', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Device', 'Communication', ['sender', 'recipient']);
  compartments.register('Encounter', 'Communication', ['encounter']);
  compartments.register('Device', 'Communication', ['sender', 'recipient']);
  compartments.register('Patient', 'Communication', ['subject', 'sender', 'recipient']);
  compartments.register('Practitioner', 'Communication', ['sender', 'recipient']);
  compartments.register('RelatedPerson', 'Communication', ['sender', 'recipient']);
end;
{$ENDIF FHIR_COMMUNICATION}

{$IFDEF FHIR_COMMUNICATIONREQUEST}
procedure TFHIRIndexBuilderR5.buildIndexesForCommunicationRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('CommunicationRequest', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('CommunicationRequest', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Device', 'CommunicationRequest', ['sender', 'recipient']);
  compartments.register('Encounter', 'CommunicationRequest', ['encounter']);
  compartments.register('Device', 'CommunicationRequest', ['sender', 'recipient']);
  compartments.register('Patient', 'CommunicationRequest', ['subject', 'sender', 'recipient', 'requester']);
  compartments.register('Practitioner', 'CommunicationRequest', ['sender', 'recipient', 'requester']);
  compartments.register('RelatedPerson', 'CommunicationRequest', ['sender', 'recipient', 'requester']);
end;
{$ENDIF FHIR_COMMUNICATIONREQUEST}

{$IFDEF FHIR_COMPARTMENTDEFINITION}
procedure TFHIRIndexBuilderR5.buildIndexesForCompartmentDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('CompartmentDefinition', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('CompartmentDefinition', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_COMPARTMENTDEFINITION}

{$IFDEF FHIR_COMPOSITION}
procedure TFHIRIndexBuilderR5.buildIndexesForComposition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Composition', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('Composition', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Device', 'Composition', ['author']);
  compartments.register('Encounter', 'Composition', ['encounter']);
  compartments.register('Patient', 'Composition', ['subject', 'author', 'attester']);
  compartments.register('Practitioner', 'Composition', ['subject', 'author', 'attester']);
  compartments.register('RelatedPerson', 'Composition', ['author']);
end;
{$ENDIF FHIR_COMPOSITION}

{$IFDEF FHIR_CONCEPTMAP}
procedure TFHIRIndexBuilderR5.buildIndexesForConceptMap(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ConceptMap', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('ConceptMap', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_CONCEPTMAP}

{$IFDEF FHIR_CONDITION}
procedure TFHIRIndexBuilderR5.buildIndexesForCondition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Condition', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('Condition', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Encounter', 'Condition', ['encounter']);
  compartments.register('Patient', 'Condition', ['patient', 'asserter']);
  compartments.register('Practitioner', 'Condition', ['asserter']);
  compartments.register('RelatedPerson', 'Condition', ['asserter']);
end;
{$ENDIF FHIR_CONDITION}

{$IFDEF FHIR_CONSENT}
procedure TFHIRIndexBuilderR5.buildIndexesForConsent(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Consent', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('Consent', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Patient', 'Consent', ['patient']);
end;
{$ENDIF FHIR_CONSENT}

{$IFDEF FHIR_CONTRACT}
procedure TFHIRIndexBuilderR5.buildIndexesForContract(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Contract', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('Contract', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_CONTRACT}

{$IFDEF FHIR_COVERAGE}
procedure TFHIRIndexBuilderR5.buildIndexesForCoverage(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Coverage', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('Coverage', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Patient', 'Coverage', ['policy-holder', 'subscriber', 'beneficiary', 'payor']);
  compartments.register('RelatedPerson', 'Coverage', ['policy-holder', 'subscriber', 'payor']);
end;
{$ENDIF FHIR_COVERAGE}

{$IFDEF FHIR_COVERAGEELIGIBILITYREQUEST}
procedure TFHIRIndexBuilderR5.buildIndexesForCoverageEligibilityRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('CoverageEligibilityRequest', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('CoverageEligibilityRequest', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Patient', 'CoverageEligibilityRequest', ['patient']);
  compartments.register('Practitioner', 'CoverageEligibilityRequest', ['enterer', 'provider']);
end;
{$ENDIF FHIR_COVERAGEELIGIBILITYREQUEST}

{$IFDEF FHIR_COVERAGEELIGIBILITYRESPONSE}
procedure TFHIRIndexBuilderR5.buildIndexesForCoverageEligibilityResponse(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('CoverageEligibilityResponse', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('CoverageEligibilityResponse', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Patient', 'CoverageEligibilityResponse', ['patient']);
  compartments.register('Practitioner', 'CoverageEligibilityResponse', ['requestor']);
end;
{$ENDIF FHIR_COVERAGEELIGIBILITYRESPONSE}

{$IFDEF FHIR_DETECTEDISSUE}
procedure TFHIRIndexBuilderR5.buildIndexesForDetectedIssue(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('DetectedIssue', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('DetectedIssue', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Device', 'DetectedIssue', ['author']);
  compartments.register('Patient', 'DetectedIssue', ['patient']);
  compartments.register('Practitioner', 'DetectedIssue', ['author']);
end;
{$ENDIF FHIR_DETECTEDISSUE}

{$IFDEF FHIR_DEVICE}
procedure TFHIRIndexBuilderR5.buildIndexesForDevice(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Device', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('Device', 'din', 'The donation identification number (DIN)', sptTOKEN, [], 'Device.extension(''http://hl7.org/fhir/SearchParameter/device-extensions-Device-din'')', sxpNormal);
  indexes.add('Device', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_DEVICE}

{$IFDEF FHIR_DEVICEDEFINITION}
procedure TFHIRIndexBuilderR5.buildIndexesForDeviceDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('DeviceDefinition', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('DeviceDefinition', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_DEVICEDEFINITION}

{$IFDEF FHIR_DEVICEMETRIC}
procedure TFHIRIndexBuilderR5.buildIndexesForDeviceMetric(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('DeviceMetric', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('DeviceMetric', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_DEVICEMETRIC}

{$IFDEF FHIR_DEVICEREQUEST}
procedure TFHIRIndexBuilderR5.buildIndexesForDeviceRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('DeviceRequest', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('DeviceRequest', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Device', 'DeviceRequest', ['device', 'subject', 'requester', 'performer']);
  compartments.register('Encounter', 'DeviceRequest', ['encounter']);
  compartments.register('Patient', 'DeviceRequest', ['subject', 'performer']);
  compartments.register('Practitioner', 'DeviceRequest', ['requester', 'performer']);
end;
{$ENDIF FHIR_DEVICEREQUEST}

{$IFDEF FHIR_DEVICEUSESTATEMENT}
procedure TFHIRIndexBuilderR5.buildIndexesForDeviceUseStatement(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('DeviceUseStatement', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('DeviceUseStatement', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Device', 'DeviceUseStatement', ['device']);
  compartments.register('Patient', 'DeviceUseStatement', ['subject']);
end;
{$ENDIF FHIR_DEVICEUSESTATEMENT}

{$IFDEF FHIR_DIAGNOSTICREPORT}
procedure TFHIRIndexBuilderR5.buildIndexesForDiagnosticReport(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('DiagnosticReport', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('DiagnosticReport', 'assessed-condition', 'Condition assessed by genetic test', sptREFERENCE, [], 'DiagnosticReport.extension(''http://hl7.org/fhir/StructureDefinition/DiagnosticReport-geneticsAssessedCondition'')', sxpNormal);
  indexes.add('DiagnosticReport', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Device', 'DiagnosticReport', ['subject']);
  compartments.register('Encounter', 'DiagnosticReport', ['encounter']);
  compartments.register('Patient', 'DiagnosticReport', ['subject']);
  compartments.register('Practitioner', 'DiagnosticReport', ['performer']);
end;
{$ENDIF FHIR_DIAGNOSTICREPORT}

{$IFDEF FHIR_DOCUMENTMANIFEST}
procedure TFHIRIndexBuilderR5.buildIndexesForDocumentManifest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('DocumentManifest', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('DocumentManifest', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Device', 'DocumentManifest', ['subject', 'author']);
  compartments.register('Encounter', 'DocumentManifest', ['related-ref']);
  compartments.register('Patient', 'DocumentManifest', ['subject', 'author', 'recipient']);
  compartments.register('Practitioner', 'DocumentManifest', ['subject', 'author', 'recipient']);
  compartments.register('RelatedPerson', 'DocumentManifest', ['author', 'recipient']);
end;
{$ENDIF FHIR_DOCUMENTMANIFEST}

{$IFDEF FHIR_DOCUMENTREFERENCE}
procedure TFHIRIndexBuilderR5.buildIndexesForDocumentReference(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('DocumentReference', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('DocumentReference', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Device', 'DocumentReference', ['subject', 'author']);
  compartments.register('Encounter', 'DocumentReference', ['encounter']);
  compartments.register('Patient', 'DocumentReference', ['subject', 'author']);
  compartments.register('Practitioner', 'DocumentReference', ['subject', 'author', 'authenticator']);
  compartments.register('RelatedPerson', 'DocumentReference', ['author']);
end;
{$ENDIF FHIR_DOCUMENTREFERENCE}

{$IFDEF FHIR_ENCOUNTER}
procedure TFHIRIndexBuilderR5.buildIndexesForEncounter(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Encounter', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('Encounter', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Encounter', 'Encounter', ['{def}']);
  compartments.register('Patient', 'Encounter', ['patient']);
  compartments.register('Practitioner', 'Encounter', ['practitioner', 'participant']);
  compartments.register('RelatedPerson', 'Encounter', ['participant']);
end;
{$ENDIF FHIR_ENCOUNTER}

{$IFDEF FHIR_ENDPOINT}
procedure TFHIRIndexBuilderR5.buildIndexesForEndpoint(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Endpoint', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('Endpoint', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_ENDPOINT}

{$IFDEF FHIR_ENROLLMENTREQUEST}
procedure TFHIRIndexBuilderR5.buildIndexesForEnrollmentRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('EnrollmentRequest', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('EnrollmentRequest', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Patient', 'EnrollmentRequest', ['subject']);
end;
{$ENDIF FHIR_ENROLLMENTREQUEST}

{$IFDEF FHIR_ENROLLMENTRESPONSE}
procedure TFHIRIndexBuilderR5.buildIndexesForEnrollmentResponse(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('EnrollmentResponse', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('EnrollmentResponse', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_ENROLLMENTRESPONSE}

{$IFDEF FHIR_EPISODEOFCARE}
procedure TFHIRIndexBuilderR5.buildIndexesForEpisodeOfCare(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('EpisodeOfCare', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('EpisodeOfCare', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Patient', 'EpisodeOfCare', ['patient']);
  compartments.register('Practitioner', 'EpisodeOfCare', ['care-manager']);
end;
{$ENDIF FHIR_EPISODEOFCARE}

{$IFDEF FHIR_EVENTDEFINITION}
procedure TFHIRIndexBuilderR5.buildIndexesForEventDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('EventDefinition', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('EventDefinition', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_EVENTDEFINITION}

{$IFDEF FHIR_EVIDENCE}
procedure TFHIRIndexBuilderR5.buildIndexesForEvidence(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Evidence', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('Evidence', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_EVIDENCE}

{$IFDEF FHIR_EVIDENCEREPORT}
procedure TFHIRIndexBuilderR5.buildIndexesForEvidenceReport(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('EvidenceReport', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('EvidenceReport', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_EVIDENCEREPORT}

{$IFDEF FHIR_EVIDENCEVARIABLE}
procedure TFHIRIndexBuilderR5.buildIndexesForEvidenceVariable(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('EvidenceVariable', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('EvidenceVariable', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_EVIDENCEVARIABLE}

{$IFDEF FHIR_EXAMPLESCENARIO}
procedure TFHIRIndexBuilderR5.buildIndexesForExampleScenario(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ExampleScenario', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('ExampleScenario', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_EXAMPLESCENARIO}

{$IFDEF FHIR_EXPLANATIONOFBENEFIT}
procedure TFHIRIndexBuilderR5.buildIndexesForExplanationOfBenefit(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ExplanationOfBenefit', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('ExplanationOfBenefit', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Device', 'ExplanationOfBenefit', ['procedure-udi', 'item-udi', 'detail-udi', 'subdetail-udi']);
  compartments.register('Encounter', 'ExplanationOfBenefit', ['encounter']);
  compartments.register('Patient', 'ExplanationOfBenefit', ['patient', 'payee']);
  compartments.register('Practitioner', 'ExplanationOfBenefit', ['enterer', 'provider', 'payee', 'care-team']);
  compartments.register('RelatedPerson', 'ExplanationOfBenefit', ['payee']);
end;
{$ENDIF FHIR_EXPLANATIONOFBENEFIT}

{$IFDEF FHIR_FAMILYMEMBERHISTORY}
procedure TFHIRIndexBuilderR5.buildIndexesForFamilyMemberHistory(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('FamilyMemberHistory', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('FamilyMemberHistory', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Patient', 'FamilyMemberHistory', ['patient']);
end;
{$ENDIF FHIR_FAMILYMEMBERHISTORY}

{$IFDEF FHIR_FLAG}
procedure TFHIRIndexBuilderR5.buildIndexesForFlag(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Flag', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('Flag', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Device', 'Flag', ['author']);
  compartments.register('Patient', 'Flag', ['patient']);
  compartments.register('Practitioner', 'Flag', ['author']);
end;
{$ENDIF FHIR_FLAG}

{$IFDEF FHIR_GOAL}
procedure TFHIRIndexBuilderR5.buildIndexesForGoal(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Goal', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('Goal', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Patient', 'Goal', ['patient']);
end;
{$ENDIF FHIR_GOAL}

{$IFDEF FHIR_GRAPHDEFINITION}
procedure TFHIRIndexBuilderR5.buildIndexesForGraphDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('GraphDefinition', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('GraphDefinition', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_GRAPHDEFINITION}

{$IFDEF FHIR_GROUP}
procedure TFHIRIndexBuilderR5.buildIndexesForGroup(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Group', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('Group', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Device', 'Group', ['member']);
  compartments.register('Patient', 'Group', ['member']);
  compartments.register('Practitioner', 'Group', ['member']);
end;
{$ENDIF FHIR_GROUP}

{$IFDEF FHIR_GUIDANCERESPONSE}
procedure TFHIRIndexBuilderR5.buildIndexesForGuidanceResponse(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('GuidanceResponse', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('GuidanceResponse', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_GUIDANCERESPONSE}

{$IFDEF FHIR_HEALTHCARESERVICE}
procedure TFHIRIndexBuilderR5.buildIndexesForHealthcareService(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('HealthcareService', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('HealthcareService', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_HEALTHCARESERVICE}

{$IFDEF FHIR_IMAGINGSTUDY}
procedure TFHIRIndexBuilderR5.buildIndexesForImagingStudy(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ImagingStudy', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('ImagingStudy', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Patient', 'ImagingStudy', ['patient']);
end;
{$ENDIF FHIR_IMAGINGSTUDY}

{$IFDEF FHIR_IMMUNIZATION}
procedure TFHIRIndexBuilderR5.buildIndexesForImmunization(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Immunization', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('Immunization', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Patient', 'Immunization', ['patient']);
  compartments.register('Practitioner', 'Immunization', ['performer']);
end;
{$ENDIF FHIR_IMMUNIZATION}

{$IFDEF FHIR_IMMUNIZATIONEVALUATION}
procedure TFHIRIndexBuilderR5.buildIndexesForImmunizationEvaluation(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ImmunizationEvaluation', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('ImmunizationEvaluation', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Patient', 'ImmunizationEvaluation', ['patient']);
end;
{$ENDIF FHIR_IMMUNIZATIONEVALUATION}

{$IFDEF FHIR_IMMUNIZATIONRECOMMENDATION}
procedure TFHIRIndexBuilderR5.buildIndexesForImmunizationRecommendation(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ImmunizationRecommendation', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('ImmunizationRecommendation', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Patient', 'ImmunizationRecommendation', ['patient']);
end;
{$ENDIF FHIR_IMMUNIZATIONRECOMMENDATION}

{$IFDEF FHIR_IMPLEMENTATIONGUIDE}
procedure TFHIRIndexBuilderR5.buildIndexesForImplementationGuide(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ImplementationGuide', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('ImplementationGuide', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_IMPLEMENTATIONGUIDE}

{$IFDEF FHIR_INGREDIENT}
procedure TFHIRIndexBuilderR5.buildIndexesForIngredient(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Ingredient', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('Ingredient', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_INGREDIENT}

{$IFDEF FHIR_INSURANCEPLAN}
procedure TFHIRIndexBuilderR5.buildIndexesForInsurancePlan(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('InsurancePlan', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('InsurancePlan', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_INSURANCEPLAN}

{$IFDEF FHIR_INVOICE}
procedure TFHIRIndexBuilderR5.buildIndexesForInvoice(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Invoice', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('Invoice', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Device', 'Invoice', ['participant']);
  compartments.register('Patient', 'Invoice', ['subject', 'patient', 'recipient']);
  compartments.register('Practitioner', 'Invoice', ['participant']);
  compartments.register('RelatedPerson', 'Invoice', ['recipient']);
end;
{$ENDIF FHIR_INVOICE}

{$IFDEF FHIR_LIBRARY}
procedure TFHIRIndexBuilderR5.buildIndexesForLibrary(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Library', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('Library', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_LIBRARY}

{$IFDEF FHIR_LINKAGE}
procedure TFHIRIndexBuilderR5.buildIndexesForLinkage(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Linkage', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('Linkage', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Practitioner', 'Linkage', ['author']);
end;
{$ENDIF FHIR_LINKAGE}

{$IFDEF FHIR_LIST}
procedure TFHIRIndexBuilderR5.buildIndexesForList(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('List', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('List', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Device', 'List', ['subject', 'source']);
  compartments.register('Patient', 'List', ['subject', 'source']);
  compartments.register('Practitioner', 'List', ['source']);
end;
{$ENDIF FHIR_LIST}

{$IFDEF FHIR_LOCATION}
procedure TFHIRIndexBuilderR5.buildIndexesForLocation(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Location', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('Location', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_LOCATION}

{$IFDEF FHIR_MANUFACTUREDITEMDEFINITION}
procedure TFHIRIndexBuilderR5.buildIndexesForManufacturedItemDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ManufacturedItemDefinition', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('ManufacturedItemDefinition', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_MANUFACTUREDITEMDEFINITION}

{$IFDEF FHIR_MEASURE}
procedure TFHIRIndexBuilderR5.buildIndexesForMeasure(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Measure', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('Measure', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_MEASURE}

{$IFDEF FHIR_MEASUREREPORT}
procedure TFHIRIndexBuilderR5.buildIndexesForMeasureReport(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('MeasureReport', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('MeasureReport', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Patient', 'MeasureReport', ['patient']);
end;
{$ENDIF FHIR_MEASUREREPORT}

{$IFDEF FHIR_MEDIA}
procedure TFHIRIndexBuilderR5.buildIndexesForMedia(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Media', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('Media', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Device', 'Media', ['subject']);
  compartments.register('Encounter', 'Media', ['encounter']);
  compartments.register('Patient', 'Media', ['subject']);
  compartments.register('Practitioner', 'Media', ['subject', 'operator']);
end;
{$ENDIF FHIR_MEDIA}

{$IFDEF FHIR_MEDICATION}
procedure TFHIRIndexBuilderR5.buildIndexesForMedication(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Medication', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('Medication', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_MEDICATION}

{$IFDEF FHIR_MEDICATIONADMINISTRATION}
procedure TFHIRIndexBuilderR5.buildIndexesForMedicationAdministration(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('MedicationAdministration', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('MedicationAdministration', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Device', 'MedicationAdministration', ['device']);
  compartments.register('Encounter', 'MedicationAdministration', ['context']);
  compartments.register('Patient', 'MedicationAdministration', ['patient', 'performer', 'subject']);
  compartments.register('Practitioner', 'MedicationAdministration', ['performer']);
  compartments.register('RelatedPerson', 'MedicationAdministration', ['performer']);
end;
{$ENDIF FHIR_MEDICATIONADMINISTRATION}

{$IFDEF FHIR_MEDICATIONDISPENSE}
procedure TFHIRIndexBuilderR5.buildIndexesForMedicationDispense(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('MedicationDispense', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('MedicationDispense', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Patient', 'MedicationDispense', ['subject', 'patient', 'receiver']);
  compartments.register('Practitioner', 'MedicationDispense', ['performer', 'receiver']);
end;
{$ENDIF FHIR_MEDICATIONDISPENSE}

{$IFDEF FHIR_MEDICATIONKNOWLEDGE}
procedure TFHIRIndexBuilderR5.buildIndexesForMedicationKnowledge(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('MedicationKnowledge', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('MedicationKnowledge', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_MEDICATIONKNOWLEDGE}

{$IFDEF FHIR_MEDICATIONREQUEST}
procedure TFHIRIndexBuilderR5.buildIndexesForMedicationRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('MedicationRequest', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('MedicationRequest', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Encounter', 'MedicationRequest', ['encounter']);
  compartments.register('Patient', 'MedicationRequest', ['subject']);
  compartments.register('Practitioner', 'MedicationRequest', ['requester']);
end;
{$ENDIF FHIR_MEDICATIONREQUEST}

{$IFDEF FHIR_MEDICATIONSTATEMENT}
procedure TFHIRIndexBuilderR5.buildIndexesForMedicationStatement(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('MedicationStatement', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('MedicationStatement', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Patient', 'MedicationStatement', ['subject']);
  compartments.register('Practitioner', 'MedicationStatement', ['source']);
  compartments.register('RelatedPerson', 'MedicationStatement', ['source']);
end;
{$ENDIF FHIR_MEDICATIONSTATEMENT}

{$IFDEF FHIR_MEDICINALPRODUCTDEFINITION}
procedure TFHIRIndexBuilderR5.buildIndexesForMedicinalProductDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('MedicinalProductDefinition', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('MedicinalProductDefinition', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_MEDICINALPRODUCTDEFINITION}

{$IFDEF FHIR_MESSAGEDEFINITION}
procedure TFHIRIndexBuilderR5.buildIndexesForMessageDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('MessageDefinition', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('MessageDefinition', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_MESSAGEDEFINITION}

{$IFDEF FHIR_MESSAGEHEADER}
procedure TFHIRIndexBuilderR5.buildIndexesForMessageHeader(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('MessageHeader', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('MessageHeader', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Device', 'MessageHeader', ['target']);
  compartments.register('Practitioner', 'MessageHeader', ['receiver', 'author', 'responsible', 'enterer']);
end;
{$ENDIF FHIR_MESSAGEHEADER}

{$IFDEF FHIR_MOLECULARSEQUENCE}
procedure TFHIRIndexBuilderR5.buildIndexesForMolecularSequence(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('MolecularSequence', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('MolecularSequence', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Patient', 'MolecularSequence', ['patient']);
end;
{$ENDIF FHIR_MOLECULARSEQUENCE}

{$IFDEF FHIR_NAMINGSYSTEM}
procedure TFHIRIndexBuilderR5.buildIndexesForNamingSystem(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('NamingSystem', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('NamingSystem', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_NAMINGSYSTEM}

{$IFDEF FHIR_NUTRITIONORDER}
procedure TFHIRIndexBuilderR5.buildIndexesForNutritionOrder(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('NutritionOrder', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('NutritionOrder', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Encounter', 'NutritionOrder', ['encounter']);
  compartments.register('Patient', 'NutritionOrder', ['patient']);
  compartments.register('Practitioner', 'NutritionOrder', ['provider']);
end;
{$ENDIF FHIR_NUTRITIONORDER}

{$IFDEF FHIR_NUTRITIONPRODUCT}
procedure TFHIRIndexBuilderR5.buildIndexesForNutritionProduct(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('NutritionProduct', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('NutritionProduct', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_NUTRITIONPRODUCT}

{$IFDEF FHIR_OBSERVATION}
procedure TFHIRIndexBuilderR5.buildIndexesForObservation(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Observation', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('Observation', 'amino-acid-change', 'HGVS Protein Change', sptSTRING, [], 'Observation.extension(''http://hl7.org/fhir/StructureDefinition/observation-geneticsAminoAcidChangeName'')', sxpNormal);
  indexes.add('Observation', 'dna-variant', 'HGVS DNA variant', sptSTRING, [], 'Observation.extension(''http://hl7.org/fhir/StructureDefinition/observation-geneticsDnaVariant'')', sxpNormal);
  indexes.add('Observation', 'gene-amino-acid-change', 'HGNC gene symbol and HGVS Protein change', sptSTRING, [], 'Observation.extension(''http://hl7.org/fhir/StructureDefinition/observation-geneticsAminoAcidChangeName'')', sxpNormal);
  indexes.add('Observation', 'gene-dnavariant', 'HGNC gene symbol and HGVS DNA Variant', sptSTRING, [], 'Observation.extension(''http://hl7.org/fhir/StructureDefinition/observation-geneticsDnaVariant'')', sxpNormal);
  indexes.add('Observation', 'gene-identifier', 'HGNC gene symbol and identifier', sptTOKEN, [], 'Observation.extension(''http://hl7.org/fhir/StructureDefinition/observation-geneticsGene'')', sxpNormal);
  indexes.add('Observation', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Device', 'Observation', ['subject', 'device']);
  compartments.register('Encounter', 'Observation', ['encounter']);
  compartments.register('Patient', 'Observation', ['subject', 'performer']);
  compartments.register('Practitioner', 'Observation', ['performer']);
  compartments.register('RelatedPerson', 'Observation', ['performer']);
end;
{$ENDIF FHIR_OBSERVATION}

{$IFDEF FHIR_OBSERVATIONDEFINITION}
procedure TFHIRIndexBuilderR5.buildIndexesForObservationDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ObservationDefinition', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('ObservationDefinition', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_OBSERVATIONDEFINITION}

{$IFDEF FHIR_OPERATIONDEFINITION}
procedure TFHIRIndexBuilderR5.buildIndexesForOperationDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('OperationDefinition', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('OperationDefinition', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_OPERATIONDEFINITION}

{$IFDEF FHIR_OPERATIONOUTCOME}
procedure TFHIRIndexBuilderR5.buildIndexesForOperationOutcome(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('OperationOutcome', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('OperationOutcome', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_OPERATIONOUTCOME}

{$IFDEF FHIR_ORGANIZATION}
procedure TFHIRIndexBuilderR5.buildIndexesForOrganization(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Organization', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('Organization', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_ORGANIZATION}

{$IFDEF FHIR_ORGANIZATIONAFFILIATION}
procedure TFHIRIndexBuilderR5.buildIndexesForOrganizationAffiliation(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('OrganizationAffiliation', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('OrganizationAffiliation', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_ORGANIZATIONAFFILIATION}

{$IFDEF FHIR_PACKAGEDPRODUCTDEFINITION}
procedure TFHIRIndexBuilderR5.buildIndexesForPackagedProductDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('PackagedProductDefinition', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('PackagedProductDefinition', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_PACKAGEDPRODUCTDEFINITION}

{$IFDEF FHIR_PARAMETERS}
procedure TFHIRIndexBuilderR5.buildIndexesForParameters(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Parameters', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('Parameters', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_PARAMETERS}

{$IFDEF FHIR_PATIENT}
procedure TFHIRIndexBuilderR5.buildIndexesForPatient(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Patient', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('Patient', 'age', 'Searches for patients based on age as calculated based on current date and date of birth.  Deceased patients are excluded from the search.', sptNUMBER, [], '', sxpNormal);
  indexes.add('Patient', 'birthOrderBoolean', 'Search based on whether a patient was part of a multiple birth or not.', sptTOKEN, [], '', sxpNormal);
  indexes.add('Patient', 'mothersMaidenName', 'Search based on patient''s mother''s maiden name', sptSTRING, [], 'Patient.extension(''http://hl7.org/fhir/StructureDefinition/patient-mothersMaidenName'')', sxpNormal);
  indexes.add('Patient', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Patient', 'Patient', ['link']);
  compartments.register('Practitioner', 'Patient', ['general-practitioner']);
  compartments.register('RelatedPerson', 'Patient', ['link']);
end;
{$ENDIF FHIR_PATIENT}

{$IFDEF FHIR_PAYMENTNOTICE}
procedure TFHIRIndexBuilderR5.buildIndexesForPaymentNotice(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('PaymentNotice', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('PaymentNotice', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Practitioner', 'PaymentNotice', ['provider']);
end;
{$ENDIF FHIR_PAYMENTNOTICE}

{$IFDEF FHIR_PAYMENTRECONCILIATION}
procedure TFHIRIndexBuilderR5.buildIndexesForPaymentReconciliation(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('PaymentReconciliation', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('PaymentReconciliation', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Practitioner', 'PaymentReconciliation', ['requestor']);
end;
{$ENDIF FHIR_PAYMENTRECONCILIATION}

{$IFDEF FHIR_PERSON}
procedure TFHIRIndexBuilderR5.buildIndexesForPerson(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Person', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('Person', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Patient', 'Person', ['patient']);
  compartments.register('Practitioner', 'Person', ['practitioner']);
  compartments.register('RelatedPerson', 'Person', ['link']);
end;
{$ENDIF FHIR_PERSON}

{$IFDEF FHIR_PLANDEFINITION}
procedure TFHIRIndexBuilderR5.buildIndexesForPlanDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('PlanDefinition', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('PlanDefinition', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_PLANDEFINITION}

{$IFDEF FHIR_PRACTITIONER}
procedure TFHIRIndexBuilderR5.buildIndexesForPractitioner(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Practitioner', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('Practitioner', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Practitioner', 'Practitioner', ['{def}']);
end;
{$ENDIF FHIR_PRACTITIONER}

{$IFDEF FHIR_PRACTITIONERROLE}
procedure TFHIRIndexBuilderR5.buildIndexesForPractitionerRole(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('PractitionerRole', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('PractitionerRole', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Practitioner', 'PractitionerRole', ['practitioner']);
end;
{$ENDIF FHIR_PRACTITIONERROLE}

{$IFDEF FHIR_PROCEDURE}
procedure TFHIRIndexBuilderR5.buildIndexesForProcedure(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Procedure', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('Procedure', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Encounter', 'Procedure', ['encounter']);
  compartments.register('Patient', 'Procedure', ['patient', 'performer']);
  compartments.register('Practitioner', 'Procedure', ['performer']);
  compartments.register('RelatedPerson', 'Procedure', ['performer']);
end;
{$ENDIF FHIR_PROCEDURE}

{$IFDEF FHIR_PROVENANCE}
procedure TFHIRIndexBuilderR5.buildIndexesForProvenance(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Provenance', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('Provenance', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Device', 'Provenance', ['agent']);
  compartments.register('Patient', 'Provenance', ['patient']);
  compartments.register('Practitioner', 'Provenance', ['agent']);
  compartments.register('RelatedPerson', 'Provenance', ['agent']);
end;
{$ENDIF FHIR_PROVENANCE}

{$IFDEF FHIR_QUESTIONNAIRE}
procedure TFHIRIndexBuilderR5.buildIndexesForQuestionnaire(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Questionnaire', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('Questionnaire', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_QUESTIONNAIRE}

{$IFDEF FHIR_QUESTIONNAIRERESPONSE}
procedure TFHIRIndexBuilderR5.buildIndexesForQuestionnaireResponse(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('QuestionnaireResponse', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('QuestionnaireResponse', 'item-subject', 'Allows searching for QuestionnaireResponses by item value where the item has isSubject=true', sptREFERENCE, [], 'QuestionnaireResponse.item.where(hasExtension(''http://hl7.org/fhir/StructureDefinition/questionnaireresponse-isSubject'')).answer.value.ofType(Reference)', sxpNormal);
  indexes.add('QuestionnaireResponse', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Device', 'QuestionnaireResponse', ['author']);
  compartments.register('Encounter', 'QuestionnaireResponse', ['encounter']);
  compartments.register('Patient', 'QuestionnaireResponse', ['subject', 'author']);
  compartments.register('Practitioner', 'QuestionnaireResponse', ['author', 'source']);
  compartments.register('RelatedPerson', 'QuestionnaireResponse', ['author', 'source']);
end;
{$ENDIF FHIR_QUESTIONNAIRERESPONSE}

{$IFDEF FHIR_REGULATEDAUTHORIZATION}
procedure TFHIRIndexBuilderR5.buildIndexesForRegulatedAuthorization(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('RegulatedAuthorization', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('RegulatedAuthorization', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_REGULATEDAUTHORIZATION}

{$IFDEF FHIR_RELATEDPERSON}
procedure TFHIRIndexBuilderR5.buildIndexesForRelatedPerson(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('RelatedPerson', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('RelatedPerson', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Patient', 'RelatedPerson', ['patient']);
  compartments.register('RelatedPerson', 'RelatedPerson', ['{def}']);
end;
{$ENDIF FHIR_RELATEDPERSON}

{$IFDEF FHIR_REQUESTGROUP}
procedure TFHIRIndexBuilderR5.buildIndexesForRequestGroup(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('RequestGroup', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('RequestGroup', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Device', 'RequestGroup', ['author']);
  compartments.register('Encounter', 'RequestGroup', ['encounter']);
  compartments.register('Patient', 'RequestGroup', ['subject', 'participant']);
  compartments.register('Practitioner', 'RequestGroup', ['participant', 'author']);
  compartments.register('RelatedPerson', 'RequestGroup', ['participant']);
end;
{$ENDIF FHIR_REQUESTGROUP}

{$IFDEF FHIR_RESEARCHDEFINITION}
procedure TFHIRIndexBuilderR5.buildIndexesForResearchDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ResearchDefinition', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('ResearchDefinition', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_RESEARCHDEFINITION}

{$IFDEF FHIR_RESEARCHELEMENTDEFINITION}
procedure TFHIRIndexBuilderR5.buildIndexesForResearchElementDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ResearchElementDefinition', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('ResearchElementDefinition', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_RESEARCHELEMENTDEFINITION}

{$IFDEF FHIR_RESEARCHSTUDY}
procedure TFHIRIndexBuilderR5.buildIndexesForResearchStudy(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ResearchStudy', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('ResearchStudy', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Practitioner', 'ResearchStudy', ['principalinvestigator']);
end;
{$ENDIF FHIR_RESEARCHSTUDY}

{$IFDEF FHIR_RESEARCHSUBJECT}
procedure TFHIRIndexBuilderR5.buildIndexesForResearchSubject(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ResearchSubject', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('ResearchSubject', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Patient', 'ResearchSubject', ['individual']);
end;
{$ENDIF FHIR_RESEARCHSUBJECT}

{$IFDEF FHIR_RISKASSESSMENT}
procedure TFHIRIndexBuilderR5.buildIndexesForRiskAssessment(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('RiskAssessment', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('RiskAssessment', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Device', 'RiskAssessment', ['performer']);
  compartments.register('Patient', 'RiskAssessment', ['subject']);
  compartments.register('Practitioner', 'RiskAssessment', ['performer']);
end;
{$ENDIF FHIR_RISKASSESSMENT}

{$IFDEF FHIR_SCHEDULE}
procedure TFHIRIndexBuilderR5.buildIndexesForSchedule(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Schedule', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('Schedule', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Device', 'Schedule', ['actor']);
  compartments.register('Patient', 'Schedule', ['actor']);
  compartments.register('Practitioner', 'Schedule', ['actor']);
  compartments.register('RelatedPerson', 'Schedule', ['actor']);
end;
{$ENDIF FHIR_SCHEDULE}

{$IFDEF FHIR_SEARCHPARAMETER}
procedure TFHIRIndexBuilderR5.buildIndexesForSearchParameter(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('SearchParameter', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('SearchParameter', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_SEARCHPARAMETER}

{$IFDEF FHIR_SERVICEREQUEST}
procedure TFHIRIndexBuilderR5.buildIndexesForServiceRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ServiceRequest', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('ServiceRequest', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Device', 'ServiceRequest', ['performer', 'requester']);
  compartments.register('Encounter', 'ServiceRequest', ['encounter']);
  compartments.register('Patient', 'ServiceRequest', ['subject', 'performer']);
  compartments.register('Practitioner', 'ServiceRequest', ['performer', 'requester']);
  compartments.register('RelatedPerson', 'ServiceRequest', ['performer']);
end;
{$ENDIF FHIR_SERVICEREQUEST}

{$IFDEF FHIR_SLOT}
procedure TFHIRIndexBuilderR5.buildIndexesForSlot(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Slot', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('Slot', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_SLOT}

{$IFDEF FHIR_SPECIMEN}
procedure TFHIRIndexBuilderR5.buildIndexesForSpecimen(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Specimen', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('Specimen', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Device', 'Specimen', ['subject']);
  compartments.register('Patient', 'Specimen', ['subject']);
  compartments.register('Practitioner', 'Specimen', ['collector']);
end;
{$ENDIF FHIR_SPECIMEN}

{$IFDEF FHIR_SPECIMENDEFINITION}
procedure TFHIRIndexBuilderR5.buildIndexesForSpecimenDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('SpecimenDefinition', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('SpecimenDefinition', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_SPECIMENDEFINITION}

{$IFDEF FHIR_STRUCTUREDEFINITION}
procedure TFHIRIndexBuilderR5.buildIndexesForStructureDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('StructureDefinition', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('StructureDefinition', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_STRUCTUREDEFINITION}

{$IFDEF FHIR_STRUCTUREMAP}
procedure TFHIRIndexBuilderR5.buildIndexesForStructureMap(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('StructureMap', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('StructureMap', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_STRUCTUREMAP}

{$IFDEF FHIR_SUBSCRIPTION}
procedure TFHIRIndexBuilderR5.buildIndexesForSubscription(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Subscription', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('Subscription', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_SUBSCRIPTION}

{$IFDEF FHIR_SUBSCRIPTIONSTATUS}
procedure TFHIRIndexBuilderR5.buildIndexesForSubscriptionStatus(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('SubscriptionStatus', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('SubscriptionStatus', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_SUBSCRIPTIONSTATUS}

{$IFDEF FHIR_SUBSCRIPTIONTOPIC}
procedure TFHIRIndexBuilderR5.buildIndexesForSubscriptionTopic(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('SubscriptionTopic', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('SubscriptionTopic', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_SUBSCRIPTIONTOPIC}

{$IFDEF FHIR_SUBSTANCE}
procedure TFHIRIndexBuilderR5.buildIndexesForSubstance(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Substance', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('Substance', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_SUBSTANCE}

{$IFDEF FHIR_SUBSTANCEDEFINITION}
procedure TFHIRIndexBuilderR5.buildIndexesForSubstanceDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('SubstanceDefinition', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('SubstanceDefinition', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_SUBSTANCEDEFINITION}

{$IFDEF FHIR_SUPPLYDELIVERY}
procedure TFHIRIndexBuilderR5.buildIndexesForSupplyDelivery(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('SupplyDelivery', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('SupplyDelivery', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Patient', 'SupplyDelivery', ['patient']);
  compartments.register('Practitioner', 'SupplyDelivery', ['supplier', 'receiver']);
end;
{$ENDIF FHIR_SUPPLYDELIVERY}

{$IFDEF FHIR_SUPPLYREQUEST}
procedure TFHIRIndexBuilderR5.buildIndexesForSupplyRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('SupplyRequest', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('SupplyRequest', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Device', 'SupplyRequest', ['requester']);
  compartments.register('Patient', 'SupplyRequest', ['subject']);
  compartments.register('Practitioner', 'SupplyRequest', ['requester']);
  compartments.register('RelatedPerson', 'SupplyRequest', ['requester']);
end;
{$ENDIF FHIR_SUPPLYREQUEST}

{$IFDEF FHIR_TASK}
procedure TFHIRIndexBuilderR5.buildIndexesForTask(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Task', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('Task', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_TASK}

{$IFDEF FHIR_TERMINOLOGYCAPABILITIES}
procedure TFHIRIndexBuilderR5.buildIndexesForTerminologyCapabilities(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('TerminologyCapabilities', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('TerminologyCapabilities', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_TERMINOLOGYCAPABILITIES}

{$IFDEF FHIR_TESTREPORT}
procedure TFHIRIndexBuilderR5.buildIndexesForTestReport(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('TestReport', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('TestReport', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_TESTREPORT}

{$IFDEF FHIR_TESTSCRIPT}
procedure TFHIRIndexBuilderR5.buildIndexesForTestScript(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('TestScript', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('TestScript', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_TESTSCRIPT}

{$IFDEF FHIR_VALUESET}
procedure TFHIRIndexBuilderR5.buildIndexesForValueSet(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ValueSet', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('ValueSet', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_VALUESET}

{$IFDEF FHIR_VERIFICATIONRESULT}
procedure TFHIRIndexBuilderR5.buildIndexesForVerificationResult(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('VerificationResult', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('VerificationResult', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_VERIFICATIONRESULT}

{$IFDEF FHIR_VISIONPRESCRIPTION}
procedure TFHIRIndexBuilderR5.buildIndexesForVisionPrescription(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('VisionPrescription', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNull);
  indexes.add('VisionPrescription', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Encounter', 'VisionPrescription', ['encounter']);
  compartments.register('Patient', 'VisionPrescription', ['patient']);
  compartments.register('Practitioner', 'VisionPrescription', ['prescriber']);
end;
{$ENDIF FHIR_VISIONPRESCRIPTION}



procedure TFHIRIndexBuilderR5.registerIndexes(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  {$IFDEF FHIR_ACCOUNT}
  buildIndexesForAccount(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_ACTIVITYDEFINITION}
  buildIndexesForActivityDefinition(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_ADMINISTRABLEPRODUCTDEFINITION}
  buildIndexesForAdministrableProductDefinition(Indexes, compartments);
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
  {$IFDEF FHIR_BIOLOGICALLYDERIVEDPRODUCT}
  buildIndexesForBiologicallyDerivedProduct(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_BODYSTRUCTURE}
  buildIndexesForBodyStructure(Indexes, compartments);
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
  {$IFDEF FHIR_CATALOGENTRY}
  buildIndexesForCatalogEntry(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_CHARGEITEM}
  buildIndexesForChargeItem(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_CHARGEITEMDEFINITION}
  buildIndexesForChargeItemDefinition(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_CITATION}
  buildIndexesForCitation(Indexes, compartments);
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
  {$IFDEF FHIR_CLINICALUSEDEFINITION}
  buildIndexesForClinicalUseDefinition(Indexes, compartments);
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
  {$IFDEF FHIR_COVERAGEELIGIBILITYREQUEST}
  buildIndexesForCoverageEligibilityRequest(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_COVERAGEELIGIBILITYRESPONSE}
  buildIndexesForCoverageEligibilityResponse(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_DETECTEDISSUE}
  buildIndexesForDetectedIssue(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_DEVICE}
  buildIndexesForDevice(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_DEVICEDEFINITION}
  buildIndexesForDeviceDefinition(Indexes, compartments);
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
  {$IFDEF FHIR_EVENTDEFINITION}
  buildIndexesForEventDefinition(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_EVIDENCE}
  buildIndexesForEvidence(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_EVIDENCEREPORT}
  buildIndexesForEvidenceReport(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_EVIDENCEVARIABLE}
  buildIndexesForEvidenceVariable(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_EXAMPLESCENARIO}
  buildIndexesForExampleScenario(Indexes, compartments);
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
  {$IFDEF FHIR_IMAGINGSTUDY}
  buildIndexesForImagingStudy(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_IMMUNIZATION}
  buildIndexesForImmunization(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_IMMUNIZATIONEVALUATION}
  buildIndexesForImmunizationEvaluation(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_IMMUNIZATIONRECOMMENDATION}
  buildIndexesForImmunizationRecommendation(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_IMPLEMENTATIONGUIDE}
  buildIndexesForImplementationGuide(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_INGREDIENT}
  buildIndexesForIngredient(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_INSURANCEPLAN}
  buildIndexesForInsurancePlan(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_INVOICE}
  buildIndexesForInvoice(Indexes, compartments);
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
  {$IFDEF FHIR_MANUFACTUREDITEMDEFINITION}
  buildIndexesForManufacturedItemDefinition(Indexes, compartments);
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
  {$IFDEF FHIR_MEDICATIONKNOWLEDGE}
  buildIndexesForMedicationKnowledge(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_MEDICATIONREQUEST}
  buildIndexesForMedicationRequest(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_MEDICATIONSTATEMENT}
  buildIndexesForMedicationStatement(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_MEDICINALPRODUCTDEFINITION}
  buildIndexesForMedicinalProductDefinition(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_MESSAGEDEFINITION}
  buildIndexesForMessageDefinition(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_MESSAGEHEADER}
  buildIndexesForMessageHeader(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_MOLECULARSEQUENCE}
  buildIndexesForMolecularSequence(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_NAMINGSYSTEM}
  buildIndexesForNamingSystem(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_NUTRITIONORDER}
  buildIndexesForNutritionOrder(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_NUTRITIONPRODUCT}
  buildIndexesForNutritionProduct(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_OBSERVATION}
  buildIndexesForObservation(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_OBSERVATIONDEFINITION}
  buildIndexesForObservationDefinition(Indexes, compartments);
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
  {$IFDEF FHIR_ORGANIZATIONAFFILIATION}
  buildIndexesForOrganizationAffiliation(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_PACKAGEDPRODUCTDEFINITION}
  buildIndexesForPackagedProductDefinition(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_PARAMETERS}
  buildIndexesForParameters(Indexes, compartments);
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
  {$IFDEF FHIR_PROVENANCE}
  buildIndexesForProvenance(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_QUESTIONNAIRE}
  buildIndexesForQuestionnaire(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_QUESTIONNAIRERESPONSE}
  buildIndexesForQuestionnaireResponse(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_REGULATEDAUTHORIZATION}
  buildIndexesForRegulatedAuthorization(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_RELATEDPERSON}
  buildIndexesForRelatedPerson(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_REQUESTGROUP}
  buildIndexesForRequestGroup(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_RESEARCHDEFINITION}
  buildIndexesForResearchDefinition(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_RESEARCHELEMENTDEFINITION}
  buildIndexesForResearchElementDefinition(Indexes, compartments);
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
  {$IFDEF FHIR_SERVICEREQUEST}
  buildIndexesForServiceRequest(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_SLOT}
  buildIndexesForSlot(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_SPECIMEN}
  buildIndexesForSpecimen(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_SPECIMENDEFINITION}
  buildIndexesForSpecimenDefinition(Indexes, compartments);
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
  {$IFDEF FHIR_SUBSCRIPTIONSTATUS}
  buildIndexesForSubscriptionStatus(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_SUBSCRIPTIONTOPIC}
  buildIndexesForSubscriptionTopic(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_SUBSTANCE}
  buildIndexesForSubstance(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_SUBSTANCEDEFINITION}
  buildIndexesForSubstanceDefinition(Indexes, compartments);
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
  {$IFDEF FHIR_TERMINOLOGYCAPABILITIES}
  buildIndexesForTerminologyCapabilities(Indexes, compartments);
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
  {$IFDEF FHIR_VERIFICATIONRESULT}
  buildIndexesForVerificationResult(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_VISIONPRESCRIPTION}
  buildIndexesForVisionPrescription(Indexes, compartments);
  {$ENDIF}

end;

end.

