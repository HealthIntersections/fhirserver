unit fhir5_indexinfo;

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

{$i fhir.inc}
{$i fhir5.inc}

interface

// Generated on Thu, Nov 10, 2022 for FHIR v5.0.0



uses
  SysUtils, Classes,
  fsl_base, fsl_utilities, fsl_stream,
  fhir_common,
  fhir5_enums, fhir5_types, fhir5_resources, fhir5_constants, fhir_indexing;

Type

  TFHIRIndexBuilderR5 = class (TFHIRIndexBuilder)
  private
    {$IFDEF FHIR_ACCOUNT}
    procedure buildIndexesForAccount(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_ACTIVITYDEFINITION}
    procedure buildIndexesForActivityDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_ACTORDEFINITION}
    procedure buildIndexesForActorDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
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
    {$IFDEF FHIR_ARTIFACTASSESSMENT}
    procedure buildIndexesForArtifactAssessment(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
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
    {$IFDEF FHIR_CONDITIONDEFINITION}
    procedure buildIndexesForConditionDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
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
    {$IFDEF FHIR_DEVICEDISPENSE}
    procedure buildIndexesForDeviceDispense(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_DEVICEMETRIC}
    procedure buildIndexesForDeviceMetric(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_DEVICEREQUEST}
    procedure buildIndexesForDeviceRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_DEVICEUSAGE}
    procedure buildIndexesForDeviceUsage(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
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
    {$IFDEF FHIR_FORMULARYITEM}
    procedure buildIndexesForFormularyItem(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_GENOMICSTUDY}
    procedure buildIndexesForGenomicStudy(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
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
    {$IFDEF FHIR_IMAGINGSELECTION}
    procedure buildIndexesForImagingSelection(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
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
    {$IFDEF FHIR_INVENTORYREPORT}
    procedure buildIndexesForInventoryReport(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
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
    {$IFDEF FHIR_MEDICATIONUSAGE}
    procedure buildIndexesForMedicationUsage(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
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
    {$IFDEF FHIR_NUTRITIONINTAKE}
    procedure buildIndexesForNutritionIntake(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
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
    {$IFDEF FHIR_PERMISSION}
    procedure buildIndexesForPermission(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
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
    {$IFDEF FHIR_REQUESTORCHESTRATION}
    procedure buildIndexesForRequestOrchestration(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_REQUIREMENTS}
    procedure buildIndexesForRequirements(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
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
    {$IFDEF FHIR_SUBSTANCENUCLEICACID}
    procedure buildIndexesForSubstanceNucleicAcid(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_SUBSTANCEPOLYMER}
    procedure buildIndexesForSubstancePolymer(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_SUBSTANCEPROTEIN}
    procedure buildIndexesForSubstanceProtein(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_SUBSTANCEREFERENCEINFORMATION}
    procedure buildIndexesForSubstanceReferenceInformation(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
    {$ENDIF}
    {$IFDEF FHIR_SUBSTANCESOURCEMATERIAL}
    procedure buildIndexesForSubstanceSourceMaterial(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
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
    {$IFDEF FHIR_TRANSPORT}
    procedure buildIndexesForTransport(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
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
  indexes.add('Account', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('Account', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('Account', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('Account', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('Account', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('Account', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('Account', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('Account', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('Account', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('Account', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('Account', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('Account', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('Account', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('Account', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('Account', 'guarantor', 'The parties ultimately responsible for balancing the Account', sptREFERENCE, ['Organization', 'Patient', 'RelatedPerson'], 'Account.guarantor.party', sxpNormal);
  indexes.add('Account', 'identifier', 'Account number', sptTOKEN, [], 'Account.identifier', sxpNormal);
  indexes.add('Account', 'name', 'Human-readable label', sptSTRING, [], 'Account.name', sxpNormal);
  indexes.add('Account', 'owner', 'Entity managing the Account', sptREFERENCE, ['Organization'], 'Account.owner', sxpNormal);
  indexes.add('Account', 'patient', 'The entity that caused the expenses', sptREFERENCE, ['Patient'], 'Account.subject.where(resolve() is Patient)', sxpNormal);
  indexes.add('Account', 'period', 'Transaction window', sptDATE, [], 'Account.servicePeriod', sxpNormal);
  indexes.add('Account', 'relatedaccount', 'Parent and other related accounts', sptREFERENCE, ['Account'], 'Account.relatedAccount.account', sxpNormal);
  indexes.add('Account', 'status', 'active | inactive | entered-in-error | on-hold | unknown', sptTOKEN, [], 'Account.status', sxpNormal);
  indexes.add('Account', 'subject', 'The entity that caused the expenses', sptREFERENCE, ['Practitioner', 'Organization', 'Device', 'Patient', 'HealthcareService', 'PractitionerRole', 'Location'], 'Account.subject', sxpNormal);
  indexes.add('Account', 'type', 'E.g. patient, expense, depreciation', sptTOKEN, [], 'Account.type', sxpNormal);
  indexes.add('Account', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Device', 'Account', ['subject']);
  compartments.register('Patient', 'Account', ['subject']);
  compartments.register('Practitioner', 'Account', ['subject']);
end;
{$ENDIF FHIR_ACCOUNT}

{$IFDEF FHIR_ACTIVITYDEFINITION}
procedure TFHIRIndexBuilderR5.buildIndexesForActivityDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ActivityDefinition', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('ActivityDefinition', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('ActivityDefinition', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('ActivityDefinition', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('ActivityDefinition', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('ActivityDefinition', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('ActivityDefinition', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('ActivityDefinition', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('ActivityDefinition', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('ActivityDefinition', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('ActivityDefinition', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('ActivityDefinition', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('ActivityDefinition', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('ActivityDefinition', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('ActivityDefinition', 'composed-of', 'What resource is being referenced', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'ActivityDefinition.relatedArtifact.where(type=''composed-of'').resource', sxpNormal);
  indexes.add('ActivityDefinition', 'context', 'A use context assigned to the activity definition', sptTOKEN, [], '(ActivityDefinition.useContext.value as CodeableConcept)', sxpNormal);
  indexes.add('ActivityDefinition', 'context-quantity', 'A quantity- or range-valued use context assigned to the activity definition', sptQUANTITY, [], '(ActivityDefinition.useContext.value as Quantity) | (ActivityDefinition.useContext.value as Range)', sxpNormal);
  indexes.add('ActivityDefinition', 'context-type', 'A type of use context assigned to the activity definition', sptTOKEN, [], 'ActivityDefinition.useContext.code', sxpNormal);
  indexes.add('ActivityDefinition', 'context-type-quantity', 'A use context type and quantity- or range-based value assigned to the activity definition', sptCOMPOSITE, [], 'ActivityDefinition.useContext', sxpNormal);
  indexes.add('ActivityDefinition', 'context-type-value', 'A use context type and value assigned to the activity definition', sptCOMPOSITE, [], 'ActivityDefinition.useContext', sxpNormal);
  indexes.add('ActivityDefinition', 'date', 'The activity definition publication date', sptDATE, [], 'ActivityDefinition.date', sxpNormal);
  indexes.add('ActivityDefinition', 'depends-on', 'What resource is being referenced', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'ActivityDefinition.relatedArtifact.where(type=''depends-on'').resource | ActivityDefinition.library', sxpNormal);
  indexes.add('ActivityDefinition', 'derived-from', 'What resource is being referenced', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'ActivityDefinition.relatedArtifact.where(type=''derived-from'').resource', sxpNormal);
  indexes.add('ActivityDefinition', 'description', 'The description of the activity definition', sptSTRING, [], 'ActivityDefinition.description', sxpNormal);
  indexes.add('ActivityDefinition', 'effective', 'The time during which the activity definition is intended to be in use', sptDATE, [], 'ActivityDefinition.effectivePeriod', sxpNormal);
  indexes.add('ActivityDefinition', 'identifier', 'External identifier for the activity definition', sptTOKEN, [], 'ActivityDefinition.identifier', sxpNormal);
  indexes.add('ActivityDefinition', 'jurisdiction', 'Intended jurisdiction for the activity definition', sptTOKEN, [], 'ActivityDefinition.jurisdiction', sxpNormal);
  indexes.add('ActivityDefinition', 'kind', 'The kind of activity definition', sptTOKEN, [], 'ActivityDefinition.kind', sxpNormal);
  indexes.add('ActivityDefinition', 'name', 'Computationally friendly name of the activity definition', sptSTRING, [], 'ActivityDefinition.name', sxpNormal);
  indexes.add('ActivityDefinition', 'predecessor', 'What resource is being referenced', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'ActivityDefinition.relatedArtifact.where(type=''predecessor'').resource', sxpNormal);
  indexes.add('ActivityDefinition', 'publisher', 'Name of the publisher of the activity definition', sptSTRING, [], 'ActivityDefinition.publisher', sxpNormal);
  indexes.add('ActivityDefinition', 'status', 'The current status of the activity definition', sptTOKEN, [], 'ActivityDefinition.status', sxpNormal);
  indexes.add('ActivityDefinition', 'successor', 'What resource is being referenced', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'ActivityDefinition.relatedArtifact.where(type=''successor'').resource', sxpNormal);
  indexes.add('ActivityDefinition', 'title', 'The human-friendly name of the activity definition', sptSTRING, [], 'ActivityDefinition.title', sxpNormal);
  indexes.add('ActivityDefinition', 'topic', 'Topics associated with the module', sptTOKEN, [], 'ActivityDefinition.topic', sxpNormal);
  indexes.add('ActivityDefinition', 'url', 'The uri that identifies the activity definition', sptURI, [], 'ActivityDefinition.url', sxpNormal);
  indexes.add('ActivityDefinition', 'version', 'The business version of the activity definition', sptTOKEN, [], 'ActivityDefinition.version', sxpNormal);
  indexes.add('ActivityDefinition', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_ACTIVITYDEFINITION}

{$IFDEF FHIR_ACTORDEFINITION}
procedure TFHIRIndexBuilderR5.buildIndexesForActorDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ActorDefinition', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('ActorDefinition', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('ActorDefinition', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('ActorDefinition', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('ActorDefinition', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('ActorDefinition', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('ActorDefinition', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('ActorDefinition', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('ActorDefinition', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('ActorDefinition', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('ActorDefinition', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('ActorDefinition', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('ActorDefinition', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('ActorDefinition', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('ActorDefinition', 'context', 'A use context assigned to the Actor Definition', sptTOKEN, [], '(ActorDefinition.useContext.value as CodeableConcept)', sxpNormal);
  indexes.add('ActorDefinition', 'context-quantity', 'A quantity- or range-valued use context assigned to the Actor Definition', sptQUANTITY, [], '(ActorDefinition.useContext.value as Quantity) | (ActorDefinition.useContext.value as Range)', sxpNormal);
  indexes.add('ActorDefinition', 'context-type', 'A type of use context assigned to the Actor Definition', sptTOKEN, [], 'ActorDefinition.useContext.code', sxpNormal);
  indexes.add('ActorDefinition', 'context-type-quantity', 'A use context type and quantity- or range-based value assigned to the Actor Definition', sptCOMPOSITE, [], 'ActorDefinition.useContext', sxpNormal);
  indexes.add('ActorDefinition', 'context-type-value', 'A use context type and value assigned to the Actor Definition', sptCOMPOSITE, [], 'ActorDefinition.useContext', sxpNormal);
  indexes.add('ActorDefinition', 'date', 'The Actor Definition publication date', sptDATE, [], 'ActorDefinition.date', sxpNormal);
  indexes.add('ActorDefinition', 'description', 'The description of the Actor Definition', sptSTRING, [], 'ActorDefinition.description', sxpNormal);
  indexes.add('ActorDefinition', 'identifier', 'External identifier for the Actor Definition', sptTOKEN, [], 'ActorDefinition.identifier', sxpNormal);
  indexes.add('ActorDefinition', 'jurisdiction', 'Intended jurisdiction for the Actor Definition', sptTOKEN, [], 'ActorDefinition.jurisdiction', sxpNormal);
  indexes.add('ActorDefinition', 'publisher', 'Name of the publisher of the Actor Definition', sptSTRING, [], 'ActorDefinition.publisher', sxpNormal);
  indexes.add('ActorDefinition', 'status', 'The current status of the Actor Definition', sptTOKEN, [], 'ActorDefinition.status', sxpNormal);
  indexes.add('ActorDefinition', 'title', 'The human-friendly name of the Actor Definition', sptSTRING, [], 'ActorDefinition.title', sxpNormal);
  indexes.add('ActorDefinition', 'type', 'The type of actor', sptTOKEN, [], 'ActorDefinition.type', sxpNormal);
  indexes.add('ActorDefinition', 'url', 'The uri that identifies the Actor Definition', sptURI, [], 'ActorDefinition.url', sxpNormal);
  indexes.add('ActorDefinition', 'version', 'The business version of the Actor Definition', sptTOKEN, [], 'ActorDefinition.version', sxpNormal);
  indexes.add('ActorDefinition', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_ACTORDEFINITION}

{$IFDEF FHIR_ADMINISTRABLEPRODUCTDEFINITION}
procedure TFHIRIndexBuilderR5.buildIndexesForAdministrableProductDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('AdministrableProductDefinition', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('AdministrableProductDefinition', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('AdministrableProductDefinition', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('AdministrableProductDefinition', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('AdministrableProductDefinition', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('AdministrableProductDefinition', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('AdministrableProductDefinition', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('AdministrableProductDefinition', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('AdministrableProductDefinition', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('AdministrableProductDefinition', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('AdministrableProductDefinition', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('AdministrableProductDefinition', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('AdministrableProductDefinition', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('AdministrableProductDefinition', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('AdministrableProductDefinition', 'device', 'A device that is integral to the medicinal product, in effect being considered as an "ingredient" of the medicinal product. This is not intended for devices that are just co-packaged', sptREFERENCE, ['DeviceDefinition'], 'AdministrableProductDefinition.device', sxpNormal);
  indexes.add('AdministrableProductDefinition', 'dose-form', 'The administrable dose form, i.e. the dose form of the final product after necessary reconstitution or processing', sptTOKEN, [], 'AdministrableProductDefinition.administrableDoseForm', sxpNormal);
  indexes.add('AdministrableProductDefinition', 'form-of', 'The medicinal product that this is an administrable form of. This is not a reference to the item(s) that make up this administrable form - it is the whole product', sptREFERENCE, ['MedicinalProductDefinition'], 'AdministrableProductDefinition.formOf', sxpNormal);
  indexes.add('AdministrableProductDefinition', 'identifier', 'An identifier for the administrable product', sptTOKEN, [], 'AdministrableProductDefinition.identifier', sxpNormal);
  indexes.add('AdministrableProductDefinition', 'ingredient', 'The ingredients of this administrable medicinal product', sptTOKEN, [], 'AdministrableProductDefinition.ingredient', sxpNormal);
  indexes.add('AdministrableProductDefinition', 'manufactured-item', 'The manufactured item(s) that this administrable product is produced from. Either a single item, or several that are mixed before administration (e.g. a power item and a solution item). Note that these are not raw ingredients', sptREFERENCE, ['ManufacturedItemDefinition'], 'AdministrableProductDefinition.producedFrom', sxpNormal);
  indexes.add('AdministrableProductDefinition', 'route', 'Coded expression for the route', sptTOKEN, [], 'AdministrableProductDefinition.routeOfAdministration.code', sxpNormal);
  indexes.add('AdministrableProductDefinition', 'status', 'The status of this administrable product. Enables tracking the life-cycle of the content.', sptTOKEN, [], 'AdministrableProductDefinition.status', sxpNormal);
  indexes.add('AdministrableProductDefinition', 'target-species', 'Coded expression for the species', sptTOKEN, [], 'AdministrableProductDefinition.routeOfAdministration.targetSpecies.code', sxpNormal);
  indexes.add('AdministrableProductDefinition', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_ADMINISTRABLEPRODUCTDEFINITION}

{$IFDEF FHIR_ADVERSEEVENT}
procedure TFHIRIndexBuilderR5.buildIndexesForAdverseEvent(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('AdverseEvent', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('AdverseEvent', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('AdverseEvent', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('AdverseEvent', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('AdverseEvent', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('AdverseEvent', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('AdverseEvent', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('AdverseEvent', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('AdverseEvent', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('AdverseEvent', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('AdverseEvent', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('AdverseEvent', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('AdverseEvent', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('AdverseEvent', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('AdverseEvent', 'actuality', 'actual | potential', sptTOKEN, [], 'AdverseEvent.actuality', sxpNormal);
  indexes.add('AdverseEvent', 'category', 'wrong-patient | procedure-mishap | medication-mishap | device | unsafe-physical-environment | hospital-aquired-infection | wrong-body-site', sptTOKEN, [], 'AdverseEvent.category', sxpNormal);
  indexes.add('AdverseEvent', 'code', 'Event or incident that occurred or was averted', sptTOKEN, [], 'AdverseEvent.code', sxpNormal);
  indexes.add('AdverseEvent', 'date', 'When the event occurred', sptDATE, [], 'AdverseEvent.occurrence', sxpNormal);
  indexes.add('AdverseEvent', 'identifier', 'Business identifier for the event', sptTOKEN, [], 'AdverseEvent.identifier', sxpNormal);
  indexes.add('AdverseEvent', 'location', 'Location where adverse event occurred', sptREFERENCE, ['Location'], 'AdverseEvent.location', sxpNormal);
  indexes.add('AdverseEvent', 'patient', 'Subject impacted by event', sptREFERENCE, ['Practitioner', 'Group', 'Patient', 'ResearchSubject', 'RelatedPerson'], 'AdverseEvent.subject', sxpNormal);
  indexes.add('AdverseEvent', 'recorder', 'Who recorded the adverse event', sptREFERENCE, ['Practitioner', 'Patient', 'PractitionerRole', 'ResearchSubject', 'RelatedPerson'], 'AdverseEvent.recorder', sxpNormal);
  indexes.add('AdverseEvent', 'resultingcondition', 'Effect on the subject due to this event', sptREFERENCE, ['Condition'], 'AdverseEvent.resultingCondition', sxpNormal);
  indexes.add('AdverseEvent', 'seriousness', 'Seriousness or gravity of the event', sptTOKEN, [], 'AdverseEvent.seriousness', sxpNormal);
  indexes.add('AdverseEvent', 'status', 'in-progress | completed | entered-in-error | unknown', sptTOKEN, [], 'AdverseEvent.status', sxpNormal);
  indexes.add('AdverseEvent', 'study', 'Research study that the subject is enrolled in', sptREFERENCE, ['ResearchStudy'], 'AdverseEvent.study', sxpNormal);
  indexes.add('AdverseEvent', 'subject', 'Subject impacted by event', sptREFERENCE, ['Practitioner', 'Group', 'Patient', 'ResearchSubject', 'RelatedPerson'], 'AdverseEvent.subject', sxpNormal);
  indexes.add('AdverseEvent', 'substance', 'Refers to the specific entity that caused the adverse event', sptREFERENCE, ['Immunization', 'BiologicallyDerivedProduct', 'Device', 'Medication', 'Procedure', 'Substance', 'MedicationAdministration', 'MedicationUsage', 'ResearchStudy'], '(AdverseEvent.suspectEntity.instance as Reference)', sxpNormal);
  indexes.add('AdverseEvent', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Patient', 'AdverseEvent', ['subject']);
  compartments.register('Practitioner', 'AdverseEvent', ['recorder']);
  compartments.register('RelatedPerson', 'AdverseEvent', ['recorder']);
end;
{$ENDIF FHIR_ADVERSEEVENT}

{$IFDEF FHIR_ALLERGYINTOLERANCE}
procedure TFHIRIndexBuilderR5.buildIndexesForAllergyIntolerance(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('AllergyIntolerance', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('AllergyIntolerance', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('AllergyIntolerance', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('AllergyIntolerance', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('AllergyIntolerance', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('AllergyIntolerance', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('AllergyIntolerance', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('AllergyIntolerance', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('AllergyIntolerance', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('AllergyIntolerance', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('AllergyIntolerance', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('AllergyIntolerance', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('AllergyIntolerance', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('AllergyIntolerance', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('AllergyIntolerance', 'category', 'food | medication | environment | biologic', sptTOKEN, [], 'AllergyIntolerance.category', sxpNormal);
  indexes.add('AllergyIntolerance', 'clinical-status', 'active | inactive | resolved', sptTOKEN, [], 'AllergyIntolerance.clinicalStatus', sxpNormal);
  indexes.add('AllergyIntolerance', 'code', '): Code that identifies the allergy or intolerance', sptTOKEN, [], 'AllergyIntolerance.code | AllergyIntolerance.reaction.substance | Condition.code | DeviceRequest.code.concept | DiagnosticReport.code | FamilyMemberHistory.condition.code | List.code | Medication.code | MedicationAdministration.medication.concept | M'
      +'edicationDispense.medication.concept | MedicationRequest.medication.concept | MedicationUsage.medication.concept | Observation.code | Procedure.code', sxpNormal);
  indexes.add('AllergyIntolerance', 'criticality', 'low | high | unable-to-assess', sptTOKEN, [], 'AllergyIntolerance.criticality', sxpNormal);
  indexes.add('AllergyIntolerance', 'date', '): Date first version of the resource instance was recorded', sptDATE, [], 'AllergyIntolerance.recordedDate | CarePlan.period | ClinicalImpression.date | Composition.date | Consent.dateTime | DiagnosticReport.effective | Encounter.actualPeriod | EpisodeOfCare.period | FamilyMemberHistory.date | Flag.period | (Immunization.oc'
      +'currence as dateTime) | List.date | Observation.effective | Procedure.occurrence | (RiskAssessment.occurrence as dateTime) | SupplyRequest.authoredOn', sxpNormal);
  indexes.add('AllergyIntolerance', 'identifier', '): External ids for this item', sptTOKEN, [], 'AllergyIntolerance.identifier | CarePlan.identifier | CareTeam.identifier | Composition.identifier | Condition.identifier | Consent.identifier | DetectedIssue.identifier | DeviceRequest.identifier | DiagnosticReport.identifier | DocumentManifest.mast'
      +'erIdentifier | DocumentManifest.identifier | DocumentReference.identifier | Encounter.identifier | EpisodeOfCare.identifier | FamilyMemberHistory.identifier | Goal.identifier | ImagingStudy.identifier | Immunization.identifier | List.identifi'
      +'er | MedicationAdministration.identifier | MedicationDispense.identifier | MedicationRequest.identifier | MedicationUsage.identifier | NutritionOrder.identifier | Observation.identifier | Procedure.identifier | RiskAssessment.identifier | Ser'
      +'viceRequest.identifier | SupplyDelivery.identifier | SupplyRequest.identifier | VisionPrescription.identifier', sxpNormal);
  indexes.add('AllergyIntolerance', 'last-date', 'Date(/time) of last known occurrence of a reaction', sptDATE, [], 'AllergyIntolerance.lastOccurrence', sxpNormal);
  indexes.add('AllergyIntolerance', 'manifestation-code', 'Clinical symptoms/signs associated with the Event', sptTOKEN, [], 'AllergyIntolerance.reaction.manifestation.concept', sxpNormal);
  indexes.add('AllergyIntolerance', 'manifestation-reference', 'Clinical symptoms/signs associated with the Event', sptREFERENCE, [], 'AllergyIntolerance.reaction.manifestation.reference', sxpNormal);
  indexes.add('AllergyIntolerance', 'participant', 'Who or what participated in the activities related to the allergy or intolerance', sptREFERENCE, ['Practitioner', 'Organization', 'CareTeam', 'Device', 'Patient', 'PractitionerRole', 'RelatedPerson'], 'AllergyIntolerance.participant.actor', sxpNormal);
  indexes.add('AllergyIntolerance', 'patient', '): Who the sensitivity is for', sptREFERENCE, ['Patient', 'Practitioner', 'Group', 'Organization', 'BiologicallyDerivedProduct', 'NutritionProduct', 'Device', 'Medication', 'Procedure', 'Substance', 'Location'], 'AllergyIntolerance.patient | CarePlan.subject.where(resolve() is Patient) | CareTeam.subject.where(resolve() is Patient) | ClinicalImpression.subject.where(resolve() is Patient) | Composition.subject.where(resolve() is Patient) | Condition.subject.wh'
      +'ere(resolve() is Patient) | Consent.subject.where(resolve() is Patient) | DetectedIssue.subject | DeviceRequest.subject.where(resolve() is Patient) | DeviceUsage.patient | DiagnosticReport.subject.where(resolve() is Patient) | DocumentManifes'
      +'t.subject.where(resolve() is Patient) | DocumentReference.subject.where(resolve() is Patient) | Encounter.subject.where(resolve() is Patient) | EpisodeOfCare.patient | FamilyMemberHistory.patient | Flag.subject.where(resolve('+
   ') is Patient) | G'
      +'oal.subject.where(resolve() is Patient) | ImagingStudy.subject.where(resolve() is Patient) | Immunization.patient | List.subject.where(resolve() is Patient) | MedicationAdministration.subject.where(resolve() is Patient) | MedicationDispense.s'
      +'ubject.where(resolve() is Patient) | MedicationRequest.subject.where(resolve() is Patient) | MedicationUsage.subject.where(resolve() is Patient) | NutritionOrder.subject.where(resolve() is Patient) | Observation.subject.where(resolve() is Pat'
      +'ient) | Procedure.subject.where(resolve() is Patient) | RiskAssessment.subject.where(resolve() is Patient) | ServiceRequest.subject.where(resolve() is Patient) | SupplyDelivery.patient | VisionPrescription.patient', sxpNormal);
  indexes.add('AllergyIntolerance', 'route', 'How the subject was exposed to the substance', sptTOKEN, [], 'AllergyIntolerance.reaction.exposureRoute', sxpNormal);
  indexes.add('AllergyIntolerance', 'severity', 'mild | moderate | severe (of event as a whole)', sptTOKEN, [], 'AllergyIntolerance.reaction.severity', sxpNormal);
  indexes.add('AllergyIntolerance', 'type', '): allergy | intolerance - Underlying mechanism (if known)', sptTOKEN, [], 'AllergyIntolerance.type | Composition.type | DocumentManifest.type | DocumentReference.type | Encounter.type | EpisodeOfCare.type', sxpNormal);
  indexes.add('AllergyIntolerance', 'verification-status', 'unconfirmed | presumed | confirmed | refuted | entered-in-error', sptTOKEN, [], 'AllergyIntolerance.verificationStatus', sxpNormal);
  indexes.add('AllergyIntolerance', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Patient', 'AllergyIntolerance', ['patient', 'participant']);
  compartments.register('Practitioner', 'AllergyIntolerance', ['participant']);
  compartments.register('RelatedPerson', 'AllergyIntolerance', ['participant']);
end;
{$ENDIF FHIR_ALLERGYINTOLERANCE}

{$IFDEF FHIR_APPOINTMENT}
procedure TFHIRIndexBuilderR5.buildIndexesForAppointment(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Appointment', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('Appointment', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('Appointment', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('Appointment', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('Appointment', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('Appointment', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('Appointment', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('Appointment', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('Appointment', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('Appointment', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('Appointment', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('Appointment', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('Appointment', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('Appointment', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('Appointment', 'actor', 'Any one of the individuals participating in the appointment', sptREFERENCE, ['Practitioner', 'Group', 'CareTeam', 'Device', 'Patient', 'HealthcareService', 'PractitionerRole', 'RelatedPerson', 'Location'], 'Appointment.participant.actor', sxpNormal);
  indexes.add('Appointment', 'appointment-type', 'The style of appointment or patient that has been booked in the slot (not service type)', sptTOKEN, [], 'Appointment.appointmentType', sxpNormal);
  indexes.add('Appointment', 'based-on', 'The service request this appointment is allocated to assess', sptREFERENCE, ['CarePlan', 'MedicationRequest', 'DeviceRequest', 'ServiceRequest'], 'Appointment.basedOn', sxpNormal);
  indexes.add('Appointment', 'date', 'Appointment date/time.', sptDATE, [], '(start | requestedPeriod.start).first()', sxpNormal);
  indexes.add('Appointment', 'group', 'One of the individuals of the appointment is this patient', sptREFERENCE, ['Group'], 'Appointment.participant.actor.where(resolve() is Group) | Appointment.subject.where(resolve() is Group)', sxpNormal);
  indexes.add('Appointment', 'identifier', 'An Identifier of the Appointment', sptTOKEN, [], 'Appointment.identifier', sxpNormal);
  indexes.add('Appointment', 'location', 'This location is listed in the participants of the appointment', sptREFERENCE, ['Location'], 'Appointment.participant.actor.where(resolve() is Location)', sxpNormal);
  indexes.add('Appointment', 'part-status', 'The Participation status of the subject, or other participant on the appointment. Can be used to locate participants that have not responded to meeting requests.', sptTOKEN, [], 'Appointment.participant.status', sxpNormal);
  indexes.add('Appointment', 'patient', 'One of the individuals of the appointment is this patient', sptREFERENCE, ['Patient'], 'Appointment.participant.actor.where(resolve() is Patient) | Appointment.subject.where(resolve() is Patient)', sxpNormal);
  indexes.add('Appointment', 'practitioner', 'One of the individuals of the appointment is this practitioner', sptREFERENCE, ['Practitioner'], 'Appointment.participant.actor.where(resolve() is Practitioner)', sxpNormal);
  indexes.add('Appointment', 'reason-code', 'Reference to a concept (by class)', sptTOKEN, [], 'Appointment.reason.concept', sxpNormal);
  indexes.add('Appointment', 'reason-reference', 'Reference to a resource (by instance)', sptREFERENCE, [], 'Appointment.reason.reference', sxpNormal);
  indexes.add('Appointment', 'requested-period', 'During what period was the Appointment requested to take place', sptDATE, [], 'requestedPeriod', sxpNormal);
  indexes.add('Appointment', 'service-category', 'A broad categorization of the service that is to be performed during this appointment', sptTOKEN, [], 'Appointment.serviceCategory', sxpNormal);
  indexes.add('Appointment', 'service-type', 'The specific service (by coding) that is to be performed during this appointment', sptTOKEN, [], 'Appointment.serviceType.concept', sxpNormal);
  indexes.add('Appointment', 'service-type-reference', 'The specific service (by HealthcareService) that is to be performed during this appointment', sptREFERENCE, [], 'Appointment.serviceType.reference', sxpNormal);
  indexes.add('Appointment', 'slot', 'The slots that this appointment is filling', sptREFERENCE, ['Slot'], 'Appointment.slot', sxpNormal);
  indexes.add('Appointment', 'specialty', 'The specialty of a practitioner that would be required to perform the service requested in this appointment', sptTOKEN, [], 'Appointment.specialty', sxpNormal);
  indexes.add('Appointment', 'status', 'The overall status of the appointment', sptTOKEN, [], 'Appointment.status', sxpNormal);
  indexes.add('Appointment', 'subject', 'One of the individuals of the appointment is this patient', sptREFERENCE, ['Group', 'Patient'], 'Appointment.subject', sxpNormal);
  indexes.add('Appointment', 'supporting-info', 'Additional information to support the appointment', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'Appointment.supportingInformation', sxpNormal);
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
  indexes.add('AppointmentResponse', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('AppointmentResponse', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('AppointmentResponse', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('AppointmentResponse', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('AppointmentResponse', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('AppointmentResponse', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('AppointmentResponse', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('AppointmentResponse', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('AppointmentResponse', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('AppointmentResponse', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('AppointmentResponse', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('AppointmentResponse', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('AppointmentResponse', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('AppointmentResponse', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('AppointmentResponse', 'actor', 'The Person, Location/HealthcareService or Device that this appointment response replies for', sptREFERENCE, ['Practitioner', 'Group', 'Device', 'Patient', 'HealthcareService', 'PractitionerRole', 'RelatedPerson', 'Location'], 'AppointmentResponse.actor', sxpNormal);
  indexes.add('AppointmentResponse', 'appointment', 'The appointment that the response is attached to', sptREFERENCE, ['Appointment'], 'AppointmentResponse.appointment', sxpNormal);
  indexes.add('AppointmentResponse', 'group', 'This Response is for this Group', sptREFERENCE, ['Group'], 'AppointmentResponse.actor.where(resolve() is Group)', sxpNormal);
  indexes.add('AppointmentResponse', 'identifier', 'An Identifier in this appointment response', sptTOKEN, [], 'AppointmentResponse.identifier', sxpNormal);
  indexes.add('AppointmentResponse', 'location', 'This Response is for this Location', sptREFERENCE, ['Location'], 'AppointmentResponse.actor.where(resolve() is Location)', sxpNormal);
  indexes.add('AppointmentResponse', 'part-status', 'The participants acceptance status for this appointment', sptTOKEN, [], 'AppointmentResponse.participantStatus', sxpNormal);
  indexes.add('AppointmentResponse', 'patient', 'This Response is for this Patient', sptREFERENCE, ['Patient'], 'AppointmentResponse.actor.where(resolve() is Patient)', sxpNormal);
  indexes.add('AppointmentResponse', 'practitioner', 'This Response is for this Practitioner', sptREFERENCE, ['Practitioner'], 'AppointmentResponse.actor.where(resolve() is Practitioner)', sxpNormal);
  indexes.add('AppointmentResponse', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Device', 'AppointmentResponse', ['actor']);
  compartments.register('Patient', 'AppointmentResponse', ['actor']);
  compartments.register('Practitioner', 'AppointmentResponse', ['actor']);
  compartments.register('RelatedPerson', 'AppointmentResponse', ['actor']);
end;
{$ENDIF FHIR_APPOINTMENTRESPONSE}

{$IFDEF FHIR_ARTIFACTASSESSMENT}
procedure TFHIRIndexBuilderR5.buildIndexesForArtifactAssessment(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ArtifactAssessment', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('ArtifactAssessment', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('ArtifactAssessment', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('ArtifactAssessment', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('ArtifactAssessment', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('ArtifactAssessment', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('ArtifactAssessment', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('ArtifactAssessment', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('ArtifactAssessment', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('ArtifactAssessment', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('ArtifactAssessment', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('ArtifactAssessment', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('ArtifactAssessment', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('ArtifactAssessment', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('ArtifactAssessment', 'date', 'The artifact assessment publication date', sptDATE, [], 'ArtifactAssessment.date', sxpNormal);
  indexes.add('ArtifactAssessment', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_ARTIFACTASSESSMENT}

{$IFDEF FHIR_AUDITEVENT}
procedure TFHIRIndexBuilderR5.buildIndexesForAuditEvent(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('AuditEvent', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('AuditEvent', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('AuditEvent', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('AuditEvent', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('AuditEvent', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('AuditEvent', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('AuditEvent', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('AuditEvent', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('AuditEvent', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('AuditEvent', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('AuditEvent', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('AuditEvent', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('AuditEvent', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('AuditEvent', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('AuditEvent', 'action', 'Type of action performed during the event', sptTOKEN, [], 'AuditEvent.action', sxpNormal);
  indexes.add('AuditEvent', 'agent', 'Identifier of who', sptREFERENCE, ['Practitioner', 'Organization', 'CareTeam', 'Device', 'Patient', 'PractitionerRole', 'RelatedPerson'], 'AuditEvent.agent.who', sxpNormal);
  indexes.add('AuditEvent', 'agent-role', 'Agent role in the event', sptTOKEN, [], 'AuditEvent.agent.role', sxpNormal);
  indexes.add('AuditEvent', 'based-on', 'Reference to the service request.', sptREFERENCE, ['CarePlan', 'MedicationRequest', 'Task', 'NutritionOrder', 'DeviceRequest', 'ServiceRequest', 'ImmunizationRecommendation'], 'AuditEvent.basedOn', sxpNormal);
  indexes.add('AuditEvent', 'category', 'Category of event', sptTOKEN, [], 'AuditEvent.category', sxpNormal);
  indexes.add('AuditEvent', 'code', 'More specific code for the event', sptTOKEN, [], 'AuditEvent.code', sxpNormal);
  indexes.add('AuditEvent', 'date', 'Time when the event was recorded', sptDATE, [], 'AuditEvent.recorded', sxpNormal);
  indexes.add('AuditEvent', 'encounter', 'Encounter related to the activity recorded in the AuditEvent', sptREFERENCE, ['Encounter'], 'AuditEvent.encounter', sxpNormal);
  indexes.add('AuditEvent', 'entity', 'Specific instance of resource', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'AuditEvent.entity.what', sxpNormal);
  indexes.add('AuditEvent', 'entity-role', 'What role the entity played', sptTOKEN, [], 'AuditEvent.entity.role', sxpNormal);
  indexes.add('AuditEvent', 'outcome', 'Whether the event succeeded or failed', sptTOKEN, [], 'AuditEvent.outcome.code', sxpNormal);
  indexes.add('AuditEvent', 'patient', 'Where the activity involved patient data', sptREFERENCE, ['Patient'], 'AuditEvent.patient', sxpNormal);
  indexes.add('AuditEvent', 'policy', 'Policy that authorized event', sptURI, [], 'AuditEvent.agent.policy', sxpNormal);
  indexes.add('AuditEvent', 'purpose', 'The authorization (purposeOfUse) of the event', sptTOKEN, [], 'AuditEvent.authorization | AuditEvent.agent.authorization', sxpNormal);
  indexes.add('AuditEvent', 'source', 'The identity of source detecting the event', sptREFERENCE, ['Practitioner', 'Organization', 'CareTeam', 'Device', 'Patient', 'PractitionerRole', 'RelatedPerson'], 'AuditEvent.source.observer', sxpNormal);
  indexes.add('AuditEvent', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Device', 'AuditEvent', ['agent']);
  compartments.register('Patient', 'AuditEvent', ['patient']);
  compartments.register('Practitioner', 'AuditEvent', ['agent']);
end;
{$ENDIF FHIR_AUDITEVENT}

{$IFDEF FHIR_BASIC}
procedure TFHIRIndexBuilderR5.buildIndexesForBasic(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Basic', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('Basic', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('Basic', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('Basic', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('Basic', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('Basic', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('Basic', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('Basic', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('Basic', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('Basic', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('Basic', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('Basic', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('Basic', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('Basic', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('Basic', 'author', 'Who created', sptREFERENCE, ['Practitioner', 'Organization', 'CareTeam', 'Device', 'Patient', 'PractitionerRole', 'RelatedPerson'], 'Basic.author', sxpNormal);
  indexes.add('Basic', 'code', 'Kind of Resource', sptTOKEN, [], 'Basic.code', sxpNormal);
  indexes.add('Basic', 'created', 'When created', sptDATE, [], 'Basic.created', sxpNormal);
  indexes.add('Basic', 'identifier', 'Business identifier', sptTOKEN, [], 'Basic.identifier', sxpNormal);
  indexes.add('Basic', 'patient', 'Identifies the focus of this resource', sptREFERENCE, ['Patient'], 'Basic.subject.where(resolve() is Patient)', sxpNormal);
  indexes.add('Basic', 'subject', 'Identifies the focus of this resource', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'Basic.subject', sxpNormal);
  indexes.add('Basic', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Patient', 'Basic', ['patient', 'author']);
  compartments.register('Practitioner', 'Basic', ['author']);
  compartments.register('RelatedPerson', 'Basic', ['author']);
end;
{$ENDIF FHIR_BASIC}

{$IFDEF FHIR_BINARY}
procedure TFHIRIndexBuilderR5.buildIndexesForBinary(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Binary', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('Binary', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('Binary', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('Binary', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('Binary', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('Binary', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('Binary', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('Binary', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('Binary', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('Binary', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('Binary', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('Binary', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('Binary', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('Binary', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('Binary', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_BINARY}

{$IFDEF FHIR_BIOLOGICALLYDERIVEDPRODUCT}
procedure TFHIRIndexBuilderR5.buildIndexesForBiologicallyDerivedProduct(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('BiologicallyDerivedProduct', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('BiologicallyDerivedProduct', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('BiologicallyDerivedProduct', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('BiologicallyDerivedProduct', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('BiologicallyDerivedProduct', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('BiologicallyDerivedProduct', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('BiologicallyDerivedProduct', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('BiologicallyDerivedProduct', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('BiologicallyDerivedProduct', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('BiologicallyDerivedProduct', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('BiologicallyDerivedProduct', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('BiologicallyDerivedProduct', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('BiologicallyDerivedProduct', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('BiologicallyDerivedProduct', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('BiologicallyDerivedProduct', 'biological-source-event', 'The biological source for the biologically derived product', sptTOKEN, [], 'BiologicallyDerivedProduct.biologicalSourceEvent', sxpNormal);
  indexes.add('BiologicallyDerivedProduct', 'collector', 'Procedure request to obtain this biologically derived product.', sptREFERENCE, ['Practitioner', 'PractitionerRole'], 'BiologicallyDerivedProduct.collection.collector', sxpNormal);
  indexes.add('BiologicallyDerivedProduct', 'identifier', 'Identifier', sptTOKEN, [], 'BiologicallyDerivedProduct.identifier', sxpNormal);
  indexes.add('BiologicallyDerivedProduct', 'product-category', 'Broad category of this product.', sptTOKEN, [], 'BiologicallyDerivedProduct.productCategory', sxpNormal);
  indexes.add('BiologicallyDerivedProduct', 'product-code', 'A code that identifies the kind of this biologically derived product (SNOMED CT code).', sptTOKEN, [], 'BiologicallyDerivedProduct.productCode', sxpNormal);
  indexes.add('BiologicallyDerivedProduct', 'product-status', 'Whether the product is currently available.', sptTOKEN, [], 'BiologicallyDerivedProduct.productStatus', sxpNormal);
  indexes.add('BiologicallyDerivedProduct', 'request', 'Procedure request to obtain this biologically derived product.', sptREFERENCE, ['ServiceRequest'], 'BiologicallyDerivedProduct.request', sxpNormal);
  indexes.add('BiologicallyDerivedProduct', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_BIOLOGICALLYDERIVEDPRODUCT}

{$IFDEF FHIR_BODYSTRUCTURE}
procedure TFHIRIndexBuilderR5.buildIndexesForBodyStructure(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('BodyStructure', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('BodyStructure', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('BodyStructure', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('BodyStructure', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('BodyStructure', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('BodyStructure', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('BodyStructure', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('BodyStructure', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('BodyStructure', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('BodyStructure', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('BodyStructure', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('BodyStructure', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('BodyStructure', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('BodyStructure', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('BodyStructure', 'identifier', 'Bodystructure identifier', sptTOKEN, [], 'BodyStructure.identifier', sxpNormal);
  indexes.add('BodyStructure', 'morphology', 'Kind of Structure', sptTOKEN, [], 'BodyStructure.morphology', sxpNormal);
  indexes.add('BodyStructure', 'patient', 'Who this is about', sptREFERENCE, ['Patient'], 'BodyStructure.patient', sxpNormal);
  indexes.add('BodyStructure', 'structure', 'Body site excludedStructure structure', sptTOKEN, [], 'BodyStructure.excludedStructure.structure', sxpNormal);
  indexes.add('BodyStructure', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Patient', 'BodyStructure', ['patient']);
end;
{$ENDIF FHIR_BODYSTRUCTURE}

{$IFDEF FHIR_BUNDLE}
procedure TFHIRIndexBuilderR5.buildIndexesForBundle(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Bundle', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('Bundle', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('Bundle', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('Bundle', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('Bundle', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('Bundle', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('Bundle', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('Bundle', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('Bundle', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('Bundle', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('Bundle', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('Bundle', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('Bundle', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('Bundle', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('Bundle', 'composition', 'The first resource in the bundle, if the bundle type is "document" - this is a composition, and this parameter provides access to search its contents', sptREFERENCE, [], 'Bundle.entry[0].resource as Composition', sxpNormal);
  indexes.add('Bundle', 'identifier', 'Persistent identifier for the bundle', sptTOKEN, [], 'Bundle.identifier', sxpNormal);
  indexes.add('Bundle', 'message', 'The first resource in the bundle, if the bundle type is "message" - this is a message header, and this parameter provides access to search its contents', sptREFERENCE, [], 'Bundle.entry[0].resource as MessageHeader', sxpNormal);
  indexes.add('Bundle', 'timestamp', 'When the bundle was assembled', sptDATE, [], 'Bundle.timestamp', sxpNormal);
  indexes.add('Bundle', 'type', 'document | message | transaction | transaction-response | batch | batch-response | history | searchset | collection | subscription-notification', sptTOKEN, [], 'Bundle.type', sxpNormal);
  indexes.add('Bundle', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_BUNDLE}

{$IFDEF FHIR_CAPABILITYSTATEMENT}
procedure TFHIRIndexBuilderR5.buildIndexesForCapabilityStatement(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('CapabilityStatement', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('CapabilityStatement', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('CapabilityStatement', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('CapabilityStatement', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('CapabilityStatement', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('CapabilityStatement', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('CapabilityStatement', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('CapabilityStatement', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('CapabilityStatement', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('CapabilityStatement', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('CapabilityStatement', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('CapabilityStatement', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('CapabilityStatement', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('CapabilityStatement', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('CapabilityStatement', 'context', '): A use context assigned to the capability statement', sptTOKEN, [], '(CapabilityStatement.useContext.value as CodeableConcept) | (CodeSystem.useContext.value as CodeableConcept) | (CompartmentDefinition.useContext.value as CodeableConcept) | (ConceptMap.useContext.value as CodeableConcept) | (GraphDefinition.useContex'
      +'t.value as CodeableConcept) | (ImplementationGuide.useContext.value as CodeableConcept) | (MessageDefinition.useContext.value as CodeableConcept) | (NamingSystem.useContext.value as CodeableConcept) | (OperationDefinition.useContext.value as '
      +'CodeableConcept) | (SearchParameter.useContext.value as CodeableConcept) | (StructureDefinition.useContext.value as CodeableConcept) | (StructureMap.useContext.value as CodeableConcept) | (TerminologyCapabilities.useContext.value as CodeableC'
      +'oncept) | (ValueSet.useContext.value as CodeableConcept)', sxpNormal);
  indexes.add('CapabilityStatement', 'context-quantity', '): A quantity- or range-valued use context assigned to the capability statement', sptQUANTITY, [], '(CapabilityStatement.useContext.value as Quantity) | (CapabilityStatement.useContext.value as Range) | (CodeSystem.useContext.value as Quantity) | (CodeSystem.useContext.value as Range) | (CompartmentDefinition.useContext.value as Quantity) | (Compar'
      +'tmentDefinition.useContext.value as Range) | (ConceptMap.useContext.value as Quantity) | (ConceptMap.useContext.value as Range) | (GraphDefinition.useContext.value as Quantity) | (GraphDefinition.useContext.value as Range) | (ImplementationGu'
      +'ide.useContext.value as Quantity) | (ImplementationGuide.useContext.value as Range) | (MessageDefinition.useContext.value as Quantity) | (MessageDefinition.useContext.value as Range) | (NamingSystem.useContext.value as Quantity) | (NamingSyst'
      +'em.useContext.value as Range) | (OperationDefinition.useContext.value as Qu'+
   'antity) | (OperationDefinition.useContext.value as Range) | (SearchParameter.useContext.value as Quantity) | (SearchParameter.useContext.value as Range) | (StructureDe'
      +'finition.useContext.value as Quantity) | (StructureDefinition.useContext.value as Range) | (StructureMap.useContext.value as Quantity) | (StructureMap.useContext.value as Range) | (TerminologyCapabilities.useContext.value as Quantity) | (Term'
      +'inologyCapabilities.useContext.value as Range) | (ValueSet.useContext.value as Quantity) | (ValueSet.useContext.value as Range)', sxpNormal);
  indexes.add('CapabilityStatement', 'context-type', '): A type of use context assigned to the capability statement', sptTOKEN, [], 'CapabilityStatement.useContext.code | CodeSystem.useContext.code | CompartmentDefinition.useContext.code | ConceptMap.useContext.code | GraphDefinition.useContext.code | ImplementationGuide.useContext.code | MessageDefinition.useContext.code | Naming'
      +'System.useContext.code | OperationDefinition.useContext.code | SearchParameter.useContext.code | StructureDefinition.useContext.code | StructureMap.useContext.code | TerminologyCapabilities.useContext.code | ValueSet.useContext.code', sxpNormal);
  indexes.add('CapabilityStatement', 'context-type-quantity', '): A use context type and quantity- or range-based value assigned to the capability statement', sptCOMPOSITE, [], 'CapabilityStatement.useContext | CodeSystem.useContext | CompartmentDefinition.useContext | ConceptMap.useContext | GraphDefinition.useContext | ImplementationGuide.useContext | MessageDefinition.useContext | NamingSystem.useContext | OperationDefini'
      +'tion.useContext | SearchParameter.useContext | StructureDefinition.useContext | StructureMap.useContext | TerminologyCapabilities.useContext | ValueSet.useContext', sxpNormal);
  indexes.add('CapabilityStatement', 'context-type-value', '): A use context type and value assigned to the capability statement', sptCOMPOSITE, [], 'CapabilityStatement.useContext | CodeSystem.useContext | CompartmentDefinition.useContext | ConceptMap.useContext | GraphDefinition.useContext | ImplementationGuide.useContext | MessageDefinition.useContext | NamingSystem.useContext | OperationDefini'
      +'tion.useContext | SearchParameter.useContext | StructureDefinition.useContext | StructureMap.useContext | TerminologyCapabilities.useContext | ValueSet.useContext', sxpNormal);
  indexes.add('CapabilityStatement', 'date', '): The capability statement publication date', sptDATE, [], 'CapabilityStatement.date | CodeSystem.date | CompartmentDefinition.date | ConceptMap.date | GraphDefinition.date | ImplementationGuide.date | MessageDefinition.date | NamingSystem.date | OperationDefinition.date | SearchParameter.date | StructureDefi'
      +'nition.date | StructureMap.date | TerminologyCapabilities.date | ValueSet.date', sxpNormal);
  indexes.add('CapabilityStatement', 'description', '): The description of the capability statement', sptSTRING, [], 'CapabilityStatement.description | CodeSystem.description | CompartmentDefinition.description | ConceptMap.description | GraphDefinition.description | ImplementationGuide.description | MessageDefinition.description | NamingSystem.description | Operati'
      +'onDefinition.description | SearchParameter.description | StructureDefinition.description | StructureMap.description | TerminologyCapabilities.description | ValueSet.description', sxpNormal);
  indexes.add('CapabilityStatement', 'fhirversion', 'The version of FHIR', sptTOKEN, [], 'CapabilityStatement.fhirVersion', sxpNormal);
  indexes.add('CapabilityStatement', 'format', 'formats supported (xml | json | ttl | mime type)', sptTOKEN, [], 'CapabilityStatement.format', sxpNormal);
  indexes.add('CapabilityStatement', 'guide', 'Implementation guides supported', sptREFERENCE, ['ImplementationGuide'], 'CapabilityStatement.implementationGuide', sxpNormal);
  indexes.add('CapabilityStatement', 'jurisdiction', '): Intended jurisdiction for the capability statement', sptTOKEN, [], 'CapabilityStatement.jurisdiction | CodeSystem.jurisdiction | ConceptMap.jurisdiction | GraphDefinition.jurisdiction | ImplementationGuide.jurisdiction | MessageDefinition.jurisdiction | NamingSystem.jurisdiction | OperationDefinition.jurisdiction | S'
      +'earchParameter.jurisdiction | StructureDefinition.jurisdiction | StructureMap.jurisdiction | TerminologyCapabilities.jurisdiction | ValueSet.jurisdiction', sxpNormal);
  indexes.add('CapabilityStatement', 'mode', 'Mode - restful (server/client) or messaging (sender/receiver)', sptTOKEN, [], 'CapabilityStatement.rest.mode', sxpNormal);
  indexes.add('CapabilityStatement', 'name', '): Computationally friendly name of the capability statement', sptSTRING, [], 'CapabilityStatement.name | CodeSystem.name | CompartmentDefinition.name | ConceptMap.name | GraphDefinition.name | ImplementationGuide.name | MessageDefinition.name | NamingSystem.name | OperationDefinition.name | SearchParameter.name | StructureDefi'
      +'nition.name | StructureMap.name | TerminologyCapabilities.name | ValueSet.name', sxpNormal);
  indexes.add('CapabilityStatement', 'publisher', '): Name of the publisher of the capability statement', sptSTRING, [], 'CapabilityStatement.publisher | CodeSystem.publisher | CompartmentDefinition.publisher | ConceptMap.publisher | GraphDefinition.publisher | ImplementationGuide.publisher | MessageDefinition.publisher | NamingSystem.publisher | OperationDefinition.pub'
      +'lisher | SearchParameter.publisher | StructureDefinition.publisher | StructureMap.publisher | TerminologyCapabilities.publisher | ValueSet.publisher', sxpNormal);
  indexes.add('CapabilityStatement', 'resource', 'Name of a resource mentioned in a capability statement', sptTOKEN, [], 'CapabilityStatement.rest.resource.type', sxpNormal);
  indexes.add('CapabilityStatement', 'resource-profile', 'A profile id invoked in a capability statement', sptREFERENCE, ['StructureDefinition'], 'CapabilityStatement.rest.resource.profile', sxpNormal);
  indexes.add('CapabilityStatement', 'security-service', 'OAuth | SMART-on-FHIR | NTLM | Basic | Kerberos | Certificates', sptTOKEN, [], 'CapabilityStatement.rest.security.service', sxpNormal);
  indexes.add('CapabilityStatement', 'software', 'Part of the name of a software application', sptSTRING, [], 'CapabilityStatement.software.name', sxpNormal);
  indexes.add('CapabilityStatement', 'status', '): The current status of the capability statement', sptTOKEN, [], 'CapabilityStatement.status | CodeSystem.status | CompartmentDefinition.status | ConceptMap.status | GraphDefinition.status | ImplementationGuide.status | MessageDefinition.status | NamingSystem.status | OperationDefinition.status | SearchParameter.st'
      +'atus | StructureDefinition.status | StructureMap.status | TerminologyCapabilities.status | ValueSet.status', sxpNormal);
  indexes.add('CapabilityStatement', 'supported-profile', 'Profiles for use cases supported', sptREFERENCE, ['StructureDefinition'], 'CapabilityStatement.rest.resource.supportedProfile', sxpNormal);
  indexes.add('CapabilityStatement', 'title', '): The human-friendly name of the capability statement', sptSTRING, [], 'CapabilityStatement.title | CodeSystem.title | ConceptMap.title | ImplementationGuide.title | MessageDefinition.title | OperationDefinition.title | StructureDefinition.title | StructureMap.title | TerminologyCapabilities.title | ValueSet.title', sxpNormal);
  indexes.add('CapabilityStatement', 'url', '): The uri that identifies the capability statement', sptURI, [], 'CapabilityStatement.url | CodeSystem.url | CompartmentDefinition.url | ConceptMap.url | GraphDefinition.url | ImplementationGuide.url | MessageDefinition.url | NamingSystem.url | OperationDefinition.url | SearchParameter.url | StructureDefinition.url'
      +' | StructureMap.url | TerminologyCapabilities.url | ValueSet.url', sxpNormal);
  indexes.add('CapabilityStatement', 'version', '): The business version of the capability statement', sptTOKEN, [], 'CapabilityStatement.version | CodeSystem.version | CompartmentDefinition.version | ConceptMap.version | GraphDefinition.version | ImplementationGuide.version | MessageDefinition.version | NamingSystem.version | OperationDefinition.version | SearchPar'
      +'ameter.version | StructureDefinition.version | StructureMap.version | TerminologyCapabilities.version | ValueSet.version', sxpNormal);
  indexes.add('CapabilityStatement', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_CAPABILITYSTATEMENT}

{$IFDEF FHIR_CAREPLAN}
procedure TFHIRIndexBuilderR5.buildIndexesForCarePlan(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('CarePlan', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('CarePlan', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('CarePlan', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('CarePlan', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('CarePlan', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('CarePlan', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('CarePlan', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('CarePlan', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('CarePlan', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('CarePlan', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('CarePlan', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('CarePlan', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('CarePlan', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('CarePlan', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('CarePlan', 'activity-code', 'Detail type of activity', sptTOKEN, [], 'CarePlan.activity.plannedActivityDetail.code', sxpNormal);
  indexes.add('CarePlan', 'activity-reference', 'Activity that is intended to be part of the care plan', sptREFERENCE, ['Appointment', 'MedicationRequest', 'Task', 'NutritionOrder', 'RequestOrchestration', 'VisionPrescription', 'DeviceRequest', 'ServiceRequest', 'CommunicationRequest', 'ImmunizationRecommendation'], 'CarePlan.activity.plannedActivityReference', sxpNormal);
  indexes.add('CarePlan', 'activity-scheduled-date', 'Specified date occurs within period specified by CarePlan.activity.plannedActivityDetail.scheduled[x]', sptDATE, [], 'CarePlan.activity.plannedActivityDetail.scheduled.ofType(Timing) | CarePlan.activity.plannedActivityDetail.scheduled.ofType(Period)', sxpNormal);
  indexes.add('CarePlan', 'activity-scheduled-string', 'When activity is to occur', sptSTRING, [], 'CarePlan.activity.plannedActivityDetail.scheduled.ofType(string)', sxpNormal);
  indexes.add('CarePlan', 'based-on', 'Fulfills CarePlan', sptREFERENCE, ['CarePlan'], 'CarePlan.basedOn', sxpNormal);
  indexes.add('CarePlan', 'care-team', 'Who''s involved in plan?', sptREFERENCE, ['CareTeam'], 'CarePlan.careTeam', sxpNormal);
  indexes.add('CarePlan', 'category', 'Type of plan', sptTOKEN, [], 'CarePlan.category', sxpNormal);
  indexes.add('CarePlan', 'condition', 'Reference to a resource (by instance)', sptREFERENCE, [], 'CarePlan.addresses.reference', sxpNormal);
  indexes.add('CarePlan', 'custodian', 'Who is the designated responsible party', sptREFERENCE, ['Practitioner', 'Organization', 'CareTeam', 'Device', 'Patient', 'PractitionerRole', 'RelatedPerson'], 'CarePlan.custodian', sxpNormal);
  indexes.add('CarePlan', 'date', '): Time period plan covers', sptDATE, [], 'AllergyIntolerance.recordedDate | CarePlan.period | ClinicalImpression.date | Composition.date | Consent.dateTime | DiagnosticReport.effective | Encounter.actualPeriod | EpisodeOfCare.period | FamilyMemberHistory.date | Flag.period | (Immunization.oc'
      +'currence as dateTime) | List.date | Observation.effective | Procedure.occurrence | (RiskAssessment.occurrence as dateTime) | SupplyRequest.authoredOn', sxpNormal);
  indexes.add('CarePlan', 'encounter', 'The Encounter during which this CarePlan was created', sptREFERENCE, ['Encounter'], 'CarePlan.encounter', sxpNormal);
  indexes.add('CarePlan', 'goal', 'Desired outcome of plan', sptREFERENCE, ['Goal'], 'CarePlan.goal', sxpNormal);
  indexes.add('CarePlan', 'identifier', '): External Ids for this plan', sptTOKEN, [], 'AllergyIntolerance.identifier | CarePlan.identifier | CareTeam.identifier | Composition.identifier | Condition.identifier | Consent.identifier | DetectedIssue.identifier | DeviceRequest.identifier | DiagnosticReport.identifier | DocumentManifest.mast'
      +'erIdentifier | DocumentManifest.identifier | DocumentReference.identifier | Encounter.identifier | EpisodeOfCare.identifier | FamilyMemberHistory.identifier | Goal.identifier | ImagingStudy.identifier | Immunization.identifier | List.identifi'
      +'er | MedicationAdministration.identifier | MedicationDispense.identifier | MedicationRequest.identifier | MedicationUsage.identifier | NutritionOrder.identifier | Observation.identifier | Procedure.identifier | RiskAssessment.identifier | Ser'
      +'viceRequest.identifier | SupplyDelivery.identifier | SupplyRequest.identifier | VisionPrescription.identifier', sxpNormal);
  indexes.add('CarePlan', 'instantiates-canonical', 'Instantiates FHIR protocol or definition', sptREFERENCE, ['Questionnaire', 'Measure', 'PlanDefinition', 'OperationDefinition', 'ActivityDefinition'], 'CarePlan.instantiatesCanonical', sxpNormal);
  indexes.add('CarePlan', 'instantiates-uri', 'Instantiates external protocol or definition', sptURI, [], 'CarePlan.instantiatesUri', sxpNormal);
  indexes.add('CarePlan', 'intent', 'proposal | plan | order | option | directive', sptTOKEN, [], 'CarePlan.intent', sxpNormal);
  indexes.add('CarePlan', 'part-of', 'Part of referenced CarePlan', sptREFERENCE, ['CarePlan'], 'CarePlan.partOf', sxpNormal);
  indexes.add('CarePlan', 'patient', '): Who the care plan is for', sptREFERENCE, ['Patient', 'Practitioner', 'Group', 'Organization', 'BiologicallyDerivedProduct', 'NutritionProduct', 'Device', 'Medication', 'Procedure', 'Substance', 'Location'], 'AllergyIntolerance.patient | CarePlan.subject.where(resolve() is Patient) | CareTeam.subject.where(resolve() is Patient) | ClinicalImpression.subject.where(resolve() is Patient) | Composition.subject.where(resolve() is Patient) | Condition.subject.wh'
      +'ere(resolve() is Patient) | Consent.subject.where(resolve() is Patient) | DetectedIssue.subject | DeviceRequest.subject.where(resolve() is Patient) | DeviceUsage.patient | DiagnosticReport.subject.where(resolve() is Patient) | DocumentManifes'
      +'t.subject.where(resolve() is Patient) | DocumentReference.subject.where(resolve() is Patient) | Encounter.subject.where(resolve() is Patient) | EpisodeOfCare.patient | FamilyMemberHistory.patient | Flag.subject.where(resolve() is Patient'+
   ') | G'
      +'oal.subject.where(resolve() is Patient) | ImagingStudy.subject.where(resolve() is Patient) | Immunization.patient | List.subject.where(resolve() is Patient) | MedicationAdministration.subject.where(resolve() is Patient) | MedicationDispense.s'
      +'ubject.where(resolve() is Patient) | MedicationRequest.subject.where(resolve() is Patient) | MedicationUsage.subject.where(resolve() is Patient) | NutritionOrder.subject.where(resolve() is Patient) | Observation.subject.where(resolve() is Pat'
      +'ient) | Procedure.subject.where(resolve() is Patient) | RiskAssessment.subject.where(resolve() is Patient) | ServiceRequest.subject.where(resolve() is Patient) | SupplyDelivery.patient | VisionPrescription.patient', sxpNormal);
  indexes.add('CarePlan', 'performer', 'Matches if the practitioner is listed as a performer in any of the "simple" activities.  (For performers of the detailed activities, chain through the activitydetail search parameter.)', sptREFERENCE, ['Practitioner', 'Organization', 'CareTeam', 'Device', 'Patient', 'HealthcareService', 'PractitionerRole', 'RelatedPerson'], 'CarePlan.activity.plannedActivityDetail.performer', sxpNormal);
  indexes.add('CarePlan', 'replaces', 'CarePlan replaced by this CarePlan', sptREFERENCE, ['CarePlan'], 'CarePlan.replaces', sxpNormal);
  indexes.add('CarePlan', 'status', 'draft | active | on-hold | revoked | completed | entered-in-error | unknown', sptTOKEN, [], 'CarePlan.status', sxpNormal);
  indexes.add('CarePlan', 'subject', 'Who the care plan is for', sptREFERENCE, ['Group', 'Patient'], 'CarePlan.subject', sxpNormal);
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
  indexes.add('CareTeam', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('CareTeam', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('CareTeam', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('CareTeam', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('CareTeam', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('CareTeam', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('CareTeam', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('CareTeam', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('CareTeam', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('CareTeam', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('CareTeam', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('CareTeam', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('CareTeam', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('CareTeam', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('CareTeam', 'category', 'Type of team', sptTOKEN, [], 'CareTeam.category', sxpNormal);
  indexes.add('CareTeam', 'date', '): A date within the coverage time period.', sptDATE, [], 'AllergyIntolerance.recordedDate | CarePlan.period | ClinicalImpression.date | Composition.date | Consent.dateTime | DiagnosticReport.effective | Encounter.actualPeriod | EpisodeOfCare.period | FamilyMemberHistory.date | Flag.period | (Immunization.oc'
      +'currence as dateTime) | List.date | Observation.effective | Procedure.occurrence | (RiskAssessment.occurrence as dateTime) | SupplyRequest.authoredOn', sxpNormal);
  indexes.add('CareTeam', 'identifier', '): External Ids for this team', sptTOKEN, [], 'AllergyIntolerance.identifier | CarePlan.identifier | CareTeam.identifier | Composition.identifier | Condition.identifier | Consent.identifier | DetectedIssue.identifier | DeviceRequest.identifier | DiagnosticReport.identifier | DocumentManifest.mast'
      +'erIdentifier | DocumentManifest.identifier | DocumentReference.identifier | Encounter.identifier | EpisodeOfCare.identifier | FamilyMemberHistory.identifier | Goal.identifier | ImagingStudy.identifier | Immunization.identifier | List.identifi'
      +'er | MedicationAdministration.identifier | MedicationDispense.identifier | MedicationRequest.identifier | MedicationUsage.identifier | NutritionOrder.identifier | Observation.identifier | Procedure.identifier | RiskAssessment.identifier | Ser'
      +'viceRequest.identifier | SupplyDelivery.identifier | SupplyRequest.identifier | VisionPrescription.identifier', sxpNormal);
  indexes.add('CareTeam', 'name', 'Name of the team, such as crisis assessment team', sptSTRING, [], 'CareTeam.name | CareTeam.extension(''http://hl7.org/fhir/StructureDefinition/careteam-alias'').value', sxpNormal);
  indexes.add('CareTeam', 'participant', 'Who is involved', sptREFERENCE, ['Practitioner', 'Organization', 'CareTeam', 'Patient', 'PractitionerRole', 'RelatedPerson'], 'CareTeam.participant.member', sxpNormal);
  indexes.add('CareTeam', 'patient', '): Who care team is for', sptREFERENCE, ['Patient', 'Practitioner', 'Group', 'Organization', 'BiologicallyDerivedProduct', 'NutritionProduct', 'Device', 'Medication', 'Procedure', 'Substance', 'Location'], 'AllergyIntolerance.patient | CarePlan.subject.where(resolve() is Patient) | CareTeam.subject.where(resolve() is Patient) | ClinicalImpression.subject.where(resolve() is Patient) | Composition.subject.where(resolve() is Patient) | Condition.subject.wh'
      +'ere(resolve() is Patient) | Consent.subject.where(resolve() is Patient) | DetectedIssue.subject | DeviceRequest.subject.where(resolve() is Patient) | DeviceUsage.patient | DiagnosticReport.subject.where(resolve() is Patient) | DocumentManifes'
      +'t.subject.where(resolve() is Patient) | DocumentReference.subject.where(resolve() is Patient) | Encounter.subject.where(resolve() is Patient) | EpisodeOfCare.patient | FamilyMemberHistory.patient | Flag.subject.where(resolve() is Patient) | '+
   'G'
      +'oal.subject.where(resolve() is Patient) | ImagingStudy.subject.where(resolve() is Patient) | Immunization.patient | List.subject.where(resolve() is Patient) | MedicationAdministration.subject.where(resolve() is Patient) | MedicationDispense.s'
      +'ubject.where(resolve() is Patient) | MedicationRequest.subject.where(resolve() is Patient) | MedicationUsage.subject.where(resolve() is Patient) | NutritionOrder.subject.where(resolve() is Patient) | Observation.subject.where(resolve() is Pat'
      +'ient) | Procedure.subject.where(resolve() is Patient) | RiskAssessment.subject.where(resolve() is Patient) | ServiceRequest.subject.where(resolve() is Patient) | SupplyDelivery.patient | VisionPrescription.patient', sxpNormal);
  indexes.add('CareTeam', 'status', 'proposed | active | suspended | inactive | entered-in-error', sptTOKEN, [], 'CareTeam.status', sxpNormal);
  indexes.add('CareTeam', 'subject', 'Who care team is for', sptREFERENCE, ['Group', 'Patient'], 'CareTeam.subject', sxpNormal);
  indexes.add('CareTeam', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Patient', 'CareTeam', ['patient', 'participant']);
  compartments.register('Practitioner', 'CareTeam', ['participant']);
  compartments.register('RelatedPerson', 'CareTeam', ['participant']);
end;
{$ENDIF FHIR_CARETEAM}

{$IFDEF FHIR_CHARGEITEM}
procedure TFHIRIndexBuilderR5.buildIndexesForChargeItem(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ChargeItem', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('ChargeItem', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('ChargeItem', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('ChargeItem', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('ChargeItem', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('ChargeItem', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('ChargeItem', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('ChargeItem', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('ChargeItem', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('ChargeItem', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('ChargeItem', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('ChargeItem', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('ChargeItem', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('ChargeItem', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('ChargeItem', 'account', 'Account to place this charge', sptREFERENCE, ['Account'], 'ChargeItem.account', sxpNormal);
  indexes.add('ChargeItem', 'code', 'A code that identifies the charge, like a billing code', sptTOKEN, [], 'ChargeItem.code', sxpNormal);
  indexes.add('ChargeItem', 'encounter', 'Encounter associated with event', sptREFERENCE, ['Encounter'], 'ChargeItem.encounter', sxpNormal);
  indexes.add('ChargeItem', 'entered-date', 'Date the charge item was entered', sptDATE, [], 'ChargeItem.enteredDate', sxpNormal);
  indexes.add('ChargeItem', 'enterer', 'Individual who was entering', sptREFERENCE, ['Practitioner', 'Organization', 'Device', 'Patient', 'PractitionerRole', 'RelatedPerson'], 'ChargeItem.enterer', sxpNormal);
  indexes.add('ChargeItem', 'factor-override', 'Factor overriding the associated rules', sptNUMBER, [], 'ChargeItem.totalPriceComponent.factor', sxpNormal);
  indexes.add('ChargeItem', 'identifier', 'Business Identifier for item', sptTOKEN, [], 'ChargeItem.identifier', sxpNormal);
  indexes.add('ChargeItem', 'occurrence', 'When the charged service was applied', sptDATE, [], 'ChargeItem.occurrence', sxpNormal);
  indexes.add('ChargeItem', 'patient', 'Individual service was done for/to', sptREFERENCE, ['Patient'], 'ChargeItem.subject.where(resolve() is Patient)', sxpNormal);
  indexes.add('ChargeItem', 'performer-actor', 'Individual who was performing', sptREFERENCE, ['Practitioner', 'Organization', 'CareTeam', 'Device', 'Patient', 'HealthcareService', 'PractitionerRole', 'RelatedPerson'], 'ChargeItem.performer.actor', sxpNormal);
  indexes.add('ChargeItem', 'performer-function', 'What type of performance was done', sptTOKEN, [], 'ChargeItem.performer.function', sxpNormal);
  indexes.add('ChargeItem', 'performing-organization', 'Organization providing the charged service', sptREFERENCE, ['Organization'], 'ChargeItem.performingOrganization', sxpNormal);
  indexes.add('ChargeItem', 'price-override', 'Price overriding the associated rules', sptQUANTITY, [], 'ChargeItem.totalPriceComponent.amount', sxpNormal);
  indexes.add('ChargeItem', 'quantity', 'Quantity of which the charge item has been serviced', sptQUANTITY, [], 'ChargeItem.quantity', sxpNormal);
  indexes.add('ChargeItem', 'requesting-organization', 'Organization requesting the charged service', sptREFERENCE, ['Organization'], 'ChargeItem.requestingOrganization', sxpNormal);
  indexes.add('ChargeItem', 'service', 'Which rendered service is being charged?', sptREFERENCE, [], 'ChargeItem.service.reference', sxpNormal);
  indexes.add('ChargeItem', 'subject', 'Individual service was done for/to', sptREFERENCE, ['Group', 'Patient'], 'ChargeItem.subject', sxpNormal);
  indexes.add('ChargeItem', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Device', 'ChargeItem', ['enterer', 'performer-actor']);
  compartments.register('Encounter', 'ChargeItem', ['encounter']);
  compartments.register('Patient', 'ChargeItem', ['subject']);
  compartments.register('Practitioner', 'ChargeItem', ['enterer', 'performer-actor']);
  compartments.register('RelatedPerson', 'ChargeItem', ['enterer', 'performer-actor']);
end;
{$ENDIF FHIR_CHARGEITEM}

{$IFDEF FHIR_CHARGEITEMDEFINITION}
procedure TFHIRIndexBuilderR5.buildIndexesForChargeItemDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ChargeItemDefinition', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('ChargeItemDefinition', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('ChargeItemDefinition', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('ChargeItemDefinition', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('ChargeItemDefinition', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('ChargeItemDefinition', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('ChargeItemDefinition', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('ChargeItemDefinition', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('ChargeItemDefinition', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('ChargeItemDefinition', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('ChargeItemDefinition', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('ChargeItemDefinition', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('ChargeItemDefinition', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('ChargeItemDefinition', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('ChargeItemDefinition', 'context', 'A use context assigned to the charge item definition', sptTOKEN, [], '(ChargeItemDefinition.useContext.value as CodeableConcept)', sxpNormal);
  indexes.add('ChargeItemDefinition', 'context-quantity', 'A quantity- or range-valued use context assigned to the charge item definition', sptQUANTITY, [], '(ChargeItemDefinition.useContext.value as Quantity) | (ChargeItemDefinition.useContext.value as Range)', sxpNormal);
  indexes.add('ChargeItemDefinition', 'context-type', 'A type of use context assigned to the charge item definition', sptTOKEN, [], 'ChargeItemDefinition.useContext.code', sxpNormal);
  indexes.add('ChargeItemDefinition', 'context-type-quantity', 'A use context type and quantity- or range-based value assigned to the charge item definition', sptCOMPOSITE, [], 'ChargeItemDefinition.useContext', sxpNormal);
  indexes.add('ChargeItemDefinition', 'context-type-value', 'A use context type and value assigned to the charge item definition', sptCOMPOSITE, [], 'ChargeItemDefinition.useContext', sxpNormal);
  indexes.add('ChargeItemDefinition', 'date', 'The charge item definition publication date', sptDATE, [], 'ChargeItemDefinition.date', sxpNormal);
  indexes.add('ChargeItemDefinition', 'description', 'The description of the charge item definition', sptSTRING, [], 'ChargeItemDefinition.description', sxpNormal);
  indexes.add('ChargeItemDefinition', 'effective', 'The time during which the charge item definition is intended to be in use', sptDATE, [], 'ChargeItemDefinition.applicability.effectivePeriod', sxpNormal);
  indexes.add('ChargeItemDefinition', 'identifier', 'External identifier for the charge item definition', sptTOKEN, [], 'ChargeItemDefinition.identifier', sxpNormal);
  indexes.add('ChargeItemDefinition', 'jurisdiction', 'Intended jurisdiction for the charge item definition', sptTOKEN, [], 'ChargeItemDefinition.jurisdiction', sxpNormal);
  indexes.add('ChargeItemDefinition', 'publisher', 'Name of the publisher of the charge item definition', sptSTRING, [], 'ChargeItemDefinition.publisher', sxpNormal);
  indexes.add('ChargeItemDefinition', 'status', 'The current status of the charge item definition', sptTOKEN, [], 'ChargeItemDefinition.status', sxpNormal);
  indexes.add('ChargeItemDefinition', 'title', 'The human-friendly name of the charge item definition', sptSTRING, [], 'ChargeItemDefinition.title', sxpNormal);
  indexes.add('ChargeItemDefinition', 'url', 'The uri that identifies the charge item definition', sptURI, [], 'ChargeItemDefinition.url', sxpNormal);
  indexes.add('ChargeItemDefinition', 'version', 'The business version of the charge item definition', sptTOKEN, [], 'ChargeItemDefinition.version', sxpNormal);
  indexes.add('ChargeItemDefinition', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_CHARGEITEMDEFINITION}

{$IFDEF FHIR_CITATION}
procedure TFHIRIndexBuilderR5.buildIndexesForCitation(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Citation', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('Citation', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('Citation', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('Citation', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('Citation', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('Citation', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('Citation', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('Citation', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('Citation', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('Citation', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('Citation', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('Citation', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('Citation', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('Citation', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('Citation', 'context', 'A use context assigned to the citation', sptTOKEN, [], '(Citation.useContext.value as CodeableConcept)', sxpNormal);
  indexes.add('Citation', 'context-quantity', 'A quantity- or range-valued use context assigned to the citation', sptQUANTITY, [], '(Citation.useContext.value as Quantity) | (Citation.useContext.value as Range)', sxpNormal);
  indexes.add('Citation', 'context-type', 'A type of use context assigned to the citation', sptTOKEN, [], 'Citation.useContext.code', sxpNormal);
  indexes.add('Citation', 'context-type-quantity', 'A use context type and quantity- or range-based value assigned to the citation', sptCOMPOSITE, [], 'Citation.useContext', sxpNormal);
  indexes.add('Citation', 'context-type-value', 'A use context type and value assigned to the citation', sptCOMPOSITE, [], 'Citation.useContext', sxpNormal);
  indexes.add('Citation', 'date', 'The citation publication date', sptDATE, [], 'Citation.date', sxpNormal);
  indexes.add('Citation', 'description', 'The description of the citation', sptSTRING, [], 'Citation.description', sxpNormal);
  indexes.add('Citation', 'effective', 'The time during which the citation is intended to be in use', sptDATE, [], 'Citation.effectivePeriod', sxpNormal);
  indexes.add('Citation', 'identifier', 'External identifier for the citation', sptTOKEN, [], 'Citation.identifier', sxpNormal);
  indexes.add('Citation', 'jurisdiction', 'Intended jurisdiction for the citation', sptTOKEN, [], 'Citation.jurisdiction', sxpNormal);
  indexes.add('Citation', 'name', 'Computationally friendly name of the citation', sptSTRING, [], 'Citation.name', sxpNormal);
  indexes.add('Citation', 'publisher', 'Name of the publisher of the citation', sptSTRING, [], 'Citation.publisher', sxpNormal);
  indexes.add('Citation', 'status', 'The current status of the citation', sptTOKEN, [], 'Citation.status', sxpNormal);
  indexes.add('Citation', 'title', 'The human-friendly name of the citation', sptSTRING, [], 'Citation.title', sxpNormal);
  indexes.add('Citation', 'url', 'The uri that identifies the citation', sptURI, [], 'Citation.url', sxpNormal);
  indexes.add('Citation', 'version', 'The business version of the citation', sptTOKEN, [], 'Citation.version', sxpNormal);
  indexes.add('Citation', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_CITATION}

{$IFDEF FHIR_CLAIM}
procedure TFHIRIndexBuilderR5.buildIndexesForClaim(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Claim', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('Claim', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('Claim', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('Claim', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('Claim', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('Claim', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('Claim', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('Claim', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('Claim', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('Claim', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('Claim', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('Claim', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('Claim', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('Claim', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('Claim', 'care-team', 'Member of the CareTeam', sptREFERENCE, ['Practitioner', 'Organization', 'PractitionerRole'], 'Claim.careTeam.provider', sxpNormal);
  indexes.add('Claim', 'created', 'The creation date for the Claim', sptDATE, [], 'Claim.created', sxpNormal);
  indexes.add('Claim', 'detail-udi', 'UDI associated with a line item, detail product or service', sptREFERENCE, ['Device'], 'Claim.item.detail.udi', sxpNormal);
  indexes.add('Claim', 'encounter', 'Encounters associated with a billed line item', sptREFERENCE, ['Encounter'], 'Claim.item.encounter', sxpNormal);
  indexes.add('Claim', 'enterer', 'The party responsible for the entry of the Claim', sptREFERENCE, ['Practitioner', 'Patient', 'PractitionerRole', 'RelatedPerson'], 'Claim.enterer', sxpNormal);
  indexes.add('Claim', 'facility', 'Facility where the products or services have been or will be provided', sptREFERENCE, ['Organization', 'Location'], 'Claim.facility', sxpNormal);
  indexes.add('Claim', 'identifier', 'The primary identifier of the financial resource', sptTOKEN, [], 'Claim.identifier', sxpNormal);
  indexes.add('Claim', 'insurer', 'The target payor/insurer for the Claim', sptREFERENCE, ['Organization'], 'Claim.insurer', sxpNormal);
  indexes.add('Claim', 'item-udi', 'UDI associated with a line item product or service', sptREFERENCE, ['Device'], 'Claim.item.udi', sxpNormal);
  indexes.add('Claim', 'patient', 'Patient receiving the products or services', sptREFERENCE, ['Patient'], 'Claim.patient', sxpNormal);
  indexes.add('Claim', 'payee', 'The party receiving any payment for the Claim', sptREFERENCE, ['Practitioner', 'Organization', 'Patient', 'PractitionerRole', 'RelatedPerson'], 'Claim.payee.party', sxpNormal);
  indexes.add('Claim', 'priority', 'Processing priority requested', sptTOKEN, [], 'Claim.priority', sxpNormal);
  indexes.add('Claim', 'procedure-udi', 'UDI associated with a procedure', sptREFERENCE, ['Device'], 'Claim.procedure.udi', sxpNormal);
  indexes.add('Claim', 'provider', 'Provider responsible for the Claim', sptREFERENCE, ['Practitioner', 'Organization', 'PractitionerRole'], 'Claim.provider', sxpNormal);
  indexes.add('Claim', 'status', 'The status of the Claim instance.', sptTOKEN, [], 'Claim.status', sxpNormal);
  indexes.add('Claim', 'subdetail-udi', 'UDI associated with a line item, detail, subdetail product or service', sptREFERENCE, ['Device'], 'Claim.item.detail.subDetail.udi', sxpNormal);
  indexes.add('Claim', 'use', 'The kind of financial resource', sptTOKEN, [], 'Claim.use', sxpNormal);
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
  indexes.add('ClaimResponse', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('ClaimResponse', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('ClaimResponse', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('ClaimResponse', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('ClaimResponse', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('ClaimResponse', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('ClaimResponse', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('ClaimResponse', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('ClaimResponse', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('ClaimResponse', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('ClaimResponse', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('ClaimResponse', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('ClaimResponse', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('ClaimResponse', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('ClaimResponse', 'created', 'The creation date', sptDATE, [], 'ClaimResponse.created', sxpNormal);
  indexes.add('ClaimResponse', 'disposition', 'The contents of the disposition message', sptSTRING, [], 'ClaimResponse.disposition', sxpNormal);
  indexes.add('ClaimResponse', 'identifier', 'The identity of the ClaimResponse', sptTOKEN, [], 'ClaimResponse.identifier', sxpNormal);
  indexes.add('ClaimResponse', 'insurer', 'The organization which generated this resource', sptREFERENCE, ['Organization'], 'ClaimResponse.insurer', sxpNormal);
  indexes.add('ClaimResponse', 'outcome', 'The processing outcome', sptTOKEN, [], 'ClaimResponse.outcome', sxpNormal);
  indexes.add('ClaimResponse', 'patient', 'The subject of care', sptREFERENCE, ['Patient'], 'ClaimResponse.patient', sxpNormal);
  indexes.add('ClaimResponse', 'payment-date', 'The expected payment date', sptDATE, [], 'ClaimResponse.payment.date', sxpNormal);
  indexes.add('ClaimResponse', 'request', 'The claim reference', sptREFERENCE, ['Claim'], 'ClaimResponse.request', sxpNormal);
  indexes.add('ClaimResponse', 'requestor', 'The Provider of the claim', sptREFERENCE, ['Practitioner', 'Organization', 'PractitionerRole'], 'ClaimResponse.requestor', sxpNormal);
  indexes.add('ClaimResponse', 'status', 'The status of the ClaimResponse', sptTOKEN, [], 'ClaimResponse.status', sxpNormal);
  indexes.add('ClaimResponse', 'use', 'The type of claim', sptTOKEN, [], 'ClaimResponse.use', sxpNormal);
  indexes.add('ClaimResponse', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Patient', 'ClaimResponse', ['patient']);
  compartments.register('Practitioner', 'ClaimResponse', ['requestor']);
end;
{$ENDIF FHIR_CLAIMRESPONSE}

{$IFDEF FHIR_CLINICALIMPRESSION}
procedure TFHIRIndexBuilderR5.buildIndexesForClinicalImpression(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ClinicalImpression', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('ClinicalImpression', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('ClinicalImpression', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('ClinicalImpression', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('ClinicalImpression', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('ClinicalImpression', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('ClinicalImpression', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('ClinicalImpression', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('ClinicalImpression', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('ClinicalImpression', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('ClinicalImpression', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('ClinicalImpression', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('ClinicalImpression', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('ClinicalImpression', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('ClinicalImpression', 'date', '): When the assessment was documented', sptDATE, [], 'AllergyIntolerance.recordedDate | CarePlan.period | ClinicalImpression.date | Composition.date | Consent.dateTime | DiagnosticReport.effective | Encounter.actualPeriod | EpisodeOfCare.period | FamilyMemberHistory.date | Flag.period | (Immunization.oc'
      +'currence as dateTime) | List.date | Observation.effective | Procedure.occurrence | (RiskAssessment.occurrence as dateTime) | SupplyRequest.authoredOn', sxpNormal);
  indexes.add('ClinicalImpression', 'encounter', 'The Encounter during which this ClinicalImpression was created', sptREFERENCE, ['Encounter'], 'ClinicalImpression.encounter', sxpNormal);
  indexes.add('ClinicalImpression', 'finding-code', 'Reference to a concept (by class)', sptTOKEN, [], 'ClinicalImpression.finding.item.concept', sxpNormal);
  indexes.add('ClinicalImpression', 'finding-ref', 'Reference to a resource (by instance)', sptREFERENCE, [], 'ClinicalImpression.finding.item.reference', sxpNormal);
  indexes.add('ClinicalImpression', 'identifier', 'Business identifier', sptTOKEN, [], 'ClinicalImpression.identifier', sxpNormal);
  indexes.add('ClinicalImpression', 'patient', '): Patient assessed', sptREFERENCE, ['Patient', 'Practitioner', 'Group', 'Organization', 'BiologicallyDerivedProduct', 'NutritionProduct', 'Device', 'Medication', 'Procedure', 'Substance', 'Location'], 'AllergyIntolerance.patient | CarePlan.subject.where(resolve() is Patient) | CareTeam.subject.where(resolve() is Patient) | ClinicalImpression.subject.where(resolve() is Patient) | Composition.subject.where(resolve() is Patient) | Condition.subject.wh'
      +'ere(resolve() is Patient) | Consent.subject.where(resolve() is Patient) | DetectedIssue.subject | DeviceRequest.subject.where(resolve() is Patient) | DeviceUsage.patient | DiagnosticReport.subject.where(resolve() is Patient) | DocumentManifes'
      +'t.subject.where(resolve() is Patient) | DocumentReference.subject.where(resolve() is Patient) | Encounter.subject.where(resolve() is Patient) | EpisodeOfCare.patient | FamilyMemberHistory.patient | Flag.subject.where(resolve() is Patie'+
   'nt) | G'
      +'oal.subject.where(resolve() is Patient) | ImagingStudy.subject.where(resolve() is Patient) | Immunization.patient | List.subject.where(resolve() is Patient) | MedicationAdministration.subject.where(resolve() is Patient) | MedicationDispense.s'
      +'ubject.where(resolve() is Patient) | MedicationRequest.subject.where(resolve() is Patient) | MedicationUsage.subject.where(resolve() is Patient) | NutritionOrder.subject.where(resolve() is Patient) | Observation.subject.where(resolve() is Pat'
      +'ient) | Procedure.subject.where(resolve() is Patient) | RiskAssessment.subject.where(resolve() is Patient) | ServiceRequest.subject.where(resolve() is Patient) | SupplyDelivery.patient | VisionPrescription.patient', sxpNormal);
  indexes.add('ClinicalImpression', 'performer', 'The clinician performing the assessment', sptREFERENCE, ['Practitioner', 'PractitionerRole'], 'ClinicalImpression.performer', sxpNormal);
  indexes.add('ClinicalImpression', 'previous', 'Reference to last assessment', sptREFERENCE, ['ClinicalImpression'], 'ClinicalImpression.previous', sxpNormal);
  indexes.add('ClinicalImpression', 'problem', 'Relevant impressions of patient state', sptREFERENCE, ['Condition', 'AllergyIntolerance'], 'ClinicalImpression.problem', sxpNormal);
  indexes.add('ClinicalImpression', 'status', 'preparation | in-progress | not-done | on-hold | stopped | completed | entered-in-error | unknown', sptTOKEN, [], 'ClinicalImpression.status', sxpNormal);
  indexes.add('ClinicalImpression', 'subject', 'Patient or group assessed', sptREFERENCE, ['Group', 'Patient'], 'ClinicalImpression.subject', sxpNormal);
  indexes.add('ClinicalImpression', 'supporting-info', 'Information supporting the clinical impression', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'ClinicalImpression.supportingInfo', sxpNormal);
  indexes.add('ClinicalImpression', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Encounter', 'ClinicalImpression', ['encounter']);
  compartments.register('Patient', 'ClinicalImpression', ['subject']);
  compartments.register('Practitioner', 'ClinicalImpression', ['performer']);
end;
{$ENDIF FHIR_CLINICALIMPRESSION}

{$IFDEF FHIR_CLINICALUSEDEFINITION}
procedure TFHIRIndexBuilderR5.buildIndexesForClinicalUseDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ClinicalUseDefinition', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('ClinicalUseDefinition', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('ClinicalUseDefinition', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('ClinicalUseDefinition', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('ClinicalUseDefinition', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('ClinicalUseDefinition', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('ClinicalUseDefinition', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('ClinicalUseDefinition', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('ClinicalUseDefinition', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('ClinicalUseDefinition', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('ClinicalUseDefinition', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('ClinicalUseDefinition', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('ClinicalUseDefinition', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('ClinicalUseDefinition', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('ClinicalUseDefinition', 'contraindication', 'The situation that is being documented as contraindicating against this item, as a code', sptTOKEN, [], 'ClinicalUseDefinition.contraindication.diseaseSymptomProcedure', sxpNormal);
  indexes.add('ClinicalUseDefinition', 'contraindication-reference', 'The situation that is being documented as contraindicating against this item, as a reference', sptREFERENCE, [], 'ClinicalUseDefinition.contraindication.diseaseSymptomProcedure', sxpNormal);
  indexes.add('ClinicalUseDefinition', 'effect', 'The situation in which the undesirable effect may manifest, as a code', sptTOKEN, [], 'ClinicalUseDefinition.undesirableEffect.symptomConditionEffect', sxpNormal);
  indexes.add('ClinicalUseDefinition', 'effect-reference', 'The situation in which the undesirable effect may manifest, as a reference', sptREFERENCE, [], 'ClinicalUseDefinition.undesirableEffect.symptomConditionEffect', sxpNormal);
  indexes.add('ClinicalUseDefinition', 'identifier', 'Business identifier for this issue', sptTOKEN, [], 'ClinicalUseDefinition.identifier', sxpNormal);
  indexes.add('ClinicalUseDefinition', 'indication', 'The situation that is being documented as an indicaton for this item, as a code', sptTOKEN, [], 'ClinicalUseDefinition.indication.diseaseSymptomProcedure', sxpNormal);
  indexes.add('ClinicalUseDefinition', 'indication-reference', 'The situation that is being documented as an indicaton for this item, as a reference', sptREFERENCE, [], 'ClinicalUseDefinition.indication.diseaseSymptomProcedure', sxpNormal);
  indexes.add('ClinicalUseDefinition', 'interaction', 'The type of the interaction e.g. drug-drug interaction, drug-food interaction, drug-lab test interaction', sptTOKEN, [], 'ClinicalUseDefinition.interaction.type', sxpNormal);
  indexes.add('ClinicalUseDefinition', 'product', 'The medicinal product for which this is a clinical usage issue', sptREFERENCE, ['MedicinalProductDefinition'], 'ClinicalUseDefinition.subject.where(resolve() is MedicinalProductDefinition)', sxpNormal);
  indexes.add('ClinicalUseDefinition', 'status', 'Whether this is a current issue or one that has been retired etc', sptTOKEN, [], 'ClinicalUseDefinition.status', sxpNormal);
  indexes.add('ClinicalUseDefinition', 'subject', 'The resource for which this is a clinical usage issue', sptREFERENCE, ['MedicinalProductDefinition', 'Device', 'Medication', 'DeviceDefinition', 'PlanDefinition', 'Substance', 'ActivityDefinition'], 'ClinicalUseDefinition.subject', sxpNormal);
  indexes.add('ClinicalUseDefinition', 'type', 'indication | contraindication | interaction | undesirable-effect | warning', sptTOKEN, [], 'ClinicalUseDefinition.type', sxpNormal);
  indexes.add('ClinicalUseDefinition', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_CLINICALUSEDEFINITION}

{$IFDEF FHIR_CODESYSTEM}
procedure TFHIRIndexBuilderR5.buildIndexesForCodeSystem(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('CodeSystem', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('CodeSystem', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('CodeSystem', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('CodeSystem', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('CodeSystem', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('CodeSystem', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('CodeSystem', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('CodeSystem', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('CodeSystem', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('CodeSystem', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('CodeSystem', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('CodeSystem', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('CodeSystem', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('CodeSystem', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('CodeSystem', 'author', 'Optional Extensions Element', sptSTRING, [], 'CodeSystem.extension(''http://hl7.org/fhir/StructureDefinition/codesystem-author'').value', sxpNormal);
  indexes.add('CodeSystem', 'code', 'A code defined in the code system', sptTOKEN, [], 'CodeSystem.concept.code', sxpNormal);
  indexes.add('CodeSystem', 'content-mode', 'not-present | example | fragment | complete | supplement', sptTOKEN, [], 'CodeSystem.content', sxpNormal);
  indexes.add('CodeSystem', 'context', '): A use context assigned to the code system', sptTOKEN, [], '(CapabilityStatement.useContext.value as CodeableConcept) | (CodeSystem.useContext.value as CodeableConcept) | (CompartmentDefinition.useContext.value as CodeableConcept) | (ConceptMap.useContext.value as CodeableConcept) | (GraphDefinition.useContex'
      +'t.value as CodeableConcept) | (ImplementationGuide.useContext.value as CodeableConcept) | (MessageDefinition.useContext.value as CodeableConcept) | (NamingSystem.useContext.value as CodeableConcept) | (OperationDefinition.useContext.value as '
      +'CodeableConcept) | (SearchParameter.useContext.value as CodeableConcept) | (StructureDefinition.useContext.value as CodeableConcept) | (StructureMap.useContext.value as CodeableConcept) | (TerminologyCapabilities.useContext.value as CodeableC'
      +'oncept) | (ValueSet.useContext.value as CodeableConcept)', sxpNormal);
  indexes.add('CodeSystem', 'context-quantity', '): A quantity- or range-valued use context assigned to the code system', sptQUANTITY, [], '(CapabilityStatement.useContext.value as Quantity) | (CapabilityStatement.useContext.value as Range) | (CodeSystem.useContext.value as Quantity) | (CodeSystem.useContext.value as Range) | (CompartmentDefinition.useContext.value as Quantity) | (Compar'
      +'tmentDefinition.useContext.value as Range) | (ConceptMap.useContext.value as Quantity) | (ConceptMap.useContext.value as Range) | (GraphDefinition.useContext.value as Quantity) | (GraphDefinition.useContext.value as Range) | (ImplementationGu'
      +'ide.useContext.value as Quantity) | (ImplementationGuide.useContext.value as Range) | (MessageDefinition.useContext.value as Quantity) | (MessageDefinition.useContext.value as Range) | (NamingSystem.useContext.value as Quantity) | (NamingSyst'
      +'em.useContext.value as Range) | (OperationDefinition.useContext.value as Quantity) | (Operati'+
   'onDefinition.useContext.value as Range) | (SearchParameter.useContext.value as Quantity) | (SearchParameter.useContext.value as Range) | (StructureDe'
      +'finition.useContext.value as Quantity) | (StructureDefinition.useContext.value as Range) | (StructureMap.useContext.value as Quantity) | (StructureMap.useContext.value as Range) | (TerminologyCapabilities.useContext.value as Quantity) | (Term'
      +'inologyCapabilities.useContext.value as Range) | (ValueSet.useContext.value as Quantity) | (ValueSet.useContext.value as Range)', sxpNormal);
  indexes.add('CodeSystem', 'context-type', '): A type of use context assigned to the code system', sptTOKEN, [], 'CapabilityStatement.useContext.code | CodeSystem.useContext.code | CompartmentDefinition.useContext.code | ConceptMap.useContext.code | GraphDefinition.useContext.code | ImplementationGuide.useContext.code | MessageDefinition.useContext.code | Naming'
      +'System.useContext.code | OperationDefinition.useContext.code | SearchParameter.useContext.code | StructureDefinition.useContext.code | StructureMap.useContext.code | TerminologyCapabilities.useContext.code | ValueSet.useContext.code', sxpNormal);
  indexes.add('CodeSystem', 'context-type-quantity', '): A use context type and quantity- or range-based value assigned to the code system', sptCOMPOSITE, [], 'CapabilityStatement.useContext | CodeSystem.useContext | CompartmentDefinition.useContext | ConceptMap.useContext | GraphDefinition.useContext | ImplementationGuide.useContext | MessageDefinition.useContext | NamingSystem.useContext | OperationDefini'
      +'tion.useContext | SearchParameter.useContext | StructureDefinition.useContext | StructureMap.useContext | TerminologyCapabilities.useContext | ValueSet.useContext', sxpNormal);
  indexes.add('CodeSystem', 'context-type-value', '): A use context type and value assigned to the code system', sptCOMPOSITE, [], 'CapabilityStatement.useContext | CodeSystem.useContext | CompartmentDefinition.useContext | ConceptMap.useContext | GraphDefinition.useContext | ImplementationGuide.useContext | MessageDefinition.useContext | NamingSystem.useContext | OperationDefini'
      +'tion.useContext | SearchParameter.useContext | StructureDefinition.useContext | StructureMap.useContext | TerminologyCapabilities.useContext | ValueSet.useContext', sxpNormal);
  indexes.add('CodeSystem', 'date', '): The code system publication date', sptDATE, [], 'CapabilityStatement.date | CodeSystem.date | CompartmentDefinition.date | ConceptMap.date | GraphDefinition.date | ImplementationGuide.date | MessageDefinition.date | NamingSystem.date | OperationDefinition.date | SearchParameter.date | StructureDefi'
      +'nition.date | StructureMap.date | TerminologyCapabilities.date | ValueSet.date', sxpNormal);
  indexes.add('CodeSystem', 'derived-from', 'A resource that the CodeSystem is derived from', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'CodeSystem.relatedArtifact.where(type=''derived-from'').resource', sxpNormal);
  indexes.add('CodeSystem', 'description', '): The description of the code system', sptSTRING, [], 'CapabilityStatement.description | CodeSystem.description | CompartmentDefinition.description | ConceptMap.description | GraphDefinition.description | ImplementationGuide.description | MessageDefinition.description | NamingSystem.description | Operati'
      +'onDefinition.description | SearchParameter.description | StructureDefinition.description | StructureMap.description | TerminologyCapabilities.description | ValueSet.description', sxpNormal);
  indexes.add('CodeSystem', 'effective', 'Optional Extensions Element', sptDATE, [], 'CodeSystem.extension(''http://hl7.org/fhir/StructureDefinition/codesystem-effectiveDate''.value)', sxpNormal);
  indexes.add('CodeSystem', 'effective', '): The time during which the CodeSystem is intended to be in use', sptDATE, [], 'CodeSystem.effectivePeriod | ConceptMap.effectivePeriod | NamingSystem.effectivePeriod | ValueSet.effectivePeriod', sxpNormal);
  indexes.add('CodeSystem', 'end', 'Optional Extensions Element', sptDATE, [], 'CodeSystem.extension(''http://hl7.org/fhir/StructureDefinition/codesystem-expirationDate'').value', sxpNormal);
  indexes.add('CodeSystem', 'identifier', '): External identifier for the code system', sptTOKEN, [], 'CodeSystem.identifier | ConceptMap.identifier | MessageDefinition.identifier | NamingSystem.identifier | StructureDefinition.identifier | StructureMap.identifier | TerminologyCapabilities.identifier | ValueSet.identifier', sxpNormal);
  indexes.add('CodeSystem', 'jurisdiction', '): Intended jurisdiction for the code system', sptTOKEN, [], 'CapabilityStatement.jurisdiction | CodeSystem.jurisdiction | ConceptMap.jurisdiction | GraphDefinition.jurisdiction | ImplementationGuide.jurisdiction | MessageDefinition.jurisdiction | NamingSystem.jurisdiction | OperationDefinition.jurisdiction | S'
      +'earchParameter.jurisdiction | StructureDefinition.jurisdiction | StructureMap.jurisdiction | TerminologyCapabilities.jurisdiction | ValueSet.jurisdiction', sxpNormal);
  indexes.add('CodeSystem', 'keyword', 'Optional Extensions Element', sptSTRING, [], 'CodeSystem.extension(''http://hl7.org/fhir/StructureDefinition/codesystem-keyWord'').value', sxpNormal);
  indexes.add('CodeSystem', 'language', 'A language in which a designation is provided', sptTOKEN, [], 'CodeSystem.concept.designation.language', sxpNormal);
  indexes.add('CodeSystem', 'name', '): Computationally friendly name of the code system', sptSTRING, [], 'CapabilityStatement.name | CodeSystem.name | CompartmentDefinition.name | ConceptMap.name | GraphDefinition.name | ImplementationGuide.name | MessageDefinition.name | NamingSystem.name | OperationDefinition.name | SearchParameter.name | StructureDefi'
      +'nition.name | StructureMap.name | TerminologyCapabilities.name | ValueSet.name', sxpNormal);
  indexes.add('CodeSystem', 'predecessor', 'The predecessor of the CodeSystem', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'CodeSystem.relatedArtifact.where(type=''predecessor'').resource', sxpNormal);
  indexes.add('CodeSystem', 'publisher', '): Name of the publisher of the code system', sptSTRING, [], 'CapabilityStatement.publisher | CodeSystem.publisher | CompartmentDefinition.publisher | ConceptMap.publisher | GraphDefinition.publisher | ImplementationGuide.publisher | MessageDefinition.publisher | NamingSystem.publisher | OperationDefinition.pub'
      +'lisher | SearchParameter.publisher | StructureDefinition.publisher | StructureMap.publisher | TerminologyCapabilities.publisher | ValueSet.publisher', sxpNormal);
  indexes.add('CodeSystem', 'status', '): The current status of the code system', sptTOKEN, [], 'CapabilityStatement.status | CodeSystem.status | CompartmentDefinition.status | ConceptMap.status | GraphDefinition.status | ImplementationGuide.status | MessageDefinition.status | NamingSystem.status | OperationDefinition.status | SearchParameter.st'
      +'atus | StructureDefinition.status | StructureMap.status | TerminologyCapabilities.status | ValueSet.status', sxpNormal);
  indexes.add('CodeSystem', 'supplements', 'Find code system supplements for the referenced code system', sptREFERENCE, ['CodeSystem'], 'CodeSystem.supplements', sxpNormal);
  indexes.add('CodeSystem', 'system', 'The system for any codes defined by this code system (same as ''url'')', sptURI, [], 'CodeSystem.url', sxpNormal);
  indexes.add('CodeSystem', 'title', '): The human-friendly name of the code system', sptSTRING, [], 'CapabilityStatement.title | CodeSystem.title | ConceptMap.title | ImplementationGuide.title | MessageDefinition.title | OperationDefinition.title | StructureDefinition.title | StructureMap.title | TerminologyCapabilities.title | ValueSet.title', sxpNormal);
  indexes.add('CodeSystem', 'topic', 'Topics associated with the CodeSystem', sptTOKEN, [], 'CodeSystem.topic', sxpNormal);
  indexes.add('CodeSystem', 'url', '): The uri that identifies the code system', sptURI, [], 'CapabilityStatement.url | CodeSystem.url | CompartmentDefinition.url | ConceptMap.url | GraphDefinition.url | ImplementationGuide.url | MessageDefinition.url | NamingSystem.url | OperationDefinition.url | SearchParameter.url | StructureDefinition.url'
      +' | StructureMap.url | TerminologyCapabilities.url | ValueSet.url', sxpNormal);
  indexes.add('CodeSystem', 'version', '): The business version of the code system', sptTOKEN, [], 'CapabilityStatement.version | CodeSystem.version | CompartmentDefinition.version | ConceptMap.version | GraphDefinition.version | ImplementationGuide.version | MessageDefinition.version | NamingSystem.version | OperationDefinition.version | SearchPar'
      +'ameter.version | StructureDefinition.version | StructureMap.version | TerminologyCapabilities.version | ValueSet.version', sxpNormal);
  indexes.add('CodeSystem', 'workflow', 'Optional Extensions Element', sptTOKEN, [], 'CodeSystem.extension(''http://hl7.org/fhir/StructureDefinition/codesystem-workflowStatus'').value', sxpNormal);
  indexes.add('CodeSystem', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_CODESYSTEM}

{$IFDEF FHIR_COMMUNICATION}
procedure TFHIRIndexBuilderR5.buildIndexesForCommunication(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Communication', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('Communication', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('Communication', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('Communication', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('Communication', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('Communication', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('Communication', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('Communication', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('Communication', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('Communication', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('Communication', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('Communication', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('Communication', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('Communication', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('Communication', 'based-on', 'Request fulfilled by this communication', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'Communication.basedOn', sxpNormal);
  indexes.add('Communication', 'category', 'Message category', sptTOKEN, [], 'Communication.category', sxpNormal);
  indexes.add('Communication', 'encounter', 'The Encounter during which this Communication was created', sptREFERENCE, ['Encounter'], 'Communication.encounter', sxpNormal);
  indexes.add('Communication', 'identifier', 'Unique identifier', sptTOKEN, [], 'Communication.identifier', sxpNormal);
  indexes.add('Communication', 'instantiates-canonical', 'Instantiates FHIR protocol or definition', sptREFERENCE, ['Questionnaire', 'Measure', 'PlanDefinition', 'OperationDefinition', 'ActivityDefinition'], 'Communication.instantiatesCanonical', sxpNormal);
  indexes.add('Communication', 'instantiates-uri', 'Instantiates external protocol or definition', sptURI, [], 'Communication.instantiatesUri', sxpNormal);
  indexes.add('Communication', 'medium', 'A channel of communication', sptTOKEN, [], 'Communication.medium', sxpNormal);
  indexes.add('Communication', 'part-of', 'Part of referenced event (e.g. Communication, Procedure)', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'Communication.partOf', sxpNormal);
  indexes.add('Communication', 'patient', 'Focus of message', sptREFERENCE, ['Patient'], 'Communication.subject.where(resolve() is Patient)', sxpNormal);
  indexes.add('Communication', 'received', 'When received', sptDATE, [], 'Communication.received', sxpNormal);
  indexes.add('Communication', 'recipient', 'Who the information is shared with', sptREFERENCE, ['Practitioner', 'Group', 'Organization', 'CareTeam', 'Endpoint', 'Device', 'Patient', 'HealthcareService', 'PractitionerRole', 'RelatedPerson', 'Location'], 'Communication.recipient', sxpNormal);
  indexes.add('Communication', 'sender', 'Who shares the information', sptREFERENCE, ['Practitioner', 'Organization', 'Endpoint', 'CareTeam', 'Device', 'Patient', 'HealthcareService', 'PractitionerRole', 'RelatedPerson'], 'Communication.sender', sxpNormal);
  indexes.add('Communication', 'sent', 'When sent', sptDATE, [], 'Communication.sent', sxpNormal);
  indexes.add('Communication', 'status', 'preparation | in-progress | not-done | on-hold | stopped | completed | entered-in-error | unknown', sptTOKEN, [], 'Communication.status', sxpNormal);
  indexes.add('Communication', 'subject', 'Focus of message', sptREFERENCE, ['Group', 'Patient'], 'Communication.subject', sxpNormal);
  indexes.add('Communication', 'topic', 'Description of the purpose/content', sptTOKEN, [], 'Communication.topic', sxpNormal);
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
  indexes.add('CommunicationRequest', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('CommunicationRequest', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('CommunicationRequest', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('CommunicationRequest', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('CommunicationRequest', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('CommunicationRequest', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('CommunicationRequest', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('CommunicationRequest', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('CommunicationRequest', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('CommunicationRequest', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('CommunicationRequest', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('CommunicationRequest', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('CommunicationRequest', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('CommunicationRequest', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('CommunicationRequest', 'authored', 'When request transitioned to being actionable', sptDATE, [], 'CommunicationRequest.authoredOn', sxpNormal);
  indexes.add('CommunicationRequest', 'based-on', 'Fulfills plan or proposal', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'CommunicationRequest.basedOn', sxpNormal);
  indexes.add('CommunicationRequest', 'category', 'Message category', sptTOKEN, [], 'CommunicationRequest.category', sxpNormal);
  indexes.add('CommunicationRequest', 'encounter', 'The Encounter during which this CommunicationRequest was created', sptREFERENCE, ['Encounter'], 'CommunicationRequest.encounter', sxpNormal);
  indexes.add('CommunicationRequest', 'group-identifier', 'Composite request this is part of', sptTOKEN, [], 'CommunicationRequest.groupIdentifier', sxpNormal);
  indexes.add('CommunicationRequest', 'identifier', 'Unique identifier', sptTOKEN, [], 'CommunicationRequest.identifier', sxpNormal);
  indexes.add('CommunicationRequest', 'information-provider', 'Who should share the information', sptREFERENCE, ['Practitioner', 'Organization', 'Endpoint', 'Device', 'Patient', 'HealthcareService', 'PractitionerRole', 'RelatedPerson'], 'CommunicationRequest.informationProvider', sxpNormal);
  indexes.add('CommunicationRequest', 'medium', 'A channel of communication', sptTOKEN, [], 'CommunicationRequest.medium', sxpNormal);
  indexes.add('CommunicationRequest', 'occurrence', 'When scheduled', sptDATE, [], 'CommunicationRequest.occurrence.ofType(dateTime) | CommunicationRequest.occurrence.ofType(Period)', sxpNormal);
  indexes.add('CommunicationRequest', 'patient', 'Focus of message', sptREFERENCE, ['Patient'], 'CommunicationRequest.subject.where(resolve() is Patient)', sxpNormal);
  indexes.add('CommunicationRequest', 'priority', 'routine | urgent | asap | stat', sptTOKEN, [], 'CommunicationRequest.priority', sxpNormal);
  indexes.add('CommunicationRequest', 'recipient', 'Who to share the information with', sptREFERENCE, ['Practitioner', 'Group', 'Organization', 'CareTeam', 'Endpoint', 'Device', 'Patient', 'HealthcareService', 'PractitionerRole', 'RelatedPerson'], 'CommunicationRequest.recipient', sxpNormal);
  indexes.add('CommunicationRequest', 'replaces', 'Request(s) replaced by this request', sptREFERENCE, ['CommunicationRequest'], 'CommunicationRequest.replaces', sxpNormal);
  indexes.add('CommunicationRequest', 'requester', 'Who asks for the information to be shared', sptREFERENCE, ['Practitioner', 'Organization', 'Device', 'Patient', 'PractitionerRole', 'RelatedPerson'], 'CommunicationRequest.requester', sxpNormal);
  indexes.add('CommunicationRequest', 'status', 'draft | active | on-hold | revoked | completed | entered-in-error | unknown', sptTOKEN, [], 'CommunicationRequest.status', sxpNormal);
  indexes.add('CommunicationRequest', 'subject', 'Focus of message', sptREFERENCE, ['Group', 'Patient'], 'CommunicationRequest.subject', sxpNormal);
  indexes.add('CommunicationRequest', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Device', 'CommunicationRequest', ['information-provider', 'recipient']);
  compartments.register('Encounter', 'CommunicationRequest', ['encounter']);
  compartments.register('Device', 'CommunicationRequest', ['sender', 'recipient']);
  compartments.register('Patient', 'CommunicationRequest', ['subject', 'information-provider', 'recipient', 'requester']);
  compartments.register('Practitioner', 'CommunicationRequest', ['information-provider', 'recipient', 'requester']);
  compartments.register('RelatedPerson', 'CommunicationRequest', ['information-provider', 'recipient', 'requester']);
end;
{$ENDIF FHIR_COMMUNICATIONREQUEST}

{$IFDEF FHIR_COMPARTMENTDEFINITION}
procedure TFHIRIndexBuilderR5.buildIndexesForCompartmentDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('CompartmentDefinition', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('CompartmentDefinition', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('CompartmentDefinition', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('CompartmentDefinition', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('CompartmentDefinition', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('CompartmentDefinition', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('CompartmentDefinition', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('CompartmentDefinition', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('CompartmentDefinition', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('CompartmentDefinition', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('CompartmentDefinition', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('CompartmentDefinition', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('CompartmentDefinition', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('CompartmentDefinition', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('CompartmentDefinition', 'code', 'Patient | Encounter | RelatedPerson | Practitioner | Device', sptTOKEN, [], 'CompartmentDefinition.code', sxpNormal);
  indexes.add('CompartmentDefinition', 'context', '): A use context assigned to the compartment definition', sptTOKEN, [], '(CapabilityStatement.useContext.value as CodeableConcept) | (CodeSystem.useContext.value as CodeableConcept) | (CompartmentDefinition.useContext.value as CodeableConcept) | (ConceptMap.useContext.value as CodeableConcept) | (GraphDefinition.useContex'
      +'t.value as CodeableConcept) | (ImplementationGuide.useContext.value as CodeableConcept) | (MessageDefinition.useContext.value as CodeableConcept) | (NamingSystem.useContext.value as CodeableConcept) | (OperationDefinition.useContext.value as '
      +'CodeableConcept) | (SearchParameter.useContext.value as CodeableConcept) | (StructureDefinition.useContext.value as CodeableConcept) | (StructureMap.useContext.value as CodeableConcept) | (TerminologyCapabilities.useContext.value as CodeableC'
      +'oncept) | (ValueSet.useContext.value as CodeableConcept)', sxpNormal);
  indexes.add('CompartmentDefinition', 'context-quantity', '): A quantity- or range-valued use context assigned to the compartment definition', sptQUANTITY, [], '(CapabilityStatement.useContext.value as Quantity) | (CapabilityStatement.useContext.value as Range) | (CodeSystem.useContext.value as Quantity) | (CodeSystem.useContext.value as Range) | (CompartmentDefinition.useContext.value as Quantity) | (Compar'
      +'tmentDefinition.useContext.value as Range) | (ConceptMap.useContext.value as Quantity) | (ConceptMap.useContext.value as Range) | (GraphDefinition.useContext.value as Quantity) | (GraphDefinition.useContext.value as Range) | (ImplementationGu'
      +'ide.useContext.value as Quantity) | (ImplementationGuide.useContext.value as Range) | (MessageDefinition.useContext.value as Quantity) | (MessageDefinition.useContext.value as Range) | (NamingSystem.useContext.value as Quantity) | (NamingSyst'
      +'em.useContext.value as Range) | (OperationDefinition.useContext.value a'+
   's Quantity) | (OperationDefinition.useContext.value as Range) | (SearchParameter.useContext.value as Quantity) | (SearchParameter.useContext.value as Range) | (StructureDe'
      +'finition.useContext.value as Quantity) | (StructureDefinition.useContext.value as Range) | (StructureMap.useContext.value as Quantity) | (StructureMap.useContext.value as Range) | (TerminologyCapabilities.useContext.value as Quantity) | (Term'
      +'inologyCapabilities.useContext.value as Range) | (ValueSet.useContext.value as Quantity) | (ValueSet.useContext.value as Range)', sxpNormal);
  indexes.add('CompartmentDefinition', 'context-type', '): A type of use context assigned to the compartment definition', sptTOKEN, [], 'CapabilityStatement.useContext.code | CodeSystem.useContext.code | CompartmentDefinition.useContext.code | ConceptMap.useContext.code | GraphDefinition.useContext.code | ImplementationGuide.useContext.code | MessageDefinition.useContext.code | Naming'
      +'System.useContext.code | OperationDefinition.useContext.code | SearchParameter.useContext.code | StructureDefinition.useContext.code | StructureMap.useContext.code | TerminologyCapabilities.useContext.code | ValueSet.useContext.code', sxpNormal);
  indexes.add('CompartmentDefinition', 'context-type-quantity', '): A use context type and quantity- or range-based value assigned to the compartment definition', sptCOMPOSITE, [], 'CapabilityStatement.useContext | CodeSystem.useContext | CompartmentDefinition.useContext | ConceptMap.useContext | GraphDefinition.useContext | ImplementationGuide.useContext | MessageDefinition.useContext | NamingSystem.useContext | OperationDefini'
      +'tion.useContext | SearchParameter.useContext | StructureDefinition.useContext | StructureMap.useContext | TerminologyCapabilities.useContext | ValueSet.useContext', sxpNormal);
  indexes.add('CompartmentDefinition', 'context-type-value', '): A use context type and value assigned to the compartment definition', sptCOMPOSITE, [], 'CapabilityStatement.useContext | CodeSystem.useContext | CompartmentDefinition.useContext | ConceptMap.useContext | GraphDefinition.useContext | ImplementationGuide.useContext | MessageDefinition.useContext | NamingSystem.useContext | OperationDefini'
      +'tion.useContext | SearchParameter.useContext | StructureDefinition.useContext | StructureMap.useContext | TerminologyCapabilities.useContext | ValueSet.useContext', sxpNormal);
  indexes.add('CompartmentDefinition', 'date', '): The compartment definition publication date', sptDATE, [], 'CapabilityStatement.date | CodeSystem.date | CompartmentDefinition.date | ConceptMap.date | GraphDefinition.date | ImplementationGuide.date | MessageDefinition.date | NamingSystem.date | OperationDefinition.date | SearchParameter.date | StructureDefi'
      +'nition.date | StructureMap.date | TerminologyCapabilities.date | ValueSet.date', sxpNormal);
  indexes.add('CompartmentDefinition', 'description', '): The description of the compartment definition', sptSTRING, [], 'CapabilityStatement.description | CodeSystem.description | CompartmentDefinition.description | ConceptMap.description | GraphDefinition.description | ImplementationGuide.description | MessageDefinition.description | NamingSystem.description | Operati'
      +'onDefinition.description | SearchParameter.description | StructureDefinition.description | StructureMap.description | TerminologyCapabilities.description | ValueSet.description', sxpNormal);
  indexes.add('CompartmentDefinition', 'name', '): Computationally friendly name of the compartment definition', sptSTRING, [], 'CapabilityStatement.name | CodeSystem.name | CompartmentDefinition.name | ConceptMap.name | GraphDefinition.name | ImplementationGuide.name | MessageDefinition.name | NamingSystem.name | OperationDefinition.name | SearchParameter.name | StructureDefi'
      +'nition.name | StructureMap.name | TerminologyCapabilities.name | ValueSet.name', sxpNormal);
  indexes.add('CompartmentDefinition', 'publisher', '): Name of the publisher of the compartment definition', sptSTRING, [], 'CapabilityStatement.publisher | CodeSystem.publisher | CompartmentDefinition.publisher | ConceptMap.publisher | GraphDefinition.publisher | ImplementationGuide.publisher | MessageDefinition.publisher | NamingSystem.publisher | OperationDefinition.pub'
      +'lisher | SearchParameter.publisher | StructureDefinition.publisher | StructureMap.publisher | TerminologyCapabilities.publisher | ValueSet.publisher', sxpNormal);
  indexes.add('CompartmentDefinition', 'resource', 'Name of resource type', sptTOKEN, [], 'CompartmentDefinition.resource.code', sxpNormal);
  indexes.add('CompartmentDefinition', 'status', '): The current status of the compartment definition', sptTOKEN, [], 'CapabilityStatement.status | CodeSystem.status | CompartmentDefinition.status | ConceptMap.status | GraphDefinition.status | ImplementationGuide.status | MessageDefinition.status | NamingSystem.status | OperationDefinition.status | SearchParameter.st'
      +'atus | StructureDefinition.status | StructureMap.status | TerminologyCapabilities.status | ValueSet.status', sxpNormal);
  indexes.add('CompartmentDefinition', 'url', '): The uri that identifies the compartment definition', sptURI, [], 'CapabilityStatement.url | CodeSystem.url | CompartmentDefinition.url | ConceptMap.url | GraphDefinition.url | ImplementationGuide.url | MessageDefinition.url | NamingSystem.url | OperationDefinition.url | SearchParameter.url | StructureDefinition.url'
      +' | StructureMap.url | TerminologyCapabilities.url | ValueSet.url', sxpNormal);
  indexes.add('CompartmentDefinition', 'version', '): The business version of the compartment definition', sptTOKEN, [], 'CapabilityStatement.version | CodeSystem.version | CompartmentDefinition.version | ConceptMap.version | GraphDefinition.version | ImplementationGuide.version | MessageDefinition.version | NamingSystem.version | OperationDefinition.version | SearchPar'
      +'ameter.version | StructureDefinition.version | StructureMap.version | TerminologyCapabilities.version | ValueSet.version', sxpNormal);
  indexes.add('CompartmentDefinition', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_COMPARTMENTDEFINITION}

{$IFDEF FHIR_COMPOSITION}
procedure TFHIRIndexBuilderR5.buildIndexesForComposition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Composition', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('Composition', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('Composition', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('Composition', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('Composition', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('Composition', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('Composition', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('Composition', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('Composition', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('Composition', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('Composition', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('Composition', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('Composition', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('Composition', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('Composition', 'attester', 'Who attested the composition', sptREFERENCE, ['Practitioner', 'Organization', 'Patient', 'PractitionerRole', 'RelatedPerson'], 'Composition.attester.party', sxpNormal);
  indexes.add('Composition', 'author', 'Who and/or what authored the composition', sptREFERENCE, ['Practitioner', 'Organization', 'Device', 'Patient', 'PractitionerRole', 'RelatedPerson'], 'Composition.author', sxpNormal);
  indexes.add('Composition', 'category', 'Categorization of Composition', sptTOKEN, [], 'Composition.category', sxpNormal);
  indexes.add('Composition', 'context', 'Code(s) that apply to the event being documented', sptTOKEN, [], 'Composition.event.code', sxpNormal);
  indexes.add('Composition', 'date', '): Composition editing time', sptDATE, [], 'AllergyIntolerance.recordedDate | CarePlan.period | ClinicalImpression.date | Composition.date | Consent.dateTime | DiagnosticReport.effective | Encounter.actualPeriod | EpisodeOfCare.period | FamilyMemberHistory.date | Flag.period | (Immunization.oc'
      +'currence as dateTime) | List.date | Observation.effective | Procedure.occurrence | (RiskAssessment.occurrence as dateTime) | SupplyRequest.authoredOn', sxpNormal);
  indexes.add('Composition', 'encounter', '): Context of the Composition', sptREFERENCE, ['Encounter'], 'Composition.encounter | DeviceRequest.encounter | DiagnosticReport.encounter | Flag.encounter | List.encounter | NutritionOrder.encounter | Observation.encounter | Procedure.encounter | RiskAssessment.encounter | ServiceRequest.encounter | VisionPres'
      +'cription.encounter', sxpNormal);
  indexes.add('Composition', 'entry', 'A reference to data that supports this section', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'Composition.section.entry', sxpNormal);
  indexes.add('Composition', 'identifier', '): Version-independent identifier for the Composition', sptTOKEN, [], 'AllergyIntolerance.identifier | CarePlan.identifier | CareTeam.identifier | Composition.identifier | Condition.identifier | Consent.identifier | DetectedIssue.identifier | DeviceRequest.identifier | DiagnosticReport.identifier | DocumentManifest.mast'
      +'erIdentifier | DocumentManifest.identifier | DocumentReference.identifier | Encounter.identifier | EpisodeOfCare.identifier | FamilyMemberHistory.identifier | Goal.identifier | ImagingStudy.identifier | Immunization.identifier | List.identifi'
      +'er | MedicationAdministration.identifier | MedicationDispense.identifier | MedicationRequest.identifier | MedicationUsage.identifier | NutritionOrder.identifier | Observation.identifier | Procedure.identifier | RiskAssessment.identifier | Ser'
      +'viceRequest.identifier | SupplyDelivery.identifier | SupplyRequest.identifier | VisionPrescription.identifier', sxpNormal);
  indexes.add('Composition', 'patient', '): Who and/or what the composition is about', sptREFERENCE, ['Patient', 'Practitioner', 'Group', 'Organization', 'BiologicallyDerivedProduct', 'NutritionProduct', 'Device', 'Medication', 'Procedure', 'Substance', 'Location'], 'AllergyIntolerance.patient | CarePlan.subject.where(resolve() is Patient) | CareTeam.subject.where(resolve() is Patient) | ClinicalImpression.subject.where(resolve() is Patient) | Composition.subject.where(resolve() is Patient) | Condition.subject.wh'
      +'ere(resolve() is Patient) | Consent.subject.where(resolve() is Patient) | DetectedIssue.subject | DeviceRequest.subject.where(resolve() is Patient) | DeviceUsage.patient | DiagnosticReport.subject.where(resolve() is Patient) | DocumentManifes'
      +'t.subject.where(resolve() is Patient) | DocumentReference.subject.where(resolve() is Patient) | Encounter.subject.where(resolve() is Patient) | EpisodeOfCare.patient | FamilyMemberHistory.patient | Flag.subject.where(r'+
   'esolve() is Patient) | G'
      +'oal.subject.where(resolve() is Patient) | ImagingStudy.subject.where(resolve() is Patient) | Immunization.patient | List.subject.where(resolve() is Patient) | MedicationAdministration.subject.where(resolve() is Patient) | MedicationDispense.s'
      +'ubject.where(resolve() is Patient) | MedicationRequest.subject.where(resolve() is Patient) | MedicationUsage.subject.where(resolve() is Patient) | NutritionOrder.subject.where(resolve() is Patient) | Observation.subject.where(resolve() is Pat'
      +'ient) | Procedure.subject.where(resolve() is Patient) | RiskAssessment.subject.where(resolve() is Patient) | ServiceRequest.subject.where(resolve() is Patient) | SupplyDelivery.patient | VisionPrescription.patient', sxpNormal);
  indexes.add('Composition', 'period', 'The period covered by the documentation', sptDATE, [], 'Composition.event.period', sxpNormal);
  indexes.add('Composition', 'related', 'Target of the relationship', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'Composition.relatesTo.resourceReference', sxpNormal);
  indexes.add('Composition', 'section', 'Classification of section (recommended)', sptTOKEN, [], 'Composition.section.code', sxpNormal);
  indexes.add('Composition', 'section-code-text', 'Search on the section narrative of the resource', sptCOMPOSITE, [], 'Composition.section', sxpNormal);
  indexes.add('Composition', 'section-text', 'Search on the section narrative of the resource', sptNULL, [], 'Composition.section.text | Composition.section.section.text', sxpNormal);
  indexes.add('Composition', 'status', 'preliminary | final | amended | entered-in-error', sptTOKEN, [], 'Composition.status', sxpNormal);
  indexes.add('Composition', 'subject', 'Who and/or what the composition is about', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'Composition.subject', sxpNormal);
  indexes.add('Composition', 'title', 'Human Readable name/title', sptSTRING, [], 'Composition.title', sxpNormal);
  indexes.add('Composition', 'type', '): Kind of composition (LOINC if possible)', sptTOKEN, [], 'AllergyIntolerance.type | Composition.type | DocumentManifest.type | DocumentReference.type | Encounter.type | EpisodeOfCare.type', sxpNormal);
  indexes.add('Composition', 'url', 'The uri that identifies the activity definition', sptURI, [], 'Composition.url', sxpNormal);
  indexes.add('Composition', 'version', 'The business version of the activity definition', sptTOKEN, [], 'Composition.version', sxpNormal);
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
  indexes.add('ConceptMap', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('ConceptMap', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('ConceptMap', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('ConceptMap', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('ConceptMap', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('ConceptMap', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('ConceptMap', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('ConceptMap', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('ConceptMap', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('ConceptMap', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('ConceptMap', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('ConceptMap', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('ConceptMap', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('ConceptMap', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('ConceptMap', 'context', '): A use context assigned to the concept map', sptTOKEN, [], '(CapabilityStatement.useContext.value as CodeableConcept) | (CodeSystem.useContext.value as CodeableConcept) | (CompartmentDefinition.useContext.value as CodeableConcept) | (ConceptMap.useContext.value as CodeableConcept) | (GraphDefinition.useContex'
      +'t.value as CodeableConcept) | (ImplementationGuide.useContext.value as CodeableConcept) | (MessageDefinition.useContext.value as CodeableConcept) | (NamingSystem.useContext.value as CodeableConcept) | (OperationDefinition.useContext.value as '
      +'CodeableConcept) | (SearchParameter.useContext.value as CodeableConcept) | (StructureDefinition.useContext.value as CodeableConcept) | (StructureMap.useContext.value as CodeableConcept) | (TerminologyCapabilities.useContext.value as CodeableC'
      +'oncept) | (ValueSet.useContext.value as CodeableConcept)', sxpNormal);
  indexes.add('ConceptMap', 'context-quantity', '): A quantity- or range-valued use context assigned to the concept map', sptQUANTITY, [], '(CapabilityStatement.useContext.value as Quantity) | (CapabilityStatement.useContext.value as Range) | (CodeSystem.useContext.value as Quantity) | (CodeSystem.useContext.value as Range) | (CompartmentDefinition.useContext.value as Quantity) | (Compar'
      +'tmentDefinition.useContext.value as Range) | (ConceptMap.useContext.value as Quantity) | (ConceptMap.useContext.value as Range) | (GraphDefinition.useContext.value as Quantity) | (GraphDefinition.useContext.value as Range) | (ImplementationGu'
      +'ide.useContext.value as Quantity) | (ImplementationGuide.useContext.value as Range) | (MessageDefinition.useContext.value as Quantity) | (MessageDefinition.useContext.value as Range) | (NamingSystem.useContext.value as Quantity) | (NamingSyst'
      +'em.useContext.value as Range) | (OperationDefinition.useContext.value as Quantity) | (Operati'+
   'onDefinition.useContext.value as Range) | (SearchParameter.useContext.value as Quantity) | (SearchParameter.useContext.value as Range) | (StructureDe'
      +'finition.useContext.value as Quantity) | (StructureDefinition.useContext.value as Range) | (StructureMap.useContext.value as Quantity) | (StructureMap.useContext.value as Range) | (TerminologyCapabilities.useContext.value as Quantity) | (Term'
      +'inologyCapabilities.useContext.value as Range) | (ValueSet.useContext.value as Quantity) | (ValueSet.useContext.value as Range)', sxpNormal);
  indexes.add('ConceptMap', 'context-type', '): A type of use context assigned to the concept map', sptTOKEN, [], 'CapabilityStatement.useContext.code | CodeSystem.useContext.code | CompartmentDefinition.useContext.code | ConceptMap.useContext.code | GraphDefinition.useContext.code | ImplementationGuide.useContext.code | MessageDefinition.useContext.code | Naming'
      +'System.useContext.code | OperationDefinition.useContext.code | SearchParameter.useContext.code | StructureDefinition.useContext.code | StructureMap.useContext.code | TerminologyCapabilities.useContext.code | ValueSet.useContext.code', sxpNormal);
  indexes.add('ConceptMap', 'context-type-quantity', '): A use context type and quantity- or range-based value assigned to the concept map', sptCOMPOSITE, [], 'CapabilityStatement.useContext | CodeSystem.useContext | CompartmentDefinition.useContext | ConceptMap.useContext | GraphDefinition.useContext | ImplementationGuide.useContext | MessageDefinition.useContext | NamingSystem.useContext | OperationDefini'
      +'tion.useContext | SearchParameter.useContext | StructureDefinition.useContext | StructureMap.useContext | TerminologyCapabilities.useContext | ValueSet.useContext', sxpNormal);
  indexes.add('ConceptMap', 'context-type-value', '): A use context type and value assigned to the concept map', sptCOMPOSITE, [], 'CapabilityStatement.useContext | CodeSystem.useContext | CompartmentDefinition.useContext | ConceptMap.useContext | GraphDefinition.useContext | ImplementationGuide.useContext | MessageDefinition.useContext | NamingSystem.useContext | OperationDefini'
      +'tion.useContext | SearchParameter.useContext | StructureDefinition.useContext | StructureMap.useContext | TerminologyCapabilities.useContext | ValueSet.useContext', sxpNormal);
  indexes.add('ConceptMap', 'date', '): The concept map publication date', sptDATE, [], 'CapabilityStatement.date | CodeSystem.date | CompartmentDefinition.date | ConceptMap.date | GraphDefinition.date | ImplementationGuide.date | MessageDefinition.date | NamingSystem.date | OperationDefinition.date | SearchParameter.date | StructureDefi'
      +'nition.date | StructureMap.date | TerminologyCapabilities.date | ValueSet.date', sxpNormal);
  indexes.add('ConceptMap', 'dependson', 'Other properties required for this mapping', sptURI, [], 'ConceptMap.group.element.target.dependsOn.property', sxpNormal);
  indexes.add('ConceptMap', 'derived-from', 'A resource that the ConceptMap is derived from', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'ConceptMap.relatedArtifact.where(type=''derived-from'').resource', sxpNormal);
  indexes.add('ConceptMap', 'description', '): The description of the concept map', sptSTRING, [], 'CapabilityStatement.description | CodeSystem.description | CompartmentDefinition.description | ConceptMap.description | GraphDefinition.description | ImplementationGuide.description | MessageDefinition.description | NamingSystem.description | Operati'
      +'onDefinition.description | SearchParameter.description | StructureDefinition.description | StructureMap.description | TerminologyCapabilities.description | ValueSet.description', sxpNormal);
  indexes.add('ConceptMap', 'effective', '): The time during which the ConceptMap is intended to be in use', sptDATE, [], 'CodeSystem.effectivePeriod | ConceptMap.effectivePeriod | NamingSystem.effectivePeriod | ValueSet.effectivePeriod', sxpNormal);
  indexes.add('ConceptMap', 'identifier', '): External identifier for the concept map', sptTOKEN, [], 'CodeSystem.identifier | ConceptMap.identifier | MessageDefinition.identifier | NamingSystem.identifier | StructureDefinition.identifier | StructureMap.identifier | TerminologyCapabilities.identifier | ValueSet.identifier', sxpNormal);
  indexes.add('ConceptMap', 'jurisdiction', '): Intended jurisdiction for the concept map', sptTOKEN, [], 'CapabilityStatement.jurisdiction | CodeSystem.jurisdiction | ConceptMap.jurisdiction | GraphDefinition.jurisdiction | ImplementationGuide.jurisdiction | MessageDefinition.jurisdiction | NamingSystem.jurisdiction | OperationDefinition.jurisdiction | S'
      +'earchParameter.jurisdiction | StructureDefinition.jurisdiction | StructureMap.jurisdiction | TerminologyCapabilities.jurisdiction | ValueSet.jurisdiction', sxpNormal);
  indexes.add('ConceptMap', 'name', '): Computationally friendly name of the concept map', sptSTRING, [], 'CapabilityStatement.name | CodeSystem.name | CompartmentDefinition.name | ConceptMap.name | GraphDefinition.name | ImplementationGuide.name | MessageDefinition.name | NamingSystem.name | OperationDefinition.name | SearchParameter.name | StructureDefi'
      +'nition.name | StructureMap.name | TerminologyCapabilities.name | ValueSet.name', sxpNormal);
  indexes.add('ConceptMap', 'other-map', 'canonical reference to an additional ConceptMap to use for mapping if the source concept is unmapped', sptREFERENCE, ['ConceptMap'], 'ConceptMap.group.unmapped.otherMap', sxpNormal);
  indexes.add('ConceptMap', 'predecessor', 'The predecessor of the ConceptMap', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'ConceptMap.relatedArtifact.where(type=''predecessor'').resource', sxpNormal);
  indexes.add('ConceptMap', 'product', 'Other properties that this mapping also produces', sptURI, [], 'ConceptMap.group.element.target.product.property', sxpNormal);
  indexes.add('ConceptMap', 'publisher', '): Name of the publisher of the concept map', sptSTRING, [], 'CapabilityStatement.publisher | CodeSystem.publisher | CompartmentDefinition.publisher | ConceptMap.publisher | GraphDefinition.publisher | ImplementationGuide.publisher | MessageDefinition.publisher | NamingSystem.publisher | OperationDefinition.pub'
      +'lisher | SearchParameter.publisher | StructureDefinition.publisher | StructureMap.publisher | TerminologyCapabilities.publisher | ValueSet.publisher', sxpNormal);
  indexes.add('ConceptMap', 'source-code', 'Identifies elements being mapped', sptTOKEN, [], 'ConceptMap.group.element.code', sxpNormal);
  indexes.add('ConceptMap', 'source-group-system', 'Source system where concepts to be mapped are defined', sptREFERENCE, ['CodeSystem'], 'ConceptMap.group.source', sxpNormal);
  indexes.add('ConceptMap', 'source-scope', 'The source value set that contains the concepts that are being mapped', sptREFERENCE, ['ValueSet'], '(ConceptMap.sourceScope as canonical)', sxpNormal);
  indexes.add('ConceptMap', 'source-scope-uri', 'The URI for the source value set that contains the concepts being mapped', sptURI, [], '(ConceptMap.sourceScope as uri)', sxpNormal);
  indexes.add('ConceptMap', 'status', '): The current status of the concept map', sptTOKEN, [], 'CapabilityStatement.status | CodeSystem.status | CompartmentDefinition.status | ConceptMap.status | GraphDefinition.status | ImplementationGuide.status | MessageDefinition.status | NamingSystem.status | OperationDefinition.status | SearchParameter.st'
      +'atus | StructureDefinition.status | StructureMap.status | TerminologyCapabilities.status | ValueSet.status', sxpNormal);
  indexes.add('ConceptMap', 'target-code', 'Code that identifies the target element', sptTOKEN, [], 'ConceptMap.group.element.target.code', sxpNormal);
  indexes.add('ConceptMap', 'target-group-system', 'Target system that the concepts are to be mapped to', sptREFERENCE, ['CodeSystem'], 'ConceptMap.group.target', sxpNormal);
  indexes.add('ConceptMap', 'target-scope', 'The target value set which provides context for the mappings', sptREFERENCE, ['ValueSet'], '(ConceptMap.targetScope as canonical)', sxpNormal);
  indexes.add('ConceptMap', 'target-scope-uri', 'The URI for the target value set that contains the concepts being mapped.', sptURI, [], '(ConceptMap.targetScope as uri)', sxpNormal);
  indexes.add('ConceptMap', 'title', '): The human-friendly name of the concept map', sptSTRING, [], 'CapabilityStatement.title | CodeSystem.title | ConceptMap.title | ImplementationGuide.title | MessageDefinition.title | OperationDefinition.title | StructureDefinition.title | StructureMap.title | TerminologyCapabilities.title | ValueSet.title', sxpNormal);
  indexes.add('ConceptMap', 'topic', 'Topics associated with the ConceptMap', sptTOKEN, [], 'ConceptMap.topic', sxpNormal);
  indexes.add('ConceptMap', 'url', '): The URI that identifies the concept map', sptURI, [], 'CapabilityStatement.url | CodeSystem.url | CompartmentDefinition.url | ConceptMap.url | GraphDefinition.url | ImplementationGuide.url | MessageDefinition.url | NamingSystem.url | OperationDefinition.url | SearchParameter.url | StructureDefinition.url'
      +' | StructureMap.url | TerminologyCapabilities.url | ValueSet.url', sxpNormal);
  indexes.add('ConceptMap', 'version', '): The business version of the concept map', sptTOKEN, [], 'CapabilityStatement.version | CodeSystem.version | CompartmentDefinition.version | ConceptMap.version | GraphDefinition.version | ImplementationGuide.version | MessageDefinition.version | NamingSystem.version | OperationDefinition.version | SearchPar'
      +'ameter.version | StructureDefinition.version | StructureMap.version | TerminologyCapabilities.version | ValueSet.version', sxpNormal);
  indexes.add('ConceptMap', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_CONCEPTMAP}

{$IFDEF FHIR_CONDITION}
procedure TFHIRIndexBuilderR5.buildIndexesForCondition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Condition', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('Condition', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('Condition', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('Condition', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('Condition', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('Condition', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('Condition', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('Condition', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('Condition', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('Condition', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('Condition', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('Condition', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('Condition', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('Condition', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('Condition', 'abatement-age', 'Abatement as age or age range', sptQUANTITY, [], 'Condition.abatement.ofType(Age) | Condition.abatement.ofType(Range)', sxpNormal);
  indexes.add('Condition', 'abatement-date', 'Date-related abatements (dateTime and period)', sptDATE, [], 'Condition.abatement.ofType(dateTime) | Condition.abatement.ofType(Period)', sxpNormal);
  indexes.add('Condition', 'abatement-string', 'Abatement as a string', sptSTRING, [], 'Condition.abatement.ofType(string)', sxpNormal);
  indexes.add('Condition', 'body-site', 'Anatomical location, if relevant', sptTOKEN, [], 'Condition.bodySite', sxpNormal);
  indexes.add('Condition', 'category', 'The category of the condition', sptTOKEN, [], 'Condition.category', sxpNormal);
  indexes.add('Condition', 'clinical-status', 'The clinical status of the condition', sptTOKEN, [], 'Condition.clinicalStatus', sxpNormal);
  indexes.add('Condition', 'code', '): Code for the condition', sptTOKEN, [], 'AllergyIntolerance.code | AllergyIntolerance.reaction.substance | Condition.code | DeviceRequest.code.concept | DiagnosticReport.code | FamilyMemberHistory.condition.code | List.code | Medication.code | MedicationAdministration.medication.concept | M'
      +'edicationDispense.medication.concept | MedicationRequest.medication.concept | MedicationUsage.medication.concept | Observation.code | Procedure.code', sxpNormal);
  indexes.add('Condition', 'encounter', 'The Encounter during which this Condition was created', sptREFERENCE, ['Encounter'], 'Condition.encounter', sxpNormal);
  indexes.add('Condition', 'evidence', 'Manifestation/symptom', sptTOKEN, [], 'Condition.evidence.concept', sxpNormal);
  indexes.add('Condition', 'evidence-detail', 'Supporting information found elsewhere', sptREFERENCE, [], 'Condition.evidence.reference', sxpNormal);
  indexes.add('Condition', 'identifier', '): A unique identifier of the condition record', sptTOKEN, [], 'AllergyIntolerance.identifier | CarePlan.identifier | CareTeam.identifier | Composition.identifier | Condition.identifier | Consent.identifier | DetectedIssue.identifier | DeviceRequest.identifier | DiagnosticReport.identifier | DocumentManifest.mast'
      +'erIdentifier | DocumentManifest.identifier | DocumentReference.identifier | Encounter.identifier | EpisodeOfCare.identifier | FamilyMemberHistory.identifier | Goal.identifier | ImagingStudy.identifier | Immunization.identifier | List.identifi'
      +'er | MedicationAdministration.identifier | MedicationDispense.identifier | MedicationRequest.identifier | MedicationUsage.identifier | NutritionOrder.identifier | Observation.identifier | Procedure.identifier | RiskAssessment.identifier | Ser'
      +'viceRequest.identifier | SupplyDelivery.identifier | SupplyRequest.identifier | VisionPrescription.identifier', sxpNormal);
  indexes.add('Condition', 'onset-age', 'Onsets as age or age range', sptQUANTITY, [], 'Condition.onset.ofType(Age) | Condition.onset.ofType(Range)', sxpNormal);
  indexes.add('Condition', 'onset-date', 'Date related onsets (dateTime and Period)', sptDATE, [], 'Condition.onset.ofType(dateTime) | Condition.onset.ofType(Period)', sxpNormal);
  indexes.add('Condition', 'onset-info', 'Onsets as a string', sptSTRING, [], 'Condition.onset.ofType(string)', sxpNormal);
  indexes.add('Condition', 'participant-actor', 'Who or what participated in the activities related to the condition', sptREFERENCE, ['Practitioner', 'Organization', 'CareTeam', 'Device', 'Patient', 'PractitionerRole', 'RelatedPerson'], 'Condition.participant.actor', sxpNormal);
  indexes.add('Condition', 'participant-function', 'Type of involvement of the actor in the activities related to the condition', sptTOKEN, [], 'Condition.participant.function', sxpNormal);
  indexes.add('Condition', 'patient', '): Who has the condition?', sptREFERENCE, ['Patient', 'Practitioner', 'Group', 'Organization', 'BiologicallyDerivedProduct', 'NutritionProduct', 'Device', 'Medication', 'Procedure', 'Substance', 'Location'], 'AllergyIntolerance.patient | CarePlan.subject.where(resolve() is Patient) | CareTeam.subject.where(resolve() is Patient) | ClinicalImpression.subject.where(resolve() is Patient) | Composition.subject.where(resolve() is Patient) | Condition.subject.wh'
      +'ere(resolve() is Patient) | Consent.subject.where(resolve() is Patient) | DetectedIssue.subject | DeviceRequest.subject.where(resolve() is Patient) | DeviceUsage.patient | DiagnosticReport.subject.where(resolve() is Patient) | DocumentManifes'
      +'t.subject.where(resolve() is Patient) | DocumentReference.subject.where(resolve() is Patient) | Encounter.subject.where(resolve() is Patient) | EpisodeOfCare.patient | FamilyMemberHistory.patient | Flag.subject.where(resolve() is Patient)'+
   ' | G'
      +'oal.subject.where(resolve() is Patient) | ImagingStudy.subject.where(resolve() is Patient) | Immunization.patient | List.subject.where(resolve() is Patient) | MedicationAdministration.subject.where(resolve() is Patient) | MedicationDispense.s'
      +'ubject.where(resolve() is Patient) | MedicationRequest.subject.where(resolve() is Patient) | MedicationUsage.subject.where(resolve() is Patient) | NutritionOrder.subject.where(resolve() is Patient) | Observation.subject.where(resolve() is Pat'
      +'ient) | Procedure.subject.where(resolve() is Patient) | RiskAssessment.subject.where(resolve() is Patient) | ServiceRequest.subject.where(resolve() is Patient) | SupplyDelivery.patient | VisionPrescription.patient', sxpNormal);
  indexes.add('Condition', 'recorded-date', 'Date record was first recorded', sptDATE, [], 'Condition.recordedDate', sxpNormal);
  indexes.add('Condition', 'severity', 'The severity of the condition', sptTOKEN, [], 'Condition.severity', sxpNormal);
  indexes.add('Condition', 'stage', 'Simple summary (disease specific)', sptTOKEN, [], 'Condition.stage.summary', sxpNormal);
  indexes.add('Condition', 'subject', 'Who has the condition?', sptREFERENCE, ['Group', 'Patient'], 'Condition.subject', sxpNormal);
  indexes.add('Condition', 'verification-status', 'unconfirmed | provisional | differential | confirmed | refuted | entered-in-error', sptTOKEN, [], 'Condition.verificationStatus', sxpNormal);
  indexes.add('Condition', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Encounter', 'Condition', ['encounter']);
  compartments.register('Patient', 'Condition', ['patient', 'participant-actor']);
  compartments.register('Practitioner', 'Condition', ['participant-actor']);
  compartments.register('RelatedPerson', 'Condition', ['participant-actor']);
end;
{$ENDIF FHIR_CONDITION}

{$IFDEF FHIR_CONDITIONDEFINITION}
procedure TFHIRIndexBuilderR5.buildIndexesForConditionDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ConditionDefinition', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('ConditionDefinition', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('ConditionDefinition', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('ConditionDefinition', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('ConditionDefinition', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('ConditionDefinition', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('ConditionDefinition', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('ConditionDefinition', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('ConditionDefinition', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('ConditionDefinition', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('ConditionDefinition', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('ConditionDefinition', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('ConditionDefinition', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('ConditionDefinition', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('ConditionDefinition', 'context', 'A use context assigned to the condition definition', sptTOKEN, [], '(ConditionDefinition.useContext.value as CodeableConcept)', sxpNormal);
  indexes.add('ConditionDefinition', 'context-quantity', 'A quantity- or range-valued use context assigned to the condition definition', sptQUANTITY, [], '(ConditionDefinition.useContext.value as Quantity) | (ConditionDefinition.useContext.value as Range)', sxpNormal);
  indexes.add('ConditionDefinition', 'context-type', 'A type of use context assigned to the condition definition', sptTOKEN, [], 'ConditionDefinition.useContext.code', sxpNormal);
  indexes.add('ConditionDefinition', 'context-type-quantity', 'A use context type and quantity- or range-based value assigned to the condition definition', sptCOMPOSITE, [], 'ConditionDefinition.useContext', sxpNormal);
  indexes.add('ConditionDefinition', 'context-type-value', 'A use context type and value assigned to the condition definition', sptCOMPOSITE, [], 'ConditionDefinition.useContext', sxpNormal);
  indexes.add('ConditionDefinition', 'date', 'The condition definition publication date', sptDATE, [], 'ConditionDefinition.date', sxpNormal);
  indexes.add('ConditionDefinition', 'description', 'The description of the condition definition', sptSTRING, [], 'ConditionDefinition.description', sxpNormal);
  indexes.add('ConditionDefinition', 'identifier', 'External identifier for the condition definition', sptTOKEN, [], 'ConditionDefinition.identifier', sxpNormal);
  indexes.add('ConditionDefinition', 'jurisdiction', 'Intended jurisdiction for the condition definition', sptTOKEN, [], 'ConditionDefinition.jurisdiction', sxpNormal);
  indexes.add('ConditionDefinition', 'name', 'Computationally friendly name of the condition definition', sptSTRING, [], 'ConditionDefinition.name', sxpNormal);
  indexes.add('ConditionDefinition', 'publisher', 'Name of the publisher of the condition definition', sptSTRING, [], 'ConditionDefinition.publisher', sxpNormal);
  indexes.add('ConditionDefinition', 'status', 'The current status of the condition definition', sptTOKEN, [], 'ConditionDefinition.status', sxpNormal);
  indexes.add('ConditionDefinition', 'title', 'The human-friendly name of the condition definition', sptSTRING, [], 'ConditionDefinition.title', sxpNormal);
  indexes.add('ConditionDefinition', 'url', 'The uri that identifies the condition definition', sptURI, [], 'ConditionDefinition.url', sxpNormal);
  indexes.add('ConditionDefinition', 'version', 'The business version of the condition definition', sptTOKEN, [], 'ConditionDefinition.version', sxpNormal);
  indexes.add('ConditionDefinition', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_CONDITIONDEFINITION}

{$IFDEF FHIR_CONSENT}
procedure TFHIRIndexBuilderR5.buildIndexesForConsent(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Consent', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('Consent', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('Consent', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('Consent', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('Consent', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('Consent', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('Consent', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('Consent', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('Consent', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('Consent', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('Consent', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('Consent', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('Consent', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('Consent', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('Consent', 'action', 'Actions controlled by this rule', sptTOKEN, [], 'Consent.provision.action', sxpNormal);
  indexes.add('Consent', 'actor', 'Resource for the actor (or group, by role)', sptREFERENCE, ['Practitioner', 'Group', 'Organization', 'CareTeam', 'Device', 'Patient', 'PractitionerRole', 'RelatedPerson'], 'Consent.provision.actor.reference', sxpNormal);
  indexes.add('Consent', 'category', 'Classification of the consent statement - for indexing/retrieval', sptTOKEN, [], 'Consent.category', sxpNormal);
  indexes.add('Consent', 'controller', 'Consent Enforcer', sptREFERENCE, ['Practitioner', 'Organization', 'Patient', 'HealthcareService'], 'Consent.controller', sxpNormal);
  indexes.add('Consent', 'data', 'The actual data reference', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'Consent.provision.data.reference', sxpNormal);
  indexes.add('Consent', 'date', '): When consent was agreed to', sptDATE, [], 'AllergyIntolerance.recordedDate | CarePlan.period | ClinicalImpression.date | Composition.date | Consent.dateTime | DiagnosticReport.effective | Encounter.actualPeriod | EpisodeOfCare.period | FamilyMemberHistory.date | Flag.period | (Immunization.oc'
      +'currence as dateTime) | List.date | Observation.effective | Procedure.occurrence | (RiskAssessment.occurrence as dateTime) | SupplyRequest.authoredOn', sxpNormal);
  indexes.add('Consent', 'grantee', 'Who is agreeing to the policy and rules', sptREFERENCE, ['Practitioner', 'Organization', 'CareTeam', 'Patient', 'HealthcareService', 'PractitionerRole', 'RelatedPerson'], 'Consent.grantee', sxpNormal);
  indexes.add('Consent', 'identifier', '): Identifier for this record (external references)', sptTOKEN, [], 'AllergyIntolerance.identifier | CarePlan.identifier | CareTeam.identifier | Composition.identifier | Condition.identifier | Consent.identifier | DetectedIssue.identifier | DeviceRequest.identifier | DiagnosticReport.identifier | DocumentManifest.mast'
      +'erIdentifier | DocumentManifest.identifier | DocumentReference.identifier | Encounter.identifier | EpisodeOfCare.identifier | FamilyMemberHistory.identifier | Goal.identifier | ImagingStudy.identifier | Immunization.identifier | List.identifi'
      +'er | MedicationAdministration.identifier | MedicationDispense.identifier | MedicationRequest.identifier | MedicationUsage.identifier | NutritionOrder.identifier | Observation.identifier | Procedure.identifier | RiskAssessment.identifier | Ser'
      +'viceRequest.identifier | SupplyDelivery.identifier | SupplyRequest.identifier | VisionPrescription.identifier', sxpNormal);
  indexes.add('Consent', 'manager', 'Consent workflow management', sptREFERENCE, ['Practitioner', 'Organization', 'Patient', 'HealthcareService'], 'Consent.manager', sxpNormal);
  indexes.add('Consent', 'patient', '): Who the consent applies to', sptREFERENCE, ['Patient', 'Practitioner', 'Group', 'Organization', 'BiologicallyDerivedProduct', 'NutritionProduct', 'Device', 'Medication', 'Procedure', 'Substance', 'Location'], 'AllergyIntolerance.patient | CarePlan.subject.where(resolve() is Patient) | CareTeam.subject.where(resolve() is Patient) | ClinicalImpression.subject.where(resolve() is Patient) | Composition.subject.where(resolve() is Patient) | Condition.subject.wh'
      +'ere(resolve() is Patient) | Consent.subject.where(resolve() is Patient) | DetectedIssue.subject | DeviceRequest.subject.where(resolve() is Patient) | DeviceUsage.patient | DiagnosticReport.subject.where(resolve() is Patient) | DocumentManifes'
      +'t.subject.where(resolve() is Patient) | DocumentReference.subject.where(resolve() is Patient) | Encounter.subject.where(resolve() is Patient) | EpisodeOfCare.patient | FamilyMemberHistory.patient | Flag.subject.where(resolve() is Patien'+
   't) | G'
      +'oal.subject.where(resolve() is Patient) | ImagingStudy.subject.where(resolve() is Patient) | Immunization.patient | List.subject.where(resolve() is Patient) | MedicationAdministration.subject.where(resolve() is Patient) | MedicationDispense.s'
      +'ubject.where(resolve() is Patient) | MedicationRequest.subject.where(resolve() is Patient) | MedicationUsage.subject.where(resolve() is Patient) | NutritionOrder.subject.where(resolve() is Patient) | Observation.subject.where(resolve() is Pat'
      +'ient) | Procedure.subject.where(resolve() is Patient) | RiskAssessment.subject.where(resolve() is Patient) | ServiceRequest.subject.where(resolve() is Patient) | SupplyDelivery.patient | VisionPrescription.patient', sxpNormal);
  indexes.add('Consent', 'period', 'Timeframe for this rule', sptDATE, [], 'Consent.provision.period', sxpNormal);
  indexes.add('Consent', 'purpose', 'Context of activities covered by this rule', sptTOKEN, [], 'Consent.provision.purpose', sxpNormal);
  indexes.add('Consent', 'security-label', 'Security Labels that define affected resources', sptTOKEN, [], 'Consent.provision.securityLabel', sxpNormal);
  indexes.add('Consent', 'source-reference', 'Search by reference to a Consent, DocumentReference, Contract  or QuestionnaireResponse', sptREFERENCE, ['Consent', 'Contract', 'QuestionnaireResponse', 'DocumentReference'], 'Consent.sourceReference', sxpNormal);
  indexes.add('Consent', 'status', 'draft | active | inactive | entered-in-error | unknown', sptTOKEN, [], 'Consent.status', sxpNormal);
  indexes.add('Consent', 'subject', 'Who the consent applies to', sptREFERENCE, ['Practitioner', 'Group', 'Patient'], 'Consent.subject', sxpNormal);
  indexes.add('Consent', 'verified', 'Has been verified', sptTOKEN, [], 'Consent.verification.verified', sxpNormal);
  indexes.add('Consent', 'verified-date', 'When consent verified', sptDATE, [], 'Consent.verification.verificationDate', sxpNormal);
  indexes.add('Consent', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Patient', 'Consent', ['subject']);
end;
{$ENDIF FHIR_CONSENT}

{$IFDEF FHIR_CONTRACT}
procedure TFHIRIndexBuilderR5.buildIndexesForContract(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Contract', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('Contract', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('Contract', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('Contract', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('Contract', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('Contract', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('Contract', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('Contract', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('Contract', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('Contract', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('Contract', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('Contract', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('Contract', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('Contract', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('Contract', 'authority', 'The authority of the contract', sptREFERENCE, ['Organization'], 'Contract.authority', sxpNormal);
  indexes.add('Contract', 'domain', 'The domain of the contract', sptREFERENCE, ['Location'], 'Contract.domain', sxpNormal);
  indexes.add('Contract', 'identifier', 'The identity of the contract', sptTOKEN, [], 'Contract.identifier', sxpNormal);
  indexes.add('Contract', 'instantiates', 'A source definition of the contract', sptURI, [], 'Contract.instantiatesUri', sxpNormal);
  indexes.add('Contract', 'issued', 'The date/time the contract was issued', sptDATE, [], 'Contract.issued', sxpNormal);
  indexes.add('Contract', 'patient', 'The identity of the subject of the contract (if a patient)', sptREFERENCE, ['Patient'], 'Contract.subject.where(resolve() is Patient)', sxpNormal);
  indexes.add('Contract', 'signer', 'Contract Signatory Party', sptREFERENCE, ['Practitioner', 'Organization', 'Patient', 'PractitionerRole', 'RelatedPerson'], 'Contract.signer.party', sxpNormal);
  indexes.add('Contract', 'status', 'The status of the contract', sptTOKEN, [], 'Contract.status', sxpNormal);
  indexes.add('Contract', 'subject', 'The identity of the subject of the contract', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'Contract.subject', sxpNormal);
  indexes.add('Contract', 'url', 'The basal contract definition', sptURI, [], 'Contract.url', sxpNormal);
  indexes.add('Contract', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_CONTRACT}

{$IFDEF FHIR_COVERAGE}
procedure TFHIRIndexBuilderR5.buildIndexesForCoverage(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Coverage', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('Coverage', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('Coverage', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('Coverage', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('Coverage', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('Coverage', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('Coverage', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('Coverage', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('Coverage', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('Coverage', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('Coverage', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('Coverage', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('Coverage', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('Coverage', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('Coverage', 'beneficiary', 'Covered party', sptREFERENCE, ['Patient'], 'Coverage.beneficiary', sxpNormal);
  indexes.add('Coverage', 'class-type', 'Coverage class (eg. plan, group)', sptTOKEN, [], 'Coverage.class.type', sxpNormal);
  indexes.add('Coverage', 'class-value', 'Value of the class (eg. Plan number, group number)', sptTOKEN, [], 'Coverage.class.value', sxpNormal);
  indexes.add('Coverage', 'dependent', 'Dependent number', sptSTRING, [], 'Coverage.dependent', sxpNormal);
  indexes.add('Coverage', 'identifier', 'The primary identifier of the insured and the coverage', sptTOKEN, [], 'Coverage.identifier', sxpNormal);
  indexes.add('Coverage', 'insurer', 'The identity of the insurer', sptREFERENCE, ['Organization'], 'Coverage.insurer', sxpNormal);
  indexes.add('Coverage', 'patient', 'Retrieve coverages for a patient', sptREFERENCE, ['Patient'], 'Coverage.beneficiary', sxpNormal);
  indexes.add('Coverage', 'paymentby-party', 'Parties who will pay for services', sptREFERENCE, ['Organization', 'Patient', 'RelatedPerson'], 'Coverage.paymentBy.party', sxpNormal);
  indexes.add('Coverage', 'policy-holder', 'Reference to the policyholder', sptREFERENCE, ['Organization', 'Patient', 'RelatedPerson'], 'Coverage.policyHolder', sxpNormal);
  indexes.add('Coverage', 'status', 'The status of the Coverage', sptTOKEN, [], 'Coverage.status', sxpNormal);
  indexes.add('Coverage', 'subscriber', 'Reference to the subscriber', sptREFERENCE, ['Patient', 'RelatedPerson'], 'Coverage.subscriber', sxpNormal);
  indexes.add('Coverage', 'subscriberid', 'Identifier of the subscriber', sptTOKEN, [], 'Coverage.subscriberId', sxpNormal);
  indexes.add('Coverage', 'type', 'The kind of coverage (health plan, auto, Workers Compensation)', sptTOKEN, [], 'Coverage.type', sxpNormal);
  indexes.add('Coverage', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Patient', 'Coverage', ['policy-holder', 'subscriber', 'beneficiary', 'paymentby-party']);
  compartments.register('RelatedPerson', 'Coverage', ['policy-holder', 'subscriber', 'paymentby-party']);
end;
{$ENDIF FHIR_COVERAGE}

{$IFDEF FHIR_COVERAGEELIGIBILITYREQUEST}
procedure TFHIRIndexBuilderR5.buildIndexesForCoverageEligibilityRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('CoverageEligibilityRequest', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('CoverageEligibilityRequest', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('CoverageEligibilityRequest', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('CoverageEligibilityRequest', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('CoverageEligibilityRequest', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('CoverageEligibilityRequest', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('CoverageEligibilityRequest', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('CoverageEligibilityRequest', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('CoverageEligibilityRequest', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('CoverageEligibilityRequest', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('CoverageEligibilityRequest', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('CoverageEligibilityRequest', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('CoverageEligibilityRequest', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('CoverageEligibilityRequest', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('CoverageEligibilityRequest', 'created', 'The creation date for the EOB', sptDATE, [], 'CoverageEligibilityRequest.created', sxpNormal);
  indexes.add('CoverageEligibilityRequest', 'enterer', 'The party who is responsible for the request', sptREFERENCE, ['Practitioner', 'PractitionerRole'], 'CoverageEligibilityRequest.enterer', sxpNormal);
  indexes.add('CoverageEligibilityRequest', 'facility', 'Facility responsible for the goods and services', sptREFERENCE, ['Location'], 'CoverageEligibilityRequest.facility', sxpNormal);
  indexes.add('CoverageEligibilityRequest', 'identifier', 'The business identifier of the Eligibility', sptTOKEN, [], 'CoverageEligibilityRequest.identifier', sxpNormal);
  indexes.add('CoverageEligibilityRequest', 'patient', 'The reference to the patient', sptREFERENCE, ['Patient'], 'CoverageEligibilityRequest.patient', sxpNormal);
  indexes.add('CoverageEligibilityRequest', 'provider', 'The reference to the provider', sptREFERENCE, ['Practitioner', 'Organization', 'PractitionerRole'], 'CoverageEligibilityRequest.provider', sxpNormal);
  indexes.add('CoverageEligibilityRequest', 'status', 'The status of the EligibilityRequest', sptTOKEN, [], 'CoverageEligibilityRequest.status', sxpNormal);
  indexes.add('CoverageEligibilityRequest', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Patient', 'CoverageEligibilityRequest', ['patient']);
  compartments.register('Practitioner', 'CoverageEligibilityRequest', ['enterer', 'provider']);
end;
{$ENDIF FHIR_COVERAGEELIGIBILITYREQUEST}

{$IFDEF FHIR_COVERAGEELIGIBILITYRESPONSE}
procedure TFHIRIndexBuilderR5.buildIndexesForCoverageEligibilityResponse(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('CoverageEligibilityResponse', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('CoverageEligibilityResponse', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('CoverageEligibilityResponse', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('CoverageEligibilityResponse', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('CoverageEligibilityResponse', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('CoverageEligibilityResponse', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('CoverageEligibilityResponse', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('CoverageEligibilityResponse', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('CoverageEligibilityResponse', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('CoverageEligibilityResponse', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('CoverageEligibilityResponse', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('CoverageEligibilityResponse', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('CoverageEligibilityResponse', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('CoverageEligibilityResponse', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('CoverageEligibilityResponse', 'created', 'The creation date', sptDATE, [], 'CoverageEligibilityResponse.created', sxpNormal);
  indexes.add('CoverageEligibilityResponse', 'disposition', 'The contents of the disposition message', sptSTRING, [], 'CoverageEligibilityResponse.disposition', sxpNormal);
  indexes.add('CoverageEligibilityResponse', 'identifier', 'The business identifier', sptTOKEN, [], 'CoverageEligibilityResponse.identifier', sxpNormal);
  indexes.add('CoverageEligibilityResponse', 'insurer', 'The organization which generated this resource', sptREFERENCE, ['Organization'], 'CoverageEligibilityResponse.insurer', sxpNormal);
  indexes.add('CoverageEligibilityResponse', 'outcome', 'The processing outcome', sptTOKEN, [], 'CoverageEligibilityResponse.outcome', sxpNormal);
  indexes.add('CoverageEligibilityResponse', 'patient', 'The reference to the patient', sptREFERENCE, ['Patient'], 'CoverageEligibilityResponse.patient', sxpNormal);
  indexes.add('CoverageEligibilityResponse', 'request', 'The EligibilityRequest reference', sptREFERENCE, ['CoverageEligibilityRequest'], 'CoverageEligibilityResponse.request', sxpNormal);
  indexes.add('CoverageEligibilityResponse', 'requestor', 'The EligibilityRequest provider', sptREFERENCE, ['Practitioner', 'Organization', 'PractitionerRole'], 'CoverageEligibilityResponse.requestor', sxpNormal);
  indexes.add('CoverageEligibilityResponse', 'status', 'The EligibilityRequest status', sptTOKEN, [], 'CoverageEligibilityResponse.status', sxpNormal);
  indexes.add('CoverageEligibilityResponse', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Patient', 'CoverageEligibilityResponse', ['patient']);
  compartments.register('Practitioner', 'CoverageEligibilityResponse', ['requestor']);
end;
{$ENDIF FHIR_COVERAGEELIGIBILITYRESPONSE}

{$IFDEF FHIR_DETECTEDISSUE}
procedure TFHIRIndexBuilderR5.buildIndexesForDetectedIssue(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('DetectedIssue', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('DetectedIssue', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('DetectedIssue', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('DetectedIssue', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('DetectedIssue', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('DetectedIssue', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('DetectedIssue', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('DetectedIssue', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('DetectedIssue', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('DetectedIssue', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('DetectedIssue', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('DetectedIssue', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('DetectedIssue', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('DetectedIssue', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('DetectedIssue', 'author', 'The provider or device that identified the issue', sptREFERENCE, ['Practitioner', 'Device', 'Patient', 'PractitionerRole', 'RelatedPerson'], 'DetectedIssue.author', sxpNormal);
  indexes.add('DetectedIssue', 'category', 'Issue Category, e.g. drug-drug, duplicate therapy, etc.', sptTOKEN, [], 'DetectedIssue.category', sxpNormal);
  indexes.add('DetectedIssue', 'code', 'Issue Type, e.g. drug-drug, duplicate therapy, etc.', sptTOKEN, [], 'DetectedIssue.code', sxpNormal);
  indexes.add('DetectedIssue', 'identified', 'When identified', sptDATE, [], 'DetectedIssue.identified', sxpNormal);
  indexes.add('DetectedIssue', 'identifier', '): Unique id for the detected issue', sptTOKEN, [], 'AllergyIntolerance.identifier | CarePlan.identifier | CareTeam.identifier | Composition.identifier | Condition.identifier | Consent.identifier | DetectedIssue.identifier | DeviceRequest.identifier | DiagnosticReport.identifier | DocumentManifest.mast'
      +'erIdentifier | DocumentManifest.identifier | DocumentReference.identifier | Encounter.identifier | EpisodeOfCare.identifier | FamilyMemberHistory.identifier | Goal.identifier | ImagingStudy.identifier | Immunization.identifier | List.identifi'
      +'er | MedicationAdministration.identifier | MedicationDispense.identifier | MedicationRequest.identifier | MedicationUsage.identifier | NutritionOrder.identifier | Observation.identifier | Procedure.identifier | RiskAssessment.identifier | Ser'
      +'viceRequest.identifier | SupplyDelivery.identifier | SupplyRequest.identifier | VisionPrescription.identifier', sxpNormal);
  indexes.add('DetectedIssue', 'implicated', 'Problem resource', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'DetectedIssue.implicated', sxpNormal);
  indexes.add('DetectedIssue', 'patient', '): Associated patient', sptREFERENCE, ['Patient', 'Practitioner', 'Group', 'Organization', 'BiologicallyDerivedProduct', 'NutritionProduct', 'Device', 'Medication', 'Procedure', 'Substance', 'Location'], 'AllergyIntolerance.patient | CarePlan.subject.where(resolve() is Patient) | CareTeam.subject.where(resolve() is Patient) | ClinicalImpression.subject.where(resolve() is Patient) | Composition.subject.where(resolve() is Patient) | Condition.subject.wh'
      +'ere(resolve() is Patient) | Consent.subject.where(resolve() is Patient) | DetectedIssue.subject | DeviceRequest.subject.where(resolve() is Patient) | DeviceUsage.patient | DiagnosticReport.subject.where(resolve() is Patient) | DocumentManifes'
      +'t.subject.where(resolve() is Patient) | DocumentReference.subject.where(resolve() is Patient) | Encounter.subject.where(resolve() is Patient) | EpisodeOfCare.patient | FamilyMemberHistory.patient | Flag.subject.where(resolve() is Patient)'+
   ' | G'
      +'oal.subject.where(resolve() is Patient) | ImagingStudy.subject.where(resolve() is Patient) | Immunization.patient | List.subject.where(resolve() is Patient) | MedicationAdministration.subject.where(resolve() is Patient) | MedicationDispense.s'
      +'ubject.where(resolve() is Patient) | MedicationRequest.subject.where(resolve() is Patient) | MedicationUsage.subject.where(resolve() is Patient) | NutritionOrder.subject.where(resolve() is Patient) | Observation.subject.where(resolve() is Pat'
      +'ient) | Procedure.subject.where(resolve() is Patient) | RiskAssessment.subject.where(resolve() is Patient) | ServiceRequest.subject.where(resolve() is Patient) | SupplyDelivery.patient | VisionPrescription.patient', sxpNormal);
  indexes.add('DetectedIssue', 'status', 'The status of the issue', sptTOKEN, [], 'DetectedIssue.status', sxpNormal);
  indexes.add('DetectedIssue', 'subject', 'Associated subject', sptREFERENCE, ['Practitioner', 'Group', 'Organization', 'BiologicallyDerivedProduct', 'NutritionProduct', 'Device', 'Medication', 'Patient', 'Procedure', 'Substance', 'Location'], 'DetectedIssue.subject', sxpNormal);
  indexes.add('DetectedIssue', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Device', 'DetectedIssue', ['author']);
  compartments.register('Patient', 'DetectedIssue', ['patient']);
  compartments.register('Practitioner', 'DetectedIssue', ['author']);
end;
{$ENDIF FHIR_DETECTEDISSUE}

{$IFDEF FHIR_DEVICE}
procedure TFHIRIndexBuilderR5.buildIndexesForDevice(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Device', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('Device', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('Device', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('Device', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('Device', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('Device', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('Device', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('Device', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('Device', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('Device', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('Device', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('Device', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('Device', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('Device', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('Device', 'biological-source-event', 'The biological source for the device', sptTOKEN, [], 'Device.biologicalSourceEvent', sxpNormal);
  indexes.add('Device', 'definition', 'The definition / type of the device', sptREFERENCE, [], 'Device.definition.reference', sxpNormal);
  indexes.add('Device', 'device-name', 'A server defined search that may match any of the string fields in Device.deviceName or Device.type.', sptSTRING, [], 'Device.deviceName.name | Device.type.coding.display | Device.type.text', sxpNormal);
  indexes.add('Device', 'din', 'The donation identification number (DIN)', sptTOKEN, [], 'Device.extension(''http://hl7.org/fhir/SearchParameter/device-extensions-Device-din'').value', sxpNormal);
  indexes.add('Device', 'expiration-date', 'The expiration date of the device', sptDATE, [], 'Device.expirationDate', sxpNormal);
  indexes.add('Device', 'identifier', 'Instance id from manufacturer, owner, and others', sptTOKEN, [], 'Device.identifier', sxpNormal);
  indexes.add('Device', 'location', 'A location, where the resource is found', sptREFERENCE, ['Location'], 'Device.location', sxpNormal);
  indexes.add('Device', 'lot-number', 'The lot number of the device', sptSTRING, [], 'Device.lotNumber', sxpNormal);
  indexes.add('Device', 'manufacture-date', 'The manufacture date of the device', sptDATE, [], 'Device.manufactureDate', sxpNormal);
  indexes.add('Device', 'manufacturer', 'The manufacturer of the device', sptSTRING, [], 'Device.manufacturer', sxpNormal);
  indexes.add('Device', 'model', 'The model of the device', sptSTRING, [], 'Device.modelNumber', sxpNormal);
  indexes.add('Device', 'organization', 'The organization responsible for the device', sptREFERENCE, ['Organization'], 'Device.owner', sxpNormal);
  indexes.add('Device', 'parent', 'The parent device', sptREFERENCE, ['Device'], 'Device.parent', sxpNormal);
  indexes.add('Device', 'patient', 'Patient information, if the resource is affixed to a person', sptREFERENCE, ['Patient'], 'Device.association.humanSubject.where(resolve() is Patient)', sxpNormal);
  indexes.add('Device', 'serial-number', 'The serial number of the device', sptSTRING, [], 'Device.serialNumber | Device.identifier.where(type=''SNO'')', sxpNormal);
  indexes.add('Device', 'status', 'active | inactive | entered-in-error | unknown', sptTOKEN, [], 'Device.status', sxpNormal);
  indexes.add('Device', 'subject', 'Subject to which the device is associated of affixed', sptREFERENCE, ['Patient'], 'Device.association.humanSubject', sxpNormal);
  indexes.add('Device', 'type', 'The type of the device', sptTOKEN, [], 'Device.type', sxpNormal);
  indexes.add('Device', 'udi-carrier', 'UDI Barcode (RFID or other technology) string in *HRF* format.', sptSTRING, [], 'Device.udiCarrier.carrierHRF', sxpNormal);
  indexes.add('Device', 'udi-di', 'The udi Device Identifier (DI)', sptSTRING, [], 'Device.udiCarrier.deviceIdentifier', sxpNormal);
  indexes.add('Device', 'url', 'Network address to contact device', sptURI, [], 'Device.url', sxpNormal);
  indexes.add('Device', 'version', 'The specific version of the device', sptSTRING, [], 'Device.version.value', sxpNormal);
  indexes.add('Device', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Patient', 'Device', ['subject']);
end;
{$ENDIF FHIR_DEVICE}

{$IFDEF FHIR_DEVICEDEFINITION}
procedure TFHIRIndexBuilderR5.buildIndexesForDeviceDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('DeviceDefinition', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('DeviceDefinition', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('DeviceDefinition', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('DeviceDefinition', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('DeviceDefinition', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('DeviceDefinition', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('DeviceDefinition', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('DeviceDefinition', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('DeviceDefinition', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('DeviceDefinition', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('DeviceDefinition', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('DeviceDefinition', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('DeviceDefinition', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('DeviceDefinition', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('DeviceDefinition', 'identifier', 'The identifier of the component', sptTOKEN, [], 'DeviceDefinition.identifier', sxpNormal);
  indexes.add('DeviceDefinition', 'parent', 'The parent DeviceDefinition resource', sptREFERENCE, ['DeviceDefinition'], 'DeviceDefinition.parentDevice', sxpNormal);
  indexes.add('DeviceDefinition', 'type', 'The device component type', sptTOKEN, [], 'DeviceDefinition.classification.type', sxpNormal);
  indexes.add('DeviceDefinition', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_DEVICEDEFINITION}

{$IFDEF FHIR_DEVICEDISPENSE}
procedure TFHIRIndexBuilderR5.buildIndexesForDeviceDispense(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('DeviceDispense', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('DeviceDispense', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('DeviceDispense', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('DeviceDispense', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('DeviceDispense', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('DeviceDispense', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('DeviceDispense', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('DeviceDispense', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('DeviceDispense', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('DeviceDispense', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('DeviceDispense', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('DeviceDispense', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('DeviceDispense', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('DeviceDispense', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('DeviceDispense', 'code', 'Search for devices that match this code', sptTOKEN, [], 'DeviceDispense.device.concept', sxpNormal);
  indexes.add('DeviceDispense', 'subject', 'The identity of a patient for whom to list dispenses', sptREFERENCE, ['Patient'], 'DeviceDispense.subject', sxpNormal);
  indexes.add('DeviceDispense', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_DEVICEDISPENSE}

{$IFDEF FHIR_DEVICEMETRIC}
procedure TFHIRIndexBuilderR5.buildIndexesForDeviceMetric(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('DeviceMetric', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('DeviceMetric', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('DeviceMetric', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('DeviceMetric', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('DeviceMetric', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('DeviceMetric', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('DeviceMetric', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('DeviceMetric', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('DeviceMetric', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('DeviceMetric', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('DeviceMetric', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('DeviceMetric', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('DeviceMetric', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('DeviceMetric', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('DeviceMetric', 'category', 'The category of the metric', sptTOKEN, [], 'DeviceMetric.category', sxpNormal);
  indexes.add('DeviceMetric', 'identifier', 'The identifier of the metric', sptTOKEN, [], 'DeviceMetric.identifier', sxpNormal);
  indexes.add('DeviceMetric', 'parent', 'The parent DeviceMetric resource', sptREFERENCE, ['Device'], 'DeviceMetric.parent', sxpNormal);
  indexes.add('DeviceMetric', 'source', 'The device resource', sptREFERENCE, ['Device'], 'DeviceMetric.source', sxpNormal);
  indexes.add('DeviceMetric', 'type', 'The component type', sptTOKEN, [], 'DeviceMetric.type', sxpNormal);
  indexes.add('DeviceMetric', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_DEVICEMETRIC}

{$IFDEF FHIR_DEVICEREQUEST}
procedure TFHIRIndexBuilderR5.buildIndexesForDeviceRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('DeviceRequest', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('DeviceRequest', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('DeviceRequest', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('DeviceRequest', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('DeviceRequest', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('DeviceRequest', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('DeviceRequest', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('DeviceRequest', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('DeviceRequest', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('DeviceRequest', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('DeviceRequest', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('DeviceRequest', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('DeviceRequest', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('DeviceRequest', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('DeviceRequest', 'authored-on', 'When the request transitioned to being actionable', sptDATE, [], 'DeviceRequest.authoredOn', sxpNormal);
  indexes.add('DeviceRequest', 'based-on', 'Plan/proposal/order fulfilled by this request', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'DeviceRequest.basedOn', sxpNormal);
  indexes.add('DeviceRequest', 'code', '): Code for what is being requested/ordered', sptTOKEN, [], 'AllergyIntolerance.code | AllergyIntolerance.reaction.substance | Condition.code | DeviceRequest.code.concept | DiagnosticReport.code | FamilyMemberHistory.condition.code | List.code | Medication.code | MedicationAdministration.medication.concept | M'
      +'edicationDispense.medication.concept | MedicationRequest.medication.concept | MedicationUsage.medication.concept | Observation.code | Procedure.code', sxpNormal);
  indexes.add('DeviceRequest', 'device', 'Reference to resource that is being requested/ordered', sptREFERENCE, [], 'DeviceRequest.code.reference', sxpNormal);
  indexes.add('DeviceRequest', 'encounter', '): Encounter during which request was created', sptREFERENCE, ['Encounter'], 'Composition.encounter | DeviceRequest.encounter | DiagnosticReport.encounter | Flag.encounter | List.encounter | NutritionOrder.encounter | Observation.encounter | Procedure.encounter | RiskAssessment.encounter | ServiceRequest.encounter | VisionPres'
      +'cription.encounter', sxpNormal);
  indexes.add('DeviceRequest', 'event-date', 'When service should occur', sptDATE, [], '(DeviceRequest.occurrence as dateTime) | (DeviceRequest.occurrence as Period)', sxpNormal);
  indexes.add('DeviceRequest', 'group-identifier', 'Composite request this is part of', sptTOKEN, [], 'DeviceRequest.groupIdentifier', sxpNormal);
  indexes.add('DeviceRequest', 'identifier', '): Business identifier for request/order', sptTOKEN, [], 'AllergyIntolerance.identifier | CarePlan.identifier | CareTeam.identifier | Composition.identifier | Condition.identifier | Consent.identifier | DetectedIssue.identifier | DeviceRequest.identifier | DiagnosticReport.identifier | DocumentManifest.mast'
      +'erIdentifier | DocumentManifest.identifier | DocumentReference.identifier | Encounter.identifier | EpisodeOfCare.identifier | FamilyMemberHistory.identifier | Goal.identifier | ImagingStudy.identifier | Immunization.identifier | List.identifi'
      +'er | MedicationAdministration.identifier | MedicationDispense.identifier | MedicationRequest.identifier | MedicationUsage.identifier | NutritionOrder.identifier | Observation.identifier | Procedure.identifier | RiskAssessment.identifier | Ser'
      +'viceRequest.identifier | SupplyDelivery.identifier | SupplyRequest.identifier | VisionPrescription.identifier', sxpNormal);
  indexes.add('DeviceRequest', 'instantiates-canonical', 'Instantiates FHIR protocol or definition', sptREFERENCE, ['PlanDefinition', 'ActivityDefinition'], 'DeviceRequest.instantiatesCanonical', sxpNormal);
  indexes.add('DeviceRequest', 'instantiates-uri', 'Instantiates external protocol or definition', sptURI, [], 'DeviceRequest.instantiatesUri', sxpNormal);
  indexes.add('DeviceRequest', 'insurance', 'Associated insurance coverage', sptREFERENCE, ['ClaimResponse', 'Coverage'], 'DeviceRequest.insurance', sxpNormal);
  indexes.add('DeviceRequest', 'intent', 'proposal | plan | original-order |reflex-order', sptTOKEN, [], 'DeviceRequest.intent', sxpNormal);
  indexes.add('DeviceRequest', 'patient', '): Individual the service is ordered for', sptREFERENCE, ['Patient', 'Practitioner', 'Group', 'Organization', 'BiologicallyDerivedProduct', 'NutritionProduct', 'Device', 'Medication', 'Procedure', 'Substance', 'Location'], 'AllergyIntolerance.patient | CarePlan.subject.where(resolve() is Patient) | CareTeam.subject.where(resolve() is Patient) | ClinicalImpression.subject.where(resolve() is Patient) | Composition.subject.where(resolve() is Patient) | Condition.subject.wh'
      +'ere(resolve() is Patient) | Consent.subject.where(resolve() is Patient) | DetectedIssue.subject | DeviceRequest.subject.where(resolve() is Patient) | DeviceUsage.patient | DiagnosticReport.subject.where(resolve() is Patient) | DocumentManifes'
      +'t.subject.where(resolve() is Patient) | DocumentReference.subject.where(resolve() is Patient) | Encounter.subject.where(resolve() is Patient) | EpisodeOfCare.patient | FamilyMemberHistory.patient | Flag.subject.where(re'+
   'solve() is Patient) | G'
      +'oal.subject.where(resolve() is Patient) | ImagingStudy.subject.where(resolve() is Patient) | Immunization.patient | List.subject.where(resolve() is Patient) | MedicationAdministration.subject.where(resolve() is Patient) | MedicationDispense.s'
      +'ubject.where(resolve() is Patient) | MedicationRequest.subject.where(resolve() is Patient) | MedicationUsage.subject.where(resolve() is Patient) | NutritionOrder.subject.where(resolve() is Patient) | Observation.subject.where(resolve() is Pat'
      +'ient) | Procedure.subject.where(resolve() is Patient) | RiskAssessment.subject.where(resolve() is Patient) | ServiceRequest.subject.where(resolve() is Patient) | SupplyDelivery.patient | VisionPrescription.patient', sxpNormal);
  indexes.add('DeviceRequest', 'performer', 'Desired performer for service', sptREFERENCE, ['Practitioner', 'Organization', 'CareTeam', 'Device', 'Patient', 'HealthcareService', 'PractitionerRole', 'RelatedPerson'], 'DeviceRequest.performer', sxpNormal);
  indexes.add('DeviceRequest', 'prior-request', 'Request takes the place of referenced completed or terminated requests', sptREFERENCE, ['DeviceRequest'], 'DeviceRequest.replaces', sxpNormal);
  indexes.add('DeviceRequest', 'requester', 'Who/what is requesting service', sptREFERENCE, ['Practitioner', 'Organization', 'Device', 'PractitionerRole'], 'DeviceRequest.requester', sxpNormal);
  indexes.add('DeviceRequest', 'status', 'entered-in-error | draft | active |suspended | completed', sptTOKEN, [], 'DeviceRequest.status', sxpNormal);
  indexes.add('DeviceRequest', 'subject', 'Individual the service is ordered for', sptREFERENCE, ['Group', 'Device', 'Patient', 'Location'], 'DeviceRequest.subject', sxpNormal);
  indexes.add('DeviceRequest', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Device', 'DeviceRequest', ['subject', 'requester', 'performer']);
  compartments.register('Encounter', 'DeviceRequest', ['encounter']);
  compartments.register('Patient', 'DeviceRequest', ['subject', 'performer']);
  compartments.register('Practitioner', 'DeviceRequest', ['requester', 'performer']);
end;
{$ENDIF FHIR_DEVICEREQUEST}

{$IFDEF FHIR_DEVICEUSAGE}
procedure TFHIRIndexBuilderR5.buildIndexesForDeviceUsage(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('DeviceUsage', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('DeviceUsage', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('DeviceUsage', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('DeviceUsage', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('DeviceUsage', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('DeviceUsage', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('DeviceUsage', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('DeviceUsage', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('DeviceUsage', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('DeviceUsage', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('DeviceUsage', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('DeviceUsage', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('DeviceUsage', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('DeviceUsage', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('DeviceUsage', 'device', 'Search by device', sptTOKEN, [], 'DeviceUsage.device.concept', sxpNormal);
  indexes.add('DeviceUsage', 'identifier', 'Search by identifier', sptTOKEN, [], 'DeviceUsage.identifier', sxpNormal);
  indexes.add('DeviceUsage', 'patient', '): Search by patient who used / uses the device', sptREFERENCE, ['Patient', 'Practitioner', 'Group', 'Organization', 'BiologicallyDerivedProduct', 'NutritionProduct', 'Device', 'Medication', 'Procedure', 'Substance', 'Location'], 'AllergyIntolerance.patient | CarePlan.subject.where(resolve() is Patient) | CareTeam.subject.where(resolve() is Patient) | ClinicalImpression.subject.where(resolve() is Patient) | Composition.subject.where(resolve() is Patient) | Condition.subject.wh'
      +'ere(resolve() is Patient) | Consent.subject.where(resolve() is Patient) | DetectedIssue.subject | DeviceRequest.subject.where(resolve() is Patient) | DeviceUsage.patient | DiagnosticReport.subject.where(resolve() is Patient) | DocumentManifes'
      +'t.subject.where(resolve() is Patient) | DocumentReference.subject.where(resolve() is Patient) | Encounter.subject.where(resolve() is Patient) | EpisodeOfCare.patient | FamilyMemberHistory.patient | Flag.subject.whe'+
   're(resolve() is Patient) | G'
      +'oal.subject.where(resolve() is Patient) | ImagingStudy.subject.where(resolve() is Patient) | Immunization.patient | List.subject.where(resolve() is Patient) | MedicationAdministration.subject.where(resolve() is Patient) | MedicationDispense.s'
      +'ubject.where(resolve() is Patient) | MedicationRequest.subject.where(resolve() is Patient) | MedicationUsage.subject.where(resolve() is Patient) | NutritionOrder.subject.where(resolve() is Patient) | Observation.subject.where(resolve() is Pat'
      +'ient) | Procedure.subject.where(resolve() is Patient) | RiskAssessment.subject.where(resolve() is Patient) | ServiceRequest.subject.where(resolve() is Patient) | SupplyDelivery.patient | VisionPrescription.patient', sxpNormal);
  indexes.add('DeviceUsage', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Patient', 'DeviceUsage', ['patient']);
end;
{$ENDIF FHIR_DEVICEUSAGE}

{$IFDEF FHIR_DIAGNOSTICREPORT}
procedure TFHIRIndexBuilderR5.buildIndexesForDiagnosticReport(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('DiagnosticReport', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('DiagnosticReport', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('DiagnosticReport', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('DiagnosticReport', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('DiagnosticReport', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('DiagnosticReport', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('DiagnosticReport', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('DiagnosticReport', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('DiagnosticReport', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('DiagnosticReport', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('DiagnosticReport', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('DiagnosticReport', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('DiagnosticReport', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('DiagnosticReport', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('DiagnosticReport', 'based-on', 'Reference to the service request.', sptREFERENCE, ['CarePlan', 'MedicationRequest', 'NutritionOrder', 'ServiceRequest', 'ImmunizationRecommendation'], 'DiagnosticReport.basedOn', sxpNormal);
  indexes.add('DiagnosticReport', 'category', 'Which diagnostic discipline/department created the report', sptTOKEN, [], 'DiagnosticReport.category', sxpNormal);
  indexes.add('DiagnosticReport', 'code', '): The code for the report, as opposed to codes for the atomic results, which are the names on the observation resource referred to from the result', sptTOKEN, [], 'AllergyIntolerance.code | AllergyIntolerance.reaction.substance | Condition.code | DeviceRequest.code.concept | DiagnosticReport.code | FamilyMemberHistory.condition.code | List.code | Medication.code | MedicationAdministration.medication.concept | M'
      +'edicationDispense.medication.concept | MedicationRequest.medication.concept | MedicationUsage.medication.concept | Observation.code | Procedure.code', sxpNormal);
  indexes.add('DiagnosticReport', 'conclusion', 'A coded conclusion (interpretation/impression) on the report', sptTOKEN, [], 'DiagnosticReport.conclusionCode', sxpNormal);
  indexes.add('DiagnosticReport', 'date', '): The clinically relevant time of the report', sptDATE, [], 'AllergyIntolerance.recordedDate | CarePlan.period | ClinicalImpression.date | Composition.date | Consent.dateTime | DiagnosticReport.effective | Encounter.actualPeriod | EpisodeOfCare.period | FamilyMemberHistory.date | Flag.period | (Immunization.oc'
      +'currence as dateTime) | List.date | Observation.effective | Procedure.occurrence | (RiskAssessment.occurrence as dateTime) | SupplyRequest.authoredOn', sxpNormal);
  indexes.add('DiagnosticReport', 'encounter', '): The Encounter when the order was made', sptREFERENCE, ['Encounter'], 'Composition.encounter | DeviceRequest.encounter | DiagnosticReport.encounter | Flag.encounter | List.encounter | NutritionOrder.encounter | Observation.encounter | Procedure.encounter | RiskAssessment.encounter | ServiceRequest.encounter | VisionPres'
      +'cription.encounter', sxpNormal);
  indexes.add('DiagnosticReport', 'identifier', '): An identifier for the report', sptTOKEN, [], 'AllergyIntolerance.identifier | CarePlan.identifier | CareTeam.identifier | Composition.identifier | Condition.identifier | Consent.identifier | DetectedIssue.identifier | DeviceRequest.identifier | DiagnosticReport.identifier | DocumentManifest.mast'
      +'erIdentifier | DocumentManifest.identifier | DocumentReference.identifier | Encounter.identifier | EpisodeOfCare.identifier | FamilyMemberHistory.identifier | Goal.identifier | ImagingStudy.identifier | Immunization.identifier | List.identifi'
      +'er | MedicationAdministration.identifier | MedicationDispense.identifier | MedicationRequest.identifier | MedicationUsage.identifier | NutritionOrder.identifier | Observation.identifier | Procedure.identifier | RiskAssessment.identifier | Ser'
      +'viceRequest.identifier | SupplyDelivery.identifier | SupplyRequest.identifier | VisionPrescription.identifier', sxpNormal);
  indexes.add('DiagnosticReport', 'issued', 'When the report was issued', sptDATE, [], 'DiagnosticReport.issued', sxpNormal);
  indexes.add('DiagnosticReport', 'media', 'A reference to the image source.', sptREFERENCE, ['DocumentReference'], 'DiagnosticReport.media.link', sxpNormal);
  indexes.add('DiagnosticReport', 'patient', '): The subject of the report if a patient', sptREFERENCE, ['Patient', 'Practitioner', 'Group', 'Organization', 'BiologicallyDerivedProduct', 'NutritionProduct', 'Device', 'Medication', 'Procedure', 'Substance', 'Location'], 'AllergyIntolerance.patient | CarePlan.subject.where(resolve() is Patient) | CareTeam.subject.where(resolve() is Patient) | ClinicalImpression.subject.where(resolve() is Patient) | Composition.subject.where(resolve() is Patient) | Condition.subject.wh'
      +'ere(resolve() is Patient) | Consent.subject.where(resolve() is Patient) | DetectedIssue.subject | DeviceRequest.subject.where(resolve() is Patient) | DeviceUsage.patient | DiagnosticReport.subject.where(resolve() is Patient) | DocumentManifes'
      +'t.subject.where(resolve() is Patient) | DocumentReference.subject.where(resolve() is Patient) | Encounter.subject.where(resolve() is Patient) | EpisodeOfCare.patient | FamilyMemberHistory.patient | Flag.subject.wher'+
   'e(resolve() is Patient) | G'
      +'oal.subject.where(resolve() is Patient) | ImagingStudy.subject.where(resolve() is Patient) | Immunization.patient | List.subject.where(resolve() is Patient) | MedicationAdministration.subject.where(resolve() is Patient) | MedicationDispense.s'
      +'ubject.where(resolve() is Patient) | MedicationRequest.subject.where(resolve() is Patient) | MedicationUsage.subject.where(resolve() is Patient) | NutritionOrder.subject.where(resolve() is Patient) | Observation.subject.where(resolve() is Pat'
      +'ient) | Procedure.subject.where(resolve() is Patient) | RiskAssessment.subject.where(resolve() is Patient) | ServiceRequest.subject.where(resolve() is Patient) | SupplyDelivery.patient | VisionPrescription.patient', sxpNormal);
  indexes.add('DiagnosticReport', 'performer', 'Who is responsible for the report', sptREFERENCE, ['Practitioner', 'Organization', 'CareTeam', 'PractitionerRole'], 'DiagnosticReport.performer', sxpNormal);
  indexes.add('DiagnosticReport', 'result', 'Link to an atomic result (observation resource)', sptREFERENCE, ['Observation'], 'DiagnosticReport.result', sxpNormal);
  indexes.add('DiagnosticReport', 'results-interpreter', 'Who was the source of the report', sptREFERENCE, ['Practitioner', 'Organization', 'CareTeam', 'PractitionerRole'], 'DiagnosticReport.resultsInterpreter', sxpNormal);
  indexes.add('DiagnosticReport', 'specimen', 'The specimen details', sptREFERENCE, ['Specimen'], 'DiagnosticReport.specimen', sxpNormal);
  indexes.add('DiagnosticReport', 'status', 'The status of the report', sptTOKEN, [], 'DiagnosticReport.status', sxpNormal);
  indexes.add('DiagnosticReport', 'study', 'Studies associated with the diagnostic report', sptREFERENCE, ['GenomicStudy', 'ImagingStudy'], 'DiagnosticReport.study', sxpNormal);
  indexes.add('DiagnosticReport', 'subject', 'The subject of the report', sptREFERENCE, ['Practitioner', 'Group', 'Organization', 'BiologicallyDerivedProduct', 'Device', 'Medication', 'Patient', 'Procedure', 'Substance', 'Location'], 'DiagnosticReport.subject', sxpNormal);
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
  indexes.add('DocumentManifest', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('DocumentManifest', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('DocumentManifest', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('DocumentManifest', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('DocumentManifest', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('DocumentManifest', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('DocumentManifest', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('DocumentManifest', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('DocumentManifest', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('DocumentManifest', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('DocumentManifest', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('DocumentManifest', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('DocumentManifest', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('DocumentManifest', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('DocumentManifest', 'author', 'Who and/or what authored the DocumentManifest', sptREFERENCE, ['Practitioner', 'Organization', 'Device', 'Patient', 'PractitionerRole', 'RelatedPerson'], 'DocumentManifest.author', sxpNormal);
  indexes.add('DocumentManifest', 'created', 'When this document manifest created', sptDATE, [], 'DocumentManifest.created', sxpNormal);
  indexes.add('DocumentManifest', 'description', 'Human-readable description (title)', sptSTRING, [], 'DocumentManifest.description', sxpNormal);
  indexes.add('DocumentManifest', 'identifier', '): Unique Identifier for the set of documents', sptTOKEN, [], 'AllergyIntolerance.identifier | CarePlan.identifier | CareTeam.identifier | Composition.identifier | Condition.identifier | Consent.identifier | DetectedIssue.identifier | DeviceRequest.identifier | DiagnosticReport.identifier | DocumentManifest.mast'
      +'erIdentifier | DocumentManifest.identifier | DocumentReference.identifier | Encounter.identifier | EpisodeOfCare.identifier | FamilyMemberHistory.identifier | Goal.identifier | ImagingStudy.identifier | Immunization.identifier | List.identifi'
      +'er | MedicationAdministration.identifier | MedicationDispense.identifier | MedicationRequest.identifier | MedicationUsage.identifier | NutritionOrder.identifier | Observation.identifier | Procedure.identifier | RiskAssessment.identifier | Ser'
      +'viceRequest.identifier | SupplyDelivery.identifier | SupplyRequest.identifier | VisionPrescription.identifier', sxpNormal);
  indexes.add('DocumentManifest', 'item', 'Items in manifest', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'DocumentManifest.content', sxpNormal);
  indexes.add('DocumentManifest', 'patient', '): The subject of the set of documents', sptREFERENCE, ['Patient', 'Practitioner', 'Group', 'Organization', 'BiologicallyDerivedProduct', 'NutritionProduct', 'Device', 'Medication', 'Procedure', 'Substance', 'Location'], 'AllergyIntolerance.patient | CarePlan.subject.where(resolve() is Patient) | CareTeam.subject.where(resolve() is Patient) | ClinicalImpression.subject.where(resolve() is Patient) | Composition.subject.where(resolve() is Patient) | Condition.subject.wh'
      +'ere(resolve() is Patient) | Consent.subject.where(resolve() is Patient) | DetectedIssue.subject | DeviceRequest.subject.where(resolve() is Patient) | DeviceUsage.patient | DiagnosticReport.subject.where(resolve() is Patient) | DocumentManifes'
      +'t.subject.where(resolve() is Patient) | DocumentReference.subject.where(resolve() is Patient) | Encounter.subject.where(resolve() is Patient) | EpisodeOfCare.patient | FamilyMemberHistory.patient | Flag.subject.where(r'+
   'esolve() is Patient) | G'
      +'oal.subject.where(resolve() is Patient) | ImagingStudy.subject.where(resolve() is Patient) | Immunization.patient | List.subject.where(resolve() is Patient) | MedicationAdministration.subject.where(resolve() is Patient) | MedicationDispense.s'
      +'ubject.where(resolve() is Patient) | MedicationRequest.subject.where(resolve() is Patient) | MedicationUsage.subject.where(resolve() is Patient) | NutritionOrder.subject.where(resolve() is Patient) | Observation.subject.where(resolve() is Pat'
      +'ient) | Procedure.subject.where(resolve() is Patient) | RiskAssessment.subject.where(resolve() is Patient) | ServiceRequest.subject.where(resolve() is Patient) | SupplyDelivery.patient | VisionPrescription.patient', sxpNormal);
  indexes.add('DocumentManifest', 'recipient', 'Intended to get notified about this set of documents', sptREFERENCE, ['Practitioner', 'Organization', 'Patient', 'PractitionerRole', 'RelatedPerson'], 'DocumentManifest.recipient', sxpNormal);
  indexes.add('DocumentManifest', 'related-id', 'Identifiers of things that are related', sptTOKEN, [], 'DocumentManifest.related.identifier', sxpNormal);
  indexes.add('DocumentManifest', 'related-ref', 'Related Resource', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'DocumentManifest.related.ref', sxpNormal);
  indexes.add('DocumentManifest', 'source', 'The source system/application/software', sptURI, [], 'DocumentManifest.source', sxpNormal);
  indexes.add('DocumentManifest', 'status', 'current | superseded | entered-in-error', sptTOKEN, [], 'DocumentManifest.status', sxpNormal);
  indexes.add('DocumentManifest', 'subject', 'The subject of the set of documents', sptREFERENCE, ['Practitioner', 'Group', 'Device', 'Patient'], 'DocumentManifest.subject', sxpNormal);
  indexes.add('DocumentManifest', 'type', '): Kind of document set', sptTOKEN, [], 'AllergyIntolerance.type | Composition.type | DocumentManifest.type | DocumentReference.type | Encounter.type | EpisodeOfCare.type', sxpNormal);
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
  indexes.add('DocumentReference', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('DocumentReference', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('DocumentReference', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('DocumentReference', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('DocumentReference', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('DocumentReference', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('DocumentReference', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('DocumentReference', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('DocumentReference', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('DocumentReference', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('DocumentReference', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('DocumentReference', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('DocumentReference', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('DocumentReference', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('DocumentReference', 'attester', 'Who attested the document', sptREFERENCE, ['Practitioner', 'Organization', 'Patient', 'PractitionerRole', 'RelatedPerson'], 'DocumentReference.attester.party', sxpNormal);
  indexes.add('DocumentReference', 'author', 'Who and/or what authored the document', sptREFERENCE, ['Practitioner', 'Organization', 'CareTeam', 'Device', 'Patient', 'PractitionerRole', 'RelatedPerson'], 'DocumentReference.author', sxpNormal);
  indexes.add('DocumentReference', 'based-on', 'Procedure that caused this media to be created', sptREFERENCE, ['Appointment', 'MedicationRequest', 'RequestOrchestration', 'VisionPrescription', 'ServiceRequest', 'SupplyRequest', 'AppointmentResponse', 'CoverageEligibilityRequest', 'CarePlan', 'EnrollmentRequest', 'EpisodeOfCare', 'NutritionOrder', 'DeviceRequest', 'Contract', 'Claim', 'CommunicationRequest', 'ImmunizationRecommendation'], 'DocumentReference.basedOn', sxpNormal);
  indexes.add('DocumentReference', 'category', 'Categorization of document', sptTOKEN, [], 'DocumentReference.category', sxpNormal);
  indexes.add('DocumentReference', 'contenttype', 'Mime type of the content, with charset etc.', sptTOKEN, [], 'DocumentReference.content.attachment.contentType', sxpNormal);
  indexes.add('DocumentReference', 'context', 'Context of the document content', sptREFERENCE, ['Appointment', 'EpisodeOfCare', 'Encounter'], 'DocumentReference.context', sxpNormal);
  indexes.add('DocumentReference', 'creation', 'Date attachment was first created', sptDATE, [], 'DocumentReference.content.attachment.creation', sxpNormal);
  indexes.add('DocumentReference', 'custodian', 'Organization which maintains the document', sptREFERENCE, ['Organization'], 'DocumentReference.custodian', sxpNormal);
  indexes.add('DocumentReference', 'date', 'When this document reference was created', sptDATE, [], 'DocumentReference.date', sxpNormal);
  indexes.add('DocumentReference', 'description', 'Human-readable description', sptSTRING, [], 'DocumentReference.description', sxpNormal);
  indexes.add('DocumentReference', 'doc-status', 'preliminary | final | amended | entered-in-error', sptTOKEN, [], 'DocumentReference.docStatus', sxpNormal);
  indexes.add('DocumentReference', 'event-code', 'Main clinical acts documented', sptTOKEN, [], 'DocumentReference.event.concept', sxpNormal);
  indexes.add('DocumentReference', 'event-reference', 'Main clinical acts documented', sptREFERENCE, [], 'DocumentReference.event.reference', sxpNormal);
  indexes.add('DocumentReference', 'facility', 'Kind of facility where patient was seen', sptTOKEN, [], 'DocumentReference.facilityType', sxpNormal);
  indexes.add('DocumentReference', 'format-canonical', 'Profile canonical content rules for the document', sptREFERENCE, [], '(DocumentReference.content.profile.value as canonical)', sxpNormal);
  indexes.add('DocumentReference', 'format-code', 'Format code content rules for the document', sptTOKEN, [], '(DocumentReference.content.profile.value as Coding)', sxpNormal);
  indexes.add('DocumentReference', 'format-uri', 'Profile URI content rules for the document', sptURI, [], '(DocumentReference.content.profile.value as uri)', sxpNormal);
  indexes.add('DocumentReference', 'identifier', '): Identifier of the attachment binary', sptTOKEN, [], 'AllergyIntolerance.identifier | CarePlan.identifier | CareTeam.identifier | Composition.identifier | Condition.identifier | Consent.identifier | DetectedIssue.identifier | DeviceRequest.identifier | DiagnosticReport.identifier | DocumentManifest.mast'
      +'erIdentifier | DocumentManifest.identifier | DocumentReference.identifier | Encounter.identifier | EpisodeOfCare.identifier | FamilyMemberHistory.identifier | Goal.identifier | ImagingStudy.identifier | Immunization.identifier | List.identifi'
      +'er | MedicationAdministration.identifier | MedicationDispense.identifier | MedicationRequest.identifier | MedicationUsage.identifier | NutritionOrder.identifier | Observation.identifier | Procedure.identifier | RiskAssessment.identifier | Ser'
      +'viceRequest.identifier | SupplyDelivery.identifier | SupplyRequest.identifier | VisionPrescription.identifier', sxpNormal);
  indexes.add('DocumentReference', 'language', 'Human language of the content (BCP-47)', sptTOKEN, [], 'DocumentReference.content.attachment.language', sxpNormal);
  indexes.add('DocumentReference', 'location', 'Uri where the data can be found', sptURI, [], 'DocumentReference.content.attachment.url', sxpNormal);
  indexes.add('DocumentReference', 'patient', '): Who/what is the subject of the document', sptREFERENCE, ['Patient', 'Practitioner', 'Group', 'Organization', 'BiologicallyDerivedProduct', 'NutritionProduct', 'Device', 'Medication', 'Procedure', 'Substance', 'Location'], 'AllergyIntolerance.patient | CarePlan.subject.where(resolve() is Patient) | CareTeam.subject.where(resolve() is Patient) | ClinicalImpression.subject.where(resolve() is Patient) | Composition.subject.where(resolve() is Patient) | Condition.subject.wh'
      +'ere(resolve() is Patient) | Consent.subject.where(resolve() is Patient) | DetectedIssue.subject | DeviceRequest.subject.where(resolve() is Patient) | DeviceUsage.patient | DiagnosticReport.subject.where(resolve() is Patient) | DocumentManifes'
      +'t.subject.where(resolve() is Patient) | DocumentReference.subject.where(resolve() is Patient) | Encounter.subject.where(resolve() is Patient) | EpisodeOfCare.patient | FamilyMemberHistory.patient | Flag.subject.wh'+
   'ere(resolve() is Patient) | G'
      +'oal.subject.where(resolve() is Patient) | ImagingStudy.subject.where(resolve() is Patient) | Immunization.patient | List.subject.where(resolve() is Patient) | MedicationAdministration.subject.where(resolve() is Patient) | MedicationDispense.s'
      +'ubject.where(resolve() is Patient) | MedicationRequest.subject.where(resolve() is Patient) | MedicationUsage.subject.where(resolve() is Patient) | NutritionOrder.subject.where(resolve() is Patient) | Observation.subject.where(resolve() is Pat'
      +'ient) | Procedure.subject.where(resolve() is Patient) | RiskAssessment.subject.where(resolve() is Patient) | ServiceRequest.subject.where(resolve() is Patient) | SupplyDelivery.patient | VisionPrescription.patient', sxpNormal);
  indexes.add('DocumentReference', 'period', 'Time of service that is being documented', sptDATE, [], 'DocumentReference.period', sxpNormal);
  indexes.add('DocumentReference', 'relatesto', 'Target of the relationship', sptREFERENCE, ['DocumentReference'], 'DocumentReference.relatesTo.target', sxpNormal);
  indexes.add('DocumentReference', 'relation', 'replaces | transforms | signs | appends', sptTOKEN, [], 'DocumentReference.relatesTo.code', sxpNormal);
  indexes.add('DocumentReference', 'relationship', 'Combination of relation and relatesTo', sptCOMPOSITE, [], 'DocumentReference.relatesTo', sxpNormal);
  indexes.add('DocumentReference', 'security-label', 'Document security-tags', sptTOKEN, [], 'DocumentReference.securityLabel', sxpNormal);
  indexes.add('DocumentReference', 'setting', 'Additional details about where the content was created (e.g. clinical specialty)', sptTOKEN, [], 'DocumentReference.practiceSetting', sxpNormal);
  indexes.add('DocumentReference', 'status', 'current | superseded | entered-in-error', sptTOKEN, [], 'DocumentReference.status', sxpNormal);
  indexes.add('DocumentReference', 'subject', 'Who/what is the subject of the document', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'DocumentReference.subject', sxpNormal);
  indexes.add('DocumentReference', 'type', '): Kind of document (LOINC if possible)', sptTOKEN, [], 'AllergyIntolerance.type | Composition.type | DocumentManifest.type | DocumentReference.type | Encounter.type | EpisodeOfCare.type', sxpNormal);
  indexes.add('DocumentReference', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Device', 'DocumentReference', ['subject', 'author']);
  compartments.register('Encounter', 'DocumentReference', ['context']);
  compartments.register('Patient', 'DocumentReference', ['subject', 'author']);
  compartments.register('Practitioner', 'DocumentReference', ['subject', 'author', 'attester']);
  compartments.register('RelatedPerson', 'DocumentReference', ['author']);
end;
{$ENDIF FHIR_DOCUMENTREFERENCE}

{$IFDEF FHIR_ENCOUNTER}
procedure TFHIRIndexBuilderR5.buildIndexesForEncounter(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Encounter', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('Encounter', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('Encounter', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('Encounter', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('Encounter', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('Encounter', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('Encounter', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('Encounter', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('Encounter', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('Encounter', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('Encounter', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('Encounter', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('Encounter', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('Encounter', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('Encounter', 'account', 'The set of accounts that may be used for billing for this Encounter', sptREFERENCE, ['Account'], 'Encounter.account', sxpNormal);
  indexes.add('Encounter', 'appointment', 'The appointment that scheduled this encounter', sptREFERENCE, ['Appointment'], 'Encounter.appointment', sxpNormal);
  indexes.add('Encounter', 'based-on', 'The ServiceRequest that initiated this encounter', sptREFERENCE, ['CarePlan', 'MedicationRequest', 'DeviceRequest', 'ServiceRequest'], 'Encounter.basedOn', sxpNormal);
  indexes.add('Encounter', 'careteam', 'Careteam allocated to participate in the encounter', sptREFERENCE, ['CareTeam'], 'Encounter.careTeam', sxpNormal);
  indexes.add('Encounter', 'class', 'Classification of patient encounter', sptTOKEN, [], 'Encounter.class', sxpNormal);
  indexes.add('Encounter', 'date', '): A date within the actualPeriod the Encounter lasted', sptDATE, [], 'AllergyIntolerance.recordedDate | CarePlan.period | ClinicalImpression.date | Composition.date | Consent.dateTime | DiagnosticReport.effective | Encounter.actualPeriod | EpisodeOfCare.period | FamilyMemberHistory.date | Flag.period | (Immunization.oc'
      +'currence as dateTime) | List.date | Observation.effective | Procedure.occurrence | (RiskAssessment.occurrence as dateTime) | SupplyRequest.authoredOn', sxpNormal);
  indexes.add('Encounter', 'date-start', 'The actual start date of the Encounter', sptDATE, [], 'Encounter.actualPeriod.start', sxpNormal);
  indexes.add('Encounter', 'diagnosis', 'The diagnosis or procedure relevant to the encounter', sptREFERENCE, ['Condition', 'Procedure'], 'Encounter.diagnosis.condition', sxpNormal);
  indexes.add('Encounter', 'end-date', 'The actual end date of the Encounter', sptDATE, [], 'Encounter.actualPeriod.end', sxpNormal);
  indexes.add('Encounter', 'episode-of-care', 'Episode(s) of care that this encounter should be recorded against', sptREFERENCE, ['EpisodeOfCare'], 'Encounter.episodeOfCare', sxpNormal);
  indexes.add('Encounter', 'identifier', '): Identifier(s) by which this encounter is known', sptTOKEN, [], 'AllergyIntolerance.identifier | CarePlan.identifier | CareTeam.identifier | Composition.identifier | Condition.identifier | Consent.identifier | DetectedIssue.identifier | DeviceRequest.identifier | DiagnosticReport.identifier | DocumentManifest.mast'
      +'erIdentifier | DocumentManifest.identifier | DocumentReference.identifier | Encounter.identifier | EpisodeOfCare.identifier | FamilyMemberHistory.identifier | Goal.identifier | ImagingStudy.identifier | Immunization.identifier | List.identifi'
      +'er | MedicationAdministration.identifier | MedicationDispense.identifier | MedicationRequest.identifier | MedicationUsage.identifier | NutritionOrder.identifier | Observation.identifier | Procedure.identifier | RiskAssessment.identifier | Ser'
      +'viceRequest.identifier | SupplyDelivery.identifier | SupplyRequest.identifier | VisionPrescription.identifier', sxpNormal);
  indexes.add('Encounter', 'length', 'Length of encounter in days', sptQUANTITY, [], 'Encounter.length', sxpNormal);
  indexes.add('Encounter', 'location', 'Location the encounter takes place', sptREFERENCE, ['Location'], 'Encounter.location.location', sxpNormal);
  indexes.add('Encounter', 'location-period', 'Time period during which the patient was present at the location', sptDATE, [], 'Encounter.location.period', sxpNormal);
  indexes.add('Encounter', 'part-of', 'Another Encounter this encounter is part of', sptREFERENCE, ['Encounter'], 'Encounter.partOf', sxpNormal);
  indexes.add('Encounter', 'participant', 'Persons involved in the encounter other than the patient', sptREFERENCE, ['Practitioner', 'Group', 'Device', 'Patient', 'HealthcareService', 'PractitionerRole', 'RelatedPerson'], 'Encounter.participant.actor', sxpNormal);
  indexes.add('Encounter', 'participant-type', 'Role of participant in encounter', sptTOKEN, [], 'Encounter.participant.type', sxpNormal);
  indexes.add('Encounter', 'patient', '): The patient present at the encounter', sptREFERENCE, ['Patient', 'Practitioner', 'Group', 'Organization', 'BiologicallyDerivedProduct', 'NutritionProduct', 'Device', 'Medication', 'Procedure', 'Substance', 'Location'], 'AllergyIntolerance.patient | CarePlan.subject.where(resolve() is Patient) | CareTeam.subject.where(resolve() is Patient) | ClinicalImpression.subject.where(resolve() is Patient) | Composition.subject.where(resolve() is Patient) | Condition.subject.wh'
      +'ere(resolve() is Patient) | Consent.subject.where(resolve() is Patient) | DetectedIssue.subject | DeviceRequest.subject.where(resolve() is Patient) | DeviceUsage.patient | DiagnosticReport.subject.where(resolve() is Patient) | DocumentManifes'
      +'t.subject.where(resolve() is Patient) | DocumentReference.subject.where(resolve() is Patient) | Encounter.subject.where(resolve() is Patient) | EpisodeOfCare.patient | FamilyMemberHistory.patient | Flag.subject.where(resolve'+
   '() is Patient) | G'
      +'oal.subject.where(resolve() is Patient) | ImagingStudy.subject.where(resolve() is Patient) | Immunization.patient | List.subject.where(resolve() is Patient) | MedicationAdministration.subject.where(resolve() is Patient) | MedicationDispense.s'
      +'ubject.where(resolve() is Patient) | MedicationRequest.subject.where(resolve() is Patient) | MedicationUsage.subject.where(resolve() is Patient) | NutritionOrder.subject.where(resolve() is Patient) | Observation.subject.where(resolve() is Pat'
      +'ient) | Procedure.subject.where(resolve() is Patient) | RiskAssessment.subject.where(resolve() is Patient) | ServiceRequest.subject.where(resolve() is Patient) | SupplyDelivery.patient | VisionPrescription.patient', sxpNormal);
  indexes.add('Encounter', 'practitioner', 'Persons involved in the encounter other than the patient', sptREFERENCE, ['Practitioner'], 'Encounter.participant.actor.where(resolve() is Practitioner)', sxpNormal);
  indexes.add('Encounter', 'reason-code', 'Reference to a concept (by class)', sptTOKEN, [], 'Encounter.reason.concept', sxpNormal);
  indexes.add('Encounter', 'reason-reference', 'Reference to a resource (by instance)', sptREFERENCE, [], 'Encounter.reason.reference', sxpNormal);
  indexes.add('Encounter', 'service-provider', 'The organization (facility) responsible for this encounter', sptREFERENCE, ['Organization'], 'Encounter.serviceProvider', sxpNormal);
  indexes.add('Encounter', 'special-arrangement', 'Wheelchair, translator, stretcher, etc.', sptTOKEN, [], 'Encounter.admission.specialArrangement', sxpNormal);
  indexes.add('Encounter', 'status', 'planned | in-progress | onhold | completed | cancelled | entered-in-error | unknown', sptTOKEN, [], 'Encounter.status', sxpNormal);
  indexes.add('Encounter', 'subject', 'The patient or group present at the encounter', sptREFERENCE, ['Group', 'Patient'], 'Encounter.subject', sxpNormal);
  indexes.add('Encounter', 'subject-status', 'The current status of the subject in relation to the Encounter', sptTOKEN, [], 'Encounter.subjectStatus', sxpNormal);
  indexes.add('Encounter', 'type', '): Specific type of encounter', sptTOKEN, [], 'AllergyIntolerance.type | Composition.type | DocumentManifest.type | DocumentReference.type | Encounter.type | EpisodeOfCare.type', sxpNormal);
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
  indexes.add('Endpoint', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('Endpoint', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('Endpoint', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('Endpoint', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('Endpoint', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('Endpoint', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('Endpoint', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('Endpoint', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('Endpoint', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('Endpoint', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('Endpoint', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('Endpoint', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('Endpoint', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('Endpoint', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('Endpoint', 'connection-type', 'Protocol/Profile/Standard to be used with this endpoint connection', sptTOKEN, [], 'Endpoint.connectionType', sxpNormal);
  indexes.add('Endpoint', 'identifier', 'Identifies this endpoint across multiple systems', sptTOKEN, [], 'Endpoint.identifier', sxpNormal);
  indexes.add('Endpoint', 'name', 'A name that this endpoint can be identified by', sptSTRING, [], 'Endpoint.name', sxpNormal);
  indexes.add('Endpoint', 'organization', 'The organization that is managing the endpoint', sptREFERENCE, ['Organization'], 'Endpoint.managingOrganization', sxpNormal);
  indexes.add('Endpoint', 'payload-type', 'The type of content that may be used at this endpoint (e.g. XDS Discharge summaries)', sptTOKEN, [], 'Endpoint.payloadType', sxpNormal);
  indexes.add('Endpoint', 'status', 'The current status of the Endpoint (usually expected to be active)', sptTOKEN, [], 'Endpoint.status', sxpNormal);
  indexes.add('Endpoint', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_ENDPOINT}

{$IFDEF FHIR_ENROLLMENTREQUEST}
procedure TFHIRIndexBuilderR5.buildIndexesForEnrollmentRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('EnrollmentRequest', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('EnrollmentRequest', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('EnrollmentRequest', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('EnrollmentRequest', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('EnrollmentRequest', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('EnrollmentRequest', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('EnrollmentRequest', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('EnrollmentRequest', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('EnrollmentRequest', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('EnrollmentRequest', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('EnrollmentRequest', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('EnrollmentRequest', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('EnrollmentRequest', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('EnrollmentRequest', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('EnrollmentRequest', 'identifier', 'The business identifier of the Enrollment', sptTOKEN, [], 'EnrollmentRequest.identifier', sxpNormal);
  indexes.add('EnrollmentRequest', 'patient', 'The party to be enrolled', sptREFERENCE, ['Patient'], 'EnrollmentRequest.candidate', sxpNormal);
  indexes.add('EnrollmentRequest', 'status', 'The status of the enrollment', sptTOKEN, [], 'EnrollmentRequest.status', sxpNormal);
  indexes.add('EnrollmentRequest', 'subject', 'The party to be enrolled', sptREFERENCE, ['Patient'], 'EnrollmentRequest.candidate', sxpNormal);
  indexes.add('EnrollmentRequest', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Patient', 'EnrollmentRequest', ['subject']);
end;
{$ENDIF FHIR_ENROLLMENTREQUEST}

{$IFDEF FHIR_ENROLLMENTRESPONSE}
procedure TFHIRIndexBuilderR5.buildIndexesForEnrollmentResponse(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('EnrollmentResponse', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('EnrollmentResponse', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('EnrollmentResponse', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('EnrollmentResponse', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('EnrollmentResponse', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('EnrollmentResponse', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('EnrollmentResponse', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('EnrollmentResponse', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('EnrollmentResponse', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('EnrollmentResponse', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('EnrollmentResponse', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('EnrollmentResponse', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('EnrollmentResponse', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('EnrollmentResponse', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('EnrollmentResponse', 'identifier', 'The business identifier of the EnrollmentResponse', sptTOKEN, [], 'EnrollmentResponse.identifier', sxpNormal);
  indexes.add('EnrollmentResponse', 'request', 'The reference to the claim', sptREFERENCE, ['EnrollmentRequest'], 'EnrollmentResponse.request', sxpNormal);
  indexes.add('EnrollmentResponse', 'status', 'The status of the enrollment response', sptTOKEN, [], 'EnrollmentResponse.status', sxpNormal);
  indexes.add('EnrollmentResponse', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_ENROLLMENTRESPONSE}

{$IFDEF FHIR_EPISODEOFCARE}
procedure TFHIRIndexBuilderR5.buildIndexesForEpisodeOfCare(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('EpisodeOfCare', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('EpisodeOfCare', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('EpisodeOfCare', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('EpisodeOfCare', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('EpisodeOfCare', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('EpisodeOfCare', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('EpisodeOfCare', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('EpisodeOfCare', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('EpisodeOfCare', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('EpisodeOfCare', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('EpisodeOfCare', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('EpisodeOfCare', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('EpisodeOfCare', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('EpisodeOfCare', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('EpisodeOfCare', 'care-manager', 'Care manager/care coordinator for the patient', sptREFERENCE, ['Practitioner'], 'EpisodeOfCare.careManager.where(resolve() is Practitioner)', sxpNormal);
  indexes.add('EpisodeOfCare', 'condition', 'Conditions/problems/diagnoses this episode of care is for (legacy)', sptREFERENCE, [], 'EpisodeOfCare.diagnosis.condition.reference', sxpNormal);
  indexes.add('EpisodeOfCare', 'condition-concept', 'Conditions/problems/diagnoses this episode of care is for (coded)', sptTOKEN, [], 'EpisodeOfCare.diagnosis.condition.concept', sxpNormal);
  indexes.add('EpisodeOfCare', 'condition-reference', 'Conditions/problems/diagnoses this episode of care is for (resource reference)', sptREFERENCE, [], 'EpisodeOfCare.diagnosis.condition.reference', sxpNormal);
  indexes.add('EpisodeOfCare', 'date', '): The provided date search value falls within the episode of care''s period', sptDATE, [], 'AllergyIntolerance.recordedDate | CarePlan.period | ClinicalImpression.date | Composition.date | Consent.dateTime | DiagnosticReport.effective | Encounter.actualPeriod | EpisodeOfCare.period | FamilyMemberHistory.date | Flag.period | (Immunization.oc'
      +'currence as dateTime) | List.date | Observation.effective | Procedure.occurrence | (RiskAssessment.occurrence as dateTime) | SupplyRequest.authoredOn', sxpNormal);
  indexes.add('EpisodeOfCare', 'identifier', '): Business Identifier(s) relevant for this EpisodeOfCare', sptTOKEN, [], 'AllergyIntolerance.identifier | CarePlan.identifier | CareTeam.identifier | Composition.identifier | Condition.identifier | Consent.identifier | DetectedIssue.identifier | DeviceRequest.identifier | DiagnosticReport.identifier | DocumentManifest.mast'
      +'erIdentifier | DocumentManifest.identifier | DocumentReference.identifier | Encounter.identifier | EpisodeOfCare.identifier | FamilyMemberHistory.identifier | Goal.identifier | ImagingStudy.identifier | Immunization.identifier | List.identifi'
      +'er | MedicationAdministration.identifier | MedicationDispense.identifier | MedicationRequest.identifier | MedicationUsage.identifier | NutritionOrder.identifier | Observation.identifier | Procedure.identifier | RiskAssessment.identifier | Ser'
      +'viceRequest.identifier | SupplyDelivery.identifier | SupplyRequest.identifier | VisionPrescription.identifier', sxpNormal);
  indexes.add('EpisodeOfCare', 'incoming-referral', 'Incoming Referral Request', sptREFERENCE, ['ServiceRequest'], 'EpisodeOfCare.referralRequest', sxpNormal);
  indexes.add('EpisodeOfCare', 'organization', 'The organization that has assumed the specific responsibilities of this EpisodeOfCare', sptREFERENCE, ['Organization'], 'EpisodeOfCare.managingOrganization', sxpNormal);
  indexes.add('EpisodeOfCare', 'patient', '): The patient who is the focus of this episode of care', sptREFERENCE, ['Patient', 'Practitioner', 'Group', 'Organization', 'BiologicallyDerivedProduct', 'NutritionProduct', 'Device', 'Medication', 'Procedure', 'Substance', 'Location'], 'AllergyIntolerance.patient | CarePlan.subject.where(resolve() is Patient) | CareTeam.subject.where(resolve() is Patient) | ClinicalImpression.subject.where(resolve() is Patient) | Composition.subject.where(resolve() is Patient) | Condition.subject.wh'
      +'ere(resolve() is Patient) | Consent.subject.where(resolve() is Patient) | DetectedIssue.subject | DeviceRequest.subject.where(resolve() is Patient) | DeviceUsage.patient | DiagnosticReport.subject.where(resolve() is Patient) | DocumentManifes'
      +'t.subject.where(resolve() is Patient) | DocumentReference.subject.where(resolve() is Patient) | Encounter.subject.where(resolve() is Patient) | EpisodeOfCare.patient | FamilyMemberHistory.patient | Flag.s'+
   'ubject.where(resolve() is Patient) | G'
      +'oal.subject.where(resolve() is Patient) | ImagingStudy.subject.where(resolve() is Patient) | Immunization.patient | List.subject.where(resolve() is Patient) | MedicationAdministration.subject.where(resolve() is Patient) | MedicationDispense.s'
      +'ubject.where(resolve() is Patient) | MedicationRequest.subject.where(resolve() is Patient) | MedicationUsage.subject.where(resolve() is Patient) | NutritionOrder.subject.where(resolve() is Patient) | Observation.subject.where(resolve() is Pat'
      +'ient) | Procedure.subject.where(resolve() is Patient) | RiskAssessment.subject.where(resolve() is Patient) | ServiceRequest.subject.where(resolve() is Patient) | SupplyDelivery.patient | VisionPrescription.patient', sxpNormal);
  indexes.add('EpisodeOfCare', 'status', 'The current status of the Episode of Care as provided (does not check the status history collection)', sptTOKEN, [], 'EpisodeOfCare.status', sxpNormal);
  indexes.add('EpisodeOfCare', 'type', '): Type/class  - e.g. specialist referral, disease management', sptTOKEN, [], 'AllergyIntolerance.type | Composition.type | DocumentManifest.type | DocumentReference.type | Encounter.type | EpisodeOfCare.type', sxpNormal);
  indexes.add('EpisodeOfCare', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Patient', 'EpisodeOfCare', ['patient']);
  compartments.register('Practitioner', 'EpisodeOfCare', ['care-manager']);
end;
{$ENDIF FHIR_EPISODEOFCARE}

{$IFDEF FHIR_EVENTDEFINITION}
procedure TFHIRIndexBuilderR5.buildIndexesForEventDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('EventDefinition', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('EventDefinition', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('EventDefinition', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('EventDefinition', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('EventDefinition', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('EventDefinition', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('EventDefinition', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('EventDefinition', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('EventDefinition', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('EventDefinition', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('EventDefinition', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('EventDefinition', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('EventDefinition', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('EventDefinition', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('EventDefinition', 'composed-of', 'What resource is being referenced', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'EventDefinition.relatedArtifact.where(type=''composed-of'').resource', sxpNormal);
  indexes.add('EventDefinition', 'context', 'A use context assigned to the event definition', sptTOKEN, [], '(EventDefinition.useContext.value as CodeableConcept)', sxpNormal);
  indexes.add('EventDefinition', 'context-quantity', 'A quantity- or range-valued use context assigned to the event definition', sptQUANTITY, [], '(EventDefinition.useContext.value as Quantity) | (EventDefinition.useContext.value as Range)', sxpNormal);
  indexes.add('EventDefinition', 'context-type', 'A type of use context assigned to the event definition', sptTOKEN, [], 'EventDefinition.useContext.code', sxpNormal);
  indexes.add('EventDefinition', 'context-type-quantity', 'A use context type and quantity- or range-based value assigned to the event definition', sptCOMPOSITE, [], 'EventDefinition.useContext', sxpNormal);
  indexes.add('EventDefinition', 'context-type-value', 'A use context type and value assigned to the event definition', sptCOMPOSITE, [], 'EventDefinition.useContext', sxpNormal);
  indexes.add('EventDefinition', 'date', 'The event definition publication date', sptDATE, [], 'EventDefinition.date', sxpNormal);
  indexes.add('EventDefinition', 'depends-on', 'What resource is being referenced', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'EventDefinition.relatedArtifact.where(type=''depends-on'').resource', sxpNormal);
  indexes.add('EventDefinition', 'derived-from', 'What resource is being referenced', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'EventDefinition.relatedArtifact.where(type=''derived-from'').resource', sxpNormal);
  indexes.add('EventDefinition', 'description', 'The description of the event definition', sptSTRING, [], 'EventDefinition.description', sxpNormal);
  indexes.add('EventDefinition', 'effective', 'The time during which the event definition is intended to be in use', sptDATE, [], 'EventDefinition.effectivePeriod', sxpNormal);
  indexes.add('EventDefinition', 'identifier', 'External identifier for the event definition', sptTOKEN, [], 'EventDefinition.identifier', sxpNormal);
  indexes.add('EventDefinition', 'jurisdiction', 'Intended jurisdiction for the event definition', sptTOKEN, [], 'EventDefinition.jurisdiction', sxpNormal);
  indexes.add('EventDefinition', 'name', 'Computationally friendly name of the event definition', sptSTRING, [], 'EventDefinition.name', sxpNormal);
  indexes.add('EventDefinition', 'predecessor', 'What resource is being referenced', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'EventDefinition.relatedArtifact.where(type=''predecessor'').resource', sxpNormal);
  indexes.add('EventDefinition', 'publisher', 'Name of the publisher of the event definition', sptSTRING, [], 'EventDefinition.publisher', sxpNormal);
  indexes.add('EventDefinition', 'status', 'The current status of the event definition', sptTOKEN, [], 'EventDefinition.status', sxpNormal);
  indexes.add('EventDefinition', 'successor', 'What resource is being referenced', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'EventDefinition.relatedArtifact.where(type=''successor'').resource', sxpNormal);
  indexes.add('EventDefinition', 'title', 'The human-friendly name of the event definition', sptSTRING, [], 'EventDefinition.title', sxpNormal);
  indexes.add('EventDefinition', 'topic', 'Topics associated with the module', sptTOKEN, [], 'EventDefinition.topic', sxpNormal);
  indexes.add('EventDefinition', 'url', 'The uri that identifies the event definition', sptURI, [], 'EventDefinition.url', sxpNormal);
  indexes.add('EventDefinition', 'version', 'The business version of the event definition', sptTOKEN, [], 'EventDefinition.version', sxpNormal);
  indexes.add('EventDefinition', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_EVENTDEFINITION}

{$IFDEF FHIR_EVIDENCE}
procedure TFHIRIndexBuilderR5.buildIndexesForEvidence(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Evidence', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('Evidence', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('Evidence', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('Evidence', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('Evidence', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('Evidence', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('Evidence', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('Evidence', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('Evidence', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('Evidence', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('Evidence', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('Evidence', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('Evidence', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('Evidence', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('Evidence', 'context', 'A use context assigned to the evidence', sptTOKEN, [], '(Evidence.useContext.value as CodeableConcept)', sxpNormal);
  indexes.add('Evidence', 'context-quantity', 'A quantity- or range-valued use context assigned to the evidence', sptQUANTITY, [], '(Evidence.useContext.value as Quantity) | (Evidence.useContext.value as Range)', sxpNormal);
  indexes.add('Evidence', 'context-type', 'A type of use context assigned to the evidence', sptTOKEN, [], 'Evidence.useContext.code', sxpNormal);
  indexes.add('Evidence', 'context-type-quantity', 'A use context type and quantity- or range-based value assigned to the evidence', sptCOMPOSITE, [], 'Evidence.useContext', sxpNormal);
  indexes.add('Evidence', 'context-type-value', 'A use context type and value assigned to the evidence', sptCOMPOSITE, [], 'Evidence.useContext', sxpNormal);
  indexes.add('Evidence', 'date', 'The evidence publication date', sptDATE, [], 'Evidence.date', sxpNormal);
  indexes.add('Evidence', 'description', 'The description of the evidence', sptSTRING, [], 'Evidence.description', sxpNormal);
  indexes.add('Evidence', 'identifier', 'External identifier for the evidence', sptTOKEN, [], 'Evidence.identifier', sxpNormal);
  indexes.add('Evidence', 'publisher', 'Name of the publisher of the evidence', sptSTRING, [], 'Evidence.publisher', sxpNormal);
  indexes.add('Evidence', 'status', 'The current status of the evidence', sptTOKEN, [], 'Evidence.status', sxpNormal);
  indexes.add('Evidence', 'title', 'The human-friendly name of the evidence', sptSTRING, [], 'Evidence.title', sxpNormal);
  indexes.add('Evidence', 'url', 'The uri that identifies the evidence', sptURI, [], 'Evidence.url', sxpNormal);
  indexes.add('Evidence', 'version', 'The business version of the evidence', sptTOKEN, [], 'Evidence.version', sxpNormal);
  indexes.add('Evidence', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_EVIDENCE}

{$IFDEF FHIR_EVIDENCEREPORT}
procedure TFHIRIndexBuilderR5.buildIndexesForEvidenceReport(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('EvidenceReport', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('EvidenceReport', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('EvidenceReport', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('EvidenceReport', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('EvidenceReport', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('EvidenceReport', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('EvidenceReport', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('EvidenceReport', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('EvidenceReport', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('EvidenceReport', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('EvidenceReport', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('EvidenceReport', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('EvidenceReport', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('EvidenceReport', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('EvidenceReport', 'context', 'A use context assigned to the evidence report', sptTOKEN, [], '(EvidenceReport.useContext.value as CodeableConcept)', sxpNormal);
  indexes.add('EvidenceReport', 'context-quantity', 'A quantity- or range-valued use context assigned to the evidence report', sptQUANTITY, [], '(EvidenceReport.useContext.value as Quantity) | (EvidenceReport.useContext.value as Range)', sxpNormal);
  indexes.add('EvidenceReport', 'context-type', 'A type of use context assigned to the evidence report', sptTOKEN, [], 'EvidenceReport.useContext.code', sxpNormal);
  indexes.add('EvidenceReport', 'context-type-quantity', 'A use context type and quantity- or range-based value assigned to the evidence report', sptCOMPOSITE, [], 'EvidenceReport.useContext', sxpNormal);
  indexes.add('EvidenceReport', 'context-type-value', 'A use context type and value assigned to the evidence report', sptCOMPOSITE, [], 'EvidenceReport.useContext', sxpNormal);
  indexes.add('EvidenceReport', 'identifier', 'External identifier for the evidence report', sptTOKEN, [], 'EvidenceReport.identifier', sxpNormal);
  indexes.add('EvidenceReport', 'publisher', 'Name of the publisher of the evidence report', sptSTRING, [], 'EvidenceReport.publisher', sxpNormal);
  indexes.add('EvidenceReport', 'status', 'The current status of the evidence report', sptTOKEN, [], 'EvidenceReport.status', sxpNormal);
  indexes.add('EvidenceReport', 'url', 'The uri that identifies the evidence report', sptURI, [], 'EvidenceReport.url', sxpNormal);
  indexes.add('EvidenceReport', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_EVIDENCEREPORT}

{$IFDEF FHIR_EVIDENCEVARIABLE}
procedure TFHIRIndexBuilderR5.buildIndexesForEvidenceVariable(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('EvidenceVariable', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('EvidenceVariable', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('EvidenceVariable', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('EvidenceVariable', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('EvidenceVariable', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('EvidenceVariable', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('EvidenceVariable', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('EvidenceVariable', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('EvidenceVariable', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('EvidenceVariable', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('EvidenceVariable', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('EvidenceVariable', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('EvidenceVariable', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('EvidenceVariable', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('EvidenceVariable', 'composed-of', 'What resource is being referenced', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'EvidenceVariable.relatedArtifact.where(type=''composed-of'').resource', sxpNormal);
  indexes.add('EvidenceVariable', 'context', 'A use context assigned to the evidence variable', sptTOKEN, [], '(EvidenceVariable.useContext.value as CodeableConcept)', sxpNormal);
  indexes.add('EvidenceVariable', 'context-quantity', 'A quantity- or range-valued use context assigned to the evidence variable', sptQUANTITY, [], '(EvidenceVariable.useContext.value as Quantity) | (EvidenceVariable.useContext.value as Range)', sxpNormal);
  indexes.add('EvidenceVariable', 'context-type', 'A type of use context assigned to the evidence variable', sptTOKEN, [], 'EvidenceVariable.useContext.code', sxpNormal);
  indexes.add('EvidenceVariable', 'context-type-quantity', 'A use context type and quantity- or range-based value assigned to the evidence variable', sptCOMPOSITE, [], 'EvidenceVariable.useContext', sxpNormal);
  indexes.add('EvidenceVariable', 'context-type-value', 'A use context type and value assigned to the evidence variable', sptCOMPOSITE, [], 'EvidenceVariable.useContext', sxpNormal);
  indexes.add('EvidenceVariable', 'date', 'The evidence variable publication date', sptDATE, [], 'EvidenceVariable.date', sxpNormal);
  indexes.add('EvidenceVariable', 'depends-on', 'What resource is being referenced', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'EvidenceVariable.relatedArtifact.where(type=''depends-on'').resource', sxpNormal);
  indexes.add('EvidenceVariable', 'derived-from', 'What resource is being referenced', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'EvidenceVariable.relatedArtifact.where(type=''derived-from'').resource', sxpNormal);
  indexes.add('EvidenceVariable', 'description', 'The description of the evidence variable', sptSTRING, [], 'EvidenceVariable.description', sxpNormal);
  indexes.add('EvidenceVariable', 'identifier', 'External identifier for the evidence variable', sptTOKEN, [], 'EvidenceVariable.identifier', sxpNormal);
  indexes.add('EvidenceVariable', 'name', 'Computationally friendly name of the evidence variable', sptSTRING, [], 'EvidenceVariable.name', sxpNormal);
  indexes.add('EvidenceVariable', 'predecessor', 'What resource is being referenced', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'EvidenceVariable.relatedArtifact.where(type=''predecessor'').resource', sxpNormal);
  indexes.add('EvidenceVariable', 'publisher', 'Name of the publisher of the evidence variable', sptSTRING, [], 'EvidenceVariable.publisher', sxpNormal);
  indexes.add('EvidenceVariable', 'status', 'The current status of the evidence variable', sptTOKEN, [], 'EvidenceVariable.status', sxpNormal);
  indexes.add('EvidenceVariable', 'successor', 'What resource is being referenced', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'EvidenceVariable.relatedArtifact.where(type=''successor'').resource', sxpNormal);
  indexes.add('EvidenceVariable', 'title', 'The human-friendly name of the evidence variable', sptSTRING, [], 'EvidenceVariable.title', sxpNormal);
  indexes.add('EvidenceVariable', 'topic', 'Topics associated with the EvidenceVariable', sptTOKEN, [], '', sxpNormal);
  indexes.add('EvidenceVariable', 'url', 'The uri that identifies the evidence variable', sptURI, [], 'EvidenceVariable.url', sxpNormal);
  indexes.add('EvidenceVariable', 'version', 'The business version of the evidence variable', sptTOKEN, [], 'EvidenceVariable.version', sxpNormal);
  indexes.add('EvidenceVariable', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_EVIDENCEVARIABLE}

{$IFDEF FHIR_EXAMPLESCENARIO}
procedure TFHIRIndexBuilderR5.buildIndexesForExampleScenario(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ExampleScenario', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('ExampleScenario', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('ExampleScenario', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('ExampleScenario', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('ExampleScenario', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('ExampleScenario', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('ExampleScenario', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('ExampleScenario', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('ExampleScenario', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('ExampleScenario', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('ExampleScenario', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('ExampleScenario', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('ExampleScenario', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('ExampleScenario', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('ExampleScenario', 'context', 'A use context assigned to the example scenario', sptTOKEN, [], '(ExampleScenario.useContext.value as CodeableConcept)', sxpNormal);
  indexes.add('ExampleScenario', 'context-quantity', 'A quantity- or range-valued use context assigned to the example scenario', sptQUANTITY, [], '(ExampleScenario.useContext.value as Quantity) | (ExampleScenario.useContext.value as Range)', sxpNormal);
  indexes.add('ExampleScenario', 'context-type', 'A type of use context assigned to the example scenario', sptTOKEN, [], 'ExampleScenario.useContext.code', sxpNormal);
  indexes.add('ExampleScenario', 'context-type-quantity', 'A use context type and quantity- or range-based value assigned to the example scenario', sptCOMPOSITE, [], 'ExampleScenario.useContext', sxpNormal);
  indexes.add('ExampleScenario', 'context-type-value', 'A use context type and value assigned to the example scenario', sptCOMPOSITE, [], 'ExampleScenario.useContext', sxpNormal);
  indexes.add('ExampleScenario', 'date', 'The example scenario publication date', sptDATE, [], 'ExampleScenario.date', sxpNormal);
  indexes.add('ExampleScenario', 'identifier', 'External identifier for the example scenario', sptTOKEN, [], 'ExampleScenario.identifier', sxpNormal);
  indexes.add('ExampleScenario', 'jurisdiction', 'Intended jurisdiction for the example scenario', sptTOKEN, [], 'ExampleScenario.jurisdiction', sxpNormal);
  indexes.add('ExampleScenario', 'name', 'Computationally friendly name of the example scenario', sptSTRING, [], 'ExampleScenario.name', sxpNormal);
  indexes.add('ExampleScenario', 'publisher', 'Name of the publisher of the example scenario', sptSTRING, [], 'ExampleScenario.publisher', sxpNormal);
  indexes.add('ExampleScenario', 'status', 'The current status of the example scenario', sptTOKEN, [], 'ExampleScenario.status', sxpNormal);
  indexes.add('ExampleScenario', 'url', 'The uri that identifies the example scenario', sptURI, [], 'ExampleScenario.url', sxpNormal);
  indexes.add('ExampleScenario', 'version', 'The business version of the example scenario', sptTOKEN, [], 'ExampleScenario.version', sxpNormal);
  indexes.add('ExampleScenario', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_EXAMPLESCENARIO}

{$IFDEF FHIR_EXPLANATIONOFBENEFIT}
procedure TFHIRIndexBuilderR5.buildIndexesForExplanationOfBenefit(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ExplanationOfBenefit', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('ExplanationOfBenefit', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('ExplanationOfBenefit', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('ExplanationOfBenefit', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('ExplanationOfBenefit', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('ExplanationOfBenefit', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('ExplanationOfBenefit', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('ExplanationOfBenefit', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('ExplanationOfBenefit', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('ExplanationOfBenefit', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('ExplanationOfBenefit', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('ExplanationOfBenefit', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('ExplanationOfBenefit', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('ExplanationOfBenefit', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('ExplanationOfBenefit', 'care-team', 'Member of the CareTeam', sptREFERENCE, ['Practitioner', 'Organization', 'PractitionerRole'], 'ExplanationOfBenefit.careTeam.provider', sxpNormal);
  indexes.add('ExplanationOfBenefit', 'claim', 'The reference to the claim', sptREFERENCE, ['Claim'], 'ExplanationOfBenefit.claim', sxpNormal);
  indexes.add('ExplanationOfBenefit', 'coverage', 'The plan under which the claim was adjudicated', sptREFERENCE, ['Coverage'], 'ExplanationOfBenefit.insurance.coverage', sxpNormal);
  indexes.add('ExplanationOfBenefit', 'created', 'The creation date for the EOB', sptDATE, [], 'ExplanationOfBenefit.created', sxpNormal);
  indexes.add('ExplanationOfBenefit', 'detail-udi', 'UDI associated with a line item detail product or service', sptREFERENCE, ['Device'], 'ExplanationOfBenefit.item.detail.udi', sxpNormal);
  indexes.add('ExplanationOfBenefit', 'disposition', 'The contents of the disposition message', sptSTRING, [], 'ExplanationOfBenefit.disposition', sxpNormal);
  indexes.add('ExplanationOfBenefit', 'encounter', 'Encounters associated with a billed line item', sptREFERENCE, ['Encounter'], 'ExplanationOfBenefit.item.encounter', sxpNormal);
  indexes.add('ExplanationOfBenefit', 'enterer', 'The party responsible for the entry of the Claim', sptREFERENCE, ['Practitioner', 'Patient', 'PractitionerRole', 'RelatedPerson'], 'ExplanationOfBenefit.enterer', sxpNormal);
  indexes.add('ExplanationOfBenefit', 'facility', 'Facility responsible for the goods and services', sptREFERENCE, ['Organization', 'Location'], 'ExplanationOfBenefit.facility', sxpNormal);
  indexes.add('ExplanationOfBenefit', 'identifier', 'The business identifier of the Explanation of Benefit', sptTOKEN, [], 'ExplanationOfBenefit.identifier', sxpNormal);
  indexes.add('ExplanationOfBenefit', 'item-udi', 'UDI associated with a line item product or service', sptREFERENCE, ['Device'], 'ExplanationOfBenefit.item.udi', sxpNormal);
  indexes.add('ExplanationOfBenefit', 'patient', 'The reference to the patient', sptREFERENCE, ['Patient'], 'ExplanationOfBenefit.patient', sxpNormal);
  indexes.add('ExplanationOfBenefit', 'payee', 'The party receiving any payment for the Claim', sptREFERENCE, ['Practitioner', 'Organization', 'Patient', 'PractitionerRole', 'RelatedPerson'], 'ExplanationOfBenefit.payee.party', sxpNormal);
  indexes.add('ExplanationOfBenefit', 'procedure-udi', 'UDI associated with a procedure', sptREFERENCE, ['Device'], 'ExplanationOfBenefit.procedure.udi', sxpNormal);
  indexes.add('ExplanationOfBenefit', 'provider', 'The reference to the provider', sptREFERENCE, ['Practitioner', 'Organization', 'PractitionerRole'], 'ExplanationOfBenefit.provider', sxpNormal);
  indexes.add('ExplanationOfBenefit', 'status', 'Status of the instance', sptTOKEN, [], 'ExplanationOfBenefit.status', sxpNormal);
  indexes.add('ExplanationOfBenefit', 'subdetail-udi', 'UDI associated with a line item detail subdetail product or service', sptREFERENCE, ['Device'], 'ExplanationOfBenefit.item.detail.subDetail.udi', sxpNormal);
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
  indexes.add('FamilyMemberHistory', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('FamilyMemberHistory', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('FamilyMemberHistory', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('FamilyMemberHistory', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('FamilyMemberHistory', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('FamilyMemberHistory', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('FamilyMemberHistory', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('FamilyMemberHistory', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('FamilyMemberHistory', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('FamilyMemberHistory', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('FamilyMemberHistory', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('FamilyMemberHistory', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('FamilyMemberHistory', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('FamilyMemberHistory', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('FamilyMemberHistory', 'code', '): A search by a condition code', sptTOKEN, [], 'AllergyIntolerance.code | AllergyIntolerance.reaction.substance | Condition.code | DeviceRequest.code.concept | DiagnosticReport.code | FamilyMemberHistory.condition.code | List.code | Medication.code | MedicationAdministration.medication.concept | M'
      +'edicationDispense.medication.concept | MedicationRequest.medication.concept | MedicationUsage.medication.concept | Observation.code | Procedure.code', sxpNormal);
  indexes.add('FamilyMemberHistory', 'date', '): When history was recorded or last updated', sptDATE, [], 'AllergyIntolerance.recordedDate | CarePlan.period | ClinicalImpression.date | Composition.date | Consent.dateTime | DiagnosticReport.effective | Encounter.actualPeriod | EpisodeOfCare.period | FamilyMemberHistory.date | Flag.period | (Immunization.oc'
      +'currence as dateTime) | List.date | Observation.effective | Procedure.occurrence | (RiskAssessment.occurrence as dateTime) | SupplyRequest.authoredOn', sxpNormal);
  indexes.add('FamilyMemberHistory', 'identifier', '): A search by a record identifier', sptTOKEN, [], 'AllergyIntolerance.identifier | CarePlan.identifier | CareTeam.identifier | Composition.identifier | Condition.identifier | Consent.identifier | DetectedIssue.identifier | DeviceRequest.identifier | DiagnosticReport.identifier | DocumentManifest.mast'
      +'erIdentifier | DocumentManifest.identifier | DocumentReference.identifier | Encounter.identifier | EpisodeOfCare.identifier | FamilyMemberHistory.identifier | Goal.identifier | ImagingStudy.identifier | Immunization.identifier | List.identifi'
      +'er | MedicationAdministration.identifier | MedicationDispense.identifier | MedicationRequest.identifier | MedicationUsage.identifier | NutritionOrder.identifier | Observation.identifier | Procedure.identifier | RiskAssessment.identifier | Ser'
      +'viceRequest.identifier | SupplyDelivery.identifier | SupplyRequest.identifier | VisionPrescription.identifier', sxpNormal);
  indexes.add('FamilyMemberHistory', 'instantiates-canonical', 'Instantiates FHIR protocol or definition', sptREFERENCE, ['Questionnaire', 'Measure', 'PlanDefinition', 'OperationDefinition', 'ActivityDefinition'], 'FamilyMemberHistory.instantiatesCanonical', sxpNormal);
  indexes.add('FamilyMemberHistory', 'instantiates-uri', 'Instantiates external protocol or definition', sptURI, [], 'FamilyMemberHistory.instantiatesUri', sxpNormal);
  indexes.add('FamilyMemberHistory', 'patient', '): The identity of a subject to list family member history items for', sptREFERENCE, ['Patient', 'Practitioner', 'Group', 'Organization', 'BiologicallyDerivedProduct', 'NutritionProduct', 'Device', 'Medication', 'Procedure', 'Substance', 'Location'], 'AllergyIntolerance.patient | CarePlan.subject.where(resolve() is Patient) | CareTeam.subject.where(resolve() is Patient) | ClinicalImpression.subject.where(resolve() is Patient) | Composition.subject.where(resolve() is Patient) | Condition.subject.wh'
      +'ere(resolve() is Patient) | Consent.subject.where(resolve() is Patient) | DetectedIssue.subject | DeviceRequest.subject.where(resolve() is Patient) | DeviceUsage.patient | DiagnosticReport.subject.where(resolve() is Patient) | DocumentManifes'
      +'t.subject.where(resolve() is Patient) | DocumentReference.subject.where(resolve() is Patient) | Encounter.subject.where(resolve() is Patient) | EpisodeOfCare.patient | FamilyMemberHisto'+
   'ry.patient | Flag.subject.where(resolve() is Patient) | G'
      +'oal.subject.where(resolve() is Patient) | ImagingStudy.subject.where(resolve() is Patient) | Immunization.patient | List.subject.where(resolve() is Patient) | MedicationAdministration.subject.where(resolve() is Patient) | MedicationDispense.s'
      +'ubject.where(resolve() is Patient) | MedicationRequest.subject.where(resolve() is Patient) | MedicationUsage.subject.where(resolve() is Patient) | NutritionOrder.subject.where(resolve() is Patient) | Observation.subject.where(resolve() is Pat'
      +'ient) | Procedure.subject.where(resolve() is Patient) | RiskAssessment.subject.where(resolve() is Patient) | ServiceRequest.subject.where(resolve() is Patient) | SupplyDelivery.patient | VisionPrescription.patient', sxpNormal);
  indexes.add('FamilyMemberHistory', 'relationship', 'A search by a relationship type', sptTOKEN, [], 'FamilyMemberHistory.relationship', sxpNormal);
  indexes.add('FamilyMemberHistory', 'sex', 'A search by a sex code of a family member', sptTOKEN, [], 'FamilyMemberHistory.sex', sxpNormal);
  indexes.add('FamilyMemberHistory', 'status', 'partial | completed | entered-in-error | health-unknown', sptTOKEN, [], 'FamilyMemberHistory.status', sxpNormal);
  indexes.add('FamilyMemberHistory', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Patient', 'FamilyMemberHistory', ['patient']);
end;
{$ENDIF FHIR_FAMILYMEMBERHISTORY}

{$IFDEF FHIR_FLAG}
procedure TFHIRIndexBuilderR5.buildIndexesForFlag(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Flag', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('Flag', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('Flag', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('Flag', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('Flag', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('Flag', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('Flag', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('Flag', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('Flag', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('Flag', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('Flag', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('Flag', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('Flag', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('Flag', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('Flag', 'author', 'Flag creator', sptREFERENCE, ['Practitioner', 'Organization', 'Device', 'Patient', 'PractitionerRole'], 'Flag.author', sxpNormal);
  indexes.add('Flag', 'date', '): Time period when flag is active', sptDATE, [], 'AllergyIntolerance.recordedDate | CarePlan.period | ClinicalImpression.date | Composition.date | Consent.dateTime | DiagnosticReport.effective | Encounter.actualPeriod | EpisodeOfCare.period | FamilyMemberHistory.date | Flag.period | (Immunization.oc'
      +'currence as dateTime) | List.date | Observation.effective | Procedure.occurrence | (RiskAssessment.occurrence as dateTime) | SupplyRequest.authoredOn', sxpNormal);
  indexes.add('Flag', 'encounter', '): Alert relevant during encounter', sptREFERENCE, ['Encounter'], 'Composition.encounter | DeviceRequest.encounter | DiagnosticReport.encounter | Flag.encounter | List.encounter | NutritionOrder.encounter | Observation.encounter | Procedure.encounter | RiskAssessment.encounter | ServiceRequest.encounter | VisionPres'
      +'cription.encounter', sxpNormal);
  indexes.add('Flag', 'identifier', 'Business identifier', sptTOKEN, [], 'Flag.identifier', sxpNormal);
  indexes.add('Flag', 'patient', '): The identity of a subject to list flags for', sptREFERENCE, ['Patient', 'Practitioner', 'Group', 'Organization', 'BiologicallyDerivedProduct', 'NutritionProduct', 'Device', 'Medication', 'Procedure', 'Substance', 'Location'], 'AllergyIntolerance.patient | CarePlan.subject.where(resolve() is Patient) | CareTeam.subject.where(resolve() is Patient) | ClinicalImpression.subject.where(resolve() is Patient) | Composition.subject.where(resolve() is Patient) | Condition.subject.wh'
      +'ere(resolve() is Patient) | Consent.subject.where(resolve() is Patient) | DetectedIssue.subject | DeviceRequest.subject.where(resolve() is Patient) | DeviceUsage.patient | DiagnosticReport.subject.where(resolve() is Patient) | DocumentManifes'
      +'t.subject.where(resolve() is Patient) | DocumentReference.subject.where(resolve() is Patient) | Encounter.subject.where(resolve() is Patient) | EpisodeOfCare.patient | FamilyMemberHistory.patient | Flag.subject.where(resol'+
   've() is Patient) | G'
      +'oal.subject.where(resolve() is Patient) | ImagingStudy.subject.where(resolve() is Patient) | Immunization.patient | List.subject.where(resolve() is Patient) | MedicationAdministration.subject.where(resolve() is Patient) | MedicationDispense.s'
      +'ubject.where(resolve() is Patient) | MedicationRequest.subject.where(resolve() is Patient) | MedicationUsage.subject.where(resolve() is Patient) | NutritionOrder.subject.where(resolve() is Patient) | Observation.subject.where(resolve() is Pat'
      +'ient) | Procedure.subject.where(resolve() is Patient) | RiskAssessment.subject.where(resolve() is Patient) | ServiceRequest.subject.where(resolve() is Patient) | SupplyDelivery.patient | VisionPrescription.patient', sxpNormal);
  indexes.add('Flag', 'status', 'active | inactive | entered-in-error', sptTOKEN, [], 'Flag.status', sxpNormal);
  indexes.add('Flag', 'subject', 'The identity of a subject to list flags for', sptREFERENCE, ['Practitioner', 'Group', 'Organization', 'Medication', 'Patient', 'PlanDefinition', 'Procedure', 'PractitionerRole', 'Location'], 'Flag.subject', sxpNormal);
  indexes.add('Flag', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Device', 'Flag', ['author']);
  compartments.register('Patient', 'Flag', ['patient']);
  compartments.register('Practitioner', 'Flag', ['author']);
end;
{$ENDIF FHIR_FLAG}

{$IFDEF FHIR_FORMULARYITEM}
procedure TFHIRIndexBuilderR5.buildIndexesForFormularyItem(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('FormularyItem', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('FormularyItem', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('FormularyItem', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('FormularyItem', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('FormularyItem', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('FormularyItem', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('FormularyItem', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('FormularyItem', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('FormularyItem', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('FormularyItem', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('FormularyItem', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('FormularyItem', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('FormularyItem', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('FormularyItem', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('FormularyItem', 'code', 'Returns formulary items for a specific code', sptTOKEN, [], 'FormularyItem.code', sxpNormal);
  indexes.add('FormularyItem', 'identifier', 'Returns formulary items with this external identifier', sptTOKEN, [], 'FormularyItem.identifier', sxpNormal);
  indexes.add('FormularyItem', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_FORMULARYITEM}

{$IFDEF FHIR_GENOMICSTUDY}
procedure TFHIRIndexBuilderR5.buildIndexesForGenomicStudy(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('GenomicStudy', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('GenomicStudy', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('GenomicStudy', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('GenomicStudy', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('GenomicStudy', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('GenomicStudy', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('GenomicStudy', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('GenomicStudy', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('GenomicStudy', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('GenomicStudy', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('GenomicStudy', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('GenomicStudy', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('GenomicStudy', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('GenomicStudy', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('GenomicStudy', 'analysis-patient', 'Who the analysis is about', sptREFERENCE, ['Practitioner', 'Group', 'Organization', 'BiologicallyDerivedProduct', 'NutritionProduct', 'Device', 'Medication', 'Patient', 'Procedure', 'Substance', 'Location'], 'GenomicStudy.analysis.subject.where(resolve() is Patient)', sxpNormal);
  indexes.add('GenomicStudy', 'analysis-subject', 'Who the analysis is about', sptREFERENCE, ['Practitioner', 'Group', 'Organization', 'BiologicallyDerivedProduct', 'NutritionProduct', 'Device', 'Medication', 'Patient', 'Procedure', 'Substance', 'Location'], 'GenomicStudy.analysis.subject', sxpNormal);
  indexes.add('GenomicStudy', 'identifier', 'Identifiers for the Study', sptTOKEN, [], 'GenomicStudy.identifier', sxpNormal);
  indexes.add('GenomicStudy', 'patient', 'Who the study is about', sptREFERENCE, ['Practitioner', 'Group', 'Organization', 'BiologicallyDerivedProduct', 'NutritionProduct', 'Device', 'Medication', 'Patient', 'Procedure', 'Substance', 'Location'], 'GenomicStudy.subject.where(resolve() is Patient)', sxpNormal);
  indexes.add('GenomicStudy', 'status', 'The status of the study', sptTOKEN, [], 'GenomicStudy.status', sxpNormal);
  indexes.add('GenomicStudy', 'subject', 'Who the study is about', sptREFERENCE, ['Practitioner', 'Group', 'Organization', 'BiologicallyDerivedProduct', 'NutritionProduct', 'Device', 'Medication', 'Patient', 'Procedure', 'Substance', 'Location'], 'GenomicStudy.subject', sxpNormal);
  indexes.add('GenomicStudy', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Patient', 'GenomicStudy', ['patient']);
end;
{$ENDIF FHIR_GENOMICSTUDY}

{$IFDEF FHIR_GOAL}
procedure TFHIRIndexBuilderR5.buildIndexesForGoal(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Goal', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('Goal', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('Goal', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('Goal', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('Goal', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('Goal', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('Goal', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('Goal', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('Goal', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('Goal', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('Goal', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('Goal', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('Goal', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('Goal', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('Goal', 'achievement-status', 'in-progress | improving | worsening | no-change | achieved | sustaining | not-achieved | no-progress | not-attainable', sptTOKEN, [], 'Goal.achievementStatus', sxpNormal);
  indexes.add('Goal', 'addresses', 'Issues addressed by this goal', sptREFERENCE, ['Condition', 'RiskAssessment', 'MedicationRequest', 'NutritionOrder', 'Observation', 'MedicationUsage', 'ServiceRequest'], 'Goal.addresses', sxpNormal);
  indexes.add('Goal', 'category', 'E.g. Treatment, dietary, behavioral, etc.', sptTOKEN, [], 'Goal.category', sxpNormal);
  indexes.add('Goal', 'identifier', '): External Ids for this goal', sptTOKEN, [], 'AllergyIntolerance.identifier | CarePlan.identifier | CareTeam.identifier | Composition.identifier | Condition.identifier | Consent.identifier | DetectedIssue.identifier | DeviceRequest.identifier | DiagnosticReport.identifier | DocumentManifest.mast'
      +'erIdentifier | DocumentManifest.identifier | DocumentReference.identifier | Encounter.identifier | EpisodeOfCare.identifier | FamilyMemberHistory.identifier | Goal.identifier | ImagingStudy.identifier | Immunization.identifier | List.identifi'
      +'er | MedicationAdministration.identifier | MedicationDispense.identifier | MedicationRequest.identifier | MedicationUsage.identifier | NutritionOrder.identifier | Observation.identifier | Procedure.identifier | RiskAssessment.identifier | Ser'
      +'viceRequest.identifier | SupplyDelivery.identifier | SupplyRequest.identifier | VisionPrescription.identifier', sxpNormal);
  indexes.add('Goal', 'lifecycle-status', 'proposed | planned | accepted | active | on-hold | completed | cancelled | entered-in-error | rejected', sptTOKEN, [], 'Goal.lifecycleStatus', sxpNormal);
  indexes.add('Goal', 'patient', '): Who this goal is intended for', sptREFERENCE, ['Patient', 'Practitioner', 'Group', 'Organization', 'BiologicallyDerivedProduct', 'NutritionProduct', 'Device', 'Medication', 'Procedure', 'Substance', 'Location'], 'AllergyIntolerance.patient | CarePlan.subject.where(resolve() is Patient) | CareTeam.subject.where(resolve() is Patient) | ClinicalImpression.subject.where(resolve() is Patient) | Composition.subject.where(resolve() is Patient) | Condition.subject.wh'
      +'ere(resolve() is Patient) | Consent.subject.where(resolve() is Patient) | DetectedIssue.subject | DeviceRequest.subject.where(resolve() is Patient) | DeviceUsage.patient | DiagnosticReport.subject.where(resolve() is Patient) | DocumentManifes'
      +'t.subject.where(resolve() is Patient) | DocumentReference.subject.where(resolve() is Patient) | Encounter.subject.where(resolve() is Patient) | EpisodeOfCare.patient | FamilyMemberHistory.patient | Flag.subject.where(resolve() is Patien'+
   't) | G'
      +'oal.subject.where(resolve() is Patient) | ImagingStudy.subject.where(resolve() is Patient) | Immunization.patient | List.subject.where(resolve() is Patient) | MedicationAdministration.subject.where(resolve() is Patient) | MedicationDispense.s'
      +'ubject.where(resolve() is Patient) | MedicationRequest.subject.where(resolve() is Patient) | MedicationUsage.subject.where(resolve() is Patient) | NutritionOrder.subject.where(resolve() is Patient) | Observation.subject.where(resolve() is Pat'
      +'ient) | Procedure.subject.where(resolve() is Patient) | RiskAssessment.subject.where(resolve() is Patient) | ServiceRequest.subject.where(resolve() is Patient) | SupplyDelivery.patient | VisionPrescription.patient', sxpNormal);
  indexes.add('Goal', 'start-date', 'When goal pursuit begins', sptDATE, [], '(Goal.start as date)', sxpNormal);
  indexes.add('Goal', 'subject', 'Who this goal is intended for', sptREFERENCE, ['Group', 'Organization', 'Patient'], 'Goal.subject', sxpNormal);
  indexes.add('Goal', 'target-date', 'Reach goal on or before', sptDATE, [], '(Goal.target.due as date)', sxpNormal);
  indexes.add('Goal', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Patient', 'Goal', ['patient']);
end;
{$ENDIF FHIR_GOAL}

{$IFDEF FHIR_GRAPHDEFINITION}
procedure TFHIRIndexBuilderR5.buildIndexesForGraphDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('GraphDefinition', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('GraphDefinition', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('GraphDefinition', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('GraphDefinition', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('GraphDefinition', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('GraphDefinition', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('GraphDefinition', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('GraphDefinition', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('GraphDefinition', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('GraphDefinition', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('GraphDefinition', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('GraphDefinition', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('GraphDefinition', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('GraphDefinition', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('GraphDefinition', 'context', '): A use context assigned to the graph definition', sptTOKEN, [], '(CapabilityStatement.useContext.value as CodeableConcept) | (CodeSystem.useContext.value as CodeableConcept) | (CompartmentDefinition.useContext.value as CodeableConcept) | (ConceptMap.useContext.value as CodeableConcept) | (GraphDefinition.useContex'
      +'t.value as CodeableConcept) | (ImplementationGuide.useContext.value as CodeableConcept) | (MessageDefinition.useContext.value as CodeableConcept) | (NamingSystem.useContext.value as CodeableConcept) | (OperationDefinition.useContext.value as '
      +'CodeableConcept) | (SearchParameter.useContext.value as CodeableConcept) | (StructureDefinition.useContext.value as CodeableConcept) | (StructureMap.useContext.value as CodeableConcept) | (TerminologyCapabilities.useContext.value as CodeableC'
      +'oncept) | (ValueSet.useContext.value as CodeableConcept)', sxpNormal);
  indexes.add('GraphDefinition', 'context-quantity', '): A quantity- or range-valued use context assigned to the graph definition', sptQUANTITY, [], '(CapabilityStatement.useContext.value as Quantity) | (CapabilityStatement.useContext.value as Range) | (CodeSystem.useContext.value as Quantity) | (CodeSystem.useContext.value as Range) | (CompartmentDefinition.useContext.value as Quantity) | (Compar'
      +'tmentDefinition.useContext.value as Range) | (ConceptMap.useContext.value as Quantity) | (ConceptMap.useContext.value as Range) | (GraphDefinition.useContext.value as Quantity) | (GraphDefinition.useContext.value as Range) | (ImplementationGu'
      +'ide.useContext.value as Quantity) | (ImplementationGuide.useContext.value as Range) | (MessageDefinition.useContext.value as Quantity) | (MessageDefinition.useContext.value as Range) | (NamingSystem.useContext.value as Quantity) | (NamingSyst'
      +'em.useContext.value as Range) | (OperationDefinition.useContext.value as Quantity) '+
   '| (OperationDefinition.useContext.value as Range) | (SearchParameter.useContext.value as Quantity) | (SearchParameter.useContext.value as Range) | (StructureDe'
      +'finition.useContext.value as Quantity) | (StructureDefinition.useContext.value as Range) | (StructureMap.useContext.value as Quantity) | (StructureMap.useContext.value as Range) | (TerminologyCapabilities.useContext.value as Quantity) | (Term'
      +'inologyCapabilities.useContext.value as Range) | (ValueSet.useContext.value as Quantity) | (ValueSet.useContext.value as Range)', sxpNormal);
  indexes.add('GraphDefinition', 'context-type', '): A type of use context assigned to the graph definition', sptTOKEN, [], 'CapabilityStatement.useContext.code | CodeSystem.useContext.code | CompartmentDefinition.useContext.code | ConceptMap.useContext.code | GraphDefinition.useContext.code | ImplementationGuide.useContext.code | MessageDefinition.useContext.code | Naming'
      +'System.useContext.code | OperationDefinition.useContext.code | SearchParameter.useContext.code | StructureDefinition.useContext.code | StructureMap.useContext.code | TerminologyCapabilities.useContext.code | ValueSet.useContext.code', sxpNormal);
  indexes.add('GraphDefinition', 'context-type-quantity', '): A use context type and quantity- or range-based value assigned to the graph definition', sptCOMPOSITE, [], 'CapabilityStatement.useContext | CodeSystem.useContext | CompartmentDefinition.useContext | ConceptMap.useContext | GraphDefinition.useContext | ImplementationGuide.useContext | MessageDefinition.useContext | NamingSystem.useContext | OperationDefini'
      +'tion.useContext | SearchParameter.useContext | StructureDefinition.useContext | StructureMap.useContext | TerminologyCapabilities.useContext | ValueSet.useContext', sxpNormal);
  indexes.add('GraphDefinition', 'context-type-value', '): A use context type and value assigned to the graph definition', sptCOMPOSITE, [], 'CapabilityStatement.useContext | CodeSystem.useContext | CompartmentDefinition.useContext | ConceptMap.useContext | GraphDefinition.useContext | ImplementationGuide.useContext | MessageDefinition.useContext | NamingSystem.useContext | OperationDefini'
      +'tion.useContext | SearchParameter.useContext | StructureDefinition.useContext | StructureMap.useContext | TerminologyCapabilities.useContext | ValueSet.useContext', sxpNormal);
  indexes.add('GraphDefinition', 'date', '): The graph definition publication date', sptDATE, [], 'CapabilityStatement.date | CodeSystem.date | CompartmentDefinition.date | ConceptMap.date | GraphDefinition.date | ImplementationGuide.date | MessageDefinition.date | NamingSystem.date | OperationDefinition.date | SearchParameter.date | StructureDefi'
      +'nition.date | StructureMap.date | TerminologyCapabilities.date | ValueSet.date', sxpNormal);
  indexes.add('GraphDefinition', 'description', '): The description of the graph definition', sptSTRING, [], 'CapabilityStatement.description | CodeSystem.description | CompartmentDefinition.description | ConceptMap.description | GraphDefinition.description | ImplementationGuide.description | MessageDefinition.description | NamingSystem.description | Operati'
      +'onDefinition.description | SearchParameter.description | StructureDefinition.description | StructureMap.description | TerminologyCapabilities.description | ValueSet.description', sxpNormal);
  indexes.add('GraphDefinition', 'jurisdiction', '): Intended jurisdiction for the graph definition', sptTOKEN, [], 'CapabilityStatement.jurisdiction | CodeSystem.jurisdiction | ConceptMap.jurisdiction | GraphDefinition.jurisdiction | ImplementationGuide.jurisdiction | MessageDefinition.jurisdiction | NamingSystem.jurisdiction | OperationDefinition.jurisdiction | S'
      +'earchParameter.jurisdiction | StructureDefinition.jurisdiction | StructureMap.jurisdiction | TerminologyCapabilities.jurisdiction | ValueSet.jurisdiction', sxpNormal);
  indexes.add('GraphDefinition', 'name', '): Computationally friendly name of the graph definition', sptSTRING, [], 'CapabilityStatement.name | CodeSystem.name | CompartmentDefinition.name | ConceptMap.name | GraphDefinition.name | ImplementationGuide.name | MessageDefinition.name | NamingSystem.name | OperationDefinition.name | SearchParameter.name | StructureDefi'
      +'nition.name | StructureMap.name | TerminologyCapabilities.name | ValueSet.name', sxpNormal);
  indexes.add('GraphDefinition', 'publisher', '): Name of the publisher of the graph definition', sptSTRING, [], 'CapabilityStatement.publisher | CodeSystem.publisher | CompartmentDefinition.publisher | ConceptMap.publisher | GraphDefinition.publisher | ImplementationGuide.publisher | MessageDefinition.publisher | NamingSystem.publisher | OperationDefinition.pub'
      +'lisher | SearchParameter.publisher | StructureDefinition.publisher | StructureMap.publisher | TerminologyCapabilities.publisher | ValueSet.publisher', sxpNormal);
  indexes.add('GraphDefinition', 'start', 'Type of resource at which the graph starts', sptTOKEN, [], 'GraphDefinition.start', sxpNormal);
  indexes.add('GraphDefinition', 'status', '): The current status of the graph definition', sptTOKEN, [], 'CapabilityStatement.status | CodeSystem.status | CompartmentDefinition.status | ConceptMap.status | GraphDefinition.status | ImplementationGuide.status | MessageDefinition.status | NamingSystem.status | OperationDefinition.status | SearchParameter.st'
      +'atus | StructureDefinition.status | StructureMap.status | TerminologyCapabilities.status | ValueSet.status', sxpNormal);
  indexes.add('GraphDefinition', 'url', '): The uri that identifies the graph definition', sptURI, [], 'CapabilityStatement.url | CodeSystem.url | CompartmentDefinition.url | ConceptMap.url | GraphDefinition.url | ImplementationGuide.url | MessageDefinition.url | NamingSystem.url | OperationDefinition.url | SearchParameter.url | StructureDefinition.url'
      +' | StructureMap.url | TerminologyCapabilities.url | ValueSet.url', sxpNormal);
  indexes.add('GraphDefinition', 'version', '): The business version of the graph definition', sptTOKEN, [], 'CapabilityStatement.version | CodeSystem.version | CompartmentDefinition.version | ConceptMap.version | GraphDefinition.version | ImplementationGuide.version | MessageDefinition.version | NamingSystem.version | OperationDefinition.version | SearchPar'
      +'ameter.version | StructureDefinition.version | StructureMap.version | TerminologyCapabilities.version | ValueSet.version', sxpNormal);
  indexes.add('GraphDefinition', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_GRAPHDEFINITION}

{$IFDEF FHIR_GROUP}
procedure TFHIRIndexBuilderR5.buildIndexesForGroup(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Group', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('Group', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('Group', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('Group', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('Group', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('Group', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('Group', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('Group', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('Group', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('Group', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('Group', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('Group', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('Group', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('Group', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('Group', 'characteristic', 'Kind of characteristic', sptTOKEN, [], 'Group.characteristic.code', sxpNormal);
  indexes.add('Group', 'characteristic-reference', 'An entity referenced in a characteristic', sptREFERENCE, [], '(Group.characteristic.value as Reference)', sxpNormal);
  indexes.add('Group', 'characteristic-value', 'A composite of both characteristic and value', sptCOMPOSITE, [], 'Group.characteristic', sxpNormal);
  indexes.add('Group', 'code', 'The kind of resources contained', sptTOKEN, [], 'Group.code', sxpNormal);
  indexes.add('Group', 'exclude', 'Group includes or excludes', sptTOKEN, [], 'Group.characteristic.exclude', sxpNormal);
  indexes.add('Group', 'identifier', 'Unique id', sptTOKEN, [], 'Group.identifier', sxpNormal);
  indexes.add('Group', 'managing-entity', 'Entity that is the custodian of the Group''s definition', sptREFERENCE, ['Practitioner', 'Organization', 'PractitionerRole', 'RelatedPerson'], 'Group.managingEntity', sxpNormal);
  indexes.add('Group', 'member', 'Reference to the group member', sptREFERENCE, ['Practitioner', 'Group', 'Specimen', 'Organization', 'CareTeam', 'Device', 'Patient', 'HealthcareService', 'PractitionerRole', 'RelatedPerson', 'Location'], 'Group.member.entity', sxpNormal);
  indexes.add('Group', 'membership', 'Definitional or enumerated group', sptTOKEN, [], 'Group.membership', sxpNormal);
  indexes.add('Group', 'name', 'A portion of the Group''s name', sptSTRING, [], 'Group.name', sxpNormal);
  indexes.add('Group', 'type', 'The type of resources the group contains', sptTOKEN, [], 'Group.type', sxpNormal);
  indexes.add('Group', 'value', 'Value held by characteristic', sptTOKEN, [], '(Group.characteristic.value as CodeableConcept) | (Group.characteristic.value as boolean)', sxpNormal);
  indexes.add('Group', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Device', 'Group', ['member']);
  compartments.register('Patient', 'Group', ['member']);
  compartments.register('Practitioner', 'Group', ['member']);
end;
{$ENDIF FHIR_GROUP}

{$IFDEF FHIR_GUIDANCERESPONSE}
procedure TFHIRIndexBuilderR5.buildIndexesForGuidanceResponse(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('GuidanceResponse', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('GuidanceResponse', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('GuidanceResponse', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('GuidanceResponse', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('GuidanceResponse', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('GuidanceResponse', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('GuidanceResponse', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('GuidanceResponse', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('GuidanceResponse', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('GuidanceResponse', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('GuidanceResponse', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('GuidanceResponse', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('GuidanceResponse', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('GuidanceResponse', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('GuidanceResponse', 'identifier', 'The identifier of the guidance response', sptTOKEN, [], 'GuidanceResponse.identifier', sxpNormal);
  indexes.add('GuidanceResponse', 'patient', 'The identity of a patient to search for guidance response results', sptREFERENCE, ['Patient'], 'GuidanceResponse.subject.where(resolve() is Patient)', sxpNormal);
  indexes.add('GuidanceResponse', 'request', 'The identifier of the request associated with the response', sptTOKEN, [], 'GuidanceResponse.requestIdentifier', sxpNormal);
  indexes.add('GuidanceResponse', 'status', 'The status of the guidance response', sptTOKEN, [], 'GuidanceResponse.status', sxpNormal);
  indexes.add('GuidanceResponse', 'subject', 'The subject that the guidance response is about', sptREFERENCE, ['Group', 'Patient'], 'GuidanceResponse.subject', sxpNormal);
  indexes.add('GuidanceResponse', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_GUIDANCERESPONSE}

{$IFDEF FHIR_HEALTHCARESERVICE}
procedure TFHIRIndexBuilderR5.buildIndexesForHealthcareService(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('HealthcareService', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('HealthcareService', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('HealthcareService', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('HealthcareService', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('HealthcareService', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('HealthcareService', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('HealthcareService', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('HealthcareService', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('HealthcareService', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('HealthcareService', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('HealthcareService', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('HealthcareService', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('HealthcareService', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('HealthcareService', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('HealthcareService', 'active', 'The Healthcare Service is currently marked as active', sptTOKEN, [], 'HealthcareService.active', sxpNormal);
  indexes.add('HealthcareService', 'characteristic', 'One of the HealthcareService''s characteristics', sptTOKEN, [], 'HealthcareService.characteristic', sxpNormal);
  indexes.add('HealthcareService', 'coverage-area', 'Location(s) service is intended for/available to', sptREFERENCE, ['Location'], 'HealthcareService.coverageArea', sxpNormal);
  indexes.add('HealthcareService', 'endpoint', 'Technical endpoints providing access to electronic services operated for the healthcare service', sptREFERENCE, ['Endpoint'], 'HealthcareService.endpoint', sxpNormal);
  indexes.add('HealthcareService', 'identifier', 'External identifiers for this item', sptTOKEN, [], 'HealthcareService.identifier', sxpNormal);
  indexes.add('HealthcareService', 'location', 'The location of the Healthcare Service', sptREFERENCE, ['Location'], 'HealthcareService.location', sxpNormal);
  indexes.add('HealthcareService', 'name', 'A portion of the Healthcare service name', sptSTRING, [], 'HealthcareService.name', sxpNormal);
  indexes.add('HealthcareService', 'offered-in', 'The service within which this service is offered', sptREFERENCE, ['HealthcareService'], 'HealthcareService.offeredIn', sxpNormal);
  indexes.add('HealthcareService', 'organization', 'The organization that provides this Healthcare Service', sptREFERENCE, ['Organization'], 'HealthcareService.providedBy', sxpNormal);
  indexes.add('HealthcareService', 'program', 'One of the Programs supported by this HealthcareService', sptTOKEN, [], 'HealthcareService.program', sxpNormal);
  indexes.add('HealthcareService', 'service-category', 'Service Category of the Healthcare Service', sptTOKEN, [], 'HealthcareService.category', sxpNormal);
  indexes.add('HealthcareService', 'service-type', 'The type of service provided by this healthcare service', sptTOKEN, [], 'HealthcareService.type', sxpNormal);
  indexes.add('HealthcareService', 'specialty', 'The specialty of the service provided by this healthcare service', sptTOKEN, [], 'HealthcareService.specialty', sxpNormal);
  indexes.add('HealthcareService', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_HEALTHCARESERVICE}

{$IFDEF FHIR_IMAGINGSELECTION}
procedure TFHIRIndexBuilderR5.buildIndexesForImagingSelection(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ImagingSelection', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('ImagingSelection', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('ImagingSelection', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('ImagingSelection', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('ImagingSelection', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('ImagingSelection', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('ImagingSelection', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('ImagingSelection', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('ImagingSelection', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('ImagingSelection', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('ImagingSelection', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('ImagingSelection', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('ImagingSelection', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('ImagingSelection', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('ImagingSelection', 'based-on', 'The request associated with an imaging selection', sptREFERENCE, ['Appointment', 'AppointmentResponse', 'CarePlan', 'Task', 'ServiceRequest'], 'ImagingSelection.basedOn', sxpNormal);
  indexes.add('ImagingSelection', 'body-site', 'The body site associated with the imaging selection', sptTOKEN, [], 'ImagingSelection.bodySite.concept', sxpNormal);
  indexes.add('ImagingSelection', 'code', 'The imaging selection status', sptTOKEN, [], 'ImagingSelection.status', sxpNormal);
  indexes.add('ImagingSelection', 'derived-from', 'The imaging study from which the imaging selection was derived', sptREFERENCE, ['ImagingStudy'], 'ImagingSelection.derivedFrom', sxpNormal);
  indexes.add('ImagingSelection', 'identifier', 'Identifiers for the imaging selection', sptTOKEN, [], 'ImagingSelection.identifier', sxpNormal);
  indexes.add('ImagingSelection', 'issued', 'The date / time the imaging selection was created', sptDATE, [], 'ImagingSelection.issued', sxpNormal);
  indexes.add('ImagingSelection', 'patient', 'Who the study is about', sptREFERENCE, ['Patient'], 'ImagingSelection.subject.where(resolve() is Patient)', sxpNormal);
  indexes.add('ImagingSelection', 'study-uid', 'The DICOM Study Instance UID from which the images were selected', sptTOKEN, [], 'ImagingSelection.studyUid', sxpNormal);
  indexes.add('ImagingSelection', 'subject', 'The subject of the Imaging Selection, such as the associated Patient', sptREFERENCE, ['Practitioner', 'Group', 'Specimen', 'Organization', 'Device', 'Medication', 'Patient', 'Procedure', 'Substance', 'Location'], 'ImagingSelection.subject', sxpNormal);
  indexes.add('ImagingSelection', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Patient', 'ImagingSelection', ['patient']);
end;
{$ENDIF FHIR_IMAGINGSELECTION}

{$IFDEF FHIR_IMAGINGSTUDY}
procedure TFHIRIndexBuilderR5.buildIndexesForImagingStudy(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ImagingStudy', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('ImagingStudy', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('ImagingStudy', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('ImagingStudy', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('ImagingStudy', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('ImagingStudy', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('ImagingStudy', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('ImagingStudy', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('ImagingStudy', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('ImagingStudy', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('ImagingStudy', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('ImagingStudy', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('ImagingStudy', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('ImagingStudy', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('ImagingStudy', 'basedon', 'The order for the image, such as Accession Number associated with a ServiceRequest', sptREFERENCE, ['Appointment', 'AppointmentResponse', 'CarePlan', 'Task', 'ServiceRequest'], 'ImagingStudy.basedOn', sxpNormal);
  indexes.add('ImagingStudy', 'bodysite', 'The body site studied', sptTOKEN, [], 'ImagingStudy.series.bodySite.concept', sxpNormal);
  indexes.add('ImagingStudy', 'dicom-class', 'The type of the instance', sptTOKEN, [], 'ImagingStudy.series.instance.sopClass', sxpNormal);
  indexes.add('ImagingStudy', 'encounter', 'The context of the study', sptREFERENCE, ['Encounter'], 'ImagingStudy.encounter', sxpNormal);
  indexes.add('ImagingStudy', 'endpoint', 'The endpoint for the study or series', sptREFERENCE, ['Endpoint'], 'ImagingStudy.endpoint | ImagingStudy.series.endpoint', sxpNormal);
  indexes.add('ImagingStudy', 'identifier', '): Identifiers for the Study, such as DICOM Study Instance UID', sptTOKEN, [], 'AllergyIntolerance.identifier | CarePlan.identifier | CareTeam.identifier | Composition.identifier | Condition.identifier | Consent.identifier | DetectedIssue.identifier | DeviceRequest.identifier | DiagnosticReport.identifier | DocumentManifest.mast'
      +'erIdentifier | DocumentManifest.identifier | DocumentReference.identifier | Encounter.identifier | EpisodeOfCare.identifier | FamilyMemberHistory.identifier | Goal.identifier | ImagingStudy.identifier | Immunization.identifier | List.identifi'
      +'er | MedicationAdministration.identifier | MedicationDispense.identifier | MedicationRequest.identifier | MedicationUsage.identifier | NutritionOrder.identifier | Observation.identifier | Procedure.identifier | RiskAssessment.identifier | Ser'
      +'viceRequest.identifier | SupplyDelivery.identifier | SupplyRequest.identifier | VisionPrescription.identifie'+
   'r', sxpNormal);
  indexes.add('ImagingStudy', 'instance', 'SOP Instance UID for an instance', sptTOKEN, [], 'ImagingStudy.series.instance.uid', sxpNormal);
  indexes.add('ImagingStudy', 'interpreter', 'Who interpreted the images', sptREFERENCE, ['Practitioner', 'PractitionerRole'], 'ImagingStudy.interpreter', sxpNormal);
  indexes.add('ImagingStudy', 'modality', 'The modality of the series', sptTOKEN, [], 'ImagingStudy.series.modality', sxpNormal);
  indexes.add('ImagingStudy', 'patient', '): Who the study is about', sptREFERENCE, ['Patient', 'Practitioner', 'Group', 'Organization', 'BiologicallyDerivedProduct', 'NutritionProduct', 'Device', 'Medication', 'Procedure', 'Substance', 'Location'], 'AllergyIntolerance.patient | CarePlan.subject.where(resolve() is Patient) | CareTeam.subject.where(resolve() is Patient) | ClinicalImpression.subject.where(resolve() is Patient) | Composition.subject.where(resolve() is Patient) | Condition.subject.wh'
      +'ere(resolve() is Patient) | Consent.subject.where(resolve() is Patient) | DetectedIssue.subject | DeviceRequest.subject.where(resolve() is Patient) | DeviceUsage.patient | DiagnosticReport.subject.where(resolve() is Patient) | DocumentManifes'
      +'t.subject.where(resolve() is Patient) | DocumentReference.subject.where(resolve() is Patient) | Encounter.subject.where(resolve() is Patient) | EpisodeOfCare.patient | FamilyMemberHistory.patient | Flag.subject.where(resolve() is Patie'+
   'nt) | G'
      +'oal.subject.where(resolve() is Patient) | ImagingStudy.subject.where(resolve() is Patient) | Immunization.patient | List.subject.where(resolve() is Patient) | MedicationAdministration.subject.where(resolve() is Patient) | MedicationDispense.s'
      +'ubject.where(resolve() is Patient) | MedicationRequest.subject.where(resolve() is Patient) | MedicationUsage.subject.where(resolve() is Patient) | NutritionOrder.subject.where(resolve() is Patient) | Observation.subject.where(resolve() is Pat'
      +'ient) | Procedure.subject.where(resolve() is Patient) | RiskAssessment.subject.where(resolve() is Patient) | ServiceRequest.subject.where(resolve() is Patient) | SupplyDelivery.patient | VisionPrescription.patient', sxpNormal);
  indexes.add('ImagingStudy', 'performer', 'The person who performed the study', sptREFERENCE, ['Practitioner', 'Organization', 'CareTeam', 'Device', 'Patient', 'HealthcareService', 'PractitionerRole', 'RelatedPerson'], 'ImagingStudy.series.performer.actor', sxpNormal);
  indexes.add('ImagingStudy', 'reason', 'The reason for the study', sptTOKEN, [], '', sxpNormal);
  indexes.add('ImagingStudy', 'referrer', 'The referring physician', sptREFERENCE, ['Practitioner', 'PractitionerRole'], 'ImagingStudy.referrer', sxpNormal);
  indexes.add('ImagingStudy', 'series', 'DICOM Series Instance UID for a series', sptTOKEN, [], 'ImagingStudy.series.uid', sxpNormal);
  indexes.add('ImagingStudy', 'started', 'When the study was started', sptDATE, [], 'ImagingStudy.started', sxpNormal);
  indexes.add('ImagingStudy', 'status', 'The status of the study', sptTOKEN, [], 'ImagingStudy.status', sxpNormal);
  indexes.add('ImagingStudy', 'subject', 'Who the study is about', sptREFERENCE, ['Group', 'Device', 'Patient'], 'ImagingStudy.subject', sxpNormal);
  indexes.add('ImagingStudy', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Patient', 'ImagingStudy', ['patient']);
end;
{$ENDIF FHIR_IMAGINGSTUDY}

{$IFDEF FHIR_IMMUNIZATION}
procedure TFHIRIndexBuilderR5.buildIndexesForImmunization(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Immunization', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('Immunization', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('Immunization', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('Immunization', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('Immunization', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('Immunization', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('Immunization', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('Immunization', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('Immunization', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('Immunization', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('Immunization', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('Immunization', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('Immunization', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('Immunization', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('Immunization', 'date', '): Vaccination  (non)-Administration Date', sptDATE, [], 'AllergyIntolerance.recordedDate | CarePlan.period | ClinicalImpression.date | Composition.date | Consent.dateTime | DiagnosticReport.effective | Encounter.actualPeriod | EpisodeOfCare.period | FamilyMemberHistory.date | Flag.period | (Immunization.oc'
      +'currence as dateTime) | List.date | Observation.effective | Procedure.occurrence | (RiskAssessment.occurrence as dateTime) | SupplyRequest.authoredOn', sxpNormal);
  indexes.add('Immunization', 'identifier', '): Business identifier', sptTOKEN, [], 'AllergyIntolerance.identifier | CarePlan.identifier | CareTeam.identifier | Composition.identifier | Condition.identifier | Consent.identifier | DetectedIssue.identifier | DeviceRequest.identifier | DiagnosticReport.identifier | DocumentManifest.mast'
      +'erIdentifier | DocumentManifest.identifier | DocumentReference.identifier | Encounter.identifier | EpisodeOfCare.identifier | FamilyMemberHistory.identifier | Goal.identifier | ImagingStudy.identifier | Immunization.identifier | List.identifi'
      +'er | MedicationAdministration.identifier | MedicationDispense.identifier | MedicationRequest.identifier | MedicationUsage.identifier | NutritionOrder.identifier | Observation.identifier | Procedure.identifier | RiskAssessment.identifier | Ser'
      +'viceRequest.identifier | SupplyDelivery.identifier | SupplyRequest.identifier | VisionPrescription.identifier', sxpNormal);
  indexes.add('Immunization', 'location', 'The service delivery location or facility in which the vaccine was / was to be administered', sptREFERENCE, ['Location'], 'Immunization.location', sxpNormal);
  indexes.add('Immunization', 'lot-number', 'Vaccine Lot Number', sptSTRING, [], 'Immunization.lotNumber', sxpNormal);
  indexes.add('Immunization', 'manufacturer', 'Vaccine Manufacturer', sptREFERENCE, [], 'Immunization.manufacturer.reference', sxpNormal);
  indexes.add('Immunization', 'patient', '): The patient for the vaccination record', sptREFERENCE, ['Patient', 'Practitioner', 'Group', 'Organization', 'BiologicallyDerivedProduct', 'NutritionProduct', 'Device', 'Medication', 'Procedure', 'Substance', 'Location'], 'AllergyIntolerance.patient | CarePlan.subject.where(resolve() is Patient) | CareTeam.subject.where(resolve() is Patient) | ClinicalImpression.subject.where(resolve() is Patient) | Composition.subject.where(resolve() is Patient) | Condition.subject.wh'
      +'ere(resolve() is Patient) | Consent.subject.where(resolve() is Patient) | DetectedIssue.subject | DeviceRequest.subject.where(resolve() is Patient) | DeviceUsage.patient | DiagnosticReport.subject.where(resolve() is Patient) | DocumentManifes'
      +'t.subject.where(resolve() is Patient) | DocumentReference.subject.where(resolve() is Patient) | Encounter.subject.where(resolve() is Patient) | EpisodeOfCare.patient | FamilyMemberHistory.patient | Flag.subject.where(re'+
   'solve() is Patient) | G'
      +'oal.subject.where(resolve() is Patient) | ImagingStudy.subject.where(resolve() is Patient) | Immunization.patient | List.subject.where(resolve() is Patient) | MedicationAdministration.subject.where(resolve() is Patient) | MedicationDispense.s'
      +'ubject.where(resolve() is Patient) | MedicationRequest.subject.where(resolve() is Patient) | MedicationUsage.subject.where(resolve() is Patient) | NutritionOrder.subject.where(resolve() is Patient) | Observation.subject.where(resolve() is Pat'
      +'ient) | Procedure.subject.where(resolve() is Patient) | RiskAssessment.subject.where(resolve() is Patient) | ServiceRequest.subject.where(resolve() is Patient) | SupplyDelivery.patient | VisionPrescription.patient', sxpNormal);
  indexes.add('Immunization', 'performer', 'The practitioner, individual or organization who played a role in the vaccination', sptREFERENCE, ['Practitioner', 'Organization', 'Patient', 'PractitionerRole', 'RelatedPerson'], 'Immunization.performer.actor', sxpNormal);
  indexes.add('Immunization', 'reaction', 'Additional information on reaction', sptREFERENCE, [], 'Immunization.reaction.manifestation.reference', sxpNormal);
  indexes.add('Immunization', 'reaction-date', 'When reaction started', sptDATE, [], 'Immunization.reaction.date', sxpNormal);
  indexes.add('Immunization', 'reason-code', 'Reason why the vaccine was administered', sptTOKEN, [], 'Immunization.reason.concept', sxpNormal);
  indexes.add('Immunization', 'reason-reference', 'Reference to a resource (by instance)', sptREFERENCE, [], 'Immunization.reason.reference', sxpNormal);
  indexes.add('Immunization', 'series', 'The series being followed by the provider', sptSTRING, [], 'Immunization.protocolApplied.series', sxpNormal);
  indexes.add('Immunization', 'status', 'Immunization event status', sptTOKEN, [], 'Immunization.status', sxpNormal);
  indexes.add('Immunization', 'status-reason', 'Reason why the vaccine was not administered', sptTOKEN, [], 'Immunization.statusReason', sxpNormal);
  indexes.add('Immunization', 'target-disease', 'The target disease the dose is being administered against', sptTOKEN, [], 'Immunization.protocolApplied.targetDisease', sxpNormal);
  indexes.add('Immunization', 'vaccine-code', 'Vaccine Product Administered', sptTOKEN, [], 'Immunization.vaccineCode', sxpNormal);
  indexes.add('Immunization', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Patient', 'Immunization', ['patient']);
  compartments.register('Practitioner', 'Immunization', ['performer']);
end;
{$ENDIF FHIR_IMMUNIZATION}

{$IFDEF FHIR_IMMUNIZATIONEVALUATION}
procedure TFHIRIndexBuilderR5.buildIndexesForImmunizationEvaluation(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ImmunizationEvaluation', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('ImmunizationEvaluation', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('ImmunizationEvaluation', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('ImmunizationEvaluation', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('ImmunizationEvaluation', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('ImmunizationEvaluation', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('ImmunizationEvaluation', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('ImmunizationEvaluation', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('ImmunizationEvaluation', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('ImmunizationEvaluation', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('ImmunizationEvaluation', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('ImmunizationEvaluation', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('ImmunizationEvaluation', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('ImmunizationEvaluation', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('ImmunizationEvaluation', 'date', 'Date the evaluation was generated', sptDATE, [], 'ImmunizationEvaluation.date', sxpNormal);
  indexes.add('ImmunizationEvaluation', 'dose-status', 'The status of the dose relative to published recommendations', sptTOKEN, [], 'ImmunizationEvaluation.doseStatus', sxpNormal);
  indexes.add('ImmunizationEvaluation', 'identifier', 'ID of the evaluation', sptTOKEN, [], 'ImmunizationEvaluation.identifier', sxpNormal);
  indexes.add('ImmunizationEvaluation', 'immunization-event', 'The vaccine administration event being evaluated', sptREFERENCE, ['Immunization'], 'ImmunizationEvaluation.immunizationEvent', sxpNormal);
  indexes.add('ImmunizationEvaluation', 'patient', 'The patient being evaluated', sptREFERENCE, ['Patient'], 'ImmunizationEvaluation.patient', sxpNormal);
  indexes.add('ImmunizationEvaluation', 'status', 'Immunization evaluation status', sptTOKEN, [], 'ImmunizationEvaluation.status', sxpNormal);
  indexes.add('ImmunizationEvaluation', 'target-disease', 'The vaccine preventable disease being evaluated against', sptTOKEN, [], 'ImmunizationEvaluation.targetDisease', sxpNormal);
  indexes.add('ImmunizationEvaluation', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Patient', 'ImmunizationEvaluation', ['patient']);
end;
{$ENDIF FHIR_IMMUNIZATIONEVALUATION}

{$IFDEF FHIR_IMMUNIZATIONRECOMMENDATION}
procedure TFHIRIndexBuilderR5.buildIndexesForImmunizationRecommendation(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ImmunizationRecommendation', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('ImmunizationRecommendation', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('ImmunizationRecommendation', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('ImmunizationRecommendation', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('ImmunizationRecommendation', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('ImmunizationRecommendation', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('ImmunizationRecommendation', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('ImmunizationRecommendation', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('ImmunizationRecommendation', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('ImmunizationRecommendation', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('ImmunizationRecommendation', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('ImmunizationRecommendation', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('ImmunizationRecommendation', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('ImmunizationRecommendation', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('ImmunizationRecommendation', 'date', 'Date recommendation(s) created', sptDATE, [], 'ImmunizationRecommendation.date', sxpNormal);
  indexes.add('ImmunizationRecommendation', 'identifier', 'Business identifier', sptTOKEN, [], 'ImmunizationRecommendation.identifier', sxpNormal);
  indexes.add('ImmunizationRecommendation', 'information', 'Patient observations supporting recommendation', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'ImmunizationRecommendation.recommendation.supportingPatientInformation', sxpNormal);
  indexes.add('ImmunizationRecommendation', 'patient', 'Who this profile is for', sptREFERENCE, ['Patient'], 'ImmunizationRecommendation.patient', sxpNormal);
  indexes.add('ImmunizationRecommendation', 'status', 'Vaccine recommendation status', sptTOKEN, [], 'ImmunizationRecommendation.recommendation.forecastStatus', sxpNormal);
  indexes.add('ImmunizationRecommendation', 'support', 'Past immunizations supporting recommendation', sptREFERENCE, ['Immunization', 'ImmunizationEvaluation'], 'ImmunizationRecommendation.recommendation.supportingImmunization', sxpNormal);
  indexes.add('ImmunizationRecommendation', 'target-disease', 'Disease to be immunized against', sptTOKEN, [], 'ImmunizationRecommendation.recommendation.targetDisease', sxpNormal);
  indexes.add('ImmunizationRecommendation', 'vaccine-type', 'Vaccine  or vaccine group recommendation applies to', sptTOKEN, [], 'ImmunizationRecommendation.recommendation.vaccineCode', sxpNormal);
  indexes.add('ImmunizationRecommendation', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Patient', 'ImmunizationRecommendation', ['patient']);
end;
{$ENDIF FHIR_IMMUNIZATIONRECOMMENDATION}

{$IFDEF FHIR_IMPLEMENTATIONGUIDE}
procedure TFHIRIndexBuilderR5.buildIndexesForImplementationGuide(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ImplementationGuide', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('ImplementationGuide', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('ImplementationGuide', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('ImplementationGuide', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('ImplementationGuide', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('ImplementationGuide', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('ImplementationGuide', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('ImplementationGuide', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('ImplementationGuide', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('ImplementationGuide', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('ImplementationGuide', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('ImplementationGuide', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('ImplementationGuide', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('ImplementationGuide', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('ImplementationGuide', 'context', '): A use context assigned to the implementation guide', sptTOKEN, [], '(CapabilityStatement.useContext.value as CodeableConcept) | (CodeSystem.useContext.value as CodeableConcept) | (CompartmentDefinition.useContext.value as CodeableConcept) | (ConceptMap.useContext.value as CodeableConcept) | (GraphDefinition.useContex'
      +'t.value as CodeableConcept) | (ImplementationGuide.useContext.value as CodeableConcept) | (MessageDefinition.useContext.value as CodeableConcept) | (NamingSystem.useContext.value as CodeableConcept) | (OperationDefinition.useContext.value as '
      +'CodeableConcept) | (SearchParameter.useContext.value as CodeableConcept) | (StructureDefinition.useContext.value as CodeableConcept) | (StructureMap.useContext.value as CodeableConcept) | (TerminologyCapabilities.useContext.value as CodeableC'
      +'oncept) | (ValueSet.useContext.value as CodeableConcept)', sxpNormal);
  indexes.add('ImplementationGuide', 'context-quantity', '): A quantity- or range-valued use context assigned to the implementation guide', sptQUANTITY, [], '(CapabilityStatement.useContext.value as Quantity) | (CapabilityStatement.useContext.value as Range) | (CodeSystem.useContext.value as Quantity) | (CodeSystem.useContext.value as Range) | (CompartmentDefinition.useContext.value as Quantity) | (Compar'
      +'tmentDefinition.useContext.value as Range) | (ConceptMap.useContext.value as Quantity) | (ConceptMap.useContext.value as Range) | (GraphDefinition.useContext.value as Quantity) | (GraphDefinition.useContext.value as Range) | (ImplementationGu'
      +'ide.useContext.value as Quantity) | (ImplementationGuide.useContext.value as Range) | (MessageDefinition.useContext.value as Quantity) | (MessageDefinition.useContext.value as Range) | (NamingSystem.useContext.value as Quantity) | (NamingSyst'
      +'em.useContext.value as Range) | (OperationDefinition.useContext.value as Qu'+
   'antity) | (OperationDefinition.useContext.value as Range) | (SearchParameter.useContext.value as Quantity) | (SearchParameter.useContext.value as Range) | (StructureDe'
      +'finition.useContext.value as Quantity) | (StructureDefinition.useContext.value as Range) | (StructureMap.useContext.value as Quantity) | (StructureMap.useContext.value as Range) | (TerminologyCapabilities.useContext.value as Quantity) | (Term'
      +'inologyCapabilities.useContext.value as Range) | (ValueSet.useContext.value as Quantity) | (ValueSet.useContext.value as Range)', sxpNormal);
  indexes.add('ImplementationGuide', 'context-type', '): A type of use context assigned to the implementation guide', sptTOKEN, [], 'CapabilityStatement.useContext.code | CodeSystem.useContext.code | CompartmentDefinition.useContext.code | ConceptMap.useContext.code | GraphDefinition.useContext.code | ImplementationGuide.useContext.code | MessageDefinition.useContext.code | Naming'
      +'System.useContext.code | OperationDefinition.useContext.code | SearchParameter.useContext.code | StructureDefinition.useContext.code | StructureMap.useContext.code | TerminologyCapabilities.useContext.code | ValueSet.useContext.code', sxpNormal);
  indexes.add('ImplementationGuide', 'context-type-quantity', '): A use context type and quantity- or range-based value assigned to the implementation guide', sptCOMPOSITE, [], 'CapabilityStatement.useContext | CodeSystem.useContext | CompartmentDefinition.useContext | ConceptMap.useContext | GraphDefinition.useContext | ImplementationGuide.useContext | MessageDefinition.useContext | NamingSystem.useContext | OperationDefini'
      +'tion.useContext | SearchParameter.useContext | StructureDefinition.useContext | StructureMap.useContext | TerminologyCapabilities.useContext | ValueSet.useContext', sxpNormal);
  indexes.add('ImplementationGuide', 'context-type-value', '): A use context type and value assigned to the implementation guide', sptCOMPOSITE, [], 'CapabilityStatement.useContext | CodeSystem.useContext | CompartmentDefinition.useContext | ConceptMap.useContext | GraphDefinition.useContext | ImplementationGuide.useContext | MessageDefinition.useContext | NamingSystem.useContext | OperationDefini'
      +'tion.useContext | SearchParameter.useContext | StructureDefinition.useContext | StructureMap.useContext | TerminologyCapabilities.useContext | ValueSet.useContext', sxpNormal);
  indexes.add('ImplementationGuide', 'date', '): The implementation guide publication date', sptDATE, [], 'CapabilityStatement.date | CodeSystem.date | CompartmentDefinition.date | ConceptMap.date | GraphDefinition.date | ImplementationGuide.date | MessageDefinition.date | NamingSystem.date | OperationDefinition.date | SearchParameter.date | StructureDefi'
      +'nition.date | StructureMap.date | TerminologyCapabilities.date | ValueSet.date', sxpNormal);
  indexes.add('ImplementationGuide', 'depends-on', 'Identity of the IG that this depends on', sptREFERENCE, ['ImplementationGuide'], 'ImplementationGuide.dependsOn.uri', sxpNormal);
  indexes.add('ImplementationGuide', 'description', '): The description of the implementation guide', sptSTRING, [], 'CapabilityStatement.description | CodeSystem.description | CompartmentDefinition.description | ConceptMap.description | GraphDefinition.description | ImplementationGuide.description | MessageDefinition.description | NamingSystem.description | Operati'
      +'onDefinition.description | SearchParameter.description | StructureDefinition.description | StructureMap.description | TerminologyCapabilities.description | ValueSet.description', sxpNormal);
  indexes.add('ImplementationGuide', 'experimental', 'For testing purposes, not real usage', sptTOKEN, [], 'ImplementationGuide.experimental', sxpNormal);
  indexes.add('ImplementationGuide', 'global', 'Profile that all resources must conform to', sptREFERENCE, ['StructureDefinition'], 'ImplementationGuide.global.profile', sxpNormal);
  indexes.add('ImplementationGuide', 'jurisdiction', '): Intended jurisdiction for the implementation guide', sptTOKEN, [], 'CapabilityStatement.jurisdiction | CodeSystem.jurisdiction | ConceptMap.jurisdiction | GraphDefinition.jurisdiction | ImplementationGuide.jurisdiction | MessageDefinition.jurisdiction | NamingSystem.jurisdiction | OperationDefinition.jurisdiction | S'
      +'earchParameter.jurisdiction | StructureDefinition.jurisdiction | StructureMap.jurisdiction | TerminologyCapabilities.jurisdiction | ValueSet.jurisdiction', sxpNormal);
  indexes.add('ImplementationGuide', 'name', '): Computationally friendly name of the implementation guide', sptSTRING, [], 'CapabilityStatement.name | CodeSystem.name | CompartmentDefinition.name | ConceptMap.name | GraphDefinition.name | ImplementationGuide.name | MessageDefinition.name | NamingSystem.name | OperationDefinition.name | SearchParameter.name | StructureDefi'
      +'nition.name | StructureMap.name | TerminologyCapabilities.name | ValueSet.name', sxpNormal);
  indexes.add('ImplementationGuide', 'publisher', '): Name of the publisher of the implementation guide', sptSTRING, [], 'CapabilityStatement.publisher | CodeSystem.publisher | CompartmentDefinition.publisher | ConceptMap.publisher | GraphDefinition.publisher | ImplementationGuide.publisher | MessageDefinition.publisher | NamingSystem.publisher | OperationDefinition.pub'
      +'lisher | SearchParameter.publisher | StructureDefinition.publisher | StructureMap.publisher | TerminologyCapabilities.publisher | ValueSet.publisher', sxpNormal);
  indexes.add('ImplementationGuide', 'resource', 'Location of the resource', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'ImplementationGuide.definition.resource.reference', sxpNormal);
  indexes.add('ImplementationGuide', 'status', '): The current status of the implementation guide', sptTOKEN, [], 'CapabilityStatement.status | CodeSystem.status | CompartmentDefinition.status | ConceptMap.status | GraphDefinition.status | ImplementationGuide.status | MessageDefinition.status | NamingSystem.status | OperationDefinition.status | SearchParameter.st'
      +'atus | StructureDefinition.status | StructureMap.status | TerminologyCapabilities.status | ValueSet.status', sxpNormal);
  indexes.add('ImplementationGuide', 'title', '): The human-friendly name of the implementation guide', sptSTRING, [], 'CapabilityStatement.title | CodeSystem.title | ConceptMap.title | ImplementationGuide.title | MessageDefinition.title | OperationDefinition.title | StructureDefinition.title | StructureMap.title | TerminologyCapabilities.title | ValueSet.title', sxpNormal);
  indexes.add('ImplementationGuide', 'url', '): The uri that identifies the implementation guide', sptURI, [], 'CapabilityStatement.url | CodeSystem.url | CompartmentDefinition.url | ConceptMap.url | GraphDefinition.url | ImplementationGuide.url | MessageDefinition.url | NamingSystem.url | OperationDefinition.url | SearchParameter.url | StructureDefinition.url'
      +' | StructureMap.url | TerminologyCapabilities.url | ValueSet.url', sxpNormal);
  indexes.add('ImplementationGuide', 'version', '): The business version of the implementation guide', sptTOKEN, [], 'CapabilityStatement.version | CodeSystem.version | CompartmentDefinition.version | ConceptMap.version | GraphDefinition.version | ImplementationGuide.version | MessageDefinition.version | NamingSystem.version | OperationDefinition.version | SearchPar'
      +'ameter.version | StructureDefinition.version | StructureMap.version | TerminologyCapabilities.version | ValueSet.version', sxpNormal);
  indexes.add('ImplementationGuide', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_IMPLEMENTATIONGUIDE}

{$IFDEF FHIR_INGREDIENT}
procedure TFHIRIndexBuilderR5.buildIndexesForIngredient(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Ingredient', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('Ingredient', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('Ingredient', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('Ingredient', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('Ingredient', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('Ingredient', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('Ingredient', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('Ingredient', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('Ingredient', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('Ingredient', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('Ingredient', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('Ingredient', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('Ingredient', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('Ingredient', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('Ingredient', 'for', 'The product which this ingredient is a constituent part of', sptREFERENCE, ['AdministrableProductDefinition', 'ManufacturedItemDefinition', 'MedicinalProductDefinition'], 'Ingredient.for', sxpNormal);
  indexes.add('Ingredient', 'function', 'A classification of the ingredient identifying its precise purpose(s) in the drug product. This extends the Ingredient.role to add more detail. Example: Antioxidant, Alkalizing Agent', sptTOKEN, [], 'Ingredient.function', sxpNormal);
  indexes.add('Ingredient', 'identifier', 'An identifier or code by which the ingredient can be referenced', sptTOKEN, [], 'Ingredient.identifier', sxpNormal);
  indexes.add('Ingredient', 'manufacturer', 'The organization that manufactures this ingredient', sptREFERENCE, [], 'Ingredient.manufacturer', sxpNormal);
  indexes.add('Ingredient', 'role', 'A classification of the ingredient identifying its purpose within the product, e.g. active, inactive', sptTOKEN, [], 'Ingredient.role', sxpNormal);
  indexes.add('Ingredient', 'status', 'The status of this ingredient. Enables tracking the life-cycle of the content', sptTOKEN, [], 'Ingredient.status', sxpNormal);
  indexes.add('Ingredient', 'substance', 'Reference to a resource (by instance)', sptREFERENCE, [], 'Ingredient.substance.code.reference', sxpNormal);
  indexes.add('Ingredient', 'substance-code', 'Reference to a concept (by class)', sptTOKEN, [], 'Ingredient.substance.code.concept', sxpNormal);
  indexes.add('Ingredient', 'substance-definition', 'Reference to a resource (by instance)', sptREFERENCE, [], 'Ingredient.substance.code.reference', sxpNormal);
  indexes.add('Ingredient', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_INGREDIENT}

{$IFDEF FHIR_INSURANCEPLAN}
procedure TFHIRIndexBuilderR5.buildIndexesForInsurancePlan(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('InsurancePlan', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('InsurancePlan', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('InsurancePlan', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('InsurancePlan', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('InsurancePlan', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('InsurancePlan', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('InsurancePlan', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('InsurancePlan', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('InsurancePlan', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('InsurancePlan', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('InsurancePlan', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('InsurancePlan', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('InsurancePlan', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('InsurancePlan', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('InsurancePlan', 'address', 'A server defined search that may match any of the string fields in the Address, including line, city, district, state, country, postalCode, and/or text', sptSTRING, [], 'InsurancePlan.contact.address', sxpNormal);
  indexes.add('InsurancePlan', 'address-city', 'A city specified in an address', sptSTRING, [], 'InsurancePlan.contact.address.city', sxpNormal);
  indexes.add('InsurancePlan', 'address-country', 'A country specified in an address', sptSTRING, [], 'InsurancePlan.contact.address.country', sxpNormal);
  indexes.add('InsurancePlan', 'address-postalcode', 'A postal code specified in an address', sptSTRING, [], 'InsurancePlan.contact.address.postalCode', sxpNormal);
  indexes.add('InsurancePlan', 'address-state', 'A state specified in an address', sptSTRING, [], 'InsurancePlan.contact.address.state', sxpNormal);
  indexes.add('InsurancePlan', 'address-use', 'A use code specified in an address', sptTOKEN, [], 'InsurancePlan.contact.address.use', sxpNormal);
  indexes.add('InsurancePlan', 'administered-by', 'Product administrator', sptREFERENCE, ['Organization'], 'InsurancePlan.administeredBy', sxpNormal);
  indexes.add('InsurancePlan', 'endpoint', 'Technical endpoint', sptREFERENCE, ['Endpoint'], 'InsurancePlan.endpoint', sxpNormal);
  indexes.add('InsurancePlan', 'identifier', 'Any identifier for the organization (not the accreditation issuer''s identifier)', sptTOKEN, [], 'InsurancePlan.identifier', sxpNormal);
  indexes.add('InsurancePlan', 'name', 'A portion of the organization''s name or alias', sptSTRING, [], 'InsurancePlan.name | InsurancePlan.alias', sxpNormal);
  indexes.add('InsurancePlan', 'owned-by', 'An organization of which this organization forms a part', sptREFERENCE, ['Organization'], 'InsurancePlan.ownedBy', sxpNormal);
  indexes.add('InsurancePlan', 'phonetic', 'A portion of the organization''s name using some kind of phonetic matching algorithm', sptSTRING, [], 'InsurancePlan.name', sxpNormal);
  indexes.add('InsurancePlan', 'status', 'Is the Organization record active', sptTOKEN, [], 'InsurancePlan.status', sxpNormal);
  indexes.add('InsurancePlan', 'type', 'A code for the type of organization', sptTOKEN, [], 'InsurancePlan.type', sxpNormal);
  indexes.add('InsurancePlan', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_INSURANCEPLAN}

{$IFDEF FHIR_INVENTORYREPORT}
procedure TFHIRIndexBuilderR5.buildIndexesForInventoryReport(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('InventoryReport', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('InventoryReport', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('InventoryReport', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('InventoryReport', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('InventoryReport', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('InventoryReport', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('InventoryReport', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('InventoryReport', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('InventoryReport', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('InventoryReport', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('InventoryReport', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('InventoryReport', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('InventoryReport', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('InventoryReport', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('InventoryReport', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_INVENTORYREPORT}

{$IFDEF FHIR_INVOICE}
procedure TFHIRIndexBuilderR5.buildIndexesForInvoice(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Invoice', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('Invoice', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('Invoice', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('Invoice', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('Invoice', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('Invoice', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('Invoice', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('Invoice', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('Invoice', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('Invoice', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('Invoice', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('Invoice', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('Invoice', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('Invoice', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('Invoice', 'account', 'Account that is being balanced', sptREFERENCE, ['Account'], 'Invoice.account', sxpNormal);
  indexes.add('Invoice', 'date', 'Invoice date / posting date', sptDATE, [], 'Invoice.date', sxpNormal);
  indexes.add('Invoice', 'identifier', 'Business Identifier for item', sptTOKEN, [], 'Invoice.identifier', sxpNormal);
  indexes.add('Invoice', 'issuer', 'Issuing Organization of Invoice', sptREFERENCE, ['Organization'], 'Invoice.issuer', sxpNormal);
  indexes.add('Invoice', 'participant', 'Individual who was involved', sptREFERENCE, ['Practitioner', 'Organization', 'Device', 'Patient', 'PractitionerRole', 'RelatedPerson'], 'Invoice.participant.actor', sxpNormal);
  indexes.add('Invoice', 'participant-role', 'Type of involvement in creation of this Invoice', sptTOKEN, [], 'Invoice.participant.role', sxpNormal);
  indexes.add('Invoice', 'patient', 'Recipient(s) of goods and services', sptREFERENCE, ['Patient'], 'Invoice.subject.where(resolve() is Patient)', sxpNormal);
  indexes.add('Invoice', 'recipient', 'Recipient of this invoice', sptREFERENCE, ['Organization', 'Patient', 'RelatedPerson'], 'Invoice.recipient', sxpNormal);
  indexes.add('Invoice', 'status', 'draft | issued | balanced | cancelled | entered-in-error', sptTOKEN, [], 'Invoice.status', sxpNormal);
  indexes.add('Invoice', 'subject', 'Recipient(s) of goods and services', sptREFERENCE, ['Group', 'Patient'], 'Invoice.subject', sxpNormal);
  indexes.add('Invoice', 'totalgross', 'Gross total of this Invoice', sptQUANTITY, [], 'Invoice.totalGross', sxpNormal);
  indexes.add('Invoice', 'totalnet', 'Net total of this Invoice', sptQUANTITY, [], 'Invoice.totalNet', sxpNormal);
  indexes.add('Invoice', 'type', 'Type of Invoice', sptTOKEN, [], 'Invoice.type', sxpNormal);
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
  indexes.add('Library', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('Library', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('Library', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('Library', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('Library', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('Library', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('Library', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('Library', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('Library', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('Library', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('Library', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('Library', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('Library', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('Library', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('Library', 'composed-of', 'What resource is being referenced', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'Library.relatedArtifact.where(type=''composed-of'').resource', sxpNormal);
  indexes.add('Library', 'content-type', 'The type of content in the library (e.g. text/cql)', sptTOKEN, [], 'Library.content.contentType', sxpNormal);
  indexes.add('Library', 'context', 'A use context assigned to the library', sptTOKEN, [], '(Library.useContext.value as CodeableConcept)', sxpNormal);
  indexes.add('Library', 'context-quantity', 'A quantity- or range-valued use context assigned to the library', sptQUANTITY, [], '(Library.useContext.value as Quantity) | (Library.useContext.value as Range)', sxpNormal);
  indexes.add('Library', 'context-type', 'A type of use context assigned to the library', sptTOKEN, [], 'Library.useContext.code', sxpNormal);
  indexes.add('Library', 'context-type-quantity', 'A use context type and quantity- or range-based value assigned to the library', sptCOMPOSITE, [], 'Library.useContext', sxpNormal);
  indexes.add('Library', 'context-type-value', 'A use context type and value assigned to the library', sptCOMPOSITE, [], 'Library.useContext', sxpNormal);
  indexes.add('Library', 'date', 'The library publication date', sptDATE, [], 'Library.date', sxpNormal);
  indexes.add('Library', 'depends-on', 'What resource is being referenced', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'Library.relatedArtifact.where(type=''depends-on'').resource', sxpNormal);
  indexes.add('Library', 'derived-from', 'What resource is being referenced', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'Library.relatedArtifact.where(type=''derived-from'').resource', sxpNormal);
  indexes.add('Library', 'description', 'The description of the library', sptSTRING, [], 'Library.description', sxpNormal);
  indexes.add('Library', 'effective', 'The time during which the library is intended to be in use', sptDATE, [], 'Library.effectivePeriod', sxpNormal);
  indexes.add('Library', 'identifier', 'External identifier for the library', sptTOKEN, [], 'Library.identifier', sxpNormal);
  indexes.add('Library', 'jurisdiction', 'Intended jurisdiction for the library', sptTOKEN, [], 'Library.jurisdiction', sxpNormal);
  indexes.add('Library', 'name', 'Computationally friendly name of the library', sptSTRING, [], 'Library.name', sxpNormal);
  indexes.add('Library', 'predecessor', 'What resource is being referenced', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'Library.relatedArtifact.where(type=''predecessor'').resource', sxpNormal);
  indexes.add('Library', 'publisher', 'Name of the publisher of the library', sptSTRING, [], 'Library.publisher', sxpNormal);
  indexes.add('Library', 'status', 'The current status of the library', sptTOKEN, [], 'Library.status', sxpNormal);
  indexes.add('Library', 'successor', 'What resource is being referenced', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'Library.relatedArtifact.where(type=''successor'').resource', sxpNormal);
  indexes.add('Library', 'title', 'The human-friendly name of the library', sptSTRING, [], 'Library.title', sxpNormal);
  indexes.add('Library', 'topic', 'Topics associated with the module', sptTOKEN, [], 'Library.topic', sxpNormal);
  indexes.add('Library', 'type', 'The type of the library (e.g. logic-library, model-definition, asset-collection, module-definition)', sptTOKEN, [], 'Library.type', sxpNormal);
  indexes.add('Library', 'url', 'The uri that identifies the library', sptURI, [], 'Library.url', sxpNormal);
  indexes.add('Library', 'version', 'The business version of the library', sptTOKEN, [], 'Library.version', sxpNormal);
  indexes.add('Library', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_LIBRARY}

{$IFDEF FHIR_LINKAGE}
procedure TFHIRIndexBuilderR5.buildIndexesForLinkage(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Linkage', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('Linkage', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('Linkage', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('Linkage', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('Linkage', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('Linkage', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('Linkage', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('Linkage', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('Linkage', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('Linkage', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('Linkage', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('Linkage', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('Linkage', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('Linkage', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('Linkage', 'author', 'Author of the Linkage', sptREFERENCE, ['Practitioner', 'Organization', 'PractitionerRole'], 'Linkage.author', sxpNormal);
  indexes.add('Linkage', 'item', 'Matches on any item in the Linkage', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'Linkage.item.resource', sxpNormal);
  indexes.add('Linkage', 'source', 'Matches on any item in the Linkage with a type of ''source''', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'Linkage.item.resource', sxpNormal);
  indexes.add('Linkage', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Practitioner', 'Linkage', ['author']);
end;
{$ENDIF FHIR_LINKAGE}

{$IFDEF FHIR_LIST}
procedure TFHIRIndexBuilderR5.buildIndexesForList(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('List', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('List', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('List', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('List', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('List', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('List', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('List', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('List', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('List', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('List', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('List', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('List', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('List', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('List', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('List', 'code', '): What the purpose of this list is', sptTOKEN, [], 'AllergyIntolerance.code | AllergyIntolerance.reaction.substance | Condition.code | DeviceRequest.code.concept | DiagnosticReport.code | FamilyMemberHistory.condition.code | List.code | Medication.code | MedicationAdministration.medication.concept | M'
      +'edicationDispense.medication.concept | MedicationRequest.medication.concept | MedicationUsage.medication.concept | Observation.code | Procedure.code', sxpNormal);
  indexes.add('List', 'date', '): When the list was prepared', sptDATE, [], 'AllergyIntolerance.recordedDate | CarePlan.period | ClinicalImpression.date | Composition.date | Consent.dateTime | DiagnosticReport.effective | Encounter.actualPeriod | EpisodeOfCare.period | FamilyMemberHistory.date | Flag.period | (Immunization.oc'
      +'currence as dateTime) | List.date | Observation.effective | Procedure.occurrence | (RiskAssessment.occurrence as dateTime) | SupplyRequest.authoredOn', sxpNormal);
  indexes.add('List', 'empty-reason', 'Why list is empty', sptTOKEN, [], 'List.emptyReason', sxpNormal);
  indexes.add('List', 'encounter', '): Context in which list created', sptREFERENCE, ['Encounter'], 'Composition.encounter | DeviceRequest.encounter | DiagnosticReport.encounter | Flag.encounter | List.encounter | NutritionOrder.encounter | Observation.encounter | Procedure.encounter | RiskAssessment.encounter | ServiceRequest.encounter | VisionPres'
      +'cription.encounter', sxpNormal);
  indexes.add('List', 'identifier', '): Business identifier', sptTOKEN, [], 'AllergyIntolerance.identifier | CarePlan.identifier | CareTeam.identifier | Composition.identifier | Condition.identifier | Consent.identifier | DetectedIssue.identifier | DeviceRequest.identifier | DiagnosticReport.identifier | DocumentManifest.mast'
      +'erIdentifier | DocumentManifest.identifier | DocumentReference.identifier | Encounter.identifier | EpisodeOfCare.identifier | FamilyMemberHistory.identifier | Goal.identifier | ImagingStudy.identifier | Immunization.identifier | List.identifi'
      +'er | MedicationAdministration.identifier | MedicationDispense.identifier | MedicationRequest.identifier | MedicationUsage.identifier | NutritionOrder.identifier | Observation.identifier | Procedure.identifier | RiskAssessment.identifier | Ser'
      +'viceRequest.identifier | SupplyDelivery.identifier | SupplyRequest.identifier | VisionPrescription.identifier', sxpNormal);
  indexes.add('List', 'item', 'Actual entry', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'List.entry.item', sxpNormal);
  indexes.add('List', 'notes', 'The annotation  - text content (as markdown)', sptSTRING, [], 'List.note.text', sxpNormal);
  indexes.add('List', 'patient', '): If all resources have the same subject', sptREFERENCE, ['Patient', 'Practitioner', 'Group', 'Organization', 'BiologicallyDerivedProduct', 'NutritionProduct', 'Device', 'Medication', 'Procedure', 'Substance', 'Location'], 'AllergyIntolerance.patient | CarePlan.subject.where(resolve() is Patient) | CareTeam.subject.where(resolve() is Patient) | ClinicalImpression.subject.where(resolve() is Patient) | Composition.subject.where(resolve() is Patient) | Condition.subject.wh'
      +'ere(resolve() is Patient) | Consent.subject.where(resolve() is Patient) | DetectedIssue.subject | DeviceRequest.subject.where(resolve() is Patient) | DeviceUsage.patient | DiagnosticReport.subject.where(resolve() is Patient) | DocumentManifes'
      +'t.subject.where(resolve() is Patient) | DocumentReference.subject.where(resolve() is Patient) | Encounter.subject.where(resolve() is Patient) | EpisodeOfCare.patient | FamilyMemberHistory.patient | Flag.subject.where(resolve() '+
   'is Patient) | G'
      +'oal.subject.where(resolve() is Patient) | ImagingStudy.subject.where(resolve() is Patient) | Immunization.patient | List.subject.where(resolve() is Patient) | MedicationAdministration.subject.where(resolve() is Patient) | MedicationDispense.s'
      +'ubject.where(resolve() is Patient) | MedicationRequest.subject.where(resolve() is Patient) | MedicationUsage.subject.where(resolve() is Patient) | NutritionOrder.subject.where(resolve() is Patient) | Observation.subject.where(resolve() is Pat'
      +'ient) | Procedure.subject.where(resolve() is Patient) | RiskAssessment.subject.where(resolve() is Patient) | ServiceRequest.subject.where(resolve() is Patient) | SupplyDelivery.patient | VisionPrescription.patient', sxpNormal);
  indexes.add('List', 'source', 'Who and/or what defined the list contents (aka Author)', sptREFERENCE, ['Practitioner', 'Organization', 'CareTeam', 'Device', 'Patient', 'PractitionerRole', 'RelatedPerson'], 'List.source', sxpNormal);
  indexes.add('List', 'status', 'current | retired | entered-in-error', sptTOKEN, [], 'List.status', sxpNormal);
  indexes.add('List', 'subject', 'If all resources have the same subject', sptREFERENCE, ['Practitioner', 'Group', 'Organization', 'Device', 'Patient', 'Location'], 'List.subject', sxpNormal);
  indexes.add('List', 'title', 'Descriptive name for the list', sptSTRING, [], 'List.title', sxpNormal);
  indexes.add('List', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Device', 'List', ['subject', 'source']);
  compartments.register('Patient', 'List', ['subject', 'source']);
  compartments.register('Practitioner', 'List', ['source']);
end;
{$ENDIF FHIR_LIST}

{$IFDEF FHIR_LOCATION}
procedure TFHIRIndexBuilderR5.buildIndexesForLocation(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Location', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('Location', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('Location', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('Location', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('Location', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('Location', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('Location', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('Location', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('Location', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('Location', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('Location', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('Location', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('Location', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('Location', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('Location', 'address', 'A (part of the) address of the location', sptSTRING, [], 'Location.address', sxpNormal);
  indexes.add('Location', 'address-city', 'A city specified in an address', sptSTRING, [], 'Location.address.city', sxpNormal);
  indexes.add('Location', 'address-country', 'A country specified in an address', sptSTRING, [], 'Location.address.country', sxpNormal);
  indexes.add('Location', 'address-postalcode', 'A postal code specified in an address', sptSTRING, [], 'Location.address.postalCode', sxpNormal);
  indexes.add('Location', 'address-state', 'A state specified in an address', sptSTRING, [], 'Location.address.state', sxpNormal);
  indexes.add('Location', 'address-use', 'A use code specified in an address', sptTOKEN, [], 'Location.address.use', sxpNormal);
  indexes.add('Location', 'characteristic', 'One of the Location''s characteristics', sptTOKEN, [], 'Location.characteristic', sxpNormal);
  indexes.add('Location', 'contains', 'Select locations that contain the specified co-ordinates', sptNULL, [], 'Location.extension(''http://hl7.org/fhir/StructureDefinition/location-boundary-geojson'').value', sxpNormal);
  indexes.add('Location', 'endpoint', 'Technical endpoints providing access to services operated for the location', sptREFERENCE, ['Endpoint'], 'Location.endpoint', sxpNormal);
  indexes.add('Location', 'identifier', 'An identifier for the location', sptTOKEN, [], 'Location.identifier', sxpNormal);
  indexes.add('Location', 'name', 'A portion of the location''s name or alias', sptSTRING, [], 'Location.name | Location.alias', sxpNormal);
  indexes.add('Location', 'near', 'Search for locations where the location.position is near to, or within a specified distance of, the provided coordinates expressed as [latitude]|[longitude]|[distance]|[units] (using the WGS84 datum, see notes).  Servers which support the near parame'
      +'ter SHALL support the unit string ''km'' for kilometers and SHOULD support ''[mi_us]'' for miles, support for other units is optional. If the units are omitted, then kms should be assumed. If the distance is omitted, then the server can use i'
      +'ts own discretion as to what distances should be considered near (and units are irrelevant).  If the server is unable to understand the units (and does support the near search parameter), it MIGHT return an OperationOutcome and fail the searc'
      +'h with a http status 400 BadRequest. If the server does not support the near parameter, the parameter MIGHT report the unused parameter in a bundled OperationOutcome and still perform the search ignoring the near parameter.  Note: The algorit'
      +'hm to determine the distance is not defined by the specification, and systems might have different engines that calculate things differently. They could consider geographic point to point, or path via road, or including current traffic condit'
      +'ions, or just simple neighboring postcodes/localities if that''s all it had access to.', sptNULL, [], 'Location.position', sxpNormal);
  indexes.add('Location', 'operational-status', 'Searches for locations (typically bed/room) that have an operational status (e.g. contaminated, housekeeping)', sptTOKEN, [], 'Location.operationalStatus', sxpNormal);
  indexes.add('Location', 'organization', 'Searches for locations that are managed by the provided organization', sptREFERENCE, ['Organization'], 'Location.managingOrganization', sxpNormal);
  indexes.add('Location', 'partof', 'A location of which this location is a part', sptREFERENCE, ['Location'], 'Location.partOf', sxpNormal);
  indexes.add('Location', 'status', 'Searches for locations with a specific kind of status', sptTOKEN, [], 'Location.status', sxpNormal);
  indexes.add('Location', 'type', 'A code for the type of location', sptTOKEN, [], 'Location.type', sxpNormal);
  indexes.add('Location', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_LOCATION}

{$IFDEF FHIR_MANUFACTUREDITEMDEFINITION}
procedure TFHIRIndexBuilderR5.buildIndexesForManufacturedItemDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ManufacturedItemDefinition', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('ManufacturedItemDefinition', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('ManufacturedItemDefinition', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('ManufacturedItemDefinition', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('ManufacturedItemDefinition', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('ManufacturedItemDefinition', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('ManufacturedItemDefinition', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('ManufacturedItemDefinition', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('ManufacturedItemDefinition', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('ManufacturedItemDefinition', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('ManufacturedItemDefinition', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('ManufacturedItemDefinition', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('ManufacturedItemDefinition', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('ManufacturedItemDefinition', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('ManufacturedItemDefinition', 'dose-form', 'Dose form as manufactured and before any transformation into the pharmaceutical product', sptTOKEN, [], 'ManufacturedItemDefinition.manufacturedDoseForm', sxpNormal);
  indexes.add('ManufacturedItemDefinition', 'identifier', 'Unique identifier', sptTOKEN, [], 'ManufacturedItemDefinition.identifier', sxpNormal);
  indexes.add('ManufacturedItemDefinition', 'ingredient', 'An ingredient of this item', sptTOKEN, [], 'ManufacturedItemDefinition.ingredient', sxpNormal);
  indexes.add('ManufacturedItemDefinition', 'name', 'A descriptive name applied to this item', sptTOKEN, [], 'ManufacturedItemDefinition.name', sxpNormal);
  indexes.add('ManufacturedItemDefinition', 'status', 'The status of this item. Enables tracking the life-cycle of the content.', sptTOKEN, [], 'ManufacturedItemDefinition.status', sxpNormal);
  indexes.add('ManufacturedItemDefinition', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_MANUFACTUREDITEMDEFINITION}

{$IFDEF FHIR_MEASURE}
procedure TFHIRIndexBuilderR5.buildIndexesForMeasure(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Measure', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('Measure', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('Measure', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('Measure', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('Measure', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('Measure', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('Measure', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('Measure', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('Measure', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('Measure', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('Measure', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('Measure', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('Measure', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('Measure', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('Measure', 'composed-of', 'What resource is being referenced', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'Measure.relatedArtifact.where(type=''composed-of'').resource', sxpNormal);
  indexes.add('Measure', 'context', 'A use context assigned to the measure', sptTOKEN, [], '(Measure.useContext.value as CodeableConcept)', sxpNormal);
  indexes.add('Measure', 'context-quantity', 'A quantity- or range-valued use context assigned to the measure', sptQUANTITY, [], '(Measure.useContext.value as Quantity) | (Measure.useContext.value as Range)', sxpNormal);
  indexes.add('Measure', 'context-type', 'A type of use context assigned to the measure', sptTOKEN, [], 'Measure.useContext.code', sxpNormal);
  indexes.add('Measure', 'context-type-quantity', 'A use context type and quantity- or range-based value assigned to the measure', sptCOMPOSITE, [], 'Measure.useContext', sxpNormal);
  indexes.add('Measure', 'context-type-value', 'A use context type and value assigned to the measure', sptCOMPOSITE, [], 'Measure.useContext', sxpNormal);
  indexes.add('Measure', 'date', 'The measure publication date', sptDATE, [], 'Measure.date', sxpNormal);
  indexes.add('Measure', 'depends-on', 'What resource is being referenced', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'Measure.relatedArtifact.where(type=''depends-on'').resource | Measure.library', sxpNormal);
  indexes.add('Measure', 'derived-from', 'What resource is being referenced', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'Measure.relatedArtifact.where(type=''derived-from'').resource', sxpNormal);
  indexes.add('Measure', 'description', 'The description of the measure', sptSTRING, [], 'Measure.description', sxpNormal);
  indexes.add('Measure', 'effective', 'The time during which the measure is intended to be in use', sptDATE, [], 'Measure.effectivePeriod', sxpNormal);
  indexes.add('Measure', 'identifier', 'External identifier for the measure', sptTOKEN, [], 'Measure.identifier', sxpNormal);
  indexes.add('Measure', 'jurisdiction', 'Intended jurisdiction for the measure', sptTOKEN, [], 'Measure.jurisdiction', sxpNormal);
  indexes.add('Measure', 'name', 'Computationally friendly name of the measure', sptSTRING, [], 'Measure.name', sxpNormal);
  indexes.add('Measure', 'predecessor', 'What resource is being referenced', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'Measure.relatedArtifact.where(type=''predecessor'').resource', sxpNormal);
  indexes.add('Measure', 'publisher', 'Name of the publisher of the measure', sptSTRING, [], 'Measure.publisher', sxpNormal);
  indexes.add('Measure', 'status', 'The current status of the measure', sptTOKEN, [], 'Measure.status', sxpNormal);
  indexes.add('Measure', 'successor', 'What resource is being referenced', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'Measure.relatedArtifact.where(type=''successor'').resource', sxpNormal);
  indexes.add('Measure', 'title', 'The human-friendly name of the measure', sptSTRING, [], 'Measure.title', sxpNormal);
  indexes.add('Measure', 'topic', 'Topics associated with the measure', sptTOKEN, [], 'Measure.topic', sxpNormal);
  indexes.add('Measure', 'url', 'The uri that identifies the measure', sptURI, [], 'Measure.url', sxpNormal);
  indexes.add('Measure', 'version', 'The business version of the measure', sptTOKEN, [], 'Measure.version', sxpNormal);
  indexes.add('Measure', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_MEASURE}

{$IFDEF FHIR_MEASUREREPORT}
procedure TFHIRIndexBuilderR5.buildIndexesForMeasureReport(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('MeasureReport', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('MeasureReport', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('MeasureReport', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('MeasureReport', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('MeasureReport', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('MeasureReport', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('MeasureReport', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('MeasureReport', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('MeasureReport', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('MeasureReport', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('MeasureReport', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('MeasureReport', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('MeasureReport', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('MeasureReport', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('MeasureReport', 'date', 'The date of the measure report', sptDATE, [], 'MeasureReport.date', sxpNormal);
  indexes.add('MeasureReport', 'evaluated-resource', 'An evaluated resource referenced by the measure report', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'MeasureReport.evaluatedResource', sxpNormal);
  indexes.add('MeasureReport', 'identifier', 'External identifier of the measure report to be returned', sptTOKEN, [], 'MeasureReport.identifier', sxpNormal);
  indexes.add('MeasureReport', 'location', 'The location to return measure report results for', sptREFERENCE, ['Location'], 'MeasureReport.location', sxpNormal);
  indexes.add('MeasureReport', 'measure', 'The measure to return measure report results for', sptREFERENCE, ['Measure'], 'MeasureReport.measure', sxpNormal);
  indexes.add('MeasureReport', 'patient', 'The identity of a patient to search for individual measure report results for', sptREFERENCE, ['Patient'], 'MeasureReport.subject.where(resolve() is Patient)', sxpNormal);
  indexes.add('MeasureReport', 'period', 'The period of the measure report', sptDATE, [], 'MeasureReport.period', sxpNormal);
  indexes.add('MeasureReport', 'reporter', 'The reporter to return measure report results for', sptREFERENCE, ['Practitioner', 'Group', 'Organization', 'PractitionerRole'], 'MeasureReport.reporter', sxpNormal);
  indexes.add('MeasureReport', 'status', 'The status of the measure report', sptTOKEN, [], 'MeasureReport.status', sxpNormal);
  indexes.add('MeasureReport', 'subject', 'The identity of a subject to search for individual measure report results for', sptREFERENCE, ['Practitioner', 'Group', 'Organization', 'CareTeam', 'Device', 'Patient', 'HealthcareService', 'PractitionerRole', 'RelatedPerson', 'Location'], 'MeasureReport.subject', sxpNormal);
  indexes.add('MeasureReport', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Patient', 'MeasureReport', ['patient']);
end;
{$ENDIF FHIR_MEASUREREPORT}

{$IFDEF FHIR_MEDICATION}
procedure TFHIRIndexBuilderR5.buildIndexesForMedication(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Medication', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('Medication', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('Medication', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('Medication', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('Medication', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('Medication', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('Medication', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('Medication', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('Medication', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('Medication', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('Medication', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('Medication', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('Medication', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('Medication', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('Medication', 'code', '): Returns medications for a specific code', sptTOKEN, [], 'AllergyIntolerance.code | AllergyIntolerance.reaction.substance | Condition.code | DeviceRequest.code.concept | DiagnosticReport.code | FamilyMemberHistory.condition.code | List.code | Medication.code | MedicationAdministration.medication.concept | M'
      +'edicationDispense.medication.concept | MedicationRequest.medication.concept | MedicationUsage.medication.concept | Observation.code | Procedure.code', sxpNormal);
  indexes.add('Medication', 'expiration-date', 'Returns medications in a batch with this expiration date', sptDATE, [], 'Medication.batch.expirationDate', sxpNormal);
  indexes.add('Medication', 'form', 'Returns medications for a specific dose form', sptTOKEN, [], '', sxpNormal);
  indexes.add('Medication', 'identifier', 'Returns medications with this external identifier', sptTOKEN, [], 'Medication.identifier', sxpNormal);
  indexes.add('Medication', 'ingredient', 'Returns medications for this ingredient reference', sptREFERENCE, [], 'Medication.ingredient.item.reference', sxpNormal);
  indexes.add('Medication', 'ingredient-code', 'Returns medications for this ingredient code', sptTOKEN, [], 'Medication.ingredient.item.concept', sxpNormal);
  indexes.add('Medication', 'lot-number', 'Returns medications in a batch with this lot number', sptTOKEN, [], 'Medication.batch.lotNumber', sxpNormal);
  indexes.add('Medication', 'marketingauthorizationholder', 'Returns medications made or sold for this marketing authorization holder', sptREFERENCE, ['Organization'], 'Medication.marketingAuthorizationHolder', sxpNormal);
  indexes.add('Medication', 'status', 'Returns medications for this status', sptTOKEN, [], 'Medication.status', sxpNormal);
  indexes.add('Medication', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_MEDICATION}

{$IFDEF FHIR_MEDICATIONADMINISTRATION}
procedure TFHIRIndexBuilderR5.buildIndexesForMedicationAdministration(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('MedicationAdministration', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('MedicationAdministration', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('MedicationAdministration', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('MedicationAdministration', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('MedicationAdministration', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('MedicationAdministration', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('MedicationAdministration', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('MedicationAdministration', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('MedicationAdministration', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('MedicationAdministration', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('MedicationAdministration', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('MedicationAdministration', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('MedicationAdministration', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('MedicationAdministration', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('MedicationAdministration', 'code', '): Return administrations of this medication code', sptTOKEN, [], 'AllergyIntolerance.code | AllergyIntolerance.reaction.substance | Condition.code | DeviceRequest.code.concept | DiagnosticReport.code | FamilyMemberHistory.condition.code | List.code | Medication.code | MedicationAdministration.medication.concept | M'
      +'edicationDispense.medication.concept | MedicationRequest.medication.concept | MedicationUsage.medication.concept | Observation.code | Procedure.code', sxpNormal);
  indexes.add('MedicationAdministration', 'date', '): Date administration happened (or did not happen)', sptDATE, [], 'MedicationAdministration.occurence', sxpNormal);
  indexes.add('MedicationAdministration', 'device', 'Return administrations with this administration device identity', sptREFERENCE, ['Device'], 'MedicationAdministration.device', sxpNormal);
  indexes.add('MedicationAdministration', 'encounter', '): Return administrations that share this encounter', sptREFERENCE, ['Encounter'], 'MedicationAdministration.encounter | MedicationRequest.encounter', sxpNormal);
  indexes.add('MedicationAdministration', 'identifier', '): Return administrations with this external identifier', sptTOKEN, [], 'AllergyIntolerance.identifier | CarePlan.identifier | CareTeam.identifier | Composition.identifier | Condition.identifier | Consent.identifier | DetectedIssue.identifier | DeviceRequest.identifier | DiagnosticReport.identifier | DocumentManifest.mast'
      +'erIdentifier | DocumentManifest.identifier | DocumentReference.identifier | Encounter.identifier | EpisodeOfCare.identifier | FamilyMemberHistory.identifier | Goal.identifier | ImagingStudy.identifier | Immunization.identifier | List.identifi'
      +'er | MedicationAdministration.identifier | MedicationDispense.identifier | MedicationRequest.identifier | MedicationUsage.identifier | NutritionOrder.identifier | Observation.identifier | Procedure.identifier | RiskAssessment.identifier | Ser'
      +'viceRequest.identifier | SupplyDelivery.identifier | SupplyRequest.identifier | VisionPrescription.iden'+
   'tifier', sxpNormal);
  indexes.add('MedicationAdministration', 'medication', '): Return administrations of this medication reference', sptREFERENCE, [], 'MedicationAdministration.medication.reference | MedicationDispense.medication.reference | MedicationRequest.medication.reference | MedicationUsage.medication.reference', sxpNormal);
  indexes.add('MedicationAdministration', 'patient', '): The identity of a patient to list administrations  for', sptREFERENCE, ['Patient', 'Practitioner', 'Group', 'Organization', 'BiologicallyDerivedProduct', 'NutritionProduct', 'Device', 'Medication', 'Procedure', 'Substance', 'Location'], 'AllergyIntolerance.patient | CarePlan.subject.where(resolve() is Patient) | CareTeam.subject.where(resolve() is Patient) | ClinicalImpression.subject.where(resolve() is Patient) | Composition.subject.where(resolve() is Patient) | Condition.subject.wh'
      +'ere(resolve() is Patient) | Consent.subject.where(resolve() is Patient) | DetectedIssue.subject | DeviceRequest.subject.where(resolve() is Patient) | DeviceUsage.patient | DiagnosticReport.subject.where(resolve() is Patient) | DocumentManifes'
      +'t.subject.where(resolve() is Patient) | DocumentReference.subject.where(resolve() is Patient) | Encounter.subject.where(resolve() is Patient) | EpisodeOfCare.patient | FamilyMemberHistory.pat'+
   'ient | Flag.subject.where(resolve() is Patient) | G'
      +'oal.subject.where(resolve() is Patient) | ImagingStudy.subject.where(resolve() is Patient) | Immunization.patient | List.subject.where(resolve() is Patient) | MedicationAdministration.subject.where(resolve() is Patient) | MedicationDispense.s'
      +'ubject.where(resolve() is Patient) | MedicationRequest.subject.where(resolve() is Patient) | MedicationUsage.subject.where(resolve() is Patient) | NutritionOrder.subject.where(resolve() is Patient) | Observation.subject.where(resolve() is Pat'
      +'ient) | Procedure.subject.where(resolve() is Patient) | RiskAssessment.subject.where(resolve() is Patient) | ServiceRequest.subject.where(resolve() is Patient) | SupplyDelivery.patient | VisionPrescription.patient', sxpNormal);
  indexes.add('MedicationAdministration', 'performer', 'The identity of the individual who administered the medication', sptREFERENCE, ['Practitioner', 'Device', 'Patient', 'PractitionerRole', 'RelatedPerson'], 'MedicationAdministration.performer.actor', sxpNormal);
  indexes.add('MedicationAdministration', 'reason-given', 'Reference to a resource (by instance)', sptREFERENCE, [], 'MedicationAdministration.reason.reference', sxpNormal);
  indexes.add('MedicationAdministration', 'reason-given-code', 'Reasons for administering the medication', sptTOKEN, [], 'MedicationAdministration.reason.concept', sxpNormal);
  indexes.add('MedicationAdministration', 'reason-not-given', 'Reasons for not administering the medication', sptTOKEN, [], 'MedicationAdministration.statusReason', sxpNormal);
  indexes.add('MedicationAdministration', 'request', 'The identity of a request to list administrations from', sptREFERENCE, ['MedicationRequest'], 'MedicationAdministration.request', sxpNormal);
  indexes.add('MedicationAdministration', 'status', '): MedicationAdministration event status (for example one of active/paused/completed/nullified)', sptTOKEN, [], 'MedicationAdministration.status | MedicationDispense.status | MedicationRequest.status | MedicationUsage.status', sxpNormal);
  indexes.add('MedicationAdministration', 'subject', 'The identity of the individual or group to list administrations for', sptREFERENCE, ['Group', 'Patient'], 'MedicationAdministration.subject', sxpNormal);
  indexes.add('MedicationAdministration', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Device', 'MedicationAdministration', ['device']);
  compartments.register('Encounter', 'MedicationAdministration', ['encounter']);
  compartments.register('Patient', 'MedicationAdministration', ['patient', 'performer', 'subject']);
  compartments.register('Practitioner', 'MedicationAdministration', ['performer']);
  compartments.register('RelatedPerson', 'MedicationAdministration', ['performer']);
end;
{$ENDIF FHIR_MEDICATIONADMINISTRATION}

{$IFDEF FHIR_MEDICATIONDISPENSE}
procedure TFHIRIndexBuilderR5.buildIndexesForMedicationDispense(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('MedicationDispense', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('MedicationDispense', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('MedicationDispense', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('MedicationDispense', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('MedicationDispense', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('MedicationDispense', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('MedicationDispense', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('MedicationDispense', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('MedicationDispense', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('MedicationDispense', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('MedicationDispense', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('MedicationDispense', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('MedicationDispense', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('MedicationDispense', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('MedicationDispense', 'code', '): Returns dispenses of this medicine code', sptTOKEN, [], 'AllergyIntolerance.code | AllergyIntolerance.reaction.substance | Condition.code | DeviceRequest.code.concept | DiagnosticReport.code | FamilyMemberHistory.condition.code | List.code | Medication.code | MedicationAdministration.medication.concept | M'
      +'edicationDispense.medication.concept | MedicationRequest.medication.concept | MedicationUsage.medication.concept | Observation.code | Procedure.code', sxpNormal);
  indexes.add('MedicationDispense', 'destination', 'Returns dispenses that should be sent to a specific destination', sptREFERENCE, ['Location'], 'MedicationDispense.destination', sxpNormal);
  indexes.add('MedicationDispense', 'encounter', 'Returns dispenses with a specific encounter', sptREFERENCE, ['Encounter'], 'MedicationDispense.encounter', sxpNormal);
  indexes.add('MedicationDispense', 'identifier', '): Returns dispenses with this external identifier', sptTOKEN, [], 'AllergyIntolerance.identifier | CarePlan.identifier | CareTeam.identifier | Composition.identifier | Condition.identifier | Consent.identifier | DetectedIssue.identifier | DeviceRequest.identifier | DiagnosticReport.identifier | DocumentManifest.mast'
      +'erIdentifier | DocumentManifest.identifier | DocumentReference.identifier | Encounter.identifier | EpisodeOfCare.identifier | FamilyMemberHistory.identifier | Goal.identifier | ImagingStudy.identifier | Immunization.identifier | List.identifi'
      +'er | MedicationAdministration.identifier | MedicationDispense.identifier | MedicationRequest.identifier | MedicationUsage.identifier | NutritionOrder.identifier | Observation.identifier | Procedure.identifier | RiskAssessment.identifier | Ser'
      +'viceRequest.identifier | SupplyDelivery.identifier | SupplyRequest.identifier | VisionPrescription.identifier', sxpNormal);
  indexes.add('MedicationDispense', 'location', 'Returns dispense for a given location', sptREFERENCE, ['Location'], 'MedicationDispense.location', sxpNormal);
  indexes.add('MedicationDispense', 'medication', '): Returns dispenses of this medicine resource', sptREFERENCE, [], 'MedicationAdministration.medication.reference | MedicationDispense.medication.reference | MedicationRequest.medication.reference | MedicationUsage.medication.reference', sxpNormal);
  indexes.add('MedicationDispense', 'patient', '): The identity of a patient to list dispenses  for', sptREFERENCE, ['Patient', 'Practitioner', 'Group', 'Organization', 'BiologicallyDerivedProduct', 'NutritionProduct', 'Device', 'Medication', 'Procedure', 'Substance', 'Location'], 'AllergyIntolerance.patient | CarePlan.subject.where(resolve() is Patient) | CareTeam.subject.where(resolve() is Patient) | ClinicalImpression.subject.where(resolve() is Patient) | Composition.subject.where(resolve() is Patient) | Condition.subject.wh'
      +'ere(resolve() is Patient) | Consent.subject.where(resolve() is Patient) | DetectedIssue.subject | DeviceRequest.subject.where(resolve() is Patient) | DeviceUsage.patient | DiagnosticReport.subject.where(resolve() is Patient) | DocumentManifes'
      +'t.subject.where(resolve() is Patient) | DocumentReference.subject.where(resolve() is Patient) | Encounter.subject.where(resolve() is Patient) | EpisodeOfCare.patient | FamilyMemberHistory.patient | Flag.'+
   'subject.where(resolve() is Patient) | G'
      +'oal.subject.where(resolve() is Patient) | ImagingStudy.subject.where(resolve() is Patient) | Immunization.patient | List.subject.where(resolve() is Patient) | MedicationAdministration.subject.where(resolve() is Patient) | MedicationDispense.s'
      +'ubject.where(resolve() is Patient) | MedicationRequest.subject.where(resolve() is Patient) | MedicationUsage.subject.where(resolve() is Patient) | NutritionOrder.subject.where(resolve() is Patient) | Observation.subject.where(resolve() is Pat'
      +'ient) | Procedure.subject.where(resolve() is Patient) | RiskAssessment.subject.where(resolve() is Patient) | ServiceRequest.subject.where(resolve() is Patient) | SupplyDelivery.patient | VisionPrescription.patient', sxpNormal);
  indexes.add('MedicationDispense', 'performer', 'Returns dispenses performed by a specific individual', sptREFERENCE, ['Practitioner', 'Organization', 'CareTeam', 'Device', 'Patient', 'PractitionerRole', 'RelatedPerson'], 'MedicationDispense.performer.actor', sxpNormal);
  indexes.add('MedicationDispense', 'prescription', '): The identity of a prescription to list dispenses from', sptREFERENCE, ['MedicationRequest'], 'MedicationDispense.authorizingPrescription', sxpNormal);
  indexes.add('MedicationDispense', 'receiver', 'The identity of a receiver to list dispenses for', sptREFERENCE, ['Practitioner', 'Patient', 'PractitionerRole', 'RelatedPerson', 'Location'], 'MedicationDispense.receiver', sxpNormal);
  indexes.add('MedicationDispense', 'recorded', 'Returns dispenses where dispensing activity began on this date', sptDATE, [], 'MedicationDispense.recorded', sxpNormal);
  indexes.add('MedicationDispense', 'responsibleparty', 'Returns dispenses with the specified responsible party', sptREFERENCE, ['Practitioner', 'Organization', 'PractitionerRole'], 'MedicationDispense.substitution.responsibleParty', sxpNormal);
  indexes.add('MedicationDispense', 'status', '): Returns dispenses with a specified dispense status', sptTOKEN, [], 'MedicationAdministration.status | MedicationDispense.status | MedicationRequest.status | MedicationUsage.status', sxpNormal);
  indexes.add('MedicationDispense', 'subject', 'The identity of a patient for whom to list dispenses', sptREFERENCE, ['Group', 'Patient'], 'MedicationDispense.subject', sxpNormal);
  indexes.add('MedicationDispense', 'type', 'Returns dispenses of a specific type', sptTOKEN, [], 'MedicationDispense.type', sxpNormal);
  indexes.add('MedicationDispense', 'whenhandedover', 'Returns dispenses handed over on this date', sptDATE, [], 'MedicationDispense.whenHandedOver', sxpNormal);
  indexes.add('MedicationDispense', 'whenprepared', 'Returns dispenses prepared on this date', sptDATE, [], 'MedicationDispense.whenPrepared', sxpNormal);
  indexes.add('MedicationDispense', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Encounter', 'MedicationDispense', ['encounter']);
  compartments.register('Patient', 'MedicationDispense', ['subject', 'patient', 'receiver']);
  compartments.register('Practitioner', 'MedicationDispense', ['performer', 'receiver']);
end;
{$ENDIF FHIR_MEDICATIONDISPENSE}

{$IFDEF FHIR_MEDICATIONKNOWLEDGE}
procedure TFHIRIndexBuilderR5.buildIndexesForMedicationKnowledge(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('MedicationKnowledge', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('MedicationKnowledge', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('MedicationKnowledge', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('MedicationKnowledge', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('MedicationKnowledge', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('MedicationKnowledge', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('MedicationKnowledge', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('MedicationKnowledge', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('MedicationKnowledge', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('MedicationKnowledge', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('MedicationKnowledge', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('MedicationKnowledge', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('MedicationKnowledge', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('MedicationKnowledge', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('MedicationKnowledge', 'classification', 'Specific category assigned to the medication', sptTOKEN, [], 'MedicationKnowledge.medicineClassification.classification', sxpNormal);
  indexes.add('MedicationKnowledge', 'classification-type', 'The type of category for the medication (for example, therapeutic classification, therapeutic sub-classification)', sptTOKEN, [], 'MedicationKnowledge.medicineClassification.type', sxpNormal);
  indexes.add('MedicationKnowledge', 'code', 'Code that identifies this medication', sptTOKEN, [], 'MedicationKnowledge.code', sxpNormal);
  indexes.add('MedicationKnowledge', 'doseform', 'powder | tablets | capsule +', sptTOKEN, [], 'MedicationKnowledge.definitional.doseForm', sxpNormal);
  indexes.add('MedicationKnowledge', 'identifier', 'Business identifier for this medication', sptTOKEN, [], 'MedicationKnowledge.identifier', sxpNormal);
  indexes.add('MedicationKnowledge', 'ingredient', 'Reference to a resource (by instance)', sptREFERENCE, [], 'MedicationKnowledge.definitional.ingredient.item.reference', sxpNormal);
  indexes.add('MedicationKnowledge', 'ingredient-code', 'Reference to a concept (by class)', sptTOKEN, [], 'MedicationKnowledge.definitional.ingredient.item.concept', sxpNormal);
  indexes.add('MedicationKnowledge', 'monitoring-program-name', 'Name of the reviewing program', sptTOKEN, [], 'MedicationKnowledge.monitoringProgram.name', sxpNormal);
  indexes.add('MedicationKnowledge', 'monitoring-program-type', 'Type of program under which the medication is monitored', sptTOKEN, [], 'MedicationKnowledge.monitoringProgram.type', sxpNormal);
  indexes.add('MedicationKnowledge', 'monograph', 'Associated documentation about the medication', sptREFERENCE, ['DocumentReference'], 'MedicationKnowledge.monograph.source', sxpNormal);
  indexes.add('MedicationKnowledge', 'monograph-type', 'The category of medication document', sptTOKEN, [], 'MedicationKnowledge.monograph.type', sxpNormal);
  indexes.add('MedicationKnowledge', 'packaging-cost', 'The cost of the packaged medication, if the cost is Money', sptQUANTITY, [], '', sxpNormal);
  indexes.add('MedicationKnowledge', 'packaging-cost-concept', 'The cost of the packaged medication, if the cost is a CodeableConcept', sptTOKEN, [], '', sxpNormal);
  indexes.add('MedicationKnowledge', 'product-type', 'Category of the medication or product', sptTOKEN, [], 'MedicationKnowledge.productType', sxpNormal);
  indexes.add('MedicationKnowledge', 'source-cost', 'The source or owner for the price information', sptTOKEN, [], 'MedicationKnowledge.cost.source', sxpNormal);
  indexes.add('MedicationKnowledge', 'status', 'active | inactive | entered-in-error', sptTOKEN, [], 'MedicationKnowledge.status', sxpNormal);
  indexes.add('MedicationKnowledge', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_MEDICATIONKNOWLEDGE}

{$IFDEF FHIR_MEDICATIONREQUEST}
procedure TFHIRIndexBuilderR5.buildIndexesForMedicationRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('MedicationRequest', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('MedicationRequest', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('MedicationRequest', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('MedicationRequest', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('MedicationRequest', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('MedicationRequest', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('MedicationRequest', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('MedicationRequest', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('MedicationRequest', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('MedicationRequest', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('MedicationRequest', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('MedicationRequest', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('MedicationRequest', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('MedicationRequest', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('MedicationRequest', 'authoredon', 'Return prescriptions written on this date', sptDATE, [], 'MedicationRequest.authoredOn', sxpNormal);
  indexes.add('MedicationRequest', 'category', 'Returns prescriptions with different categories', sptTOKEN, [], 'MedicationRequest.category', sxpNormal);
  indexes.add('MedicationRequest', 'code', '): Return prescriptions of this medication code', sptTOKEN, [], 'AllergyIntolerance.code | AllergyIntolerance.reaction.substance | Condition.code | DeviceRequest.code.concept | DiagnosticReport.code | FamilyMemberHistory.condition.code | List.code | Medication.code | MedicationAdministration.medication.concept | M'
      +'edicationDispense.medication.concept | MedicationRequest.medication.concept | MedicationUsage.medication.concept | Observation.code | Procedure.code', sxpNormal);
  indexes.add('MedicationRequest', 'combo-date', 'Returns medication request to be administered on a specific date or within a date range', sptDATE, [], 'MedicationRequest.dose.dosageInstruction.timing.event | (MedicationRequest.dose.dosageInstruction.timing.repeat.bounds as Period)', sxpNormal);
  indexes.add('MedicationRequest', 'encounter', '): Return prescriptions with this encounter identifier', sptREFERENCE, ['Encounter'], 'MedicationAdministration.encounter | MedicationRequest.encounter', sxpNormal);
  indexes.add('MedicationRequest', 'identifier', '): Return prescriptions with this external identifier', sptTOKEN, [], 'AllergyIntolerance.identifier | CarePlan.identifier | CareTeam.identifier | Composition.identifier | Condition.identifier | Consent.identifier | DetectedIssue.identifier | DeviceRequest.identifier | DiagnosticReport.identifier | DocumentManifest.mast'
      +'erIdentifier | DocumentManifest.identifier | DocumentReference.identifier | Encounter.identifier | EpisodeOfCare.identifier | FamilyMemberHistory.identifier | Goal.identifier | ImagingStudy.identifier | Immunization.identifier | List.identifi'
      +'er | MedicationAdministration.identifier | MedicationDispense.identifier | MedicationRequest.identifier | MedicationUsage.identifier | NutritionOrder.identifier | Observation.identifier | Procedure.identifier | RiskAssessment.identifier | Ser'
      +'viceRequest.identifier | SupplyDelivery.identifier | SupplyRequest.identifier | VisionPrescription.identifier', sxpNormal);
  indexes.add('MedicationRequest', 'intended-dispenser', 'Returns prescriptions intended to be dispensed by this Organization', sptREFERENCE, ['Organization'], 'MedicationRequest.dispenseRequest.dispenser', sxpNormal);
  indexes.add('MedicationRequest', 'intended-performer', 'Returns the intended performer of the administration of the medication request', sptREFERENCE, ['Practitioner', 'Organization', 'CareTeam', 'DeviceDefinition', 'Patient', 'HealthcareService', 'PractitionerRole', 'RelatedPerson'], 'MedicationRequest.performer', sxpNormal);
  indexes.add('MedicationRequest', 'intended-performertype', 'Returns requests for a specific type of performer', sptTOKEN, [], 'MedicationRequest.performerType', sxpNormal);
  indexes.add('MedicationRequest', 'intent', 'Returns prescriptions with different intents', sptTOKEN, [], 'MedicationRequest.intent', sxpNormal);
  indexes.add('MedicationRequest', 'medication', '): Return prescriptions for this medication reference', sptREFERENCE, [], 'MedicationAdministration.medication.reference | MedicationDispense.medication.reference | MedicationRequest.medication.reference | MedicationUsage.medication.reference', sxpNormal);
  indexes.add('MedicationRequest', 'patient', '): Returns prescriptions for a specific patient', sptREFERENCE, ['Patient', 'Practitioner', 'Group', 'Organization', 'BiologicallyDerivedProduct', 'NutritionProduct', 'Device', 'Medication', 'Procedure', 'Substance', 'Location'], 'AllergyIntolerance.patient | CarePlan.subject.where(resolve() is Patient) | CareTeam.subject.where(resolve() is Patient) | ClinicalImpression.subject.where(resolve() is Patient) | Composition.subject.where(resolve() is Patient) | Condition.subject.wh'
      +'ere(resolve() is Patient) | Consent.subject.where(resolve() is Patient) | DetectedIssue.subject | DeviceRequest.subject.where(resolve() is Patient) | DeviceUsage.patient | DiagnosticReport.subject.where(resolve() is Patient) | DocumentManifes'
      +'t.subject.where(resolve() is Patient) | DocumentReference.subject.where(resolve() is Patient) | Encounter.subject.where(resolve() is Patient) | EpisodeOfCare.patient | FamilyMemberHistory.patient | Flag.subje'+
   'ct.where(resolve() is Patient) | G'
      +'oal.subject.where(resolve() is Patient) | ImagingStudy.subject.where(resolve() is Patient) | Immunization.patient | List.subject.where(resolve() is Patient) | MedicationAdministration.subject.where(resolve() is Patient) | MedicationDispense.s'
      +'ubject.where(resolve() is Patient) | MedicationRequest.subject.where(resolve() is Patient) | MedicationUsage.subject.where(resolve() is Patient) | NutritionOrder.subject.where(resolve() is Patient) | Observation.subject.where(resolve() is Pat'
      +'ient) | Procedure.subject.where(resolve() is Patient) | RiskAssessment.subject.where(resolve() is Patient) | ServiceRequest.subject.where(resolve() is Patient) | SupplyDelivery.patient | VisionPrescription.patient', sxpNormal);
  indexes.add('MedicationRequest', 'priority', 'Returns prescriptions with different priorities', sptTOKEN, [], 'MedicationRequest.priority', sxpNormal);
  indexes.add('MedicationRequest', 'requester', 'Returns prescriptions prescribed by this prescriber', sptREFERENCE, ['Practitioner', 'Organization', 'Device', 'Patient', 'PractitionerRole', 'RelatedPerson'], 'MedicationRequest.requester', sxpNormal);
  indexes.add('MedicationRequest', 'status', '): Status of the prescription', sptTOKEN, [], 'MedicationAdministration.status | MedicationDispense.status | MedicationRequest.status | MedicationUsage.status', sxpNormal);
  indexes.add('MedicationRequest', 'subject', 'The identity of a patient to list orders  for', sptREFERENCE, ['Group', 'Patient'], 'MedicationRequest.subject', sxpNormal);
  indexes.add('MedicationRequest', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Encounter', 'MedicationRequest', ['encounter']);
  compartments.register('Patient', 'MedicationRequest', ['subject']);
  compartments.register('Practitioner', 'MedicationRequest', ['requester']);
end;
{$ENDIF FHIR_MEDICATIONREQUEST}

{$IFDEF FHIR_MEDICATIONUSAGE}
procedure TFHIRIndexBuilderR5.buildIndexesForMedicationUsage(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('MedicationUsage', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('MedicationUsage', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('MedicationUsage', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('MedicationUsage', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('MedicationUsage', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('MedicationUsage', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('MedicationUsage', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('MedicationUsage', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('MedicationUsage', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('MedicationUsage', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('MedicationUsage', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('MedicationUsage', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('MedicationUsage', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('MedicationUsage', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('MedicationUsage', 'adherence', 'Returns statements based on adherence or compliance', sptTOKEN, [], 'MedicationUsage.adherence', sxpNormal);
  indexes.add('MedicationUsage', 'category', 'Returns statements of this category of MedicationUsage', sptTOKEN, [], 'MedicationUsage.category', sxpNormal);
  indexes.add('MedicationUsage', 'code', '): Return statements of this medication code', sptTOKEN, [], 'AllergyIntolerance.code | AllergyIntolerance.reaction.substance | Condition.code | DeviceRequest.code.concept | DiagnosticReport.code | FamilyMemberHistory.condition.code | List.code | Medication.code | MedicationAdministration.medication.concept | M'
      +'edicationDispense.medication.concept | MedicationRequest.medication.concept | MedicationUsage.medication.concept | Observation.code | Procedure.code', sxpNormal);
  indexes.add('MedicationUsage', 'effective', 'Date when patient was taking (or not taking) the medication', sptDATE, [], 'MedicationUsage.effective', sxpNormal);
  indexes.add('MedicationUsage', 'encounter', 'Returns statements for a specific encounter', sptREFERENCE, ['Encounter'], 'MedicationUsage.encounter', sxpNormal);
  indexes.add('MedicationUsage', 'identifier', '): Return statements with this external identifier', sptTOKEN, [], 'AllergyIntolerance.identifier | CarePlan.identifier | CareTeam.identifier | Composition.identifier | Condition.identifier | Consent.identifier | DetectedIssue.identifier | DeviceRequest.identifier | DiagnosticReport.identifier | DocumentManifest.mast'
      +'erIdentifier | DocumentManifest.identifier | DocumentReference.identifier | Encounter.identifier | EpisodeOfCare.identifier | FamilyMemberHistory.identifier | Goal.identifier | ImagingStudy.identifier | Immunization.identifier | List.identifi'
      +'er | MedicationAdministration.identifier | MedicationDispense.identifier | MedicationRequest.identifier | MedicationUsage.identifier | NutritionOrder.identifier | Observation.identifier | Procedure.identifier | RiskAssessment.identifier | Ser'
      +'viceRequest.identifier | SupplyDelivery.identifier | SupplyRequest.identifier | VisionPrescription.identifier', sxpNormal);
  indexes.add('MedicationUsage', 'medication', '): Return statements of this medication reference', sptREFERENCE, [], 'MedicationAdministration.medication.reference | MedicationDispense.medication.reference | MedicationRequest.medication.reference | MedicationUsage.medication.reference', sxpNormal);
  indexes.add('MedicationUsage', 'patient', '): Returns statements for a specific patient.', sptREFERENCE, ['Patient', 'Practitioner', 'Group', 'Organization', 'BiologicallyDerivedProduct', 'NutritionProduct', 'Device', 'Medication', 'Procedure', 'Substance', 'Location'], 'AllergyIntolerance.patient | CarePlan.subject.where(resolve() is Patient) | CareTeam.subject.where(resolve() is Patient) | ClinicalImpression.subject.where(resolve() is Patient) | Composition.subject.where(resolve() is Patient) | Condition.subject.wh'
      +'ere(resolve() is Patient) | Consent.subject.where(resolve() is Patient) | DetectedIssue.subject | DeviceRequest.subject.where(resolve() is Patient) | DeviceUsage.patient | DiagnosticReport.subject.where(resolve() is Patient) | DocumentManifes'
      +'t.subject.where(resolve() is Patient) | DocumentReference.subject.where(resolve() is Patient) | Encounter.subject.where(resolve() is Patient) | EpisodeOfCare.patient | FamilyMemberHistory.patient | Flag.subject.w'+
   'here(resolve() is Patient) | G'
      +'oal.subject.where(resolve() is Patient) | ImagingStudy.subject.where(resolve() is Patient) | Immunization.patient | List.subject.where(resolve() is Patient) | MedicationAdministration.subject.where(resolve() is Patient) | MedicationDispense.s'
      +'ubject.where(resolve() is Patient) | MedicationRequest.subject.where(resolve() is Patient) | MedicationUsage.subject.where(resolve() is Patient) | NutritionOrder.subject.where(resolve() is Patient) | Observation.subject.where(resolve() is Pat'
      +'ient) | Procedure.subject.where(resolve() is Patient) | RiskAssessment.subject.where(resolve() is Patient) | ServiceRequest.subject.where(resolve() is Patient) | SupplyDelivery.patient | VisionPrescription.patient', sxpNormal);
  indexes.add('MedicationUsage', 'source', 'Who or where the information in the statement came from', sptREFERENCE, ['Practitioner', 'Organization', 'Patient', 'PractitionerRole', 'RelatedPerson'], 'MedicationUsage.informationSource', sxpNormal);
  indexes.add('MedicationUsage', 'status', '): Return statements that match the given status', sptTOKEN, [], 'MedicationAdministration.status | MedicationDispense.status | MedicationRequest.status | MedicationUsage.status', sxpNormal);
  indexes.add('MedicationUsage', 'subject', 'The identity of a patient, animal or group to list statements for', sptREFERENCE, ['Group', 'Patient'], 'MedicationUsage.subject', sxpNormal);
  indexes.add('MedicationUsage', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Encounter', 'MedicationUsage', ['encounter']);
  compartments.register('Patient', 'MedicationUsage', ['subject']);
  compartments.register('Practitioner', 'MedicationUsage', ['source']);
  compartments.register('RelatedPerson', 'MedicationUsage', ['source']);
end;
{$ENDIF FHIR_MEDICATIONUSAGE}

{$IFDEF FHIR_MEDICINALPRODUCTDEFINITION}
procedure TFHIRIndexBuilderR5.buildIndexesForMedicinalProductDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('MedicinalProductDefinition', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('MedicinalProductDefinition', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('MedicinalProductDefinition', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('MedicinalProductDefinition', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('MedicinalProductDefinition', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('MedicinalProductDefinition', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('MedicinalProductDefinition', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('MedicinalProductDefinition', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('MedicinalProductDefinition', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('MedicinalProductDefinition', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('MedicinalProductDefinition', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('MedicinalProductDefinition', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('MedicinalProductDefinition', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('MedicinalProductDefinition', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('MedicinalProductDefinition', 'characteristic', 'Allows the key product features to be recorded, such as "sugar free", "modified release", "parallel import"', sptTOKEN, [], 'MedicinalProductDefinition.characteristic.value', sxpNormal);
  indexes.add('MedicinalProductDefinition', 'characteristic-type', 'A category for the characteristic', sptTOKEN, [], 'MedicinalProductDefinition.characteristic.type', sxpNormal);
  indexes.add('MedicinalProductDefinition', 'contact', 'A product specific contact, person (in a role), or an organization', sptREFERENCE, ['Organization', 'PractitionerRole'], 'MedicinalProductDefinition.contact.contact', sxpNormal);
  indexes.add('MedicinalProductDefinition', 'domain', 'If this medicine applies to human or veterinary uses', sptTOKEN, [], 'MedicinalProductDefinition.domain', sxpNormal);
  indexes.add('MedicinalProductDefinition', 'identifier', 'Business identifier for this product. Could be an MPID', sptTOKEN, [], 'MedicinalProductDefinition.identifier', sxpNormal);
  indexes.add('MedicinalProductDefinition', 'ingredient', 'An ingredient of this product', sptTOKEN, [], 'MedicinalProductDefinition.ingredient', sxpNormal);
  indexes.add('MedicinalProductDefinition', 'master-file', 'A master file for to the medicinal product (e.g. Pharmacovigilance System Master File)', sptREFERENCE, ['DocumentReference'], 'MedicinalProductDefinition.masterFile', sxpNormal);
  indexes.add('MedicinalProductDefinition', 'name', 'The full product name', sptSTRING, [], 'MedicinalProductDefinition.name.productName', sxpNormal);
  indexes.add('MedicinalProductDefinition', 'name-language', 'Language code for this name', sptTOKEN, [], 'MedicinalProductDefinition.name.usage.language', sxpNormal);
  indexes.add('MedicinalProductDefinition', 'product-classification', 'Allows the product to be classified by various systems', sptTOKEN, [], 'MedicinalProductDefinition.classification', sxpNormal);
  indexes.add('MedicinalProductDefinition', 'status', 'The status within the lifecycle of this product record. A high-level status, this is not intended to duplicate details carried elsewhere such as legal status, or authorization status', sptTOKEN, [], 'MedicinalProductDefinition.status', sxpNormal);
  indexes.add('MedicinalProductDefinition', 'type', 'Regulatory type, e.g. Investigational or Authorized', sptTOKEN, [], 'MedicinalProductDefinition.type', sxpNormal);
  indexes.add('MedicinalProductDefinition', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_MEDICINALPRODUCTDEFINITION}

{$IFDEF FHIR_MESSAGEDEFINITION}
procedure TFHIRIndexBuilderR5.buildIndexesForMessageDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('MessageDefinition', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('MessageDefinition', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('MessageDefinition', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('MessageDefinition', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('MessageDefinition', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('MessageDefinition', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('MessageDefinition', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('MessageDefinition', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('MessageDefinition', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('MessageDefinition', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('MessageDefinition', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('MessageDefinition', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('MessageDefinition', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('MessageDefinition', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('MessageDefinition', 'category', 'The behavior associated with the message', sptTOKEN, [], 'MessageDefinition.category', sxpNormal);
  indexes.add('MessageDefinition', 'context', '): A use context assigned to the message definition', sptTOKEN, [], '(CapabilityStatement.useContext.value as CodeableConcept) | (CodeSystem.useContext.value as CodeableConcept) | (CompartmentDefinition.useContext.value as CodeableConcept) | (ConceptMap.useContext.value as CodeableConcept) | (GraphDefinition.useContex'
      +'t.value as CodeableConcept) | (ImplementationGuide.useContext.value as CodeableConcept) | (MessageDefinition.useContext.value as CodeableConcept) | (NamingSystem.useContext.value as CodeableConcept) | (OperationDefinition.useContext.value as '
      +'CodeableConcept) | (SearchParameter.useContext.value as CodeableConcept) | (StructureDefinition.useContext.value as CodeableConcept) | (StructureMap.useContext.value as CodeableConcept) | (TerminologyCapabilities.useContext.value as CodeableC'
      +'oncept) | (ValueSet.useContext.value as CodeableConcept)', sxpNormal);
  indexes.add('MessageDefinition', 'context-quantity', '): A quantity- or range-valued use context assigned to the message definition', sptQUANTITY, [], '(CapabilityStatement.useContext.value as Quantity) | (CapabilityStatement.useContext.value as Range) | (CodeSystem.useContext.value as Quantity) | (CodeSystem.useContext.value as Range) | (CompartmentDefinition.useContext.value as Quantity) | (Compar'
      +'tmentDefinition.useContext.value as Range) | (ConceptMap.useContext.value as Quantity) | (ConceptMap.useContext.value as Range) | (GraphDefinition.useContext.value as Quantity) | (GraphDefinition.useContext.value as Range) | (ImplementationGu'
      +'ide.useContext.value as Quantity) | (ImplementationGuide.useContext.value as Range) | (MessageDefinition.useContext.value as Quantity) | (MessageDefinition.useContext.value as Range) | (NamingSystem.useContext.value as Quantity) | (NamingSyst'
      +'em.useContext.value as Range) | (OperationDefinition.useContext.value as Quanti'+
   'ty) | (OperationDefinition.useContext.value as Range) | (SearchParameter.useContext.value as Quantity) | (SearchParameter.useContext.value as Range) | (StructureDe'
      +'finition.useContext.value as Quantity) | (StructureDefinition.useContext.value as Range) | (StructureMap.useContext.value as Quantity) | (StructureMap.useContext.value as Range) | (TerminologyCapabilities.useContext.value as Quantity) | (Term'
      +'inologyCapabilities.useContext.value as Range) | (ValueSet.useContext.value as Quantity) | (ValueSet.useContext.value as Range)', sxpNormal);
  indexes.add('MessageDefinition', 'context-type', '): A type of use context assigned to the message definition', sptTOKEN, [], 'CapabilityStatement.useContext.code | CodeSystem.useContext.code | CompartmentDefinition.useContext.code | ConceptMap.useContext.code | GraphDefinition.useContext.code | ImplementationGuide.useContext.code | MessageDefinition.useContext.code | Naming'
      +'System.useContext.code | OperationDefinition.useContext.code | SearchParameter.useContext.code | StructureDefinition.useContext.code | StructureMap.useContext.code | TerminologyCapabilities.useContext.code | ValueSet.useContext.code', sxpNormal);
  indexes.add('MessageDefinition', 'context-type-quantity', '): A use context type and quantity- or range-based value assigned to the message definition', sptCOMPOSITE, [], 'CapabilityStatement.useContext | CodeSystem.useContext | CompartmentDefinition.useContext | ConceptMap.useContext | GraphDefinition.useContext | ImplementationGuide.useContext | MessageDefinition.useContext | NamingSystem.useContext | OperationDefini'
      +'tion.useContext | SearchParameter.useContext | StructureDefinition.useContext | StructureMap.useContext | TerminologyCapabilities.useContext | ValueSet.useContext', sxpNormal);
  indexes.add('MessageDefinition', 'context-type-value', '): A use context type and value assigned to the message definition', sptCOMPOSITE, [], 'CapabilityStatement.useContext | CodeSystem.useContext | CompartmentDefinition.useContext | ConceptMap.useContext | GraphDefinition.useContext | ImplementationGuide.useContext | MessageDefinition.useContext | NamingSystem.useContext | OperationDefini'
      +'tion.useContext | SearchParameter.useContext | StructureDefinition.useContext | StructureMap.useContext | TerminologyCapabilities.useContext | ValueSet.useContext', sxpNormal);
  indexes.add('MessageDefinition', 'date', '): The message definition publication date', sptDATE, [], 'CapabilityStatement.date | CodeSystem.date | CompartmentDefinition.date | ConceptMap.date | GraphDefinition.date | ImplementationGuide.date | MessageDefinition.date | NamingSystem.date | OperationDefinition.date | SearchParameter.date | StructureDefi'
      +'nition.date | StructureMap.date | TerminologyCapabilities.date | ValueSet.date', sxpNormal);
  indexes.add('MessageDefinition', 'description', '): The description of the message definition', sptSTRING, [], 'CapabilityStatement.description | CodeSystem.description | CompartmentDefinition.description | ConceptMap.description | GraphDefinition.description | ImplementationGuide.description | MessageDefinition.description | NamingSystem.description | Operati'
      +'onDefinition.description | SearchParameter.description | StructureDefinition.description | StructureMap.description | TerminologyCapabilities.description | ValueSet.description', sxpNormal);
  indexes.add('MessageDefinition', 'event', 'The event that triggers the message or link to the event definition.', sptTOKEN, [], 'MessageDefinition.event', sxpNormal);
  indexes.add('MessageDefinition', 'focus', 'A resource that is a permitted focus of the message', sptTOKEN, [], 'MessageDefinition.focus.code', sxpNormal);
  indexes.add('MessageDefinition', 'identifier', '): External identifier for the message definition', sptTOKEN, [], 'CodeSystem.identifier | ConceptMap.identifier | MessageDefinition.identifier | NamingSystem.identifier | StructureDefinition.identifier | StructureMap.identifier | TerminologyCapabilities.identifier | ValueSet.identifier', sxpNormal);
  indexes.add('MessageDefinition', 'jurisdiction', '): Intended jurisdiction for the message definition', sptTOKEN, [], 'CapabilityStatement.jurisdiction | CodeSystem.jurisdiction | ConceptMap.jurisdiction | GraphDefinition.jurisdiction | ImplementationGuide.jurisdiction | MessageDefinition.jurisdiction | NamingSystem.jurisdiction | OperationDefinition.jurisdiction | S'
      +'earchParameter.jurisdiction | StructureDefinition.jurisdiction | StructureMap.jurisdiction | TerminologyCapabilities.jurisdiction | ValueSet.jurisdiction', sxpNormal);
  indexes.add('MessageDefinition', 'name', '): Computationally friendly name of the message definition', sptSTRING, [], 'CapabilityStatement.name | CodeSystem.name | CompartmentDefinition.name | ConceptMap.name | GraphDefinition.name | ImplementationGuide.name | MessageDefinition.name | NamingSystem.name | OperationDefinition.name | SearchParameter.name | StructureDefi'
      +'nition.name | StructureMap.name | TerminologyCapabilities.name | ValueSet.name', sxpNormal);
  indexes.add('MessageDefinition', 'parent', 'A resource that is the parent of the definition', sptREFERENCE, ['PlanDefinition', 'ActivityDefinition'], 'MessageDefinition.parent', sxpNormal);
  indexes.add('MessageDefinition', 'publisher', '): Name of the publisher of the message definition', sptSTRING, [], 'CapabilityStatement.publisher | CodeSystem.publisher | CompartmentDefinition.publisher | ConceptMap.publisher | GraphDefinition.publisher | ImplementationGuide.publisher | MessageDefinition.publisher | NamingSystem.publisher | OperationDefinition.pub'
      +'lisher | SearchParameter.publisher | StructureDefinition.publisher | StructureMap.publisher | TerminologyCapabilities.publisher | ValueSet.publisher', sxpNormal);
  indexes.add('MessageDefinition', 'status', '): The current status of the message definition', sptTOKEN, [], 'CapabilityStatement.status | CodeSystem.status | CompartmentDefinition.status | ConceptMap.status | GraphDefinition.status | ImplementationGuide.status | MessageDefinition.status | NamingSystem.status | OperationDefinition.status | SearchParameter.st'
      +'atus | StructureDefinition.status | StructureMap.status | TerminologyCapabilities.status | ValueSet.status', sxpNormal);
  indexes.add('MessageDefinition', 'title', '): The human-friendly name of the message definition', sptSTRING, [], 'CapabilityStatement.title | CodeSystem.title | ConceptMap.title | ImplementationGuide.title | MessageDefinition.title | OperationDefinition.title | StructureDefinition.title | StructureMap.title | TerminologyCapabilities.title | ValueSet.title', sxpNormal);
  indexes.add('MessageDefinition', 'url', '): The uri that identifies the message definition', sptURI, [], 'CapabilityStatement.url | CodeSystem.url | CompartmentDefinition.url | ConceptMap.url | GraphDefinition.url | ImplementationGuide.url | MessageDefinition.url | NamingSystem.url | OperationDefinition.url | SearchParameter.url | StructureDefinition.url'
      +' | StructureMap.url | TerminologyCapabilities.url | ValueSet.url', sxpNormal);
  indexes.add('MessageDefinition', 'version', '): The business version of the message definition', sptTOKEN, [], 'CapabilityStatement.version | CodeSystem.version | CompartmentDefinition.version | ConceptMap.version | GraphDefinition.version | ImplementationGuide.version | MessageDefinition.version | NamingSystem.version | OperationDefinition.version | SearchPar'
      +'ameter.version | StructureDefinition.version | StructureMap.version | TerminologyCapabilities.version | ValueSet.version', sxpNormal);
  indexes.add('MessageDefinition', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_MESSAGEDEFINITION}

{$IFDEF FHIR_MESSAGEHEADER}
procedure TFHIRIndexBuilderR5.buildIndexesForMessageHeader(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('MessageHeader', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('MessageHeader', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('MessageHeader', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('MessageHeader', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('MessageHeader', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('MessageHeader', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('MessageHeader', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('MessageHeader', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('MessageHeader', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('MessageHeader', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('MessageHeader', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('MessageHeader', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('MessageHeader', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('MessageHeader', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('MessageHeader', 'author', 'The source of the decision', sptREFERENCE, ['Practitioner', 'PractitionerRole'], 'MessageHeader.author', sxpNormal);
  indexes.add('MessageHeader', 'code', 'ok | transient-error | fatal-error', sptTOKEN, [], 'MessageHeader.response.code', sxpNormal);
  indexes.add('MessageHeader', 'destination', 'Name of system', sptSTRING, [], 'MessageHeader.destination.name', sxpNormal);
  indexes.add('MessageHeader', 'destination-uri', 'Actual destination address or id', sptURI, [], 'MessageHeader.destination.endpoint', sxpNormal);
  indexes.add('MessageHeader', 'enterer', 'The source of the data entry', sptREFERENCE, ['Practitioner', 'PractitionerRole'], 'MessageHeader.enterer', sxpNormal);
  indexes.add('MessageHeader', 'event', 'Code for the event this message represents or link to event definition', sptTOKEN, [], 'MessageHeader.event', sxpNormal);
  indexes.add('MessageHeader', 'focus', 'The actual content of the message', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'MessageHeader.focus', sxpNormal);
  indexes.add('MessageHeader', 'receiver', 'Intended "real-world" recipient for the data', sptREFERENCE, ['Practitioner', 'Organization', 'PractitionerRole'], 'MessageHeader.destination.receiver', sxpNormal);
  indexes.add('MessageHeader', 'response-id', 'Id of original message', sptTOKEN, [], 'MessageHeader.response.identifier', sxpNormal);
  indexes.add('MessageHeader', 'responsible', 'Final responsibility for event', sptREFERENCE, ['Practitioner', 'Organization', 'PractitionerRole'], 'MessageHeader.responsible', sxpNormal);
  indexes.add('MessageHeader', 'sender', 'Real world sender of the message', sptREFERENCE, ['Practitioner', 'Organization', 'PractitionerRole'], 'MessageHeader.sender', sxpNormal);
  indexes.add('MessageHeader', 'source', 'Name of system', sptSTRING, [], 'MessageHeader.source.name', sxpNormal);
  indexes.add('MessageHeader', 'source-uri', 'Actual message source address or id', sptURI, [], 'MessageHeader.source.endpoint', sxpNormal);
  indexes.add('MessageHeader', 'target', 'Particular delivery destination within the destination', sptREFERENCE, ['Device'], 'MessageHeader.destination.target', sxpNormal);
  indexes.add('MessageHeader', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Device', 'MessageHeader', ['target']);
  compartments.register('Practitioner', 'MessageHeader', ['receiver', 'author', 'responsible', 'enterer']);
end;
{$ENDIF FHIR_MESSAGEHEADER}

{$IFDEF FHIR_MOLECULARSEQUENCE}
procedure TFHIRIndexBuilderR5.buildIndexesForMolecularSequence(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('MolecularSequence', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('MolecularSequence', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('MolecularSequence', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('MolecularSequence', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('MolecularSequence', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('MolecularSequence', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('MolecularSequence', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('MolecularSequence', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('MolecularSequence', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('MolecularSequence', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('MolecularSequence', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('MolecularSequence', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('MolecularSequence', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('MolecularSequence', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('MolecularSequence', 'identifier', 'The unique identity for a particular sequence', sptTOKEN, [], 'MolecularSequence.identifier', sxpNormal);
  indexes.add('MolecularSequence', 'patient', 'The subject that the sequence is about', sptREFERENCE, ['Practitioner', 'Group', 'Organization', 'BiologicallyDerivedProduct', 'NutritionProduct', 'Device', 'Medication', 'Patient', 'Procedure', 'Substance', 'Location'], 'MolecularSequence.subject', sxpNormal);
  indexes.add('MolecularSequence', 'subject', 'The subject that the sequence is about', sptREFERENCE, ['Practitioner', 'Group', 'Organization', 'BiologicallyDerivedProduct', 'NutritionProduct', 'Device', 'Medication', 'Patient', 'Procedure', 'Substance', 'Location'], 'MolecularSequence.subject', sxpNormal);
  indexes.add('MolecularSequence', 'type', 'Amino Acid Sequence/ DNA Sequence / RNA Sequence', sptTOKEN, [], 'MolecularSequence.type', sxpNormal);
  indexes.add('MolecularSequence', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Patient', 'MolecularSequence', ['subject']);
end;
{$ENDIF FHIR_MOLECULARSEQUENCE}

{$IFDEF FHIR_NAMINGSYSTEM}
procedure TFHIRIndexBuilderR5.buildIndexesForNamingSystem(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('NamingSystem', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('NamingSystem', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('NamingSystem', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('NamingSystem', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('NamingSystem', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('NamingSystem', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('NamingSystem', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('NamingSystem', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('NamingSystem', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('NamingSystem', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('NamingSystem', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('NamingSystem', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('NamingSystem', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('NamingSystem', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('NamingSystem', 'contact', 'Name of an individual to contact', sptSTRING, [], 'NamingSystem.contact.name', sxpNormal);
  indexes.add('NamingSystem', 'context', '): A use context assigned to the naming system', sptTOKEN, [], '(CapabilityStatement.useContext.value as CodeableConcept) | (CodeSystem.useContext.value as CodeableConcept) | (CompartmentDefinition.useContext.value as CodeableConcept) | (ConceptMap.useContext.value as CodeableConcept) | (GraphDefinition.useContex'
      +'t.value as CodeableConcept) | (ImplementationGuide.useContext.value as CodeableConcept) | (MessageDefinition.useContext.value as CodeableConcept) | (NamingSystem.useContext.value as CodeableConcept) | (OperationDefinition.useContext.value as '
      +'CodeableConcept) | (SearchParameter.useContext.value as CodeableConcept) | (StructureDefinition.useContext.value as CodeableConcept) | (StructureMap.useContext.value as CodeableConcept) | (TerminologyCapabilities.useContext.value as CodeableC'
      +'oncept) | (ValueSet.useContext.value as CodeableConcept)', sxpNormal);
  indexes.add('NamingSystem', 'context-quantity', '): A quantity- or range-valued use context assigned to the naming system', sptQUANTITY, [], '(CapabilityStatement.useContext.value as Quantity) | (CapabilityStatement.useContext.value as Range) | (CodeSystem.useContext.value as Quantity) | (CodeSystem.useContext.value as Range) | (CompartmentDefinition.useContext.value as Quantity) | (Compar'
      +'tmentDefinition.useContext.value as Range) | (ConceptMap.useContext.value as Quantity) | (ConceptMap.useContext.value as Range) | (GraphDefinition.useContext.value as Quantity) | (GraphDefinition.useContext.value as Range) | (ImplementationGu'
      +'ide.useContext.value as Quantity) | (ImplementationGuide.useContext.value as Range) | (MessageDefinition.useContext.value as Quantity) | (MessageDefinition.useContext.value as Range) | (NamingSystem.useContext.value as Quantity) | (NamingSyst'
      +'em.useContext.value as Range) | (OperationDefinition.useContext.value as Quantity) | (Ope'+
   'rationDefinition.useContext.value as Range) | (SearchParameter.useContext.value as Quantity) | (SearchParameter.useContext.value as Range) | (StructureDe'
      +'finition.useContext.value as Quantity) | (StructureDefinition.useContext.value as Range) | (StructureMap.useContext.value as Quantity) | (StructureMap.useContext.value as Range) | (TerminologyCapabilities.useContext.value as Quantity) | (Term'
      +'inologyCapabilities.useContext.value as Range) | (ValueSet.useContext.value as Quantity) | (ValueSet.useContext.value as Range)', sxpNormal);
  indexes.add('NamingSystem', 'context-type', '): A type of use context assigned to the naming system', sptTOKEN, [], 'CapabilityStatement.useContext.code | CodeSystem.useContext.code | CompartmentDefinition.useContext.code | ConceptMap.useContext.code | GraphDefinition.useContext.code | ImplementationGuide.useContext.code | MessageDefinition.useContext.code | Naming'
      +'System.useContext.code | OperationDefinition.useContext.code | SearchParameter.useContext.code | StructureDefinition.useContext.code | StructureMap.useContext.code | TerminologyCapabilities.useContext.code | ValueSet.useContext.code', sxpNormal);
  indexes.add('NamingSystem', 'context-type-quantity', '): A use context type and quantity- or range-based value assigned to the naming system', sptCOMPOSITE, [], 'CapabilityStatement.useContext | CodeSystem.useContext | CompartmentDefinition.useContext | ConceptMap.useContext | GraphDefinition.useContext | ImplementationGuide.useContext | MessageDefinition.useContext | NamingSystem.useContext | OperationDefini'
      +'tion.useContext | SearchParameter.useContext | StructureDefinition.useContext | StructureMap.useContext | TerminologyCapabilities.useContext | ValueSet.useContext', sxpNormal);
  indexes.add('NamingSystem', 'context-type-value', '): A use context type and value assigned to the naming system', sptCOMPOSITE, [], 'CapabilityStatement.useContext | CodeSystem.useContext | CompartmentDefinition.useContext | ConceptMap.useContext | GraphDefinition.useContext | ImplementationGuide.useContext | MessageDefinition.useContext | NamingSystem.useContext | OperationDefini'
      +'tion.useContext | SearchParameter.useContext | StructureDefinition.useContext | StructureMap.useContext | TerminologyCapabilities.useContext | ValueSet.useContext', sxpNormal);
  indexes.add('NamingSystem', 'date', '): The naming system publication date', sptDATE, [], 'CapabilityStatement.date | CodeSystem.date | CompartmentDefinition.date | ConceptMap.date | GraphDefinition.date | ImplementationGuide.date | MessageDefinition.date | NamingSystem.date | OperationDefinition.date | SearchParameter.date | StructureDefi'
      +'nition.date | StructureMap.date | TerminologyCapabilities.date | ValueSet.date', sxpNormal);
  indexes.add('NamingSystem', 'derived-from', 'A resource that the NamingSystem is derived from', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'NamingSystem.relatedArtifact.where(type=''derived-from'').resource', sxpNormal);
  indexes.add('NamingSystem', 'description', '): The description of the naming system', sptSTRING, [], 'CapabilityStatement.description | CodeSystem.description | CompartmentDefinition.description | ConceptMap.description | GraphDefinition.description | ImplementationGuide.description | MessageDefinition.description | NamingSystem.description | Operati'
      +'onDefinition.description | SearchParameter.description | StructureDefinition.description | StructureMap.description | TerminologyCapabilities.description | ValueSet.description', sxpNormal);
  indexes.add('NamingSystem', 'effective', '): The time during which the NamingSystem is intended to be in use', sptDATE, [], 'CodeSystem.effectivePeriod | ConceptMap.effectivePeriod | NamingSystem.effectivePeriod | ValueSet.effectivePeriod', sxpNormal);
  indexes.add('NamingSystem', 'id-type', 'oid | uuid | uri | other', sptTOKEN, [], 'NamingSystem.uniqueId.type', sxpNormal);
  indexes.add('NamingSystem', 'identifier', '): External identifier for the naming system', sptTOKEN, [], 'CodeSystem.identifier | ConceptMap.identifier | MessageDefinition.identifier | NamingSystem.identifier | StructureDefinition.identifier | StructureMap.identifier | TerminologyCapabilities.identifier | ValueSet.identifier', sxpNormal);
  indexes.add('NamingSystem', 'jurisdiction', '): Intended jurisdiction for the naming system', sptTOKEN, [], 'CapabilityStatement.jurisdiction | CodeSystem.jurisdiction | ConceptMap.jurisdiction | GraphDefinition.jurisdiction | ImplementationGuide.jurisdiction | MessageDefinition.jurisdiction | NamingSystem.jurisdiction | OperationDefinition.jurisdiction | S'
      +'earchParameter.jurisdiction | StructureDefinition.jurisdiction | StructureMap.jurisdiction | TerminologyCapabilities.jurisdiction | ValueSet.jurisdiction', sxpNormal);
  indexes.add('NamingSystem', 'kind', 'codesystem | identifier | root', sptTOKEN, [], 'NamingSystem.kind', sxpNormal);
  indexes.add('NamingSystem', 'name', '): Computationally friendly name of the naming system', sptSTRING, [], 'CapabilityStatement.name | CodeSystem.name | CompartmentDefinition.name | ConceptMap.name | GraphDefinition.name | ImplementationGuide.name | MessageDefinition.name | NamingSystem.name | OperationDefinition.name | SearchParameter.name | StructureDefi'
      +'nition.name | StructureMap.name | TerminologyCapabilities.name | ValueSet.name', sxpNormal);
  indexes.add('NamingSystem', 'period', 'When is identifier valid?', sptDATE, [], 'NamingSystem.uniqueId.period', sxpNormal);
  indexes.add('NamingSystem', 'predecessor', 'The predecessor of the NamingSystem', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'NamingSystem.relatedArtifact.where(type=''predecessor'').resource', sxpNormal);
  indexes.add('NamingSystem', 'publisher', '): Name of the publisher of the naming system', sptSTRING, [], 'CapabilityStatement.publisher | CodeSystem.publisher | CompartmentDefinition.publisher | ConceptMap.publisher | GraphDefinition.publisher | ImplementationGuide.publisher | MessageDefinition.publisher | NamingSystem.publisher | OperationDefinition.pub'
      +'lisher | SearchParameter.publisher | StructureDefinition.publisher | StructureMap.publisher | TerminologyCapabilities.publisher | ValueSet.publisher', sxpNormal);
  indexes.add('NamingSystem', 'responsible', 'Who maintains system namespace?', sptSTRING, [], 'NamingSystem.responsible', sxpNormal);
  indexes.add('NamingSystem', 'status', '): The current status of the naming system', sptTOKEN, [], 'CapabilityStatement.status | CodeSystem.status | CompartmentDefinition.status | ConceptMap.status | GraphDefinition.status | ImplementationGuide.status | MessageDefinition.status | NamingSystem.status | OperationDefinition.status | SearchParameter.st'
      +'atus | StructureDefinition.status | StructureMap.status | TerminologyCapabilities.status | ValueSet.status', sxpNormal);
  indexes.add('NamingSystem', 'telecom', 'Contact details for individual or organization', sptTOKEN, [], 'NamingSystem.contact.telecom', sxpNormal);
  indexes.add('NamingSystem', 'topic', 'Topics associated with the NamingSystem', sptTOKEN, [], 'NamingSystem.topic', sxpNormal);
  indexes.add('NamingSystem', 'type', 'e.g. driver,  provider,  patient, bank etc.', sptTOKEN, [], 'NamingSystem.type', sxpNormal);
  indexes.add('NamingSystem', 'url', '): The uri that identifies the naming system', sptURI, [], 'CapabilityStatement.url | CodeSystem.url | CompartmentDefinition.url | ConceptMap.url | GraphDefinition.url | ImplementationGuide.url | MessageDefinition.url | NamingSystem.url | OperationDefinition.url | SearchParameter.url | StructureDefinition.url'
      +' | StructureMap.url | TerminologyCapabilities.url | ValueSet.url', sxpNormal);
  indexes.add('NamingSystem', 'value', 'The unique identifier', sptSTRING, [], 'NamingSystem.uniqueId.value', sxpNormal);
  indexes.add('NamingSystem', 'version', '): The business version of the naming system', sptTOKEN, [], 'CapabilityStatement.version | CodeSystem.version | CompartmentDefinition.version | ConceptMap.version | GraphDefinition.version | ImplementationGuide.version | MessageDefinition.version | NamingSystem.version | OperationDefinition.version | SearchPar'
      +'ameter.version | StructureDefinition.version | StructureMap.version | TerminologyCapabilities.version | ValueSet.version', sxpNormal);
  indexes.add('NamingSystem', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_NAMINGSYSTEM}

{$IFDEF FHIR_NUTRITIONINTAKE}
procedure TFHIRIndexBuilderR5.buildIndexesForNutritionIntake(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('NutritionIntake', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('NutritionIntake', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('NutritionIntake', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('NutritionIntake', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('NutritionIntake', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('NutritionIntake', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('NutritionIntake', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('NutritionIntake', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('NutritionIntake', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('NutritionIntake', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('NutritionIntake', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('NutritionIntake', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('NutritionIntake', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('NutritionIntake', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('NutritionIntake', 'code', 'Returns statements of this code of NutritionIntake', sptTOKEN, [], 'NutritionIntake.code', sxpNormal);
  indexes.add('NutritionIntake', 'date', 'Date when patient was taking (or not taking) the medication', sptDATE, [], 'NutritionIntake.occurrence', sxpNormal);
  indexes.add('NutritionIntake', 'encounter', 'Returns statements for a specific encounter', sptREFERENCE, ['Encounter'], 'NutritionIntake.encounter', sxpNormal);
  indexes.add('NutritionIntake', 'identifier', 'Return statements with this external identifier', sptTOKEN, [], 'NutritionIntake.identifier', sxpNormal);
  indexes.add('NutritionIntake', 'nutrition', 'Return intakes for a specific consumed item', sptTOKEN, [], 'NutritionIntake.consumedItem.nutritionProduct.concept', sxpNormal);
  indexes.add('NutritionIntake', 'patient', 'Returns statements for a specific patient.', sptREFERENCE, ['Patient'], 'NutritionIntake.subject.where(resolve() is Patient)', sxpNormal);
  indexes.add('NutritionIntake', 'source', 'Who or where the information in the statement came from', sptREFERENCE, ['Practitioner', 'Organization', 'Patient', 'PractitionerRole', 'RelatedPerson'], '(NutritionIntake.reported as Reference)', sxpNormal);
  indexes.add('NutritionIntake', 'status', 'Return statements that match the given status', sptTOKEN, [], 'NutritionIntake.status', sxpNormal);
  indexes.add('NutritionIntake', 'subject', 'The identity of a patient, animal or group to list statements for', sptREFERENCE, ['Group', 'Patient'], 'NutritionIntake.subject', sxpNormal);
  indexes.add('NutritionIntake', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Encounter', 'NutritionIntake', ['encounter']);
  compartments.register('Patient', 'NutritionIntake', ['subject', 'source']);
  compartments.register('Practitioner', 'NutritionIntake', ['source']);
  compartments.register('RelatedPerson', 'NutritionIntake', ['source']);
end;
{$ENDIF FHIR_NUTRITIONINTAKE}

{$IFDEF FHIR_NUTRITIONORDER}
procedure TFHIRIndexBuilderR5.buildIndexesForNutritionOrder(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('NutritionOrder', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('NutritionOrder', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('NutritionOrder', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('NutritionOrder', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('NutritionOrder', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('NutritionOrder', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('NutritionOrder', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('NutritionOrder', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('NutritionOrder', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('NutritionOrder', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('NutritionOrder', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('NutritionOrder', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('NutritionOrder', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('NutritionOrder', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('NutritionOrder', 'additive', 'Type of module component to add to the feeding', sptTOKEN, [], 'NutritionOrder.enteralFormula.additive.type.concept', sxpNormal);
  indexes.add('NutritionOrder', 'datetime', 'Return nutrition orders requested on this date', sptDATE, [], 'NutritionOrder.dateTime', sxpNormal);
  indexes.add('NutritionOrder', 'encounter', '): Return nutrition orders with this encounter identifier', sptREFERENCE, ['Encounter'], 'Composition.encounter | DeviceRequest.encounter | DiagnosticReport.encounter | Flag.encounter | List.encounter | NutritionOrder.encounter | Observation.encounter | Procedure.encounter | RiskAssessment.encounter | ServiceRequest.encounter | VisionPres'
      +'cription.encounter', sxpNormal);
  indexes.add('NutritionOrder', 'formula', 'Type of enteral or infant formula', sptTOKEN, [], 'NutritionOrder.enteralFormula.baseFormulaType.concept', sxpNormal);
  indexes.add('NutritionOrder', 'identifier', '): Return nutrition orders with this external identifier', sptTOKEN, [], 'AllergyIntolerance.identifier | CarePlan.identifier | CareTeam.identifier | Composition.identifier | Condition.identifier | Consent.identifier | DetectedIssue.identifier | DeviceRequest.identifier | DiagnosticReport.identifier | DocumentManifest.mast'
      +'erIdentifier | DocumentManifest.identifier | DocumentReference.identifier | Encounter.identifier | EpisodeOfCare.identifier | FamilyMemberHistory.identifier | Goal.identifier | ImagingStudy.identifier | Immunization.identifier | List.identifi'
      +'er | MedicationAdministration.identifier | MedicationDispense.identifier | MedicationRequest.identifier | MedicationUsage.identifier | NutritionOrder.identifier | Observation.identifier | Procedure.identifier | RiskAssessment.identifier | Ser'
      +'viceRequest.identifier | SupplyDelivery.identifier | SupplyRequest.identifier | VisionPrescription.identifier', sxpNormal);
  indexes.add('NutritionOrder', 'oraldiet', 'Type of diet that can be consumed orally (i.e., take via the mouth).', sptTOKEN, [], 'NutritionOrder.oralDiet.type', sxpNormal);
  indexes.add('NutritionOrder', 'patient', '): The identity of the individual or set of individuals who requires the diet, formula or nutritional supplement', sptREFERENCE, ['Patient', 'Practitioner', 'Group', 'Organization', 'BiologicallyDerivedProduct', 'NutritionProduct', 'Device', 'Medication', 'Procedure', 'Substance', 'Location'], 'AllergyIntolerance.patient | CarePlan.subject.where(resolve() is Patient) | CareTeam.subject.where(resolve() is Patient) | ClinicalImpression.subject.where(resolve() is Patient) | Composition.subject.where(resolve() is Patient) | Condition.subject.wh'
      +'ere(resolve() is Patient) | Consent.subject.where(resolve() is Patient) | DetectedIssue.subject | DeviceRequest.subject.where(resolve() is Patient) | DeviceUsage.patient | DiagnosticReport.subject.where(resolve() is Patient) | DocumentManifes'
      +'t.subject.where(resolve() is Patient) | DocumentReference.subject.where(resolve() is Patient) | Encounter.subject.where(resolve() is Patient) | Ep'+
   'isodeOfCare.patient | FamilyMemberHistory.patient | Flag.subject.where(resolve() is Patient) | G'
      +'oal.subject.where(resolve() is Patient) | ImagingStudy.subject.where(resolve() is Patient) | Immunization.patient | List.subject.where(resolve() is Patient) | MedicationAdministration.subject.where(resolve() is Patient) | MedicationDispense.s'
      +'ubject.where(resolve() is Patient) | MedicationRequest.subject.where(resolve() is Patient) | MedicationUsage.subject.where(resolve() is Patient) | NutritionOrder.subject.where(resolve() is Patient) | Observation.subject.where(resolve() is Pat'
      +'ient) | Procedure.subject.where(resolve() is Patient) | RiskAssessment.subject.where(resolve() is Patient) | ServiceRequest.subject.where(resolve() is Patient) | SupplyDelivery.patient | VisionPrescription.patient', sxpNormal);
  indexes.add('NutritionOrder', 'provider', 'The identity of the provider who placed the nutrition order', sptREFERENCE, ['Practitioner', 'PractitionerRole'], 'NutritionOrder.orderer', sxpNormal);
  indexes.add('NutritionOrder', 'status', 'Status of the nutrition order.', sptTOKEN, [], 'NutritionOrder.status', sxpNormal);
  indexes.add('NutritionOrder', 'subject', 'The identity of the individual or set of individuals who requires the diet, formula or nutritional supplement', sptREFERENCE, ['Group', 'Patient'], 'NutritionOrder.subject', sxpNormal);
  indexes.add('NutritionOrder', 'supplement', 'Type of supplement product requested', sptTOKEN, [], 'NutritionOrder.supplement.type.concept', sxpNormal);
  indexes.add('NutritionOrder', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Encounter', 'NutritionOrder', ['encounter']);
  compartments.register('Patient', 'NutritionOrder', ['patient']);
  compartments.register('Practitioner', 'NutritionOrder', ['provider']);
end;
{$ENDIF FHIR_NUTRITIONORDER}

{$IFDEF FHIR_NUTRITIONPRODUCT}
procedure TFHIRIndexBuilderR5.buildIndexesForNutritionProduct(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('NutritionProduct', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('NutritionProduct', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('NutritionProduct', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('NutritionProduct', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('NutritionProduct', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('NutritionProduct', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('NutritionProduct', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('NutritionProduct', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('NutritionProduct', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('NutritionProduct', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('NutritionProduct', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('NutritionProduct', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('NutritionProduct', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('NutritionProduct', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('NutritionProduct', 'code', 'The code identifying a specific nutrition product', sptTOKEN, [], 'NutritionProduct.code', sxpNormal);
  indexes.add('NutritionProduct', 'identifier', 'The identifier for the physical instance, typically a serial number', sptTOKEN, [], 'NutritionProduct.instance.identifier', sxpNormal);
  indexes.add('NutritionProduct', 'status', 'The broad product group or category of the nutrition product', sptTOKEN, [], 'NutritionProduct.category', sxpNormal);
  indexes.add('NutritionProduct', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_NUTRITIONPRODUCT}

{$IFDEF FHIR_OBSERVATION}
procedure TFHIRIndexBuilderR5.buildIndexesForObservation(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Observation', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('Observation', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('Observation', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('Observation', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('Observation', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('Observation', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('Observation', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('Observation', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('Observation', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('Observation', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('Observation', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('Observation', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('Observation', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('Observation', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('Observation', 'based-on', 'Reference to the service request.', sptREFERENCE, ['CarePlan', 'MedicationRequest', 'NutritionOrder', 'DeviceRequest', 'ServiceRequest', 'ImmunizationRecommendation'], 'Observation.basedOn', sxpNormal);
  indexes.add('Observation', 'category', 'The classification of the type of observation', sptTOKEN, [], 'Observation.category', sxpNormal);
  indexes.add('Observation', 'code', '): The code of the observation type', sptTOKEN, [], 'AllergyIntolerance.code | AllergyIntolerance.reaction.substance | Condition.code | DeviceRequest.code.concept | DiagnosticReport.code | FamilyMemberHistory.condition.code | List.code | Medication.code | MedicationAdministration.medication.concept | M'
      +'edicationDispense.medication.concept | MedicationRequest.medication.concept | MedicationUsage.medication.concept | Observation.code | Procedure.code', sxpNormal);
  indexes.add('Observation', 'code-value-concept', 'Code and coded value parameter pair', sptCOMPOSITE, [], 'Observation', sxpNormal);
  indexes.add('Observation', 'code-value-date', 'Code and date/time value parameter pair', sptCOMPOSITE, [], 'Observation', sxpNormal);
  indexes.add('Observation', 'code-value-quantity', 'Code and quantity value parameter pair', sptCOMPOSITE, [], 'Observation', sxpNormal);
  indexes.add('Observation', 'code-value-string', 'Code and string value parameter pair', sptCOMPOSITE, [], 'Observation', sxpNormal);
  indexes.add('Observation', 'combo-code', 'The code of the observation type or component type', sptTOKEN, [], 'Observation.code | Observation.component.code', sxpNormal);
  indexes.add('Observation', 'combo-code-value-concept', 'Code and coded value parameter pair, including in components', sptCOMPOSITE, [], 'Observation | Observation.component', sxpNormal);
  indexes.add('Observation', 'combo-code-value-quantity', 'Code and quantity value parameter pair, including in components', sptCOMPOSITE, [], 'Observation | Observation.component', sxpNormal);
  indexes.add('Observation', 'combo-data-absent-reason', 'The reason why the expected value in the element Observation.value[x] or Observation.component.value[x] is missing.', sptTOKEN, [], 'Observation.dataAbsentReason | Observation.component.dataAbsentReason', sxpNormal);
  indexes.add('Observation', 'combo-value-concept', 'The value or component value of the observation, if the value is a CodeableConcept', sptTOKEN, [], 'Observation.value.ofType(CodeableConcept) | Observation.component.value.ofType(CodeableConcept)', sxpNormal);
  indexes.add('Observation', 'combo-value-quantity', 'The value or component value of the observation, if the value is a Quantity, or a SampledData (just search on the bounds of the values in sampled data)', sptQUANTITY, [], 'Observation.value.ofType(Quantity) | Observation.value.ofType(SampledData) | Observation.component.value.ofType(Quantity) | Observation.component.value.ofType(SampledData)', sxpNormal);
  indexes.add('Observation', 'component-code', 'The component code of the observation type', sptTOKEN, [], 'Observation.component.code', sxpNormal);
  indexes.add('Observation', 'component-code-value-concept', 'Component code and component coded value parameter pair', sptCOMPOSITE, [], 'Observation.component', sxpNormal);
  indexes.add('Observation', 'component-code-value-quantity', 'Component code and component quantity value parameter pair', sptCOMPOSITE, [], 'Observation.component', sxpNormal);
  indexes.add('Observation', 'component-data-absent-reason', 'The reason why the expected value in the element Observation.component.value[x] is missing.', sptTOKEN, [], 'Observation.component.dataAbsentReason', sxpNormal);
  indexes.add('Observation', 'component-value-concept', 'The value of the component observation, if the value is a CodeableConcept', sptTOKEN, [], 'Observation.component.value.ofType(CodeableConcept)', sxpNormal);
  indexes.add('Observation', 'component-value-quantity', 'The value of the component observation, if the value is a Quantity, or a SampledData (just search on the bounds of the values in sampled data)', sptQUANTITY, [], 'Observation.component.value.ofType(Quantity) | Observation.component.value.ofType(SampledData)', sxpNormal);
  indexes.add('Observation', 'data-absent-reason', 'The reason why the expected value in the element Observation.value[x] is missing.', sptTOKEN, [], 'Observation.dataAbsentReason', sxpNormal);
  indexes.add('Observation', 'date', '): Obtained date/time. If the obtained element is a period, a date that falls in the period', sptDATE, [], 'AllergyIntolerance.recordedDate | CarePlan.period | ClinicalImpression.date | Composition.date | Consent.dateTime | DiagnosticReport.effective | Encounter.actualPeriod | EpisodeOfCare.period | FamilyMemberHistory.date | Flag.period | (Immunization.oc'
      +'currence as dateTime) | List.date | Observation.effective | Procedure.occurrence | (RiskAssessment.occurrence as dateTime) | SupplyRequest.authoredOn', sxpNormal);
  indexes.add('Observation', 'derived-from', 'Related measurements the observation is made from', sptREFERENCE, ['GenomicStudy', 'Observation', 'ImagingStudy', 'MolecularSequence', 'ImagingSelection', 'QuestionnaireResponse', 'DocumentReference'], 'Observation.derivedFrom', sxpNormal);
  indexes.add('Observation', 'device', 'The Device that generated the observation data.', sptREFERENCE, ['Device', 'DeviceMetric'], 'Observation.device', sxpNormal);
  indexes.add('Observation', 'encounter', '): Encounter related to the observation', sptREFERENCE, ['Encounter'], 'Composition.encounter | DeviceRequest.encounter | DiagnosticReport.encounter | Flag.encounter | List.encounter | NutritionOrder.encounter | Observation.encounter | Procedure.encounter | RiskAssessment.encounter | ServiceRequest.encounter | VisionPres'
      +'cription.encounter', sxpNormal);
  indexes.add('Observation', 'focus', 'The focus of an observation when the focus is not the patient of record.', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'Observation.focus', sxpNormal);
  indexes.add('Observation', 'has-member', 'Related resource that belongs to the Observation group', sptREFERENCE, ['Observation', 'MolecularSequence', 'QuestionnaireResponse'], 'Observation.hasMember', sxpNormal);
  indexes.add('Observation', 'identifier', '): The unique id for a particular observation', sptTOKEN, [], 'AllergyIntolerance.identifier | CarePlan.identifier | CareTeam.identifier | Composition.identifier | Condition.identifier | Consent.identifier | DetectedIssue.identifier | DeviceRequest.identifier | DiagnosticReport.identifier | DocumentManifest.mast'
      +'erIdentifier | DocumentManifest.identifier | DocumentReference.identifier | Encounter.identifier | EpisodeOfCare.identifier | FamilyMemberHistory.identifier | Goal.identifier | ImagingStudy.identifier | Immunization.identifier | List.identifi'
      +'er | MedicationAdministration.identifier | MedicationDispense.identifier | MedicationRequest.identifier | MedicationUsage.identifier | NutritionOrder.identifier | Observation.identifier | Procedure.identifier | RiskAssessment.identifier | Ser'
      +'viceRequest.identifier | SupplyDelivery.identifier | SupplyRequest.identifier | VisionPrescription.identifier', sxpNormal);
  indexes.add('Observation', 'method', 'The method used for the observation', sptTOKEN, [], 'Observation.method', sxpNormal);
  indexes.add('Observation', 'part-of', 'Part of referenced event', sptREFERENCE, ['GenomicStudy', 'Immunization', 'MedicationDispense', 'MedicationAdministration', 'Procedure', 'ImagingStudy', 'MedicationUsage'], 'Observation.partOf', sxpNormal);
  indexes.add('Observation', 'patient', '): The subject that the observation is about (if patient)', sptREFERENCE, ['Patient', 'Practitioner', 'Group', 'Organization', 'BiologicallyDerivedProduct', 'NutritionProduct', 'Device', 'Medication', 'Procedure', 'Substance', 'Location'], 'AllergyIntolerance.patient | CarePlan.subject.where(resolve() is Patient) | CareTeam.subject.where(resolve() is Patient) | ClinicalImpression.subject.where(resolve() is Patient) | Composition.subject.where(resolve() is Patient) | Condition.subject.wh'
      +'ere(resolve() is Patient) | Consent.subject.where(resolve() is Patient) | DetectedIssue.subject | DeviceRequest.subject.where(resolve() is Patient) | DeviceUsage.patient | DiagnosticReport.subject.where(resolve() is Patient) | DocumentManifes'
      +'t.subject.where(resolve() is Patient) | DocumentReference.subject.where(resolve() is Patient) | Encounter.subject.where(resolve() is Patient) | EpisodeOfCare.patient | FamilyMemberHistory.patient | Flag.s'+
   'ubject.where(resolve() is Patient) | G'
      +'oal.subject.where(resolve() is Patient) | ImagingStudy.subject.where(resolve() is Patient) | Immunization.patient | List.subject.where(resolve() is Patient) | MedicationAdministration.subject.where(resolve() is Patient) | MedicationDispense.s'
      +'ubject.where(resolve() is Patient) | MedicationRequest.subject.where(resolve() is Patient) | MedicationUsage.subject.where(resolve() is Patient) | NutritionOrder.subject.where(resolve() is Patient) | Observation.subject.where(resolve() is Pat'
      +'ient) | Procedure.subject.where(resolve() is Patient) | RiskAssessment.subject.where(resolve() is Patient) | ServiceRequest.subject.where(resolve() is Patient) | SupplyDelivery.patient | VisionPrescription.patient', sxpNormal);
  indexes.add('Observation', 'performer', 'Who performed the observation', sptREFERENCE, ['Practitioner', 'Organization', 'CareTeam', 'Patient', 'PractitionerRole', 'RelatedPerson'], 'Observation.performer', sxpNormal);
  indexes.add('Observation', 'specimen', 'Specimen used for this observation', sptREFERENCE, ['Specimen', 'Group'], 'Observation.specimen', sxpNormal);
  indexes.add('Observation', 'status', 'The status of the observation', sptTOKEN, [], 'Observation.status', sxpNormal);
  indexes.add('Observation', 'subject', 'The subject that the observation is about', sptREFERENCE, ['Practitioner', 'Group', 'Organization', 'BiologicallyDerivedProduct', 'NutritionProduct', 'Device', 'Medication', 'Patient', 'Procedure', 'Substance', 'Location'], 'Observation.subject', sxpNormal);
  indexes.add('Observation', 'value-concept', 'The value of the observation, if the value is a CodeableConcept', sptTOKEN, [], 'Observation.value.ofType(CodeableConcept)', sxpNormal);
  indexes.add('Observation', 'value-date', 'The value of the observation, if the value is a date or period of time', sptDATE, [], 'Observation.value.ofType(dateTime) | Observation.value.ofType(Period)', sxpNormal);
  indexes.add('Observation', 'value-quantity', 'The value of the observation, if the value is a Quantity, or a SampledData (just search on the bounds of the values in sampled data)', sptQUANTITY, [], 'Observation.value.ofType(Quantity) | Observation.value.ofType(SampledData)', sxpNormal);
  indexes.add('Observation', 'value-string', 'The value of the observation, if the value is a string, and also searches in CodeableConcept.text', sptSTRING, [], 'Observation.value.ofType(string) | Observation.value.ofType(CodeableConcept).text', sxpNormal);
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
  indexes.add('ObservationDefinition', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('ObservationDefinition', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('ObservationDefinition', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('ObservationDefinition', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('ObservationDefinition', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('ObservationDefinition', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('ObservationDefinition', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('ObservationDefinition', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('ObservationDefinition', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('ObservationDefinition', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('ObservationDefinition', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('ObservationDefinition', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('ObservationDefinition', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('ObservationDefinition', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('ObservationDefinition', 'category', 'Category (class) of observation', sptTOKEN, [], 'ObservationDefinition.category', sxpNormal);
  indexes.add('ObservationDefinition', 'code', 'Observation code', sptTOKEN, [], 'ObservationDefinition.code', sxpNormal);
  indexes.add('ObservationDefinition', 'experimental', 'Not for genuine usage (true)', sptTOKEN, [], 'ObservationDefinition.experimental', sxpNormal);
  indexes.add('ObservationDefinition', 'identifier', 'The unique identifier associated with the specimen definition', sptTOKEN, [], 'ObservationDefinition.identifier', sxpNormal);
  indexes.add('ObservationDefinition', 'method', 'Method of observation', sptTOKEN, [], 'ObservationDefinition.method', sxpNormal);
  indexes.add('ObservationDefinition', 'status', 'Publication status of the ObservationDefinition: draft, active, retired, unknown', sptTOKEN, [], 'ObservationDefinition.status', sxpNormal);
  indexes.add('ObservationDefinition', 'title', 'Human-friendly name of the ObservationDefinition', sptSTRING, [], 'ObservationDefinition.title', sxpNormal);
  indexes.add('ObservationDefinition', 'url', 'The uri that identifies the observation definition', sptURI, [], 'ObservationDefinition.url', sxpNormal);
  indexes.add('ObservationDefinition', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_OBSERVATIONDEFINITION}

{$IFDEF FHIR_OPERATIONDEFINITION}
procedure TFHIRIndexBuilderR5.buildIndexesForOperationDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('OperationDefinition', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('OperationDefinition', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('OperationDefinition', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('OperationDefinition', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('OperationDefinition', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('OperationDefinition', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('OperationDefinition', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('OperationDefinition', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('OperationDefinition', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('OperationDefinition', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('OperationDefinition', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('OperationDefinition', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('OperationDefinition', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('OperationDefinition', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('OperationDefinition', 'base', 'Marks this as a profile of the base', sptREFERENCE, ['OperationDefinition'], 'OperationDefinition.base', sxpNormal);
  indexes.add('OperationDefinition', 'code', 'Name used to invoke the operation', sptTOKEN, [], 'OperationDefinition.code', sxpNormal);
  indexes.add('OperationDefinition', 'context', '): A use context assigned to the operation definition', sptTOKEN, [], '(CapabilityStatement.useContext.value as CodeableConcept) | (CodeSystem.useContext.value as CodeableConcept) | (CompartmentDefinition.useContext.value as CodeableConcept) | (ConceptMap.useContext.value as CodeableConcept) | (GraphDefinition.useContex'
      +'t.value as CodeableConcept) | (ImplementationGuide.useContext.value as CodeableConcept) | (MessageDefinition.useContext.value as CodeableConcept) | (NamingSystem.useContext.value as CodeableConcept) | (OperationDefinition.useContext.value as '
      +'CodeableConcept) | (SearchParameter.useContext.value as CodeableConcept) | (StructureDefinition.useContext.value as CodeableConcept) | (StructureMap.useContext.value as CodeableConcept) | (TerminologyCapabilities.useContext.value as CodeableC'
      +'oncept) | (ValueSet.useContext.value as CodeableConcept)', sxpNormal);
  indexes.add('OperationDefinition', 'context-quantity', '): A quantity- or range-valued use context assigned to the operation definition', sptQUANTITY, [], '(CapabilityStatement.useContext.value as Quantity) | (CapabilityStatement.useContext.value as Range) | (CodeSystem.useContext.value as Quantity) | (CodeSystem.useContext.value as Range) | (CompartmentDefinition.useContext.value as Quantity) | (Compar'
      +'tmentDefinition.useContext.value as Range) | (ConceptMap.useContext.value as Quantity) | (ConceptMap.useContext.value as Range) | (GraphDefinition.useContext.value as Quantity) | (GraphDefinition.useContext.value as Range) | (ImplementationGu'
      +'ide.useContext.value as Quantity) | (ImplementationGuide.useContext.value as Range) | (MessageDefinition.useContext.value as Quantity) | (MessageDefinition.useContext.value as Range) | (NamingSystem.useContext.value as Quantity) | (NamingSyst'
      +'em.useContext.value as Range) | (OperationDefinition.useContext.value as Qu'+
   'antity) | (OperationDefinition.useContext.value as Range) | (SearchParameter.useContext.value as Quantity) | (SearchParameter.useContext.value as Range) | (StructureDe'
      +'finition.useContext.value as Quantity) | (StructureDefinition.useContext.value as Range) | (StructureMap.useContext.value as Quantity) | (StructureMap.useContext.value as Range) | (TerminologyCapabilities.useContext.value as Quantity) | (Term'
      +'inologyCapabilities.useContext.value as Range) | (ValueSet.useContext.value as Quantity) | (ValueSet.useContext.value as Range)', sxpNormal);
  indexes.add('OperationDefinition', 'context-type', '): A type of use context assigned to the operation definition', sptTOKEN, [], 'CapabilityStatement.useContext.code | CodeSystem.useContext.code | CompartmentDefinition.useContext.code | ConceptMap.useContext.code | GraphDefinition.useContext.code | ImplementationGuide.useContext.code | MessageDefinition.useContext.code | Naming'
      +'System.useContext.code | OperationDefinition.useContext.code | SearchParameter.useContext.code | StructureDefinition.useContext.code | StructureMap.useContext.code | TerminologyCapabilities.useContext.code | ValueSet.useContext.code', sxpNormal);
  indexes.add('OperationDefinition', 'context-type-quantity', '): A use context type and quantity- or range-based value assigned to the operation definition', sptCOMPOSITE, [], 'CapabilityStatement.useContext | CodeSystem.useContext | CompartmentDefinition.useContext | ConceptMap.useContext | GraphDefinition.useContext | ImplementationGuide.useContext | MessageDefinition.useContext | NamingSystem.useContext | OperationDefini'
      +'tion.useContext | SearchParameter.useContext | StructureDefinition.useContext | StructureMap.useContext | TerminologyCapabilities.useContext | ValueSet.useContext', sxpNormal);
  indexes.add('OperationDefinition', 'context-type-value', '): A use context type and value assigned to the operation definition', sptCOMPOSITE, [], 'CapabilityStatement.useContext | CodeSystem.useContext | CompartmentDefinition.useContext | ConceptMap.useContext | GraphDefinition.useContext | ImplementationGuide.useContext | MessageDefinition.useContext | NamingSystem.useContext | OperationDefini'
      +'tion.useContext | SearchParameter.useContext | StructureDefinition.useContext | StructureMap.useContext | TerminologyCapabilities.useContext | ValueSet.useContext', sxpNormal);
  indexes.add('OperationDefinition', 'date', '): The operation definition publication date', sptDATE, [], 'CapabilityStatement.date | CodeSystem.date | CompartmentDefinition.date | ConceptMap.date | GraphDefinition.date | ImplementationGuide.date | MessageDefinition.date | NamingSystem.date | OperationDefinition.date | SearchParameter.date | StructureDefi'
      +'nition.date | StructureMap.date | TerminologyCapabilities.date | ValueSet.date', sxpNormal);
  indexes.add('OperationDefinition', 'description', '): The description of the operation definition', sptSTRING, [], 'CapabilityStatement.description | CodeSystem.description | CompartmentDefinition.description | ConceptMap.description | GraphDefinition.description | ImplementationGuide.description | MessageDefinition.description | NamingSystem.description | Operati'
      +'onDefinition.description | SearchParameter.description | StructureDefinition.description | StructureMap.description | TerminologyCapabilities.description | ValueSet.description', sxpNormal);
  indexes.add('OperationDefinition', 'input-profile', 'Validation information for in parameters', sptREFERENCE, ['StructureDefinition'], 'OperationDefinition.inputProfile', sxpNormal);
  indexes.add('OperationDefinition', 'instance', 'Invoke on an instance?', sptTOKEN, [], 'OperationDefinition.instance', sxpNormal);
  indexes.add('OperationDefinition', 'jurisdiction', '): Intended jurisdiction for the operation definition', sptTOKEN, [], 'CapabilityStatement.jurisdiction | CodeSystem.jurisdiction | ConceptMap.jurisdiction | GraphDefinition.jurisdiction | ImplementationGuide.jurisdiction | MessageDefinition.jurisdiction | NamingSystem.jurisdiction | OperationDefinition.jurisdiction | S'
      +'earchParameter.jurisdiction | StructureDefinition.jurisdiction | StructureMap.jurisdiction | TerminologyCapabilities.jurisdiction | ValueSet.jurisdiction', sxpNormal);
  indexes.add('OperationDefinition', 'kind', 'operation | query', sptTOKEN, [], 'OperationDefinition.kind', sxpNormal);
  indexes.add('OperationDefinition', 'name', '): Computationally friendly name of the operation definition', sptSTRING, [], 'CapabilityStatement.name | CodeSystem.name | CompartmentDefinition.name | ConceptMap.name | GraphDefinition.name | ImplementationGuide.name | MessageDefinition.name | NamingSystem.name | OperationDefinition.name | SearchParameter.name | StructureDefi'
      +'nition.name | StructureMap.name | TerminologyCapabilities.name | ValueSet.name', sxpNormal);
  indexes.add('OperationDefinition', 'output-profile', 'Validation information for out parameters', sptREFERENCE, ['StructureDefinition'], 'OperationDefinition.outputProfile', sxpNormal);
  indexes.add('OperationDefinition', 'publisher', '): Name of the publisher of the operation definition', sptSTRING, [], 'CapabilityStatement.publisher | CodeSystem.publisher | CompartmentDefinition.publisher | ConceptMap.publisher | GraphDefinition.publisher | ImplementationGuide.publisher | MessageDefinition.publisher | NamingSystem.publisher | OperationDefinition.pub'
      +'lisher | SearchParameter.publisher | StructureDefinition.publisher | StructureMap.publisher | TerminologyCapabilities.publisher | ValueSet.publisher', sxpNormal);
  indexes.add('OperationDefinition', 'status', '): The current status of the operation definition', sptTOKEN, [], 'CapabilityStatement.status | CodeSystem.status | CompartmentDefinition.status | ConceptMap.status | GraphDefinition.status | ImplementationGuide.status | MessageDefinition.status | NamingSystem.status | OperationDefinition.status | SearchParameter.st'
      +'atus | StructureDefinition.status | StructureMap.status | TerminologyCapabilities.status | ValueSet.status', sxpNormal);
  indexes.add('OperationDefinition', 'system', 'Invoke at the system level?', sptTOKEN, [], 'OperationDefinition.system', sxpNormal);
  indexes.add('OperationDefinition', 'title', '): The human-friendly name of the operation definition', sptSTRING, [], 'CapabilityStatement.title | CodeSystem.title | ConceptMap.title | ImplementationGuide.title | MessageDefinition.title | OperationDefinition.title | StructureDefinition.title | StructureMap.title | TerminologyCapabilities.title | ValueSet.title', sxpNormal);
  indexes.add('OperationDefinition', 'type', 'Invoke at the type level?', sptTOKEN, [], 'OperationDefinition.type', sxpNormal);
  indexes.add('OperationDefinition', 'url', '): The uri that identifies the operation definition', sptURI, [], 'CapabilityStatement.url | CodeSystem.url | CompartmentDefinition.url | ConceptMap.url | GraphDefinition.url | ImplementationGuide.url | MessageDefinition.url | NamingSystem.url | OperationDefinition.url | SearchParameter.url | StructureDefinition.url'
      +' | StructureMap.url | TerminologyCapabilities.url | ValueSet.url', sxpNormal);
  indexes.add('OperationDefinition', 'version', '): The business version of the operation definition', sptTOKEN, [], 'CapabilityStatement.version | CodeSystem.version | CompartmentDefinition.version | ConceptMap.version | GraphDefinition.version | ImplementationGuide.version | MessageDefinition.version | NamingSystem.version | OperationDefinition.version | SearchPar'
      +'ameter.version | StructureDefinition.version | StructureMap.version | TerminologyCapabilities.version | ValueSet.version', sxpNormal);
  indexes.add('OperationDefinition', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_OPERATIONDEFINITION}

{$IFDEF FHIR_OPERATIONOUTCOME}
procedure TFHIRIndexBuilderR5.buildIndexesForOperationOutcome(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('OperationOutcome', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('OperationOutcome', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('OperationOutcome', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('OperationOutcome', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('OperationOutcome', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('OperationOutcome', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('OperationOutcome', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('OperationOutcome', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('OperationOutcome', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('OperationOutcome', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('OperationOutcome', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('OperationOutcome', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('OperationOutcome', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('OperationOutcome', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('OperationOutcome', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_OPERATIONOUTCOME}

{$IFDEF FHIR_ORGANIZATION}
procedure TFHIRIndexBuilderR5.buildIndexesForOrganization(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Organization', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('Organization', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('Organization', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('Organization', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('Organization', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('Organization', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('Organization', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('Organization', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('Organization', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('Organization', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('Organization', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('Organization', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('Organization', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('Organization', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('Organization', 'active', 'Is the Organization record active', sptTOKEN, [], 'Organization.active', sxpNormal);
  indexes.add('Organization', 'address', 'A server defined search that may match any of the string fields in the Address, including line, city, district, state, country, postalCode, and/or text', sptSTRING, [], 'Organization.contact.address', sxpNormal);
  indexes.add('Organization', 'address-city', 'A city specified in an address', sptSTRING, [], 'Organization.contact.address.city', sxpNormal);
  indexes.add('Organization', 'address-country', 'A country specified in an address', sptSTRING, [], 'Organization.contact.address.country', sxpNormal);
  indexes.add('Organization', 'address-postalcode', 'A postal code specified in an address', sptSTRING, [], 'Organization.contact.address.postalCode', sxpNormal);
  indexes.add('Organization', 'address-state', 'A state specified in an address', sptSTRING, [], 'Organization.contact.address.state', sxpNormal);
  indexes.add('Organization', 'address-use', 'A use code specified in an address', sptTOKEN, [], 'Organization.contact.address.use', sxpNormal);
  indexes.add('Organization', 'endpoint', 'Technical endpoints providing access to services operated for the organization', sptREFERENCE, ['Endpoint'], 'Organization.endpoint', sxpNormal);
  indexes.add('Organization', 'identifier', 'Any identifier for the organization (not the accreditation issuer''s identifier)', sptTOKEN, [], 'Organization.identifier | Organization.qualification.identifier', sxpNormal);
  indexes.add('Organization', 'name', 'A portion of the organization''s name or alias', sptSTRING, [], 'Organization.name | Organization.alias', sxpNormal);
  indexes.add('Organization', 'partof', 'An organization of which this organization forms a part', sptREFERENCE, ['Organization'], 'Organization.partOf', sxpNormal);
  indexes.add('Organization', 'phonetic', 'A portion of the organization''s name using some kind of phonetic matching algorithm', sptSTRING, [], 'Organization.name', sxpNormal);
  indexes.add('Organization', 'type', 'A code for the type of organization', sptTOKEN, [], 'Organization.type', sxpNormal);
  indexes.add('Organization', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_ORGANIZATION}

{$IFDEF FHIR_ORGANIZATIONAFFILIATION}
procedure TFHIRIndexBuilderR5.buildIndexesForOrganizationAffiliation(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('OrganizationAffiliation', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('OrganizationAffiliation', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('OrganizationAffiliation', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('OrganizationAffiliation', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('OrganizationAffiliation', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('OrganizationAffiliation', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('OrganizationAffiliation', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('OrganizationAffiliation', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('OrganizationAffiliation', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('OrganizationAffiliation', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('OrganizationAffiliation', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('OrganizationAffiliation', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('OrganizationAffiliation', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('OrganizationAffiliation', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('OrganizationAffiliation', 'active', 'Whether this organization affiliation record is in active use', sptTOKEN, [], 'OrganizationAffiliation.active', sxpNormal);
  indexes.add('OrganizationAffiliation', 'date', 'The period during which the participatingOrganization is affiliated with the primary organization', sptDATE, [], 'OrganizationAffiliation.period', sxpNormal);
  indexes.add('OrganizationAffiliation', 'email', 'A value in an email contact', sptTOKEN, [], 'OrganizationAffiliation.contact.telecom.where(system=''email'')', sxpNormal);
  indexes.add('OrganizationAffiliation', 'endpoint', 'Technical endpoints providing access to services operated for this role', sptREFERENCE, ['Endpoint'], 'OrganizationAffiliation.endpoint', sxpNormal);
  indexes.add('OrganizationAffiliation', 'identifier', 'An organization affiliation''s Identifier', sptTOKEN, [], 'OrganizationAffiliation.identifier', sxpNormal);
  indexes.add('OrganizationAffiliation', 'location', 'The location(s) at which the role occurs', sptREFERENCE, ['Location'], 'OrganizationAffiliation.location', sxpNormal);
  indexes.add('OrganizationAffiliation', 'network', 'Health insurance provider network in which the participatingOrganization provides the role''s services (if defined) at the indicated locations (if defined)', sptREFERENCE, ['Organization'], 'OrganizationAffiliation.network', sxpNormal);
  indexes.add('OrganizationAffiliation', 'participating-organization', 'The organization that provides services to the primary organization', sptREFERENCE, ['Organization'], 'OrganizationAffiliation.participatingOrganization', sxpNormal);
  indexes.add('OrganizationAffiliation', 'phone', 'A value in a phone contact', sptTOKEN, [], 'OrganizationAffiliation.contact.telecom.where(system=''phone'')', sxpNormal);
  indexes.add('OrganizationAffiliation', 'primary-organization', 'The organization that receives the services from the participating organization', sptREFERENCE, ['Organization'], 'OrganizationAffiliation.organization', sxpNormal);
  indexes.add('OrganizationAffiliation', 'role', 'Definition of the role the participatingOrganization plays', sptTOKEN, [], 'OrganizationAffiliation.code', sxpNormal);
  indexes.add('OrganizationAffiliation', 'service', 'Healthcare services provided through the role', sptREFERENCE, ['HealthcareService'], 'OrganizationAffiliation.healthcareService', sxpNormal);
  indexes.add('OrganizationAffiliation', 'specialty', 'Specific specialty of the participatingOrganization in the context of the role', sptTOKEN, [], 'OrganizationAffiliation.specialty', sxpNormal);
  indexes.add('OrganizationAffiliation', 'telecom', 'The value in any kind of contact', sptTOKEN, [], 'OrganizationAffiliation.contact.telecom', sxpNormal);
  indexes.add('OrganizationAffiliation', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_ORGANIZATIONAFFILIATION}

{$IFDEF FHIR_PACKAGEDPRODUCTDEFINITION}
procedure TFHIRIndexBuilderR5.buildIndexesForPackagedProductDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('PackagedProductDefinition', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('PackagedProductDefinition', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('PackagedProductDefinition', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('PackagedProductDefinition', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('PackagedProductDefinition', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('PackagedProductDefinition', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('PackagedProductDefinition', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('PackagedProductDefinition', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('PackagedProductDefinition', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('PackagedProductDefinition', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('PackagedProductDefinition', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('PackagedProductDefinition', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('PackagedProductDefinition', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('PackagedProductDefinition', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('PackagedProductDefinition', 'biological', 'A biologically derived product within this packaged product', sptREFERENCE, [], 'PackagedProductDefinition.packaging.containedItem.item.reference', sxpNormal);
  indexes.add('PackagedProductDefinition', 'contained-item', 'Any of the contained items within this packaged product', sptREFERENCE, [], 'PackagedProductDefinition.packaging.containedItem.item.reference', sxpNormal);
  indexes.add('PackagedProductDefinition', 'device', 'A device within this packaged product', sptREFERENCE, [], 'PackagedProductDefinition.packaging.containedItem.item.reference', sxpNormal);
  indexes.add('PackagedProductDefinition', 'identifier', 'Unique identifier', sptTOKEN, [], 'PackagedProductDefinition.identifier', sxpNormal);
  indexes.add('PackagedProductDefinition', 'manufactured-item', 'A manufactured item of medication within this packaged product', sptREFERENCE, [], 'PackagedProductDefinition.packaging.containedItem.item.reference', sxpNormal);
  indexes.add('PackagedProductDefinition', 'medication', 'A manufactured item of medication within this packaged product', sptREFERENCE, [], 'PackagedProductDefinition.packaging.containedItem.item.reference', sxpNormal);
  indexes.add('PackagedProductDefinition', 'name', 'A name for this package. Typically what it would be listed as in a drug formulary or catalogue, inventory etc', sptTOKEN, [], 'PackagedProductDefinition.name', sxpNormal);
  indexes.add('PackagedProductDefinition', 'nutrition', 'A nutrition product within this packaged product', sptREFERENCE, [], 'PackagedProductDefinition.packaging.containedItem.item.reference', sxpNormal);
  indexes.add('PackagedProductDefinition', 'package', 'A complete packaged product within this packaged product', sptREFERENCE, [], 'PackagedProductDefinition.packaging.containedItem.item.reference', sxpNormal);
  indexes.add('PackagedProductDefinition', 'package-for', 'The product that this is a pack for', sptREFERENCE, ['MedicinalProductDefinition'], 'PackagedProductDefinition.packageFor', sxpNormal);
  indexes.add('PackagedProductDefinition', 'status', 'The status within the lifecycle of this item. A high level status, this is not intended to duplicate details carried elsewhere such as legal status, or authorization or marketing status', sptTOKEN, [], 'PackagedProductDefinition.status', sxpNormal);
  indexes.add('PackagedProductDefinition', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_PACKAGEDPRODUCTDEFINITION}

{$IFDEF FHIR_PARAMETERS}
procedure TFHIRIndexBuilderR5.buildIndexesForParameters(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Parameters', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('Parameters', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('Parameters', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('Parameters', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('Parameters', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('Parameters', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('Parameters', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('Parameters', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('Parameters', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('Parameters', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('Parameters', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('Parameters', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('Parameters', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('Parameters', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('Parameters', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_PARAMETERS}

{$IFDEF FHIR_PATIENT}
procedure TFHIRIndexBuilderR5.buildIndexesForPatient(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Patient', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('Patient', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('Patient', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('Patient', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('Patient', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('Patient', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('Patient', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('Patient', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('Patient', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('Patient', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('Patient', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('Patient', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('Patient', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('Patient', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('Patient', 'active', 'Whether the patient record is active', sptTOKEN, [], 'Patient.active', sxpNormal);
  indexes.add('Patient', 'address', '): A server defined search that may match any of the string fields in the Address, including line, city, district, state, country, postalCode, and/or text', sptSTRING, [], 'Patient.address | Person.address | Practitioner.address | RelatedPerson.address', sxpNormal);
  indexes.add('Patient', 'address-city', '): A city specified in an address', sptSTRING, [], 'Patient.address.city | Person.address.city | Practitioner.address.city | RelatedPerson.address.city', sxpNormal);
  indexes.add('Patient', 'address-country', '): A country specified in an address', sptSTRING, [], 'Patient.address.country | Person.address.country | Practitioner.address.country | RelatedPerson.address.country', sxpNormal);
  indexes.add('Patient', 'address-postalcode', '): A postalCode specified in an address', sptSTRING, [], 'Patient.address.postalCode | Person.address.postalCode | Practitioner.address.postalCode | RelatedPerson.address.postalCode', sxpNormal);
  indexes.add('Patient', 'address-state', '): A state specified in an address', sptSTRING, [], 'Patient.address.state | Person.address.state | Practitioner.address.state | RelatedPerson.address.state', sxpNormal);
  indexes.add('Patient', 'address-use', '): A use code specified in an address', sptTOKEN, [], 'Patient.address.use | Person.address.use | Practitioner.address.use | RelatedPerson.address.use', sxpNormal);
  indexes.add('Patient', 'age', 'Searches for patients based on age as calculated based on current date and date of birth.  Deceased patients are excluded from the search.', sptNUMBER, [], 'Patient.birthDate', sxpNormal);
  indexes.add('Patient', 'birthOrderBoolean', 'Search based on whether a patient was part of a multiple birth or not.', sptTOKEN, [], 'Patient.multipleBirthBoolean | Patient.multipleBirthInteger', sxpNormal);
  indexes.add('Patient', 'birthdate', '): The patient''s date of birth', sptDATE, [], 'Patient.birthDate | Person.birthDate | RelatedPerson.birthDate', sxpNormal);
  indexes.add('Patient', 'death-date', 'The date of death has been provided and satisfies this search value', sptDATE, [], '(Patient.deceased as dateTime)', sxpNormal);
  indexes.add('Patient', 'deceased', 'This patient has been marked as deceased, or has a death date entered', sptTOKEN, [], 'Patient.deceased.exists() and Patient.deceased != false', sxpNormal);
  indexes.add('Patient', 'email', '): A value in an email contact', sptTOKEN, [], 'Patient.telecom.where(system=''email'') | Person.telecom.where(system=''email'') | Practitioner.telecom.where(system=''email'') | PractitionerRole.contact.telecom.where(system=''email'') | RelatedPerson.telecom.where(system=''email'')', sxpNormal);
  indexes.add('Patient', 'family', '): A portion of the family name of the patient', sptSTRING, [], 'Patient.name.family | Practitioner.name.family', sxpNormal);
  indexes.add('Patient', 'gender', '): Gender of the patient', sptTOKEN, [], 'Patient.gender | Person.gender | Practitioner.gender | RelatedPerson.gender', sxpNormal);
  indexes.add('Patient', 'general-practitioner', 'Patient''s nominated general practitioner, not the organization that manages the record', sptREFERENCE, ['Practitioner', 'Organization', 'PractitionerRole'], 'Patient.generalPractitioner', sxpNormal);
  indexes.add('Patient', 'given', '): A portion of the given name of the patient', sptSTRING, [], 'Patient.name.given | Practitioner.name.given', sxpNormal);
  indexes.add('Patient', 'identifier', 'A patient identifier', sptTOKEN, [], 'Patient.identifier', sxpNormal);
  indexes.add('Patient', 'language', 'Language code (irrespective of use value)', sptTOKEN, [], 'Patient.communication.language', sxpNormal);
  indexes.add('Patient', 'link', 'All patients/related persons linked to the given patient', sptREFERENCE, ['Patient', 'RelatedPerson'], 'Patient.link.other', sxpNormal);
  indexes.add('Patient', 'mothersMaidenName', 'Search based on patient''s mother''s maiden name', sptSTRING, [], 'Patient.extension(''http://hl7.org/fhir/StructureDefinition/patient-mothersMaidenName'').value', sxpNormal);
  indexes.add('Patient', 'name', 'A server defined search that may match any of the string fields in the HumanName, including family, given, prefix, suffix, and/or text', sptSTRING, [], 'Patient.name', sxpNormal);
  indexes.add('Patient', 'organization', 'The organization that is the custodian of the patient record', sptREFERENCE, ['Organization'], 'Patient.managingOrganization', sxpNormal);
  indexes.add('Patient', 'phone', '): A value in a phone contact', sptTOKEN, [], 'Patient.telecom.where(system=''phone'') | Person.telecom.where(system=''phone'') | Practitioner.telecom.where(system=''phone'') | PractitionerRole.contact.telecom.where(system=''phone'') | RelatedPerson.telecom.where(system=''phone'')', sxpNormal);
  indexes.add('Patient', 'phonetic', '): A portion of either family or given name using some kind of phonetic matching algorithm', sptSTRING, [], 'Patient.name | Person.name | Practitioner.name | RelatedPerson.name', sxpNormal);
  indexes.add('Patient', 'telecom', '): The value in any kind of telecom details of the patient', sptTOKEN, [], 'Patient.telecom | Person.telecom | Practitioner.telecom | PractitionerRole.contact.telecom | RelatedPerson.telecom', sxpNormal);
  indexes.add('Patient', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Patient', 'Patient', ['{def}', 'link']);
  compartments.register('Practitioner', 'Patient', ['general-practitioner']);
  compartments.register('RelatedPerson', 'Patient', ['link']);
end;
{$ENDIF FHIR_PATIENT}

{$IFDEF FHIR_PAYMENTNOTICE}
procedure TFHIRIndexBuilderR5.buildIndexesForPaymentNotice(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('PaymentNotice', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('PaymentNotice', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('PaymentNotice', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('PaymentNotice', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('PaymentNotice', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('PaymentNotice', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('PaymentNotice', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('PaymentNotice', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('PaymentNotice', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('PaymentNotice', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('PaymentNotice', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('PaymentNotice', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('PaymentNotice', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('PaymentNotice', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('PaymentNotice', 'created', 'Creation date for the notice', sptDATE, [], 'PaymentNotice.created', sxpNormal);
  indexes.add('PaymentNotice', 'identifier', 'The business identifier of the notice', sptTOKEN, [], 'PaymentNotice.identifier', sxpNormal);
  indexes.add('PaymentNotice', 'payment-status', 'The type of payment notice', sptTOKEN, [], 'PaymentNotice.paymentStatus', sxpNormal);
  indexes.add('PaymentNotice', 'provider', 'The reference to the provider', sptREFERENCE, ['Practitioner', 'Organization', 'PractitionerRole'], 'PaymentNotice.provider', sxpNormal);
  indexes.add('PaymentNotice', 'request', 'The Claim', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'PaymentNotice.request', sxpNormal);
  indexes.add('PaymentNotice', 'response', 'The ClaimResponse', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'PaymentNotice.response', sxpNormal);
  indexes.add('PaymentNotice', 'status', 'The status of the payment notice', sptTOKEN, [], 'PaymentNotice.status', sxpNormal);
  indexes.add('PaymentNotice', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Practitioner', 'PaymentNotice', ['provider']);
end;
{$ENDIF FHIR_PAYMENTNOTICE}

{$IFDEF FHIR_PAYMENTRECONCILIATION}
procedure TFHIRIndexBuilderR5.buildIndexesForPaymentReconciliation(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('PaymentReconciliation', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('PaymentReconciliation', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('PaymentReconciliation', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('PaymentReconciliation', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('PaymentReconciliation', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('PaymentReconciliation', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('PaymentReconciliation', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('PaymentReconciliation', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('PaymentReconciliation', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('PaymentReconciliation', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('PaymentReconciliation', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('PaymentReconciliation', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('PaymentReconciliation', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('PaymentReconciliation', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('PaymentReconciliation', 'allocation-account', 'The account to which payment or adjustment was applied.', sptREFERENCE, ['Account'], 'PaymentReconciliation.allocation.account', sxpNormal);
  indexes.add('PaymentReconciliation', 'allocation-encounter', 'The encounter to which payment or adjustment was applied.', sptREFERENCE, ['Encounter'], 'PaymentReconciliation.allocation.encounter', sxpNormal);
  indexes.add('PaymentReconciliation', 'created', 'The creation date', sptDATE, [], 'PaymentReconciliation.created', sxpNormal);
  indexes.add('PaymentReconciliation', 'disposition', 'The contents of the disposition message', sptSTRING, [], 'PaymentReconciliation.disposition', sxpNormal);
  indexes.add('PaymentReconciliation', 'identifier', 'The business identifier of the ExplanationOfBenefit', sptTOKEN, [], 'PaymentReconciliation.identifier', sxpNormal);
  indexes.add('PaymentReconciliation', 'outcome', 'The processing outcome', sptTOKEN, [], 'PaymentReconciliation.outcome', sxpNormal);
  indexes.add('PaymentReconciliation', 'payment-issuer', 'The organization which generated this resource', sptREFERENCE, ['Organization', 'Patient', 'Person', 'RelatedPerson'], 'PaymentReconciliation.paymentIssuer', sxpNormal);
  indexes.add('PaymentReconciliation', 'request', 'The reference to the claim', sptREFERENCE, ['Task'], 'PaymentReconciliation.request', sxpNormal);
  indexes.add('PaymentReconciliation', 'requestor', 'The reference to the provider who submitted the claim', sptREFERENCE, ['Practitioner', 'Organization', 'PractitionerRole'], 'PaymentReconciliation.requestor', sxpNormal);
  indexes.add('PaymentReconciliation', 'status', 'The status of the payment reconciliation', sptTOKEN, [], 'PaymentReconciliation.status', sxpNormal);
  indexes.add('PaymentReconciliation', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Practitioner', 'PaymentReconciliation', ['requestor']);
end;
{$ENDIF FHIR_PAYMENTRECONCILIATION}

{$IFDEF FHIR_PERMISSION}
procedure TFHIRIndexBuilderR5.buildIndexesForPermission(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Permission', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('Permission', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('Permission', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('Permission', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('Permission', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('Permission', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('Permission', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('Permission', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('Permission', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('Permission', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('Permission', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('Permission', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('Permission', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('Permission', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('Permission', 'status', 'active | entered-in-error | draft | rejected', sptTOKEN, [], 'Permission.status', sxpNormal);
  indexes.add('Permission', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_PERMISSION}

{$IFDEF FHIR_PERSON}
procedure TFHIRIndexBuilderR5.buildIndexesForPerson(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Person', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('Person', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('Person', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('Person', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('Person', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('Person', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('Person', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('Person', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('Person', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('Person', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('Person', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('Person', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('Person', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('Person', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('Person', 'address', '): A server defined search that may match any of the string fields in the Address, including line, city, district, state, country, postalCode, and/or text', sptSTRING, [], 'Patient.address | Person.address | Practitioner.address | RelatedPerson.address', sxpNormal);
  indexes.add('Person', 'address-city', '): A city specified in an address', sptSTRING, [], 'Patient.address.city | Person.address.city | Practitioner.address.city | RelatedPerson.address.city', sxpNormal);
  indexes.add('Person', 'address-country', '): A country specified in an address', sptSTRING, [], 'Patient.address.country | Person.address.country | Practitioner.address.country | RelatedPerson.address.country', sxpNormal);
  indexes.add('Person', 'address-postalcode', '): A postal code specified in an address', sptSTRING, [], 'Patient.address.postalCode | Person.address.postalCode | Practitioner.address.postalCode | RelatedPerson.address.postalCode', sxpNormal);
  indexes.add('Person', 'address-state', '): A state specified in an address', sptSTRING, [], 'Patient.address.state | Person.address.state | Practitioner.address.state | RelatedPerson.address.state', sxpNormal);
  indexes.add('Person', 'address-use', '): A use code specified in an address', sptTOKEN, [], 'Patient.address.use | Person.address.use | Practitioner.address.use | RelatedPerson.address.use', sxpNormal);
  indexes.add('Person', 'birthdate', '): The person''s date of birth', sptDATE, [], 'Patient.birthDate | Person.birthDate | RelatedPerson.birthDate', sxpNormal);
  indexes.add('Person', 'death-date', 'The date of death has been provided and satisfies this search value', sptDATE, [], '(Person.deceased as dateTime)', sxpNormal);
  indexes.add('Person', 'deceased', 'This person has been marked as deceased, or has a death date entered', sptTOKEN, [], 'Person.deceased.exists() and Person.deceased != false', sxpNormal);
  indexes.add('Person', 'email', '): A value in an email contact', sptTOKEN, [], 'Patient.telecom.where(system=''email'') | Person.telecom.where(system=''email'') | Practitioner.telecom.where(system=''email'') | PractitionerRole.contact.telecom.where(system=''email'') | RelatedPerson.telecom.where(system=''email'')', sxpNormal);
  indexes.add('Person', 'family', 'A portion of the family name of the person', sptSTRING, [], 'Person.name.family', sxpNormal);
  indexes.add('Person', 'gender', '): The gender of the person', sptTOKEN, [], 'Patient.gender | Person.gender | Practitioner.gender | RelatedPerson.gender', sxpNormal);
  indexes.add('Person', 'given', 'A portion of the given name of the person', sptSTRING, [], 'Person.name.given', sxpNormal);
  indexes.add('Person', 'identifier', 'A person Identifier', sptTOKEN, [], 'Person.identifier', sxpNormal);
  indexes.add('Person', 'link', 'Any link has this Patient, Person, RelatedPerson or Practitioner reference', sptREFERENCE, ['Practitioner', 'Patient', 'Person', 'RelatedPerson'], 'Person.link.target', sxpNormal);
  indexes.add('Person', 'name', 'A server defined search that may match any of the string fields in the HumanName, including family, give, prefix, suffix, suffix, and/or text', sptSTRING, [], 'Person.name', sxpNormal);
  indexes.add('Person', 'organization', 'The organization at which this person record is being managed', sptREFERENCE, ['Organization'], 'Person.managingOrganization', sxpNormal);
  indexes.add('Person', 'patient', 'The Person links to this Patient', sptREFERENCE, ['Patient'], 'Person.link.target.where(resolve() is Patient)', sxpNormal);
  indexes.add('Person', 'phone', '): A value in a phone contact', sptTOKEN, [], 'Patient.telecom.where(system=''phone'') | Person.telecom.where(system=''phone'') | Practitioner.telecom.where(system=''phone'') | PractitionerRole.contact.telecom.where(system=''phone'') | RelatedPerson.telecom.where(system=''phone'')', sxpNormal);
  indexes.add('Person', 'phonetic', '): A portion of name using some kind of phonetic matching algorithm', sptSTRING, [], 'Patient.name | Person.name | Practitioner.name | RelatedPerson.name', sxpNormal);
  indexes.add('Person', 'practitioner', 'The Person links to this Practitioner', sptREFERENCE, ['Practitioner'], 'Person.link.target.where(resolve() is Practitioner)', sxpNormal);
  indexes.add('Person', 'relatedperson', 'The Person links to this RelatedPerson', sptREFERENCE, ['RelatedPerson'], 'Person.link.target.where(resolve() is RelatedPerson)', sxpNormal);
  indexes.add('Person', 'telecom', '): The value in any kind of contact', sptTOKEN, [], 'Patient.telecom | Person.telecom | Practitioner.telecom | PractitionerRole.contact.telecom | RelatedPerson.telecom', sxpNormal);
  indexes.add('Person', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Patient', 'Person', ['patient']);
  compartments.register('Practitioner', 'Person', ['practitioner']);
  compartments.register('RelatedPerson', 'Person', ['link']);
end;
{$ENDIF FHIR_PERSON}

{$IFDEF FHIR_PLANDEFINITION}
procedure TFHIRIndexBuilderR5.buildIndexesForPlanDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('PlanDefinition', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('PlanDefinition', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('PlanDefinition', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('PlanDefinition', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('PlanDefinition', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('PlanDefinition', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('PlanDefinition', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('PlanDefinition', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('PlanDefinition', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('PlanDefinition', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('PlanDefinition', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('PlanDefinition', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('PlanDefinition', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('PlanDefinition', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('PlanDefinition', 'composed-of', 'What resource is being referenced', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'PlanDefinition.relatedArtifact.where(type=''composed-of'').resource', sxpNormal);
  indexes.add('PlanDefinition', 'context', 'A use context assigned to the plan definition', sptTOKEN, [], '(PlanDefinition.useContext.value as CodeableConcept)', sxpNormal);
  indexes.add('PlanDefinition', 'context-quantity', 'A quantity- or range-valued use context assigned to the plan definition', sptQUANTITY, [], '(PlanDefinition.useContext.value as Quantity) | (PlanDefinition.useContext.value as Range)', sxpNormal);
  indexes.add('PlanDefinition', 'context-type', 'A type of use context assigned to the plan definition', sptTOKEN, [], 'PlanDefinition.useContext.code', sxpNormal);
  indexes.add('PlanDefinition', 'context-type-quantity', 'A use context type and quantity- or range-based value assigned to the plan definition', sptCOMPOSITE, [], 'PlanDefinition.useContext', sxpNormal);
  indexes.add('PlanDefinition', 'context-type-value', 'A use context type and value assigned to the plan definition', sptCOMPOSITE, [], 'PlanDefinition.useContext', sxpNormal);
  indexes.add('PlanDefinition', 'date', 'The plan definition publication date', sptDATE, [], 'PlanDefinition.date', sxpNormal);
  indexes.add('PlanDefinition', 'definition', 'Activity or plan definitions used by plan definition', sptREFERENCE, ['SpecimenDefinition', 'Questionnaire', 'ObservationDefinition', 'PlanDefinition', 'ActivityDefinition'], 'PlanDefinition.action.definition', sxpNormal);
  indexes.add('PlanDefinition', 'depends-on', 'What resource is being referenced', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'PlanDefinition.relatedArtifact.where(type=''depends-on'').resource | PlanDefinition.library', sxpNormal);
  indexes.add('PlanDefinition', 'derived-from', 'What resource is being referenced', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'PlanDefinition.relatedArtifact.where(type=''derived-from'').resource', sxpNormal);
  indexes.add('PlanDefinition', 'description', 'The description of the plan definition', sptSTRING, [], 'PlanDefinition.description', sxpNormal);
  indexes.add('PlanDefinition', 'effective', 'The time during which the plan definition is intended to be in use', sptDATE, [], 'PlanDefinition.effectivePeriod', sxpNormal);
  indexes.add('PlanDefinition', 'identifier', 'External identifier for the plan definition', sptTOKEN, [], 'PlanDefinition.identifier', sxpNormal);
  indexes.add('PlanDefinition', 'jurisdiction', 'Intended jurisdiction for the plan definition', sptTOKEN, [], 'PlanDefinition.jurisdiction', sxpNormal);
  indexes.add('PlanDefinition', 'name', 'Computationally friendly name of the plan definition', sptSTRING, [], 'PlanDefinition.name', sxpNormal);
  indexes.add('PlanDefinition', 'predecessor', 'What resource is being referenced', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'PlanDefinition.relatedArtifact.where(type=''predecessor'').resource', sxpNormal);
  indexes.add('PlanDefinition', 'publisher', 'Name of the publisher of the plan definition', sptSTRING, [], 'PlanDefinition.publisher', sxpNormal);
  indexes.add('PlanDefinition', 'status', 'The current status of the plan definition', sptTOKEN, [], 'PlanDefinition.status', sxpNormal);
  indexes.add('PlanDefinition', 'successor', 'What resource is being referenced', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'PlanDefinition.relatedArtifact.where(type=''successor'').resource', sxpNormal);
  indexes.add('PlanDefinition', 'title', 'The human-friendly name of the plan definition', sptSTRING, [], 'PlanDefinition.title', sxpNormal);
  indexes.add('PlanDefinition', 'topic', 'Topics associated with the module', sptTOKEN, [], 'PlanDefinition.topic', sxpNormal);
  indexes.add('PlanDefinition', 'type', 'The type of artifact the plan (e.g. order-set, eca-rule, protocol)', sptTOKEN, [], 'PlanDefinition.type', sxpNormal);
  indexes.add('PlanDefinition', 'url', 'The uri that identifies the plan definition', sptURI, [], 'PlanDefinition.url', sxpNormal);
  indexes.add('PlanDefinition', 'version', 'The business version of the plan definition', sptTOKEN, [], 'PlanDefinition.version', sxpNormal);
  indexes.add('PlanDefinition', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_PLANDEFINITION}

{$IFDEF FHIR_PRACTITIONER}
procedure TFHIRIndexBuilderR5.buildIndexesForPractitioner(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Practitioner', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('Practitioner', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('Practitioner', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('Practitioner', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('Practitioner', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('Practitioner', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('Practitioner', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('Practitioner', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('Practitioner', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('Practitioner', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('Practitioner', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('Practitioner', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('Practitioner', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('Practitioner', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('Practitioner', 'active', 'Whether the practitioner record is active', sptTOKEN, [], 'Practitioner.active', sxpNormal);
  indexes.add('Practitioner', 'address', '): A server defined search that may match any of the string fields in the Address, including line, city, district, state, country, postalCode, and/or text', sptSTRING, [], 'Patient.address | Person.address | Practitioner.address | RelatedPerson.address', sxpNormal);
  indexes.add('Practitioner', 'address-city', '): A city specified in an address', sptSTRING, [], 'Patient.address.city | Person.address.city | Practitioner.address.city | RelatedPerson.address.city', sxpNormal);
  indexes.add('Practitioner', 'address-country', '): A country specified in an address', sptSTRING, [], 'Patient.address.country | Person.address.country | Practitioner.address.country | RelatedPerson.address.country', sxpNormal);
  indexes.add('Practitioner', 'address-postalcode', '): A postalCode specified in an address', sptSTRING, [], 'Patient.address.postalCode | Person.address.postalCode | Practitioner.address.postalCode | RelatedPerson.address.postalCode', sxpNormal);
  indexes.add('Practitioner', 'address-state', '): A state specified in an address', sptSTRING, [], 'Patient.address.state | Person.address.state | Practitioner.address.state | RelatedPerson.address.state', sxpNormal);
  indexes.add('Practitioner', 'address-use', '): A use code specified in an address', sptTOKEN, [], 'Patient.address.use | Person.address.use | Practitioner.address.use | RelatedPerson.address.use', sxpNormal);
  indexes.add('Practitioner', 'communication', 'One of the languages that the practitioner can communicate with', sptTOKEN, [], 'Practitioner.communication', sxpNormal);
  indexes.add('Practitioner', 'death-date', 'The date of death has been provided and satisfies this search value', sptDATE, [], '(Practitioner.deceased as dateTime)', sxpNormal);
  indexes.add('Practitioner', 'deceased', 'This Practitioner has been marked as deceased, or has a death date entered', sptTOKEN, [], 'Practitioner.deceased.exists() and Practitioner.deceased != false', sxpNormal);
  indexes.add('Practitioner', 'email', '): A value in an email contact', sptTOKEN, [], 'Patient.telecom.where(system=''email'') | Person.telecom.where(system=''email'') | Practitioner.telecom.where(system=''email'') | PractitionerRole.contact.telecom.where(system=''email'') | RelatedPerson.telecom.where(system=''email'')', sxpNormal);
  indexes.add('Practitioner', 'family', '): A portion of the family name', sptSTRING, [], 'Patient.name.family | Practitioner.name.family', sxpNormal);
  indexes.add('Practitioner', 'gender', '): Gender of the practitioner', sptTOKEN, [], 'Patient.gender | Person.gender | Practitioner.gender | RelatedPerson.gender', sxpNormal);
  indexes.add('Practitioner', 'given', '): A portion of the given name', sptSTRING, [], 'Patient.name.given | Practitioner.name.given', sxpNormal);
  indexes.add('Practitioner', 'identifier', 'A practitioner''s Identifier', sptTOKEN, [], 'Practitioner.identifier | Practitioner.qualification.identifier', sxpNormal);
  indexes.add('Practitioner', 'name', 'A server defined search that may match any of the string fields in the HumanName, including family, give, prefix, suffix, suffix, and/or text', sptSTRING, [], 'Practitioner.name', sxpNormal);
  indexes.add('Practitioner', 'phone', '): A value in a phone contact', sptTOKEN, [], 'Patient.telecom.where(system=''phone'') | Person.telecom.where(system=''phone'') | Practitioner.telecom.where(system=''phone'') | PractitionerRole.contact.telecom.where(system=''phone'') | RelatedPerson.telecom.where(system=''phone'')', sxpNormal);
  indexes.add('Practitioner', 'phonetic', '): A portion of either family or given name using some kind of phonetic matching algorithm', sptSTRING, [], 'Patient.name | Person.name | Practitioner.name | RelatedPerson.name', sxpNormal);
  indexes.add('Practitioner', 'qualification-period', 'The date(s) a qualification is valid for', sptDATE, [], 'Practitioner.qualification.period', sxpNormal);
  indexes.add('Practitioner', 'telecom', '): The value in any kind of contact', sptTOKEN, [], 'Patient.telecom | Person.telecom | Practitioner.telecom | PractitionerRole.contact.telecom | RelatedPerson.telecom', sxpNormal);
  indexes.add('Practitioner', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Practitioner', 'Practitioner', ['{def}']);
end;
{$ENDIF FHIR_PRACTITIONER}

{$IFDEF FHIR_PRACTITIONERROLE}
procedure TFHIRIndexBuilderR5.buildIndexesForPractitionerRole(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('PractitionerRole', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('PractitionerRole', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('PractitionerRole', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('PractitionerRole', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('PractitionerRole', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('PractitionerRole', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('PractitionerRole', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('PractitionerRole', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('PractitionerRole', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('PractitionerRole', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('PractitionerRole', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('PractitionerRole', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('PractitionerRole', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('PractitionerRole', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('PractitionerRole', 'active', 'Whether this practitioner role record is in active use', sptTOKEN, [], 'PractitionerRole.active', sxpNormal);
  indexes.add('PractitionerRole', 'date', 'The period during which the practitioner is authorized to perform in these role(s)', sptDATE, [], 'PractitionerRole.period', sxpNormal);
  indexes.add('PractitionerRole', 'email', '): A value in an email contact', sptTOKEN, [], 'Patient.telecom.where(system=''email'') | Person.telecom.where(system=''email'') | Practitioner.telecom.where(system=''email'') | PractitionerRole.contact.telecom.where(system=''email'') | RelatedPerson.telecom.where(system=''email'')', sxpNormal);
  indexes.add('PractitionerRole', 'endpoint', 'Technical endpoints providing access to services operated for the practitioner with this role', sptREFERENCE, ['Endpoint'], 'PractitionerRole.endpoint', sxpNormal);
  indexes.add('PractitionerRole', 'identifier', 'A practitioner''s Identifier', sptTOKEN, [], 'PractitionerRole.identifier', sxpNormal);
  indexes.add('PractitionerRole', 'location', 'One of the locations at which this practitioner provides care', sptREFERENCE, ['Location'], 'PractitionerRole.location', sxpNormal);
  indexes.add('PractitionerRole', 'organization', 'The identity of the organization the practitioner represents / acts on behalf of', sptREFERENCE, ['Organization'], 'PractitionerRole.organization', sxpNormal);
  indexes.add('PractitionerRole', 'phone', '): A value in a phone contact', sptTOKEN, [], 'Patient.telecom.where(system=''phone'') | Person.telecom.where(system=''phone'') | Practitioner.telecom.where(system=''phone'') | PractitionerRole.contact.telecom.where(system=''phone'') | RelatedPerson.telecom.where(system=''phone'')', sxpNormal);
  indexes.add('PractitionerRole', 'practitioner', 'Practitioner that is able to provide the defined services for the organization', sptREFERENCE, ['Practitioner'], 'PractitionerRole.practitioner', sxpNormal);
  indexes.add('PractitionerRole', 'role', 'The practitioner can perform this role at for the organization', sptTOKEN, [], 'PractitionerRole.code', sxpNormal);
  indexes.add('PractitionerRole', 'service', 'The list of healthcare services that this worker provides for this role''s Organization/Location(s)', sptREFERENCE, ['HealthcareService'], 'PractitionerRole.healthcareService', sxpNormal);
  indexes.add('PractitionerRole', 'specialty', 'The practitioner has this specialty at an organization', sptTOKEN, [], 'PractitionerRole.specialty', sxpNormal);
  indexes.add('PractitionerRole', 'telecom', '): The value in any kind of contact', sptTOKEN, [], 'Patient.telecom | Person.telecom | Practitioner.telecom | PractitionerRole.contact.telecom | RelatedPerson.telecom', sxpNormal);
  indexes.add('PractitionerRole', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Practitioner', 'PractitionerRole', ['practitioner']);
end;
{$ENDIF FHIR_PRACTITIONERROLE}

{$IFDEF FHIR_PROCEDURE}
procedure TFHIRIndexBuilderR5.buildIndexesForProcedure(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Procedure', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('Procedure', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('Procedure', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('Procedure', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('Procedure', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('Procedure', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('Procedure', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('Procedure', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('Procedure', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('Procedure', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('Procedure', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('Procedure', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('Procedure', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('Procedure', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('Procedure', 'based-on', 'A request for this procedure', sptREFERENCE, ['CarePlan', 'ServiceRequest'], 'Procedure.basedOn', sxpNormal);
  indexes.add('Procedure', 'category', 'Classification of the procedure', sptTOKEN, [], 'Procedure.category', sxpNormal);
  indexes.add('Procedure', 'code', '): A code to identify a  procedure', sptTOKEN, [], 'AllergyIntolerance.code | AllergyIntolerance.reaction.substance | Condition.code | DeviceRequest.code.concept | DiagnosticReport.code | FamilyMemberHistory.condition.code | List.code | Medication.code | MedicationAdministration.medication.concept | M'
      +'edicationDispense.medication.concept | MedicationRequest.medication.concept | MedicationUsage.medication.concept | Observation.code | Procedure.code', sxpNormal);
  indexes.add('Procedure', 'date', '): When the procedure occurred or is occurring', sptDATE, [], 'AllergyIntolerance.recordedDate | CarePlan.period | ClinicalImpression.date | Composition.date | Consent.dateTime | DiagnosticReport.effective | Encounter.actualPeriod | EpisodeOfCare.period | FamilyMemberHistory.date | Flag.period | (Immunization.oc'
      +'currence as dateTime) | List.date | Observation.effective | Procedure.occurrence | (RiskAssessment.occurrence as dateTime) | SupplyRequest.authoredOn', sxpNormal);
  indexes.add('Procedure', 'encounter', '): The Encounter during which this Procedure was created', sptREFERENCE, ['Encounter'], 'Composition.encounter | DeviceRequest.encounter | DiagnosticReport.encounter | Flag.encounter | List.encounter | NutritionOrder.encounter | Observation.encounter | Procedure.encounter | RiskAssessment.encounter | ServiceRequest.encounter | VisionPres'
      +'cription.encounter', sxpNormal);
  indexes.add('Procedure', 'identifier', '): A unique identifier for a procedure', sptTOKEN, [], 'AllergyIntolerance.identifier | CarePlan.identifier | CareTeam.identifier | Composition.identifier | Condition.identifier | Consent.identifier | DetectedIssue.identifier | DeviceRequest.identifier | DiagnosticReport.identifier | DocumentManifest.mast'
      +'erIdentifier | DocumentManifest.identifier | DocumentReference.identifier | Encounter.identifier | EpisodeOfCare.identifier | FamilyMemberHistory.identifier | Goal.identifier | ImagingStudy.identifier | Immunization.identifier | List.identifi'
      +'er | MedicationAdministration.identifier | MedicationDispense.identifier | MedicationRequest.identifier | MedicationUsage.identifier | NutritionOrder.identifier | Observation.identifier | Procedure.identifier | RiskAssessment.identifier | Ser'
      +'viceRequest.identifier | SupplyDelivery.identifier | SupplyRequest.identifier | VisionPrescription.identifier', sxpNormal);
  indexes.add('Procedure', 'instantiates-canonical', 'Instantiates FHIR protocol or definition', sptREFERENCE, ['Questionnaire', 'Measure', 'PlanDefinition', 'OperationDefinition', 'ActivityDefinition'], 'Procedure.instantiatesCanonical', sxpNormal);
  indexes.add('Procedure', 'instantiates-uri', 'Instantiates external protocol or definition', sptURI, [], 'Procedure.instantiatesUri', sxpNormal);
  indexes.add('Procedure', 'location', 'Where the procedure happened', sptREFERENCE, ['Location'], 'Procedure.location', sxpNormal);
  indexes.add('Procedure', 'part-of', 'Part of referenced event', sptREFERENCE, ['Observation', 'Procedure', 'MedicationAdministration'], 'Procedure.partOf', sxpNormal);
  indexes.add('Procedure', 'patient', '): Search by subject - a patient', sptREFERENCE, ['Patient', 'Practitioner', 'Group', 'Organization', 'BiologicallyDerivedProduct', 'NutritionProduct', 'Device', 'Medication', 'Procedure', 'Substance', 'Location'], 'AllergyIntolerance.patient | CarePlan.subject.where(resolve() is Patient) | CareTeam.subject.where(resolve() is Patient) | ClinicalImpression.subject.where(resolve() is Patient) | Composition.subject.where(resolve() is Patient) | Condition.subject.wh'
      +'ere(resolve() is Patient) | Consent.subject.where(resolve() is Patient) | DetectedIssue.subject | DeviceRequest.subject.where(resolve() is Patient) | DeviceUsage.patient | DiagnosticReport.subject.where(resolve() is Patient) | DocumentManifes'
      +'t.subject.where(resolve() is Patient) | DocumentReference.subject.where(resolve() is Patient) | Encounter.subject.where(resolve() is Patient) | EpisodeOfCare.patient | FamilyMemberHistory.patient | Flag.subject.where(resolve() is P'+
   'atient) | G'
      +'oal.subject.where(resolve() is Patient) | ImagingStudy.subject.where(resolve() is Patient) | Immunization.patient | List.subject.where(resolve() is Patient) | MedicationAdministration.subject.where(resolve() is Patient) | MedicationDispense.s'
      +'ubject.where(resolve() is Patient) | MedicationRequest.subject.where(resolve() is Patient) | MedicationUsage.subject.where(resolve() is Patient) | NutritionOrder.subject.where(resolve() is Patient) | Observation.subject.where(resolve() is Pat'
      +'ient) | Procedure.subject.where(resolve() is Patient) | RiskAssessment.subject.where(resolve() is Patient) | ServiceRequest.subject.where(resolve() is Patient) | SupplyDelivery.patient | VisionPrescription.patient', sxpNormal);
  indexes.add('Procedure', 'performer', 'Who performed the procedure', sptREFERENCE, ['Practitioner', 'Organization', 'CareTeam', 'Device', 'Patient', 'HealthcareService', 'PractitionerRole', 'RelatedPerson'], 'Procedure.performer.actor', sxpNormal);
  indexes.add('Procedure', 'reason-code', 'Reference to a concept (by class)', sptTOKEN, [], 'Procedure.reason.concept', sxpNormal);
  indexes.add('Procedure', 'reason-reference', 'Reference to a resource (by instance)', sptREFERENCE, [], 'Procedure.reason.reference', sxpNormal);
  indexes.add('Procedure', 'report', 'Any report resulting from the procedure', sptREFERENCE, ['Composition', 'DiagnosticReport', 'DocumentReference'], 'Procedure.report', sxpNormal);
  indexes.add('Procedure', 'status', 'preparation | in-progress | not-done | on-hold | stopped | completed | entered-in-error | unknown', sptTOKEN, [], 'Procedure.status', sxpNormal);
  indexes.add('Procedure', 'subject', 'Search by subject', sptREFERENCE, ['Practitioner', 'Group', 'Organization', 'Device', 'Patient', 'Location'], 'Procedure.subject', sxpNormal);
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
  indexes.add('Provenance', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('Provenance', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('Provenance', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('Provenance', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('Provenance', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('Provenance', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('Provenance', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('Provenance', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('Provenance', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('Provenance', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('Provenance', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('Provenance', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('Provenance', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('Provenance', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('Provenance', 'activity', 'Activity that occurred', sptTOKEN, [], 'Provenance.activity', sxpNormal);
  indexes.add('Provenance', 'agent', 'Who participated', sptREFERENCE, ['Practitioner', 'Organization', 'CareTeam', 'Device', 'Patient', 'PractitionerRole', 'RelatedPerson'], 'Provenance.agent.who', sxpNormal);
  indexes.add('Provenance', 'agent-role', 'What the agents role was', sptTOKEN, [], 'Provenance.agent.role', sxpNormal);
  indexes.add('Provenance', 'agent-type', 'How the agent participated', sptTOKEN, [], 'Provenance.agent.type', sxpNormal);
  indexes.add('Provenance', 'based-on', 'Reference to the service request.', sptREFERENCE, ['CarePlan', 'MedicationRequest', 'Task', 'NutritionOrder', 'DeviceRequest', 'ServiceRequest', 'ImmunizationRecommendation'], 'Provenance.basedOn', sxpNormal);
  indexes.add('Provenance', 'encounter', 'Encounter related to the Provenance', sptREFERENCE, ['Encounter'], 'Provenance.encounter', sxpNormal);
  indexes.add('Provenance', 'entity', 'Identity of entity', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'Provenance.entity.what', sxpNormal);
  indexes.add('Provenance', 'location', 'Where the activity occurred, if relevant', sptREFERENCE, ['Location'], 'Provenance.location', sxpNormal);
  indexes.add('Provenance', 'patient', 'Where the activity involved patient data', sptREFERENCE, ['Patient'], 'Provenance.patient', sxpNormal);
  indexes.add('Provenance', 'recorded', 'When the activity was recorded / updated', sptDATE, [], 'Provenance.recorded', sxpNormal);
  indexes.add('Provenance', 'signature-type', 'Indication of the reason the entity signed the object(s)', sptTOKEN, [], 'Provenance.signature.type', sxpNormal);
  indexes.add('Provenance', 'target', 'Target Reference(s) (usually version specific)', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'Provenance.target', sxpNormal);
  indexes.add('Provenance', 'when', 'When the activity occurred', sptDATE, [], '(Provenance.occurred as dateTime)', sxpNormal);
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
  indexes.add('Questionnaire', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('Questionnaire', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('Questionnaire', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('Questionnaire', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('Questionnaire', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('Questionnaire', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('Questionnaire', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('Questionnaire', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('Questionnaire', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('Questionnaire', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('Questionnaire', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('Questionnaire', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('Questionnaire', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('Questionnaire', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('Questionnaire', 'combo-code', 'A code that corresponds to one of its items in the questionnaire', sptTOKEN, [], 'Questionnaire.code | Questionnaire.item.code', sxpNormal);
  indexes.add('Questionnaire', 'context', 'A use context assigned to the questionnaire', sptTOKEN, [], '(Questionnaire.useContext.value as CodeableConcept)', sxpNormal);
  indexes.add('Questionnaire', 'context-quantity', 'A quantity- or range-valued use context assigned to the questionnaire', sptQUANTITY, [], '(Questionnaire.useContext.value as Quantity) | (Questionnaire.useContext.value as Range)', sxpNormal);
  indexes.add('Questionnaire', 'context-type', 'A type of use context assigned to the questionnaire', sptTOKEN, [], 'Questionnaire.useContext.code', sxpNormal);
  indexes.add('Questionnaire', 'context-type-quantity', 'A use context type and quantity- or range-based value assigned to the questionnaire', sptCOMPOSITE, [], 'Questionnaire.useContext', sxpNormal);
  indexes.add('Questionnaire', 'context-type-value', 'A use context type and value assigned to the questionnaire', sptCOMPOSITE, [], 'Questionnaire.useContext', sxpNormal);
  indexes.add('Questionnaire', 'date', 'The questionnaire publication date', sptDATE, [], 'Questionnaire.date', sxpNormal);
  indexes.add('Questionnaire', 'definition', 'ElementDefinition - details for the item', sptURI, [], 'Questionnaire.item.definition', sxpNormal);
  indexes.add('Questionnaire', 'description', 'The description of the questionnaire', sptSTRING, [], 'Questionnaire.description', sxpNormal);
  indexes.add('Questionnaire', 'effective', 'The time during which the questionnaire is intended to be in use', sptDATE, [], 'Questionnaire.effectivePeriod', sxpNormal);
  indexes.add('Questionnaire', 'identifier', 'External identifier for the questionnaire', sptTOKEN, [], 'Questionnaire.identifier', sxpNormal);
  indexes.add('Questionnaire', 'item-code', 'A code that corresponds to one of the items in the questionnaire', sptTOKEN, [], 'Questionnaire.item.code', sxpNormal);
  indexes.add('Questionnaire', 'jurisdiction', 'Intended jurisdiction for the questionnaire', sptTOKEN, [], 'Questionnaire.jurisdiction', sxpNormal);
  indexes.add('Questionnaire', 'name', 'Computationally friendly name of the questionnaire', sptSTRING, [], 'Questionnaire.name', sxpNormal);
  indexes.add('Questionnaire', 'publisher', 'Name of the publisher of the questionnaire', sptSTRING, [], 'Questionnaire.publisher', sxpNormal);
  indexes.add('Questionnaire', 'questionnaire-code', 'A code that matches one of the Questionnaire.code codings', sptTOKEN, [], 'Questionnaire.code', sxpNormal);
  indexes.add('Questionnaire', 'status', 'The current status of the questionnaire', sptTOKEN, [], 'Questionnaire.status', sxpNormal);
  indexes.add('Questionnaire', 'subject-type', 'Resource that can be subject of QuestionnaireResponse', sptTOKEN, [], 'Questionnaire.subjectType', sxpNormal);
  indexes.add('Questionnaire', 'title', 'The human-friendly name of the questionnaire', sptSTRING, [], 'Questionnaire.title', sxpNormal);
  indexes.add('Questionnaire', 'url', 'The uri that identifies the questionnaire', sptURI, [], 'Questionnaire.url', sxpNormal);
  indexes.add('Questionnaire', 'version', 'The business version of the questionnaire', sptTOKEN, [], 'Questionnaire.version', sxpNormal);
  indexes.add('Questionnaire', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_QUESTIONNAIRE}

{$IFDEF FHIR_QUESTIONNAIRERESPONSE}
procedure TFHIRIndexBuilderR5.buildIndexesForQuestionnaireResponse(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('QuestionnaireResponse', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('QuestionnaireResponse', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('QuestionnaireResponse', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('QuestionnaireResponse', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('QuestionnaireResponse', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('QuestionnaireResponse', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('QuestionnaireResponse', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('QuestionnaireResponse', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('QuestionnaireResponse', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('QuestionnaireResponse', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('QuestionnaireResponse', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('QuestionnaireResponse', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('QuestionnaireResponse', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('QuestionnaireResponse', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('QuestionnaireResponse', 'author', 'The author of the questionnaire response', sptREFERENCE, ['Practitioner', 'Organization', 'Device', 'Patient', 'PractitionerRole', 'RelatedPerson'], 'QuestionnaireResponse.author', sxpNormal);
  indexes.add('QuestionnaireResponse', 'authored', 'When the questionnaire response was last changed', sptDATE, [], 'QuestionnaireResponse.authored', sxpNormal);
  indexes.add('QuestionnaireResponse', 'based-on', 'Plan/proposal/order fulfilled by this questionnaire response', sptREFERENCE, ['CarePlan', 'ServiceRequest'], 'QuestionnaireResponse.basedOn', sxpNormal);
  indexes.add('QuestionnaireResponse', 'encounter', 'Encounter associated with the questionnaire response', sptREFERENCE, ['Encounter'], 'QuestionnaireResponse.encounter', sxpNormal);
  indexes.add('QuestionnaireResponse', 'identifier', 'The unique identifier for the questionnaire response', sptTOKEN, [], 'QuestionnaireResponse.identifier', sxpNormal);
  indexes.add('QuestionnaireResponse', 'item-subject', 'Allows searching for QuestionnaireResponses by item value where the item has isSubject=true', sptREFERENCE, [], 'QuestionnaireResponse.item.where(extension(''http://hl7.org/fhir/StructureDefinition/questionnaireresponse-isSubject'').exists()).answer.value.ofType(Reference)', sxpNormal);
  indexes.add('QuestionnaireResponse', 'part-of', 'Procedure or observation this questionnaire response was performed as a part of', sptREFERENCE, ['Observation', 'Procedure'], 'QuestionnaireResponse.partOf', sxpNormal);
  indexes.add('QuestionnaireResponse', 'patient', 'The patient that is the subject of the questionnaire response', sptREFERENCE, ['Patient'], 'QuestionnaireResponse.subject.where(resolve() is Patient)', sxpNormal);
  indexes.add('QuestionnaireResponse', 'questionnaire', 'The questionnaire the answers are provided for', sptREFERENCE, ['Questionnaire'], 'QuestionnaireResponse.questionnaire', sxpNormal);
  indexes.add('QuestionnaireResponse', 'source', 'The individual providing the information reflected in the questionnaire respose', sptREFERENCE, ['Practitioner', 'Organization', 'Device', 'Patient', 'PractitionerRole', 'RelatedPerson'], 'QuestionnaireResponse.source', sxpNormal);
  indexes.add('QuestionnaireResponse', 'status', 'The status of the questionnaire response', sptTOKEN, [], 'QuestionnaireResponse.status', sxpNormal);
  indexes.add('QuestionnaireResponse', 'subject', 'The subject of the questionnaire response', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'QuestionnaireResponse.subject', sxpNormal);
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
  indexes.add('RegulatedAuthorization', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('RegulatedAuthorization', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('RegulatedAuthorization', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('RegulatedAuthorization', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('RegulatedAuthorization', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('RegulatedAuthorization', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('RegulatedAuthorization', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('RegulatedAuthorization', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('RegulatedAuthorization', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('RegulatedAuthorization', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('RegulatedAuthorization', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('RegulatedAuthorization', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('RegulatedAuthorization', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('RegulatedAuthorization', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('RegulatedAuthorization', 'case', 'The case or procedure number', sptTOKEN, [], 'RegulatedAuthorization.case.identifier', sxpNormal);
  indexes.add('RegulatedAuthorization', 'case-type', 'The defining type of case', sptTOKEN, [], 'RegulatedAuthorization.case.type', sxpNormal);
  indexes.add('RegulatedAuthorization', 'holder', 'The organization that holds the granted authorization', sptREFERENCE, ['Organization'], 'RegulatedAuthorization.holder', sxpNormal);
  indexes.add('RegulatedAuthorization', 'identifier', 'Business identifier for the authorization, typically assigned by the authorizing body', sptTOKEN, [], 'RegulatedAuthorization.identifier', sxpNormal);
  indexes.add('RegulatedAuthorization', 'region', 'The territory (e.g., country, jurisdiction etc.) in which the authorization has been granted', sptTOKEN, [], 'RegulatedAuthorization.region', sxpNormal);
  indexes.add('RegulatedAuthorization', 'status', 'The status that is authorised e.g. approved. Intermediate states can be tracked with cases and applications', sptTOKEN, [], 'RegulatedAuthorization.status', sxpNormal);
  indexes.add('RegulatedAuthorization', 'subject', 'The type of regulated product, treatment, facility or activity that is being authorized', sptREFERENCE, ['SubstanceDefinition', 'ManufacturedItemDefinition', 'Organization', 'BiologicallyDerivedProduct', 'PackagedProductDefinition', 'ResearchStudy', 'Practitioner', 'MedicinalProductDefinition', 'NutritionProduct', 'Ingredient', 'DeviceDefinition', 'ObservationDefinition', 'PlanDefinition', 'ActivityDefinition', 'Location'], 'RegulatedAuthorization.subject', sxpNormal);
  indexes.add('RegulatedAuthorization', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_REGULATEDAUTHORIZATION}

{$IFDEF FHIR_RELATEDPERSON}
procedure TFHIRIndexBuilderR5.buildIndexesForRelatedPerson(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('RelatedPerson', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('RelatedPerson', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('RelatedPerson', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('RelatedPerson', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('RelatedPerson', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('RelatedPerson', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('RelatedPerson', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('RelatedPerson', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('RelatedPerson', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('RelatedPerson', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('RelatedPerson', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('RelatedPerson', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('RelatedPerson', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('RelatedPerson', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('RelatedPerson', 'active', 'Indicates if the related person record is active', sptTOKEN, [], 'RelatedPerson.active', sxpNormal);
  indexes.add('RelatedPerson', 'address', '): A server defined search that may match any of the string fields in the Address, including line, city, district, state, country, postalCode, and/or text', sptSTRING, [], 'Patient.address | Person.address | Practitioner.address | RelatedPerson.address', sxpNormal);
  indexes.add('RelatedPerson', 'address-city', '): A city specified in an address', sptSTRING, [], 'Patient.address.city | Person.address.city | Practitioner.address.city | RelatedPerson.address.city', sxpNormal);
  indexes.add('RelatedPerson', 'address-country', '): A country specified in an address', sptSTRING, [], 'Patient.address.country | Person.address.country | Practitioner.address.country | RelatedPerson.address.country', sxpNormal);
  indexes.add('RelatedPerson', 'address-postalcode', '): A postal code specified in an address', sptSTRING, [], 'Patient.address.postalCode | Person.address.postalCode | Practitioner.address.postalCode | RelatedPerson.address.postalCode', sxpNormal);
  indexes.add('RelatedPerson', 'address-state', '): A state specified in an address', sptSTRING, [], 'Patient.address.state | Person.address.state | Practitioner.address.state | RelatedPerson.address.state', sxpNormal);
  indexes.add('RelatedPerson', 'address-use', '): A use code specified in an address', sptTOKEN, [], 'Patient.address.use | Person.address.use | Practitioner.address.use | RelatedPerson.address.use', sxpNormal);
  indexes.add('RelatedPerson', 'birthdate', '): The Related Person''s date of birth', sptDATE, [], 'Patient.birthDate | Person.birthDate | RelatedPerson.birthDate', sxpNormal);
  indexes.add('RelatedPerson', 'email', '): A value in an email contact', sptTOKEN, [], 'Patient.telecom.where(system=''email'') | Person.telecom.where(system=''email'') | Practitioner.telecom.where(system=''email'') | PractitionerRole.contact.telecom.where(system=''email'') | RelatedPerson.telecom.where(system=''email'')', sxpNormal);
  indexes.add('RelatedPerson', 'family', 'A portion of the family name of the related person', sptSTRING, [], 'RelatedPerson.name.family', sxpNormal);
  indexes.add('RelatedPerson', 'gender', '): Gender of the related person', sptTOKEN, [], 'Patient.gender | Person.gender | Practitioner.gender | RelatedPerson.gender', sxpNormal);
  indexes.add('RelatedPerson', 'given', 'A portion of the given name of the related person', sptSTRING, [], 'RelatedPerson.name.given', sxpNormal);
  indexes.add('RelatedPerson', 'identifier', 'An Identifier of the RelatedPerson', sptTOKEN, [], 'RelatedPerson.identifier', sxpNormal);
  indexes.add('RelatedPerson', 'name', 'A server defined search that may match any of the string fields in the HumanName, including family, give, prefix, suffix, suffix, and/or text', sptSTRING, [], 'RelatedPerson.name', sxpNormal);
  indexes.add('RelatedPerson', 'patient', 'The patient this related person is related to', sptREFERENCE, ['Patient'], 'RelatedPerson.patient', sxpNormal);
  indexes.add('RelatedPerson', 'phone', '): A value in a phone contact', sptTOKEN, [], 'Patient.telecom.where(system=''phone'') | Person.telecom.where(system=''phone'') | Practitioner.telecom.where(system=''phone'') | PractitionerRole.contact.telecom.where(system=''phone'') | RelatedPerson.telecom.where(system=''phone'')', sxpNormal);
  indexes.add('RelatedPerson', 'phonetic', '): A portion of name using some kind of phonetic matching algorithm', sptSTRING, [], 'Patient.name | Person.name | Practitioner.name | RelatedPerson.name', sxpNormal);
  indexes.add('RelatedPerson', 'relationship', 'The relationship between the patient and the relatedperson', sptTOKEN, [], 'RelatedPerson.relationship', sxpNormal);
  indexes.add('RelatedPerson', 'telecom', '): The value in any kind of contact', sptTOKEN, [], 'Patient.telecom | Person.telecom | Practitioner.telecom | PractitionerRole.contact.telecom | RelatedPerson.telecom', sxpNormal);
  indexes.add('RelatedPerson', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Patient', 'RelatedPerson', ['patient']);
  compartments.register('RelatedPerson', 'RelatedPerson', ['{def}']);
end;
{$ENDIF FHIR_RELATEDPERSON}

{$IFDEF FHIR_REQUESTGROUP}
procedure TFHIRIndexBuilderR5.buildIndexesForRequestGroup(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('RequestGroup', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('RequestGroup', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('RequestGroup', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('RequestGroup', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('RequestGroup', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('RequestGroup', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('RequestGroup', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('RequestGroup', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('RequestGroup', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('RequestGroup', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('RequestGroup', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('RequestGroup', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('RequestGroup', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('RequestGroup', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('RequestGroup', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_REQUESTGROUP}

{$IFDEF FHIR_REQUESTORCHESTRATION}
procedure TFHIRIndexBuilderR5.buildIndexesForRequestOrchestration(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('RequestOrchestration', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('RequestOrchestration', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('RequestOrchestration', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('RequestOrchestration', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('RequestOrchestration', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('RequestOrchestration', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('RequestOrchestration', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('RequestOrchestration', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('RequestOrchestration', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('RequestOrchestration', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('RequestOrchestration', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('RequestOrchestration', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('RequestOrchestration', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('RequestOrchestration', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('RequestOrchestration', 'author', 'The author of the request orchestration', sptREFERENCE, ['Practitioner', 'Device', 'PractitionerRole'], 'RequestOrchestration.author', sxpNormal);
  indexes.add('RequestOrchestration', 'authored', 'The date the request orchestration was authored', sptDATE, [], 'RequestOrchestration.authoredOn', sxpNormal);
  indexes.add('RequestOrchestration', 'code', 'The code of the request orchestration', sptTOKEN, [], 'RequestOrchestration.code', sxpNormal);
  indexes.add('RequestOrchestration', 'encounter', 'The encounter the request orchestration applies to', sptREFERENCE, ['Encounter'], 'RequestOrchestration.encounter', sxpNormal);
  indexes.add('RequestOrchestration', 'group-identifier', 'The group identifier for the request orchestration', sptTOKEN, [], 'RequestOrchestration.groupIdentifier', sxpNormal);
  indexes.add('RequestOrchestration', 'identifier', 'External identifiers for the request orchestration', sptTOKEN, [], 'RequestOrchestration.identifier', sxpNormal);
  indexes.add('RequestOrchestration', 'instantiates-canonical', 'The FHIR-based definition from which the request orchestration is realized', sptREFERENCE, [], 'RequestOrchestration.instantiatesCanonical', sxpNormal);
  indexes.add('RequestOrchestration', 'instantiates-uri', 'The external definition from which the request orchestration is realized', sptURI, [], 'RequestOrchestration.instantiatesUri', sxpNormal);
  indexes.add('RequestOrchestration', 'intent', 'The intent of the request orchestration', sptTOKEN, [], 'RequestOrchestration.intent', sxpNormal);
  indexes.add('RequestOrchestration', 'participant', 'The participant in the requests in the orchestration', sptREFERENCE, ['Group', 'Organization', 'CareTeam', 'Device', 'Patient', 'HealthcareService', 'PractitionerRole', 'RelatedPerson', 'Practitioner', 'Endpoint', 'CapabilityStatement', 'DeviceDefinition', 'Location'], 'RequestOrchestration.action.participant.actor', sxpNormal);
  indexes.add('RequestOrchestration', 'patient', 'The identity of a patient to search for request orchestrations', sptREFERENCE, ['Patient'], 'RequestOrchestration.subject.where(resolve() is Patient)', sxpNormal);
  indexes.add('RequestOrchestration', 'priority', 'The priority of the request orchestration', sptTOKEN, [], 'RequestOrchestration.priority', sxpNormal);
  indexes.add('RequestOrchestration', 'status', 'The status of the request orchestration', sptTOKEN, [], 'RequestOrchestration.status', sxpNormal);
  indexes.add('RequestOrchestration', 'subject', 'The subject that the request orchestration is about', sptREFERENCE, ['Group', 'Patient'], 'RequestOrchestration.subject', sxpNormal);
  indexes.add('RequestOrchestration', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Device', 'RequestOrchestration', ['author']);
  compartments.register('Encounter', 'RequestOrchestration', ['encounter']);
  compartments.register('Patient', 'RequestOrchestration', ['subject', 'participant']);
  compartments.register('Practitioner', 'RequestOrchestration', ['participant', 'author']);
  compartments.register('RelatedPerson', 'RequestOrchestration', ['participant']);
end;
{$ENDIF FHIR_REQUESTORCHESTRATION}

{$IFDEF FHIR_REQUIREMENTS}
procedure TFHIRIndexBuilderR5.buildIndexesForRequirements(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Requirements', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('Requirements', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('Requirements', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('Requirements', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('Requirements', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('Requirements', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('Requirements', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('Requirements', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('Requirements', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('Requirements', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('Requirements', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('Requirements', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('Requirements', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('Requirements', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('Requirements', 'actor', 'An actor these requirements are for', sptREFERENCE, ['ActorDefinition'], 'Requirements.actor', sxpNormal);
  indexes.add('Requirements', 'context', 'A use context assigned to the requirements', sptTOKEN, [], '(Requirements.useContext.value as CodeableConcept)', sxpNormal);
  indexes.add('Requirements', 'context-quantity', 'A quantity- or range-valued use context assigned to the requirements', sptQUANTITY, [], '(Requirements.useContext.value as Quantity) | (Requirements.useContext.value as Range)', sxpNormal);
  indexes.add('Requirements', 'context-type', 'A type of use context assigned to the requirements', sptTOKEN, [], 'Requirements.useContext.code', sxpNormal);
  indexes.add('Requirements', 'context-type-quantity', 'A use context type and quantity- or range-based value assigned to the requirements', sptCOMPOSITE, [], 'Requirements.useContext', sxpNormal);
  indexes.add('Requirements', 'context-type-value', 'A use context type and value assigned to the requirements', sptCOMPOSITE, [], 'Requirements.useContext', sxpNormal);
  indexes.add('Requirements', 'date', 'The requirements publication date', sptDATE, [], 'Requirements.date', sxpNormal);
  indexes.add('Requirements', 'derived-from', 'The requirements these are derived from', sptREFERENCE, ['Requirements'], 'Requirements.derivedFrom', sxpNormal);
  indexes.add('Requirements', 'description', 'The description of the requirements', sptSTRING, [], 'Requirements.description', sxpNormal);
  indexes.add('Requirements', 'identifier', 'External identifier for the requirements', sptTOKEN, [], 'Requirements.identifier', sxpNormal);
  indexes.add('Requirements', 'jurisdiction', 'Intended jurisdiction for the requirements', sptTOKEN, [], 'Requirements.jurisdiction', sxpNormal);
  indexes.add('Requirements', 'name', 'Computationally friendly name of the requirements', sptSTRING, [], 'Requirements.name', sxpNormal);
  indexes.add('Requirements', 'publisher', 'Name of the publisher of the requirements', sptSTRING, [], 'Requirements.publisher', sxpNormal);
  indexes.add('Requirements', 'status', 'The current status of the requirements', sptTOKEN, [], 'Requirements.status', sxpNormal);
  indexes.add('Requirements', 'title', 'The human-friendly name of the requirements', sptSTRING, [], 'Requirements.title', sxpNormal);
  indexes.add('Requirements', 'url', 'The uri that identifies the requirements', sptURI, [], 'Requirements.url', sxpNormal);
  indexes.add('Requirements', 'version', 'The business version of the requirements', sptTOKEN, [], 'Requirements.version', sxpNormal);
  indexes.add('Requirements', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_REQUIREMENTS}

{$IFDEF FHIR_RESEARCHSTUDY}
procedure TFHIRIndexBuilderR5.buildIndexesForResearchStudy(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ResearchStudy', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('ResearchStudy', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('ResearchStudy', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('ResearchStudy', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('ResearchStudy', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('ResearchStudy', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('ResearchStudy', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('ResearchStudy', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('ResearchStudy', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('ResearchStudy', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('ResearchStudy', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('ResearchStudy', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('ResearchStudy', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('ResearchStudy', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('ResearchStudy', 'condition', 'Condition being studied', sptTOKEN, [], 'ResearchStudy.condition', sxpNormal);
  indexes.add('ResearchStudy', 'date', 'When the study began and ended', sptDATE, [], 'ResearchStudy.period', sxpNormal);
  indexes.add('ResearchStudy', 'focus', 'Drugs, devices, etc. under study', sptTOKEN, [], 'ResearchStudy.focus', sxpNormal);
  indexes.add('ResearchStudy', 'identifier', 'Business Identifier for study', sptTOKEN, [], 'ResearchStudy.identifier', sxpNormal);
  indexes.add('ResearchStudy', 'keyword', 'Used to search for the study', sptTOKEN, [], 'ResearchStudy.keyword', sxpNormal);
  indexes.add('ResearchStudy', 'partof', 'Part of larger study', sptREFERENCE, ['ResearchStudy'], 'ResearchStudy.partOf', sxpNormal);
  indexes.add('ResearchStudy', 'protocol', 'Steps followed in executing study', sptREFERENCE, ['PlanDefinition'], 'ResearchStudy.protocol', sxpNormal);
  indexes.add('ResearchStudy', 'recruitment_actual', 'Actual number of participants enrolled in study across all groups', sptNUMBER, [], 'ResearchStudy.recruitment.actualNumber', sxpNormal);
  indexes.add('ResearchStudy', 'recruitment_target', 'Target number of participants enrolled in study across all groups', sptNUMBER, [], 'ResearchStudy.recruitment.targetNumber', sxpNormal);
  indexes.add('ResearchStudy', 'region', 'Geographic area for the study', sptTOKEN, [], 'ResearchStudy.region', sxpNormal);
  indexes.add('ResearchStudy', 'site', 'Facility where study activities are conducted', sptREFERENCE, ['Organization', 'Location', 'ResearchStudy'], 'ResearchStudy.site', sxpNormal);
  indexes.add('ResearchStudy', 'status', 'active | active-but-not-recruiting | administratively-completed | approved | closed-to-accrual | closed-to-accrual-and-intervention | completed | disapproved | enrolling-by-invitation | in-review | not-yet-recruiting | recruiting | temporarily-closed'
      +'-to-accrual | temporarily-closed-to-accrual-and-intervention | terminated | withdrawn', sptTOKEN, [], 'ResearchStudy.status', sxpNormal);
  indexes.add('ResearchStudy', 'title', 'Name for this study', sptSTRING, [], 'ResearchStudy.title', sxpNormal);
  indexes.add('ResearchStudy', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_RESEARCHSTUDY}

{$IFDEF FHIR_RESEARCHSUBJECT}
procedure TFHIRIndexBuilderR5.buildIndexesForResearchSubject(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ResearchSubject', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('ResearchSubject', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('ResearchSubject', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('ResearchSubject', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('ResearchSubject', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('ResearchSubject', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('ResearchSubject', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('ResearchSubject', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('ResearchSubject', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('ResearchSubject', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('ResearchSubject', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('ResearchSubject', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('ResearchSubject', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('ResearchSubject', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('ResearchSubject', 'date', 'Start and end of participation', sptDATE, [], 'ResearchSubject.period', sxpNormal);
  indexes.add('ResearchSubject', 'identifier', 'Business Identifier for research subject in a study', sptTOKEN, [], 'ResearchSubject.identifier', sxpNormal);
  indexes.add('ResearchSubject', 'patient', 'Who or what is part of study', sptREFERENCE, ['Group', 'Specimen', 'BiologicallyDerivedProduct', 'Device', 'Medication', 'Patient', 'Substance'], 'ResearchSubject.subject', sxpNormal);
  indexes.add('ResearchSubject', 'status', 'draft | active | retired | unknown', sptTOKEN, [], 'ResearchSubject.status', sxpNormal);
  indexes.add('ResearchSubject', 'study', 'Study subject is part of', sptREFERENCE, ['ResearchStudy'], 'ResearchSubject.study', sxpNormal);
  indexes.add('ResearchSubject', 'subject', 'Who or what is part of study', sptREFERENCE, ['Group', 'Specimen', 'BiologicallyDerivedProduct', 'Device', 'Medication', 'Patient', 'Substance'], 'ResearchSubject.subject', sxpNormal);
  indexes.add('ResearchSubject', 'subject_state', 'candidate | eligible | follow-up | ineligible | not-registered | off-study | on-study | on-study-intervention | on-study-observation | pending-on-study | potential-candidate | screening | withdrawn', sptTOKEN, [], 'ResearchSubject.progress.subjectState', sxpNormal);
  indexes.add('ResearchSubject', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Device', 'ResearchSubject', ['subject']);
  compartments.register('Patient', 'ResearchSubject', ['subject']);
end;
{$ENDIF FHIR_RESEARCHSUBJECT}

{$IFDEF FHIR_RISKASSESSMENT}
procedure TFHIRIndexBuilderR5.buildIndexesForRiskAssessment(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('RiskAssessment', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('RiskAssessment', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('RiskAssessment', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('RiskAssessment', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('RiskAssessment', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('RiskAssessment', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('RiskAssessment', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('RiskAssessment', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('RiskAssessment', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('RiskAssessment', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('RiskAssessment', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('RiskAssessment', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('RiskAssessment', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('RiskAssessment', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('RiskAssessment', 'condition', 'Condition assessed', sptREFERENCE, ['Condition'], 'RiskAssessment.condition', sxpNormal);
  indexes.add('RiskAssessment', 'date', '): When was assessment made?', sptDATE, [], 'AllergyIntolerance.recordedDate | CarePlan.period | ClinicalImpression.date | Composition.date | Consent.dateTime | DiagnosticReport.effective | Encounter.actualPeriod | EpisodeOfCare.period | FamilyMemberHistory.date | Flag.period | (Immunization.oc'
      +'currence as dateTime) | List.date | Observation.effective | Procedure.occurrence | (RiskAssessment.occurrence as dateTime) | SupplyRequest.authoredOn', sxpNormal);
  indexes.add('RiskAssessment', 'encounter', '): Where was assessment performed?', sptREFERENCE, ['Encounter'], 'Composition.encounter | DeviceRequest.encounter | DiagnosticReport.encounter | Flag.encounter | List.encounter | NutritionOrder.encounter | Observation.encounter | Procedure.encounter | RiskAssessment.encounter | ServiceRequest.encounter | VisionPres'
      +'cription.encounter', sxpNormal);
  indexes.add('RiskAssessment', 'identifier', '): Unique identifier for the assessment', sptTOKEN, [], 'AllergyIntolerance.identifier | CarePlan.identifier | CareTeam.identifier | Composition.identifier | Condition.identifier | Consent.identifier | DetectedIssue.identifier | DeviceRequest.identifier | DiagnosticReport.identifier | DocumentManifest.mast'
      +'erIdentifier | DocumentManifest.identifier | DocumentReference.identifier | Encounter.identifier | EpisodeOfCare.identifier | FamilyMemberHistory.identifier | Goal.identifier | ImagingStudy.identifier | Immunization.identifier | List.identifi'
      +'er | MedicationAdministration.identifier | MedicationDispense.identifier | MedicationRequest.identifier | MedicationUsage.identifier | NutritionOrder.identifier | Observation.identifier | Procedure.identifier | RiskAssessment.identifier | Ser'
      +'viceRequest.identifier | SupplyDelivery.identifier | SupplyRequest.identifier | VisionPrescription.identifier', sxpNormal);
  indexes.add('RiskAssessment', 'method', 'Evaluation mechanism', sptTOKEN, [], 'RiskAssessment.method', sxpNormal);
  indexes.add('RiskAssessment', 'patient', '): Who/what does assessment apply to?', sptREFERENCE, ['Patient', 'Practitioner', 'Group', 'Organization', 'BiologicallyDerivedProduct', 'NutritionProduct', 'Device', 'Medication', 'Procedure', 'Substance', 'Location'], 'AllergyIntolerance.patient | CarePlan.subject.where(resolve() is Patient) | CareTeam.subject.where(resolve() is Patient) | ClinicalImpression.subject.where(resolve() is Patient) | Composition.subject.where(resolve() is Patient) | Condition.subject.wh'
      +'ere(resolve() is Patient) | Consent.subject.where(resolve() is Patient) | DetectedIssue.subject | DeviceRequest.subject.where(resolve() is Patient) | DeviceUsage.patient | DiagnosticReport.subject.where(resolve() is Patient) | DocumentManifes'
      +'t.subject.where(resolve() is Patient) | DocumentReference.subject.where(resolve() is Patient) | Encounter.subject.where(resolve() is Patient) | EpisodeOfCare.patient | FamilyMemberHistory.patient | Flag.subject.where(reso'+
   'lve() is Patient) | G'
      +'oal.subject.where(resolve() is Patient) | ImagingStudy.subject.where(resolve() is Patient) | Immunization.patient | List.subject.where(resolve() is Patient) | MedicationAdministration.subject.where(resolve() is Patient) | MedicationDispense.s'
      +'ubject.where(resolve() is Patient) | MedicationRequest.subject.where(resolve() is Patient) | MedicationUsage.subject.where(resolve() is Patient) | NutritionOrder.subject.where(resolve() is Patient) | Observation.subject.where(resolve() is Pat'
      +'ient) | Procedure.subject.where(resolve() is Patient) | RiskAssessment.subject.where(resolve() is Patient) | ServiceRequest.subject.where(resolve() is Patient) | SupplyDelivery.patient | VisionPrescription.patient', sxpNormal);
  indexes.add('RiskAssessment', 'performer', 'Who did assessment?', sptREFERENCE, ['Practitioner', 'Device', 'Patient', 'PractitionerRole', 'RelatedPerson'], 'RiskAssessment.performer', sxpNormal);
  indexes.add('RiskAssessment', 'probability', 'Likelihood of specified outcome', sptNUMBER, [], 'RiskAssessment.prediction.probability', sxpNormal);
  indexes.add('RiskAssessment', 'risk', 'Likelihood of specified outcome as a qualitative value', sptTOKEN, [], 'RiskAssessment.prediction.qualitativeRisk', sxpNormal);
  indexes.add('RiskAssessment', 'subject', 'Who/what does assessment apply to?', sptREFERENCE, ['Group', 'Patient'], 'RiskAssessment.subject', sxpNormal);
  indexes.add('RiskAssessment', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Device', 'RiskAssessment', ['performer']);
  compartments.register('Patient', 'RiskAssessment', ['subject']);
  compartments.register('Practitioner', 'RiskAssessment', ['performer']);
end;
{$ENDIF FHIR_RISKASSESSMENT}

{$IFDEF FHIR_SCHEDULE}
procedure TFHIRIndexBuilderR5.buildIndexesForSchedule(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Schedule', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('Schedule', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('Schedule', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('Schedule', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('Schedule', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('Schedule', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('Schedule', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('Schedule', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('Schedule', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('Schedule', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('Schedule', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('Schedule', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('Schedule', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('Schedule', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('Schedule', 'active', 'Is the schedule in active use', sptTOKEN, [], 'Schedule.active', sxpNormal);
  indexes.add('Schedule', 'actor', 'The individual(HealthcareService, Practitioner, Location, ...) to find a Schedule for', sptREFERENCE, ['Practitioner', 'CareTeam', 'Device', 'Patient', 'HealthcareService', 'PractitionerRole', 'RelatedPerson', 'Location'], 'Schedule.actor', sxpNormal);
  indexes.add('Schedule', 'date', 'Search for Schedule resources that have a period that contains this date specified', sptDATE, [], 'Schedule.planningHorizon', sxpNormal);
  indexes.add('Schedule', 'identifier', 'A Schedule Identifier', sptTOKEN, [], 'Schedule.identifier', sxpNormal);
  indexes.add('Schedule', 'name', 'A portion of the Schedule name', sptSTRING, [], 'Schedule.name', sxpNormal);
  indexes.add('Schedule', 'service-category', 'High-level category', sptTOKEN, [], 'Schedule.serviceCategory', sxpNormal);
  indexes.add('Schedule', 'service-type', 'The type (by coding) of appointments that can be booked into associated slot(s)', sptTOKEN, [], 'Schedule.serviceType.concept', sxpNormal);
  indexes.add('Schedule', 'service-type-reference', 'The type (by HealthcareService) of appointments that can be booked into associated slot(s)', sptREFERENCE, [], 'Schedule.serviceType.reference', sxpNormal);
  indexes.add('Schedule', 'specialty', 'Type of specialty needed', sptTOKEN, [], 'Schedule.specialty', sxpNormal);
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
  indexes.add('SearchParameter', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('SearchParameter', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('SearchParameter', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('SearchParameter', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('SearchParameter', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('SearchParameter', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('SearchParameter', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('SearchParameter', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('SearchParameter', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('SearchParameter', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('SearchParameter', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('SearchParameter', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('SearchParameter', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('SearchParameter', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('SearchParameter', 'base', 'The resource type(s) this search parameter applies to', sptTOKEN, [], 'SearchParameter.base', sxpNormal);
  indexes.add('SearchParameter', 'code', 'Code used in URL', sptTOKEN, [], 'SearchParameter.code', sxpNormal);
  indexes.add('SearchParameter', 'component', 'Defines how the part works', sptREFERENCE, ['SearchParameter'], 'SearchParameter.component.definition', sxpNormal);
  indexes.add('SearchParameter', 'context', '): A use context assigned to the search parameter', sptTOKEN, [], '(CapabilityStatement.useContext.value as CodeableConcept) | (CodeSystem.useContext.value as CodeableConcept) | (CompartmentDefinition.useContext.value as CodeableConcept) | (ConceptMap.useContext.value as CodeableConcept) | (GraphDefinition.useContex'
      +'t.value as CodeableConcept) | (ImplementationGuide.useContext.value as CodeableConcept) | (MessageDefinition.useContext.value as CodeableConcept) | (NamingSystem.useContext.value as CodeableConcept) | (OperationDefinition.useContext.value as '
      +'CodeableConcept) | (SearchParameter.useContext.value as CodeableConcept) | (StructureDefinition.useContext.value as CodeableConcept) | (StructureMap.useContext.value as CodeableConcept) | (TerminologyCapabilities.useContext.value as CodeableC'
      +'oncept) | (ValueSet.useContext.value as CodeableConcept)', sxpNormal);
  indexes.add('SearchParameter', 'context-quantity', '): A quantity- or range-valued use context assigned to the search parameter', sptQUANTITY, [], '(CapabilityStatement.useContext.value as Quantity) | (CapabilityStatement.useContext.value as Range) | (CodeSystem.useContext.value as Quantity) | (CodeSystem.useContext.value as Range) | (CompartmentDefinition.useContext.value as Quantity) | (Compar'
      +'tmentDefinition.useContext.value as Range) | (ConceptMap.useContext.value as Quantity) | (ConceptMap.useContext.value as Range) | (GraphDefinition.useContext.value as Quantity) | (GraphDefinition.useContext.value as Range) | (ImplementationGu'
      +'ide.useContext.value as Quantity) | (ImplementationGuide.useContext.value as Range) | (MessageDefinition.useContext.value as Quantity) | (MessageDefinition.useContext.value as Range) | (NamingSystem.useContext.value as Quantity) | (NamingSyst'
      +'em.useContext.value as Range) | (OperationDefinition.useContext.value as Quantity) '+
   '| (OperationDefinition.useContext.value as Range) | (SearchParameter.useContext.value as Quantity) | (SearchParameter.useContext.value as Range) | (StructureDe'
      +'finition.useContext.value as Quantity) | (StructureDefinition.useContext.value as Range) | (StructureMap.useContext.value as Quantity) | (StructureMap.useContext.value as Range) | (TerminologyCapabilities.useContext.value as Quantity) | (Term'
      +'inologyCapabilities.useContext.value as Range) | (ValueSet.useContext.value as Quantity) | (ValueSet.useContext.value as Range)', sxpNormal);
  indexes.add('SearchParameter', 'context-type', '): A type of use context assigned to the search parameter', sptTOKEN, [], 'CapabilityStatement.useContext.code | CodeSystem.useContext.code | CompartmentDefinition.useContext.code | ConceptMap.useContext.code | GraphDefinition.useContext.code | ImplementationGuide.useContext.code | MessageDefinition.useContext.code | Naming'
      +'System.useContext.code | OperationDefinition.useContext.code | SearchParameter.useContext.code | StructureDefinition.useContext.code | StructureMap.useContext.code | TerminologyCapabilities.useContext.code | ValueSet.useContext.code', sxpNormal);
  indexes.add('SearchParameter', 'context-type-quantity', '): A use context type and quantity- or range-based value assigned to the search parameter', sptCOMPOSITE, [], 'CapabilityStatement.useContext | CodeSystem.useContext | CompartmentDefinition.useContext | ConceptMap.useContext | GraphDefinition.useContext | ImplementationGuide.useContext | MessageDefinition.useContext | NamingSystem.useContext | OperationDefini'
      +'tion.useContext | SearchParameter.useContext | StructureDefinition.useContext | StructureMap.useContext | TerminologyCapabilities.useContext | ValueSet.useContext', sxpNormal);
  indexes.add('SearchParameter', 'context-type-value', '): A use context type and value assigned to the search parameter', sptCOMPOSITE, [], 'CapabilityStatement.useContext | CodeSystem.useContext | CompartmentDefinition.useContext | ConceptMap.useContext | GraphDefinition.useContext | ImplementationGuide.useContext | MessageDefinition.useContext | NamingSystem.useContext | OperationDefini'
      +'tion.useContext | SearchParameter.useContext | StructureDefinition.useContext | StructureMap.useContext | TerminologyCapabilities.useContext | ValueSet.useContext', sxpNormal);
  indexes.add('SearchParameter', 'date', '): The search parameter publication date', sptDATE, [], 'CapabilityStatement.date | CodeSystem.date | CompartmentDefinition.date | ConceptMap.date | GraphDefinition.date | ImplementationGuide.date | MessageDefinition.date | NamingSystem.date | OperationDefinition.date | SearchParameter.date | StructureDefi'
      +'nition.date | StructureMap.date | TerminologyCapabilities.date | ValueSet.date', sxpNormal);
  indexes.add('SearchParameter', 'derived-from', 'Original definition for the search parameter', sptREFERENCE, ['SearchParameter'], 'SearchParameter.derivedFrom', sxpNormal);
  indexes.add('SearchParameter', 'description', '): The description of the search parameter', sptSTRING, [], 'CapabilityStatement.description | CodeSystem.description | CompartmentDefinition.description | ConceptMap.description | GraphDefinition.description | ImplementationGuide.description | MessageDefinition.description | NamingSystem.description | Operati'
      +'onDefinition.description | SearchParameter.description | StructureDefinition.description | StructureMap.description | TerminologyCapabilities.description | ValueSet.description', sxpNormal);
  indexes.add('SearchParameter', 'jurisdiction', '): Intended jurisdiction for the search parameter', sptTOKEN, [], 'CapabilityStatement.jurisdiction | CodeSystem.jurisdiction | ConceptMap.jurisdiction | GraphDefinition.jurisdiction | ImplementationGuide.jurisdiction | MessageDefinition.jurisdiction | NamingSystem.jurisdiction | OperationDefinition.jurisdiction | S'
      +'earchParameter.jurisdiction | StructureDefinition.jurisdiction | StructureMap.jurisdiction | TerminologyCapabilities.jurisdiction | ValueSet.jurisdiction', sxpNormal);
  indexes.add('SearchParameter', 'name', '): Computationally friendly name of the search parameter', sptSTRING, [], 'CapabilityStatement.name | CodeSystem.name | CompartmentDefinition.name | ConceptMap.name | GraphDefinition.name | ImplementationGuide.name | MessageDefinition.name | NamingSystem.name | OperationDefinition.name | SearchParameter.name | StructureDefi'
      +'nition.name | StructureMap.name | TerminologyCapabilities.name | ValueSet.name', sxpNormal);
  indexes.add('SearchParameter', 'publisher', '): Name of the publisher of the search parameter', sptSTRING, [], 'CapabilityStatement.publisher | CodeSystem.publisher | CompartmentDefinition.publisher | ConceptMap.publisher | GraphDefinition.publisher | ImplementationGuide.publisher | MessageDefinition.publisher | NamingSystem.publisher | OperationDefinition.pub'
      +'lisher | SearchParameter.publisher | StructureDefinition.publisher | StructureMap.publisher | TerminologyCapabilities.publisher | ValueSet.publisher', sxpNormal);
  indexes.add('SearchParameter', 'status', '): The current status of the search parameter', sptTOKEN, [], 'CapabilityStatement.status | CodeSystem.status | CompartmentDefinition.status | ConceptMap.status | GraphDefinition.status | ImplementationGuide.status | MessageDefinition.status | NamingSystem.status | OperationDefinition.status | SearchParameter.st'
      +'atus | StructureDefinition.status | StructureMap.status | TerminologyCapabilities.status | ValueSet.status', sxpNormal);
  indexes.add('SearchParameter', 'target', 'Types of resource (if a resource reference)', sptTOKEN, [], 'SearchParameter.target', sxpNormal);
  indexes.add('SearchParameter', 'type', 'number | date | string | token | reference | composite | quantity | uri | special', sptTOKEN, [], 'SearchParameter.type', sxpNormal);
  indexes.add('SearchParameter', 'url', '): The uri that identifies the search parameter', sptURI, [], 'CapabilityStatement.url | CodeSystem.url | CompartmentDefinition.url | ConceptMap.url | GraphDefinition.url | ImplementationGuide.url | MessageDefinition.url | NamingSystem.url | OperationDefinition.url | SearchParameter.url | StructureDefinition.url'
      +' | StructureMap.url | TerminologyCapabilities.url | ValueSet.url', sxpNormal);
  indexes.add('SearchParameter', 'version', '): The business version of the search parameter', sptTOKEN, [], 'CapabilityStatement.version | CodeSystem.version | CompartmentDefinition.version | ConceptMap.version | GraphDefinition.version | ImplementationGuide.version | MessageDefinition.version | NamingSystem.version | OperationDefinition.version | SearchPar'
      +'ameter.version | StructureDefinition.version | StructureMap.version | TerminologyCapabilities.version | ValueSet.version', sxpNormal);
  indexes.add('SearchParameter', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_SEARCHPARAMETER}

{$IFDEF FHIR_SERVICEREQUEST}
procedure TFHIRIndexBuilderR5.buildIndexesForServiceRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ServiceRequest', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('ServiceRequest', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('ServiceRequest', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('ServiceRequest', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('ServiceRequest', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('ServiceRequest', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('ServiceRequest', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('ServiceRequest', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('ServiceRequest', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('ServiceRequest', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('ServiceRequest', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('ServiceRequest', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('ServiceRequest', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('ServiceRequest', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('ServiceRequest', 'authored', 'Date request signed', sptDATE, [], 'ServiceRequest.authoredOn', sxpNormal);
  indexes.add('ServiceRequest', 'based-on', 'What request fulfills', sptREFERENCE, ['CarePlan', 'MedicationRequest', 'ServiceRequest'], 'ServiceRequest.basedOn', sxpNormal);
  indexes.add('ServiceRequest', 'body-site', 'Where procedure is going to be done', sptTOKEN, [], 'ServiceRequest.bodySite', sxpNormal);
  indexes.add('ServiceRequest', 'body-structure', 'Body structure Where procedure is going to be done', sptREFERENCE, ['BodyStructure'], 'ServiceRequest.bodyStructure', sxpNormal);
  indexes.add('ServiceRequest', 'category', 'Classification of service', sptTOKEN, [], 'ServiceRequest.category', sxpNormal);
  indexes.add('ServiceRequest', 'code-concept', 'What is being requested/ordered', sptTOKEN, [], 'ServiceRequest.code.concept', sxpNormal);
  indexes.add('ServiceRequest', 'code-reference', 'What is being requested/ordered', sptREFERENCE, [], 'ServiceRequest.code.reference', sxpNormal);
  indexes.add('ServiceRequest', 'encounter', '): An encounter in which this request is made', sptREFERENCE, ['Encounter'], 'Composition.encounter | DeviceRequest.encounter | DiagnosticReport.encounter | Flag.encounter | List.encounter | NutritionOrder.encounter | Observation.encounter | Procedure.encounter | RiskAssessment.encounter | ServiceRequest.encounter | VisionPres'
      +'cription.encounter', sxpNormal);
  indexes.add('ServiceRequest', 'identifier', '): Identifiers assigned to this order', sptTOKEN, [], 'AllergyIntolerance.identifier | CarePlan.identifier | CareTeam.identifier | Composition.identifier | Condition.identifier | Consent.identifier | DetectedIssue.identifier | DeviceRequest.identifier | DiagnosticReport.identifier | DocumentManifest.mast'
      +'erIdentifier | DocumentManifest.identifier | DocumentReference.identifier | Encounter.identifier | EpisodeOfCare.identifier | FamilyMemberHistory.identifier | Goal.identifier | ImagingStudy.identifier | Immunization.identifier | List.identifi'
      +'er | MedicationAdministration.identifier | MedicationDispense.identifier | MedicationRequest.identifier | MedicationUsage.identifier | NutritionOrder.identifier | Observation.identifier | Procedure.identifier | RiskAssessment.identifier | Ser'
      +'viceRequest.identifier | SupplyDelivery.identifier | SupplyRequest.identifier | VisionPrescription.identifier', sxpNormal);
  indexes.add('ServiceRequest', 'instantiates-canonical', 'Instantiates FHIR protocol or definition', sptREFERENCE, ['PlanDefinition', 'ActivityDefinition'], 'ServiceRequest.instantiatesCanonical', sxpNormal);
  indexes.add('ServiceRequest', 'instantiates-uri', 'Instantiates external protocol or definition', sptURI, [], 'ServiceRequest.instantiatesUri', sxpNormal);
  indexes.add('ServiceRequest', 'intent', 'proposal | plan | directive | order +', sptTOKEN, [], 'ServiceRequest.intent', sxpNormal);
  indexes.add('ServiceRequest', 'occurrence', 'When service should occur', sptDATE, [], 'ServiceRequest.occurrence', sxpNormal);
  indexes.add('ServiceRequest', 'patient', '): Search by subject - a patient', sptREFERENCE, ['Patient', 'Practitioner', 'Group', 'Organization', 'BiologicallyDerivedProduct', 'NutritionProduct', 'Device', 'Medication', 'Procedure', 'Substance', 'Location'], 'AllergyIntolerance.patient | CarePlan.subject.where(resolve() is Patient) | CareTeam.subject.where(resolve() is Patient) | ClinicalImpression.subject.where(resolve() is Patient) | Composition.subject.where(resolve() is Patient) | Condition.subject.wh'
      +'ere(resolve() is Patient) | Consent.subject.where(resolve() is Patient) | DetectedIssue.subject | DeviceRequest.subject.where(resolve() is Patient) | DeviceUsage.patient | DiagnosticReport.subject.where(resolve() is Patient) | DocumentManifes'
      +'t.subject.where(resolve() is Patient) | DocumentReference.subject.where(resolve() is Patient) | Encounter.subject.where(resolve() is Patient) | EpisodeOfCare.patient | FamilyMemberHistory.patient | Flag.subject.where(resolve()'+
   ' is Patient) | G'
      +'oal.subject.where(resolve() is Patient) | ImagingStudy.subject.where(resolve() is Patient) | Immunization.patient | List.subject.where(resolve() is Patient) | MedicationAdministration.subject.where(resolve() is Patient) | MedicationDispense.s'
      +'ubject.where(resolve() is Patient) | MedicationRequest.subject.where(resolve() is Patient) | MedicationUsage.subject.where(resolve() is Patient) | NutritionOrder.subject.where(resolve() is Patient) | Observation.subject.where(resolve() is Pat'
      +'ient) | Procedure.subject.where(resolve() is Patient) | RiskAssessment.subject.where(resolve() is Patient) | ServiceRequest.subject.where(resolve() is Patient) | SupplyDelivery.patient | VisionPrescription.patient', sxpNormal);
  indexes.add('ServiceRequest', 'performer', 'Requested performer', sptREFERENCE, ['Practitioner', 'Organization', 'CareTeam', 'Device', 'Patient', 'HealthcareService', 'PractitionerRole', 'RelatedPerson'], 'ServiceRequest.performer', sxpNormal);
  indexes.add('ServiceRequest', 'performer-type', 'Performer role', sptTOKEN, [], 'ServiceRequest.performerType', sxpNormal);
  indexes.add('ServiceRequest', 'priority', 'routine | urgent | asap | stat', sptTOKEN, [], 'ServiceRequest.priority', sxpNormal);
  indexes.add('ServiceRequest', 'replaces', 'What request replaces', sptREFERENCE, ['ServiceRequest'], 'ServiceRequest.replaces', sxpNormal);
  indexes.add('ServiceRequest', 'requester', 'Who/what is requesting service', sptREFERENCE, ['Practitioner', 'Organization', 'Device', 'Patient', 'PractitionerRole', 'RelatedPerson'], 'ServiceRequest.requester', sxpNormal);
  indexes.add('ServiceRequest', 'requisition', 'Composite Request ID', sptTOKEN, [], 'ServiceRequest.requisition', sxpNormal);
  indexes.add('ServiceRequest', 'specimen', 'Specimen to be tested', sptREFERENCE, ['Specimen'], 'ServiceRequest.specimen', sxpNormal);
  indexes.add('ServiceRequest', 'status', 'draft | active | on-hold | revoked | completed | entered-in-error | unknown', sptTOKEN, [], 'ServiceRequest.status', sxpNormal);
  indexes.add('ServiceRequest', 'subject', 'Search by subject', sptREFERENCE, ['Group', 'Device', 'Patient', 'Location'], 'ServiceRequest.subject', sxpNormal);
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
  indexes.add('Slot', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('Slot', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('Slot', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('Slot', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('Slot', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('Slot', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('Slot', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('Slot', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('Slot', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('Slot', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('Slot', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('Slot', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('Slot', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('Slot', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('Slot', 'appointment-type', 'The style of appointment or patient that may be booked in the slot (not service type)', sptTOKEN, [], 'Slot.appointmentType', sxpNormal);
  indexes.add('Slot', 'identifier', 'A Slot Identifier', sptTOKEN, [], 'Slot.identifier', sxpNormal);
  indexes.add('Slot', 'schedule', 'The Schedule Resource that we are seeking a slot within', sptREFERENCE, ['Schedule'], 'Slot.schedule', sxpNormal);
  indexes.add('Slot', 'service-category', 'A broad categorization of the service that is to be performed during this appointment', sptTOKEN, [], 'Slot.serviceCategory', sxpNormal);
  indexes.add('Slot', 'service-type', 'The type (by coding) of appointments that can be booked into the slot', sptTOKEN, [], 'Slot.serviceType.concept', sxpNormal);
  indexes.add('Slot', 'service-type-reference', 'The type (by HealthcareService) of appointments that can be booked into the slot', sptREFERENCE, [], 'Slot.serviceType.reference', sxpNormal);
  indexes.add('Slot', 'specialty', 'The specialty of a practitioner that would be required to perform the service requested in this appointment', sptTOKEN, [], 'Slot.specialty', sxpNormal);
  indexes.add('Slot', 'start', 'Appointment date/time.', sptDATE, [], 'Slot.start', sxpNormal);
  indexes.add('Slot', 'status', 'The free/busy status of the appointment', sptTOKEN, [], 'Slot.status', sxpNormal);
  indexes.add('Slot', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_SLOT}

{$IFDEF FHIR_SPECIMEN}
procedure TFHIRIndexBuilderR5.buildIndexesForSpecimen(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Specimen', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('Specimen', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('Specimen', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('Specimen', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('Specimen', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('Specimen', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('Specimen', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('Specimen', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('Specimen', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('Specimen', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('Specimen', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('Specimen', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('Specimen', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('Specimen', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('Specimen', 'accession', 'The accession number associated with the specimen', sptTOKEN, [], 'Specimen.accessionIdentifier', sxpNormal);
  indexes.add('Specimen', 'bodysite', 'Reference to a resource (by instance)', sptREFERENCE, [], 'Specimen.collection.bodySite.reference', sxpNormal);
  indexes.add('Specimen', 'collected', 'The date the specimen was collected', sptDATE, [], 'Specimen.collection.collected', sxpNormal);
  indexes.add('Specimen', 'collector', 'Who collected the specimen', sptREFERENCE, ['Practitioner', 'Patient', 'PractitionerRole', 'RelatedPerson'], 'Specimen.collection.collector', sxpNormal);
  indexes.add('Specimen', 'container-device', 'The unique identifier associated with the specimen container', sptREFERENCE, ['Device'], 'Specimen.container.device.where(resolve() is Device)', sxpNormal);
  indexes.add('Specimen', 'identifier', 'The unique identifier associated with the specimen', sptTOKEN, [], 'Specimen.identifier', sxpNormal);
  indexes.add('Specimen', 'parent', 'The parent of the specimen', sptREFERENCE, ['Specimen'], 'Specimen.parent', sxpNormal);
  indexes.add('Specimen', 'patient', 'The patient the specimen comes from', sptREFERENCE, ['Patient'], 'Specimen.subject.where(resolve() is Patient)', sxpNormal);
  indexes.add('Specimen', 'procedure', 'The procedure that collected the specimen', sptREFERENCE, ['Procedure'], 'Specimen.collection.procedure', sxpNormal);
  indexes.add('Specimen', 'status', 'available | unavailable | unsatisfactory | entered-in-error', sptTOKEN, [], 'Specimen.status', sxpNormal);
  indexes.add('Specimen', 'subject', 'The subject of the specimen', sptREFERENCE, ['Group', 'BiologicallyDerivedProduct', 'Device', 'Patient', 'Substance', 'Location'], 'Specimen.subject', sxpNormal);
  indexes.add('Specimen', 'type', 'The specimen type', sptTOKEN, [], 'Specimen.type', sxpNormal);
  indexes.add('Specimen', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Device', 'Specimen', ['subject']);
  compartments.register('Patient', 'Specimen', ['subject']);
  compartments.register('Practitioner', 'Specimen', ['collector']);
end;
{$ENDIF FHIR_SPECIMEN}

{$IFDEF FHIR_SPECIMENDEFINITION}
procedure TFHIRIndexBuilderR5.buildIndexesForSpecimenDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('SpecimenDefinition', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('SpecimenDefinition', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('SpecimenDefinition', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('SpecimenDefinition', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('SpecimenDefinition', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('SpecimenDefinition', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('SpecimenDefinition', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('SpecimenDefinition', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('SpecimenDefinition', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('SpecimenDefinition', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('SpecimenDefinition', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('SpecimenDefinition', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('SpecimenDefinition', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('SpecimenDefinition', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('SpecimenDefinition', 'container', 'The type of specimen conditioned in container expected by the lab', sptTOKEN, [], 'SpecimenDefinition.typeTested.container.type', sxpNormal);
  indexes.add('SpecimenDefinition', 'experimental', 'Not for genuine usage (true)', sptTOKEN, [], 'SpecimenDefinition.experimental', sxpNormal);
  indexes.add('SpecimenDefinition', 'identifier', 'The unique identifier associated with the SpecimenDefinition', sptTOKEN, [], 'SpecimenDefinition.identifier', sxpNormal);
  indexes.add('SpecimenDefinition', 'is-derived', 'Primary specimen (false) or derived specimen (true)', sptTOKEN, [], 'SpecimenDefinition.typeTested.isDerived', sxpNormal);
  indexes.add('SpecimenDefinition', 'status', 'Publication status of the SpecimenDefinition: draft, active, retired, unknown', sptTOKEN, [], 'SpecimenDefinition.status', sxpNormal);
  indexes.add('SpecimenDefinition', 'title', 'Human-friendly name of the SpecimenDefinition', sptSTRING, [], 'SpecimenDefinition.title', sxpNormal);
  indexes.add('SpecimenDefinition', 'type', 'The type of collected specimen', sptTOKEN, [], 'SpecimenDefinition.typeCollected', sxpNormal);
  indexes.add('SpecimenDefinition', 'type-tested', 'The type of specimen conditioned for testing', sptTOKEN, [], 'SpecimenDefinition.typeTested.type', sxpNormal);
  indexes.add('SpecimenDefinition', 'url', 'The uri that identifies the specimen definition', sptURI, [], 'SpecimenDefinition.url', sxpNormal);
  indexes.add('SpecimenDefinition', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_SPECIMENDEFINITION}

{$IFDEF FHIR_STRUCTUREDEFINITION}
procedure TFHIRIndexBuilderR5.buildIndexesForStructureDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('StructureDefinition', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('StructureDefinition', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('StructureDefinition', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('StructureDefinition', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('StructureDefinition', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('StructureDefinition', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('StructureDefinition', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('StructureDefinition', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('StructureDefinition', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('StructureDefinition', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('StructureDefinition', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('StructureDefinition', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('StructureDefinition', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('StructureDefinition', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('StructureDefinition', 'abstract', 'Whether the structure is abstract', sptTOKEN, [], 'StructureDefinition.abstract', sxpNormal);
  indexes.add('StructureDefinition', 'base', 'Definition that this type is constrained/specialized from', sptREFERENCE, ['StructureDefinition'], 'StructureDefinition.baseDefinition', sxpNormal);
  indexes.add('StructureDefinition', 'base-path', 'Path that identifies the base element', sptTOKEN, [], 'StructureDefinition.snapshot.element.base.path | StructureDefinition.differential.element.base.path', sxpNormal);
  indexes.add('StructureDefinition', 'context', '): A use context assigned to the structure definition', sptTOKEN, [], '(CapabilityStatement.useContext.value as CodeableConcept) | (CodeSystem.useContext.value as CodeableConcept) | (CompartmentDefinition.useContext.value as CodeableConcept) | (ConceptMap.useContext.value as CodeableConcept) | (GraphDefinition.useContex'
      +'t.value as CodeableConcept) | (ImplementationGuide.useContext.value as CodeableConcept) | (MessageDefinition.useContext.value as CodeableConcept) | (NamingSystem.useContext.value as CodeableConcept) | (OperationDefinition.useContext.value as '
      +'CodeableConcept) | (SearchParameter.useContext.value as CodeableConcept) | (StructureDefinition.useContext.value as CodeableConcept) | (StructureMap.useContext.value as CodeableConcept) | (TerminologyCapabilities.useContext.value as CodeableC'
      +'oncept) | (ValueSet.useContext.value as CodeableConcept)', sxpNormal);
  indexes.add('StructureDefinition', 'context-quantity', '): A quantity- or range-valued use context assigned to the structure definition', sptQUANTITY, [], '(CapabilityStatement.useContext.value as Quantity) | (CapabilityStatement.useContext.value as Range) | (CodeSystem.useContext.value as Quantity) | (CodeSystem.useContext.value as Range) | (CompartmentDefinition.useContext.value as Quantity) | (Compar'
      +'tmentDefinition.useContext.value as Range) | (ConceptMap.useContext.value as Quantity) | (ConceptMap.useContext.value as Range) | (GraphDefinition.useContext.value as Quantity) | (GraphDefinition.useContext.value as Range) | (ImplementationGu'
      +'ide.useContext.value as Quantity) | (ImplementationGuide.useContext.value as Range) | (MessageDefinition.useContext.value as Quantity) | (MessageDefinition.useContext.value as Range) | (NamingSystem.useContext.value as Quantity) | (NamingSyst'
      +'em.useContext.value as Range) | (OperationDefinition.useContext.value as Qu'+
   'antity) | (OperationDefinition.useContext.value as Range) | (SearchParameter.useContext.value as Quantity) | (SearchParameter.useContext.value as Range) | (StructureDe'
      +'finition.useContext.value as Quantity) | (StructureDefinition.useContext.value as Range) | (StructureMap.useContext.value as Quantity) | (StructureMap.useContext.value as Range) | (TerminologyCapabilities.useContext.value as Quantity) | (Term'
      +'inologyCapabilities.useContext.value as Range) | (ValueSet.useContext.value as Quantity) | (ValueSet.useContext.value as Range)', sxpNormal);
  indexes.add('StructureDefinition', 'context-type', '): A type of use context assigned to the structure definition', sptTOKEN, [], 'CapabilityStatement.useContext.code | CodeSystem.useContext.code | CompartmentDefinition.useContext.code | ConceptMap.useContext.code | GraphDefinition.useContext.code | ImplementationGuide.useContext.code | MessageDefinition.useContext.code | Naming'
      +'System.useContext.code | OperationDefinition.useContext.code | SearchParameter.useContext.code | StructureDefinition.useContext.code | StructureMap.useContext.code | TerminologyCapabilities.useContext.code | ValueSet.useContext.code', sxpNormal);
  indexes.add('StructureDefinition', 'context-type-quantity', '): A use context type and quantity- or range-based value assigned to the structure definition', sptCOMPOSITE, [], 'CapabilityStatement.useContext | CodeSystem.useContext | CompartmentDefinition.useContext | ConceptMap.useContext | GraphDefinition.useContext | ImplementationGuide.useContext | MessageDefinition.useContext | NamingSystem.useContext | OperationDefini'
      +'tion.useContext | SearchParameter.useContext | StructureDefinition.useContext | StructureMap.useContext | TerminologyCapabilities.useContext | ValueSet.useContext', sxpNormal);
  indexes.add('StructureDefinition', 'context-type-value', '): A use context type and value assigned to the structure definition', sptCOMPOSITE, [], 'CapabilityStatement.useContext | CodeSystem.useContext | CompartmentDefinition.useContext | ConceptMap.useContext | GraphDefinition.useContext | ImplementationGuide.useContext | MessageDefinition.useContext | NamingSystem.useContext | OperationDefini'
      +'tion.useContext | SearchParameter.useContext | StructureDefinition.useContext | StructureMap.useContext | TerminologyCapabilities.useContext | ValueSet.useContext', sxpNormal);
  indexes.add('StructureDefinition', 'date', '): The structure definition publication date', sptDATE, [], 'CapabilityStatement.date | CodeSystem.date | CompartmentDefinition.date | ConceptMap.date | GraphDefinition.date | ImplementationGuide.date | MessageDefinition.date | NamingSystem.date | OperationDefinition.date | SearchParameter.date | StructureDefi'
      +'nition.date | StructureMap.date | TerminologyCapabilities.date | ValueSet.date', sxpNormal);
  indexes.add('StructureDefinition', 'derivation', 'specialization | constraint - How relates to base definition', sptTOKEN, [], 'StructureDefinition.derivation', sxpNormal);
  indexes.add('StructureDefinition', 'description', '): The description of the structure definition', sptSTRING, [], 'CapabilityStatement.description | CodeSystem.description | CompartmentDefinition.description | ConceptMap.description | GraphDefinition.description | ImplementationGuide.description | MessageDefinition.description | NamingSystem.description | Operati'
      +'onDefinition.description | SearchParameter.description | StructureDefinition.description | StructureMap.description | TerminologyCapabilities.description | ValueSet.description', sxpNormal);
  indexes.add('StructureDefinition', 'experimental', 'For testing purposes, not real usage', sptTOKEN, [], 'StructureDefinition.experimental', sxpNormal);
  indexes.add('StructureDefinition', 'ext-context', 'An extension context assigned to the structure definition', sptCOMPOSITE, [], 'StructureDefinition.context', sxpNormal);
  indexes.add('StructureDefinition', 'ext-context-expression', 'An expression of extension context assigned to the structure definition', sptTOKEN, [], 'StructureDefinition.context.expression', sxpNormal);
  indexes.add('StructureDefinition', 'ext-context-type', 'A type of extension context assigned to the structure definition', sptTOKEN, [], 'StructureDefinition.context.type', sxpNormal);
  indexes.add('StructureDefinition', 'identifier', '): External identifier for the structure definition', sptTOKEN, [], 'CodeSystem.identifier | ConceptMap.identifier | MessageDefinition.identifier | NamingSystem.identifier | StructureDefinition.identifier | StructureMap.identifier | TerminologyCapabilities.identifier | ValueSet.identifier', sxpNormal);
  indexes.add('StructureDefinition', 'jurisdiction', '): Intended jurisdiction for the structure definition', sptTOKEN, [], 'CapabilityStatement.jurisdiction | CodeSystem.jurisdiction | ConceptMap.jurisdiction | GraphDefinition.jurisdiction | ImplementationGuide.jurisdiction | MessageDefinition.jurisdiction | NamingSystem.jurisdiction | OperationDefinition.jurisdiction | S'
      +'earchParameter.jurisdiction | StructureDefinition.jurisdiction | StructureMap.jurisdiction | TerminologyCapabilities.jurisdiction | ValueSet.jurisdiction', sxpNormal);
  indexes.add('StructureDefinition', 'keyword', 'A code for the StructureDefinition', sptTOKEN, [], 'StructureDefinition.keyword', sxpNormal);
  indexes.add('StructureDefinition', 'kind', 'primitive-type | complex-type | resource | logical', sptTOKEN, [], 'StructureDefinition.kind', sxpNormal);
  indexes.add('StructureDefinition', 'name', '): Computationally friendly name of the structure definition', sptSTRING, [], 'CapabilityStatement.name | CodeSystem.name | CompartmentDefinition.name | ConceptMap.name | GraphDefinition.name | ImplementationGuide.name | MessageDefinition.name | NamingSystem.name | OperationDefinition.name | SearchParameter.name | StructureDefi'
      +'nition.name | StructureMap.name | TerminologyCapabilities.name | ValueSet.name', sxpNormal);
  indexes.add('StructureDefinition', 'path', 'A path that is constrained in the StructureDefinition', sptTOKEN, [], 'StructureDefinition.snapshot.element.path | StructureDefinition.differential.element.path', sxpNormal);
  indexes.add('StructureDefinition', 'publisher', '): Name of the publisher of the structure definition', sptSTRING, [], 'CapabilityStatement.publisher | CodeSystem.publisher | CompartmentDefinition.publisher | ConceptMap.publisher | GraphDefinition.publisher | ImplementationGuide.publisher | MessageDefinition.publisher | NamingSystem.publisher | OperationDefinition.pub'
      +'lisher | SearchParameter.publisher | StructureDefinition.publisher | StructureMap.publisher | TerminologyCapabilities.publisher | ValueSet.publisher', sxpNormal);
  indexes.add('StructureDefinition', 'status', '): The current status of the structure definition', sptTOKEN, [], 'CapabilityStatement.status | CodeSystem.status | CompartmentDefinition.status | ConceptMap.status | GraphDefinition.status | ImplementationGuide.status | MessageDefinition.status | NamingSystem.status | OperationDefinition.status | SearchParameter.st'
      +'atus | StructureDefinition.status | StructureMap.status | TerminologyCapabilities.status | ValueSet.status', sxpNormal);
  indexes.add('StructureDefinition', 'title', '): The human-friendly name of the structure definition', sptSTRING, [], 'CapabilityStatement.title | CodeSystem.title | ConceptMap.title | ImplementationGuide.title | MessageDefinition.title | OperationDefinition.title | StructureDefinition.title | StructureMap.title | TerminologyCapabilities.title | ValueSet.title', sxpNormal);
  indexes.add('StructureDefinition', 'type', 'Type defined or constrained by this structure', sptURI, [], 'StructureDefinition.type', sxpNormal);
  indexes.add('StructureDefinition', 'url', '): The uri that identifies the structure definition', sptURI, [], 'CapabilityStatement.url | CodeSystem.url | CompartmentDefinition.url | ConceptMap.url | GraphDefinition.url | ImplementationGuide.url | MessageDefinition.url | NamingSystem.url | OperationDefinition.url | SearchParameter.url | StructureDefinition.url'
      +' | StructureMap.url | TerminologyCapabilities.url | ValueSet.url', sxpNormal);
  indexes.add('StructureDefinition', 'valueset', 'A vocabulary binding reference', sptREFERENCE, ['ValueSet'], 'StructureDefinition.snapshot.element.binding.valueSet', sxpNormal);
  indexes.add('StructureDefinition', 'version', '): The business version of the structure definition', sptTOKEN, [], 'CapabilityStatement.version | CodeSystem.version | CompartmentDefinition.version | ConceptMap.version | GraphDefinition.version | ImplementationGuide.version | MessageDefinition.version | NamingSystem.version | OperationDefinition.version | SearchPar'
      +'ameter.version | StructureDefinition.version | StructureMap.version | TerminologyCapabilities.version | ValueSet.version', sxpNormal);
  indexes.add('StructureDefinition', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_STRUCTUREDEFINITION}

{$IFDEF FHIR_STRUCTUREMAP}
procedure TFHIRIndexBuilderR5.buildIndexesForStructureMap(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('StructureMap', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('StructureMap', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('StructureMap', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('StructureMap', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('StructureMap', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('StructureMap', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('StructureMap', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('StructureMap', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('StructureMap', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('StructureMap', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('StructureMap', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('StructureMap', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('StructureMap', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('StructureMap', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('StructureMap', 'context', '): A use context assigned to the structure map', sptTOKEN, [], '(CapabilityStatement.useContext.value as CodeableConcept) | (CodeSystem.useContext.value as CodeableConcept) | (CompartmentDefinition.useContext.value as CodeableConcept) | (ConceptMap.useContext.value as CodeableConcept) | (GraphDefinition.useContex'
      +'t.value as CodeableConcept) | (ImplementationGuide.useContext.value as CodeableConcept) | (MessageDefinition.useContext.value as CodeableConcept) | (NamingSystem.useContext.value as CodeableConcept) | (OperationDefinition.useContext.value as '
      +'CodeableConcept) | (SearchParameter.useContext.value as CodeableConcept) | (StructureDefinition.useContext.value as CodeableConcept) | (StructureMap.useContext.value as CodeableConcept) | (TerminologyCapabilities.useContext.value as CodeableC'
      +'oncept) | (ValueSet.useContext.value as CodeableConcept)', sxpNormal);
  indexes.add('StructureMap', 'context-quantity', '): A quantity- or range-valued use context assigned to the structure map', sptQUANTITY, [], '(CapabilityStatement.useContext.value as Quantity) | (CapabilityStatement.useContext.value as Range) | (CodeSystem.useContext.value as Quantity) | (CodeSystem.useContext.value as Range) | (CompartmentDefinition.useContext.value as Quantity) | (Compar'
      +'tmentDefinition.useContext.value as Range) | (ConceptMap.useContext.value as Quantity) | (ConceptMap.useContext.value as Range) | (GraphDefinition.useContext.value as Quantity) | (GraphDefinition.useContext.value as Range) | (ImplementationGu'
      +'ide.useContext.value as Quantity) | (ImplementationGuide.useContext.value as Range) | (MessageDefinition.useContext.value as Quantity) | (MessageDefinition.useContext.value as Range) | (NamingSystem.useContext.value as Quantity) | (NamingSyst'
      +'em.useContext.value as Range) | (OperationDefinition.useContext.value as Quantity) | (Ope'+
   'rationDefinition.useContext.value as Range) | (SearchParameter.useContext.value as Quantity) | (SearchParameter.useContext.value as Range) | (StructureDe'
      +'finition.useContext.value as Quantity) | (StructureDefinition.useContext.value as Range) | (StructureMap.useContext.value as Quantity) | (StructureMap.useContext.value as Range) | (TerminologyCapabilities.useContext.value as Quantity) | (Term'
      +'inologyCapabilities.useContext.value as Range) | (ValueSet.useContext.value as Quantity) | (ValueSet.useContext.value as Range)', sxpNormal);
  indexes.add('StructureMap', 'context-type', '): A type of use context assigned to the structure map', sptTOKEN, [], 'CapabilityStatement.useContext.code | CodeSystem.useContext.code | CompartmentDefinition.useContext.code | ConceptMap.useContext.code | GraphDefinition.useContext.code | ImplementationGuide.useContext.code | MessageDefinition.useContext.code | Naming'
      +'System.useContext.code | OperationDefinition.useContext.code | SearchParameter.useContext.code | StructureDefinition.useContext.code | StructureMap.useContext.code | TerminologyCapabilities.useContext.code | ValueSet.useContext.code', sxpNormal);
  indexes.add('StructureMap', 'context-type-quantity', '): A use context type and quantity- or range-based value assigned to the structure map', sptCOMPOSITE, [], 'CapabilityStatement.useContext | CodeSystem.useContext | CompartmentDefinition.useContext | ConceptMap.useContext | GraphDefinition.useContext | ImplementationGuide.useContext | MessageDefinition.useContext | NamingSystem.useContext | OperationDefini'
      +'tion.useContext | SearchParameter.useContext | StructureDefinition.useContext | StructureMap.useContext | TerminologyCapabilities.useContext | ValueSet.useContext', sxpNormal);
  indexes.add('StructureMap', 'context-type-value', '): A use context type and value assigned to the structure map', sptCOMPOSITE, [], 'CapabilityStatement.useContext | CodeSystem.useContext | CompartmentDefinition.useContext | ConceptMap.useContext | GraphDefinition.useContext | ImplementationGuide.useContext | MessageDefinition.useContext | NamingSystem.useContext | OperationDefini'
      +'tion.useContext | SearchParameter.useContext | StructureDefinition.useContext | StructureMap.useContext | TerminologyCapabilities.useContext | ValueSet.useContext', sxpNormal);
  indexes.add('StructureMap', 'date', '): The structure map publication date', sptDATE, [], 'CapabilityStatement.date | CodeSystem.date | CompartmentDefinition.date | ConceptMap.date | GraphDefinition.date | ImplementationGuide.date | MessageDefinition.date | NamingSystem.date | OperationDefinition.date | SearchParameter.date | StructureDefi'
      +'nition.date | StructureMap.date | TerminologyCapabilities.date | ValueSet.date', sxpNormal);
  indexes.add('StructureMap', 'description', '): The description of the structure map', sptSTRING, [], 'CapabilityStatement.description | CodeSystem.description | CompartmentDefinition.description | ConceptMap.description | GraphDefinition.description | ImplementationGuide.description | MessageDefinition.description | NamingSystem.description | Operati'
      +'onDefinition.description | SearchParameter.description | StructureDefinition.description | StructureMap.description | TerminologyCapabilities.description | ValueSet.description', sxpNormal);
  indexes.add('StructureMap', 'identifier', '): External identifier for the structure map', sptTOKEN, [], 'CodeSystem.identifier | ConceptMap.identifier | MessageDefinition.identifier | NamingSystem.identifier | StructureDefinition.identifier | StructureMap.identifier | TerminologyCapabilities.identifier | ValueSet.identifier', sxpNormal);
  indexes.add('StructureMap', 'jurisdiction', '): Intended jurisdiction for the structure map', sptTOKEN, [], 'CapabilityStatement.jurisdiction | CodeSystem.jurisdiction | ConceptMap.jurisdiction | GraphDefinition.jurisdiction | ImplementationGuide.jurisdiction | MessageDefinition.jurisdiction | NamingSystem.jurisdiction | OperationDefinition.jurisdiction | S'
      +'earchParameter.jurisdiction | StructureDefinition.jurisdiction | StructureMap.jurisdiction | TerminologyCapabilities.jurisdiction | ValueSet.jurisdiction', sxpNormal);
  indexes.add('StructureMap', 'name', '): Computationally friendly name of the structure map', sptSTRING, [], 'CapabilityStatement.name | CodeSystem.name | CompartmentDefinition.name | ConceptMap.name | GraphDefinition.name | ImplementationGuide.name | MessageDefinition.name | NamingSystem.name | OperationDefinition.name | SearchParameter.name | StructureDefi'
      +'nition.name | StructureMap.name | TerminologyCapabilities.name | ValueSet.name', sxpNormal);
  indexes.add('StructureMap', 'publisher', '): Name of the publisher of the structure map', sptSTRING, [], 'CapabilityStatement.publisher | CodeSystem.publisher | CompartmentDefinition.publisher | ConceptMap.publisher | GraphDefinition.publisher | ImplementationGuide.publisher | MessageDefinition.publisher | NamingSystem.publisher | OperationDefinition.pub'
      +'lisher | SearchParameter.publisher | StructureDefinition.publisher | StructureMap.publisher | TerminologyCapabilities.publisher | ValueSet.publisher', sxpNormal);
  indexes.add('StructureMap', 'status', '): The current status of the structure map', sptTOKEN, [], 'CapabilityStatement.status | CodeSystem.status | CompartmentDefinition.status | ConceptMap.status | GraphDefinition.status | ImplementationGuide.status | MessageDefinition.status | NamingSystem.status | OperationDefinition.status | SearchParameter.st'
      +'atus | StructureDefinition.status | StructureMap.status | TerminologyCapabilities.status | ValueSet.status', sxpNormal);
  indexes.add('StructureMap', 'title', '): The human-friendly name of the structure map', sptSTRING, [], 'CapabilityStatement.title | CodeSystem.title | ConceptMap.title | ImplementationGuide.title | MessageDefinition.title | OperationDefinition.title | StructureDefinition.title | StructureMap.title | TerminologyCapabilities.title | ValueSet.title', sxpNormal);
  indexes.add('StructureMap', 'url', '): The uri that identifies the structure map', sptURI, [], 'CapabilityStatement.url | CodeSystem.url | CompartmentDefinition.url | ConceptMap.url | GraphDefinition.url | ImplementationGuide.url | MessageDefinition.url | NamingSystem.url | OperationDefinition.url | SearchParameter.url | StructureDefinition.url'
      +' | StructureMap.url | TerminologyCapabilities.url | ValueSet.url', sxpNormal);
  indexes.add('StructureMap', 'version', '): The business version of the structure map', sptTOKEN, [], 'CapabilityStatement.version | CodeSystem.version | CompartmentDefinition.version | ConceptMap.version | GraphDefinition.version | ImplementationGuide.version | MessageDefinition.version | NamingSystem.version | OperationDefinition.version | SearchPar'
      +'ameter.version | StructureDefinition.version | StructureMap.version | TerminologyCapabilities.version | ValueSet.version', sxpNormal);
  indexes.add('StructureMap', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_STRUCTUREMAP}

{$IFDEF FHIR_SUBSCRIPTION}
procedure TFHIRIndexBuilderR5.buildIndexesForSubscription(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Subscription', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('Subscription', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('Subscription', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('Subscription', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('Subscription', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('Subscription', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('Subscription', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('Subscription', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('Subscription', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('Subscription', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('Subscription', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('Subscription', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('Subscription', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('Subscription', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('Subscription', 'contact', 'Contact details for the subscription', sptTOKEN, [], 'Subscription.contact', sxpNormal);
  indexes.add('Subscription', 'identifier', 'A subscription identifier', sptTOKEN, [], 'Subscription.identifier', sxpNormal);
  indexes.add('Subscription', 'payload', 'The mime-type of the notification payload', sptTOKEN, [], '', sxpNormal);
  indexes.add('Subscription', 'status', 'The current state of the subscription', sptTOKEN, [], 'Subscription.status', sxpNormal);
  indexes.add('Subscription', 'type', 'The type of channel for the sent notifications', sptTOKEN, [], '', sxpNormal);
  indexes.add('Subscription', 'url', 'The uri that will receive the notifications', sptURI, [], '', sxpNormal);
  indexes.add('Subscription', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_SUBSCRIPTION}

{$IFDEF FHIR_SUBSCRIPTIONSTATUS}
procedure TFHIRIndexBuilderR5.buildIndexesForSubscriptionStatus(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('SubscriptionStatus', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('SubscriptionStatus', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('SubscriptionStatus', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('SubscriptionStatus', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('SubscriptionStatus', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('SubscriptionStatus', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('SubscriptionStatus', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('SubscriptionStatus', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('SubscriptionStatus', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('SubscriptionStatus', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('SubscriptionStatus', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('SubscriptionStatus', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('SubscriptionStatus', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('SubscriptionStatus', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('SubscriptionStatus', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_SUBSCRIPTIONSTATUS}

{$IFDEF FHIR_SUBSCRIPTIONTOPIC}
procedure TFHIRIndexBuilderR5.buildIndexesForSubscriptionTopic(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('SubscriptionTopic', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('SubscriptionTopic', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('SubscriptionTopic', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('SubscriptionTopic', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('SubscriptionTopic', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('SubscriptionTopic', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('SubscriptionTopic', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('SubscriptionTopic', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('SubscriptionTopic', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('SubscriptionTopic', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('SubscriptionTopic', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('SubscriptionTopic', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('SubscriptionTopic', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('SubscriptionTopic', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('SubscriptionTopic', 'date', 'Date status first applied', sptDATE, [], 'SubscriptionTopic.date', sxpNormal);
  indexes.add('SubscriptionTopic', 'derived-or-self', 'A server defined search that matches either the url or derivedFrom', sptURI, [], 'SubscriptionTopic.url | SubscriptionTopic.derivedFrom', sxpNormal);
  indexes.add('SubscriptionTopic', 'identifier', 'Business Identifier for SubscriptionTopic', sptTOKEN, [], 'SubscriptionTopic.identifier', sxpNormal);
  indexes.add('SubscriptionTopic', 'resource', 'Allowed Data type or Resource (reference to definition) for this definition, searches resourceTrigger, eventTrigger, and notificationShape for matches.', sptURI, [], 'SubscriptionTopic.resourceTrigger.resource', sxpNormal);
  indexes.add('SubscriptionTopic', 'status', 'draft | active | retired | unknown', sptTOKEN, [], 'SubscriptionTopic.status', sxpNormal);
  indexes.add('SubscriptionTopic', 'title', 'Name for this SubscriptionTopic (Human friendly)', sptSTRING, [], 'SubscriptionTopic.title', sxpNormal);
  indexes.add('SubscriptionTopic', 'trigger-description', 'Text representation of the trigger', sptSTRING, [], 'SubscriptionTopic.resourceTrigger.description', sxpNormal);
  indexes.add('SubscriptionTopic', 'url', 'Logical canonical URL to reference this SubscriptionTopic (globally unique)', sptURI, [], 'SubscriptionTopic.url', sxpNormal);
  indexes.add('SubscriptionTopic', 'version', 'Business version of the SubscriptionTopic', sptTOKEN, [], 'SubscriptionTopic.version', sxpNormal);
  indexes.add('SubscriptionTopic', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_SUBSCRIPTIONTOPIC}

{$IFDEF FHIR_SUBSTANCE}
procedure TFHIRIndexBuilderR5.buildIndexesForSubstance(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Substance', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('Substance', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('Substance', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('Substance', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('Substance', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('Substance', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('Substance', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('Substance', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('Substance', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('Substance', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('Substance', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('Substance', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('Substance', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('Substance', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('Substance', 'category', 'The category of the substance', sptTOKEN, [], 'Substance.category', sxpNormal);
  indexes.add('Substance', 'code', 'The code of the substance or ingredient', sptTOKEN, [], 'Substance.code.concept | (Substance.ingredient.substance as CodeableConcept)', sxpNormal);
  indexes.add('Substance', 'code-reference', 'A reference to the defining substance', sptREFERENCE, [], 'Substance.code.reference', sxpNormal);
  indexes.add('Substance', 'expiry', 'Expiry date of package or container of substance', sptDATE, [], 'Substance.expiry', sxpNormal);
  indexes.add('Substance', 'identifier', 'Unique identifier for the substance', sptTOKEN, [], 'Substance.identifier', sxpNormal);
  indexes.add('Substance', 'quantity', 'Amount of substance in the package', sptQUANTITY, [], 'Substance.quantity', sxpNormal);
  indexes.add('Substance', 'status', 'active | inactive | entered-in-error', sptTOKEN, [], 'Substance.status', sxpNormal);
  indexes.add('Substance', 'substance-reference', 'A component of the substance', sptREFERENCE, ['Substance'], '(Substance.ingredient.substance as Reference)', sxpNormal);
  indexes.add('Substance', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_SUBSTANCE}

{$IFDEF FHIR_SUBSTANCEDEFINITION}
procedure TFHIRIndexBuilderR5.buildIndexesForSubstanceDefinition(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('SubstanceDefinition', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('SubstanceDefinition', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('SubstanceDefinition', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('SubstanceDefinition', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('SubstanceDefinition', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('SubstanceDefinition', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('SubstanceDefinition', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('SubstanceDefinition', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('SubstanceDefinition', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('SubstanceDefinition', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('SubstanceDefinition', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('SubstanceDefinition', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('SubstanceDefinition', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('SubstanceDefinition', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('SubstanceDefinition', 'classification', 'High or low level categorization, e.g. polymer vs. nucleic acid or linear vs. branch chain', sptTOKEN, [], 'SubstanceDefinition.classification', sxpNormal);
  indexes.add('SubstanceDefinition', 'code', 'The specific code', sptTOKEN, [], 'SubstanceDefinition.code.code', sxpNormal);
  indexes.add('SubstanceDefinition', 'domain', 'If the substance applies to only human or veterinary use', sptTOKEN, [], 'SubstanceDefinition.domain', sxpNormal);
  indexes.add('SubstanceDefinition', 'identifier', 'Identifier by which this substance is known', sptTOKEN, [], 'SubstanceDefinition.identifier', sxpNormal);
  indexes.add('SubstanceDefinition', 'name', 'The actual name', sptSTRING, [], 'SubstanceDefinition.name.name', sxpNormal);
  indexes.add('SubstanceDefinition', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_SUBSTANCEDEFINITION}

{$IFDEF FHIR_SUBSTANCENUCLEICACID}
procedure TFHIRIndexBuilderR5.buildIndexesForSubstanceNucleicAcid(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('SubstanceNucleicAcid', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('SubstanceNucleicAcid', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('SubstanceNucleicAcid', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('SubstanceNucleicAcid', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('SubstanceNucleicAcid', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('SubstanceNucleicAcid', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('SubstanceNucleicAcid', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('SubstanceNucleicAcid', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('SubstanceNucleicAcid', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('SubstanceNucleicAcid', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('SubstanceNucleicAcid', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('SubstanceNucleicAcid', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('SubstanceNucleicAcid', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('SubstanceNucleicAcid', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('SubstanceNucleicAcid', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_SUBSTANCENUCLEICACID}

{$IFDEF FHIR_SUBSTANCEPOLYMER}
procedure TFHIRIndexBuilderR5.buildIndexesForSubstancePolymer(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('SubstancePolymer', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('SubstancePolymer', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('SubstancePolymer', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('SubstancePolymer', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('SubstancePolymer', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('SubstancePolymer', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('SubstancePolymer', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('SubstancePolymer', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('SubstancePolymer', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('SubstancePolymer', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('SubstancePolymer', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('SubstancePolymer', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('SubstancePolymer', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('SubstancePolymer', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('SubstancePolymer', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_SUBSTANCEPOLYMER}

{$IFDEF FHIR_SUBSTANCEPROTEIN}
procedure TFHIRIndexBuilderR5.buildIndexesForSubstanceProtein(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('SubstanceProtein', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('SubstanceProtein', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('SubstanceProtein', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('SubstanceProtein', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('SubstanceProtein', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('SubstanceProtein', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('SubstanceProtein', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('SubstanceProtein', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('SubstanceProtein', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('SubstanceProtein', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('SubstanceProtein', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('SubstanceProtein', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('SubstanceProtein', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('SubstanceProtein', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('SubstanceProtein', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_SUBSTANCEPROTEIN}

{$IFDEF FHIR_SUBSTANCEREFERENCEINFORMATION}
procedure TFHIRIndexBuilderR5.buildIndexesForSubstanceReferenceInformation(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('SubstanceReferenceInformation', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('SubstanceReferenceInformation', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('SubstanceReferenceInformation', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('SubstanceReferenceInformation', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('SubstanceReferenceInformation', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('SubstanceReferenceInformation', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('SubstanceReferenceInformation', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('SubstanceReferenceInformation', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('SubstanceReferenceInformation', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('SubstanceReferenceInformation', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('SubstanceReferenceInformation', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('SubstanceReferenceInformation', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('SubstanceReferenceInformation', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('SubstanceReferenceInformation', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('SubstanceReferenceInformation', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_SUBSTANCEREFERENCEINFORMATION}

{$IFDEF FHIR_SUBSTANCESOURCEMATERIAL}
procedure TFHIRIndexBuilderR5.buildIndexesForSubstanceSourceMaterial(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('SubstanceSourceMaterial', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('SubstanceSourceMaterial', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('SubstanceSourceMaterial', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('SubstanceSourceMaterial', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('SubstanceSourceMaterial', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('SubstanceSourceMaterial', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('SubstanceSourceMaterial', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('SubstanceSourceMaterial', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('SubstanceSourceMaterial', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('SubstanceSourceMaterial', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('SubstanceSourceMaterial', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('SubstanceSourceMaterial', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('SubstanceSourceMaterial', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('SubstanceSourceMaterial', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('SubstanceSourceMaterial', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_SUBSTANCESOURCEMATERIAL}

{$IFDEF FHIR_SUPPLYDELIVERY}
procedure TFHIRIndexBuilderR5.buildIndexesForSupplyDelivery(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('SupplyDelivery', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('SupplyDelivery', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('SupplyDelivery', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('SupplyDelivery', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('SupplyDelivery', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('SupplyDelivery', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('SupplyDelivery', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('SupplyDelivery', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('SupplyDelivery', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('SupplyDelivery', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('SupplyDelivery', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('SupplyDelivery', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('SupplyDelivery', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('SupplyDelivery', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('SupplyDelivery', 'identifier', '): External identifier', sptTOKEN, [], 'AllergyIntolerance.identifier | CarePlan.identifier | CareTeam.identifier | Composition.identifier | Condition.identifier | Consent.identifier | DetectedIssue.identifier | DeviceRequest.identifier | DiagnosticReport.identifier | DocumentManifest.mast'
      +'erIdentifier | DocumentManifest.identifier | DocumentReference.identifier | Encounter.identifier | EpisodeOfCare.identifier | FamilyMemberHistory.identifier | Goal.identifier | ImagingStudy.identifier | Immunization.identifier | List.identifi'
      +'er | MedicationAdministration.identifier | MedicationDispense.identifier | MedicationRequest.identifier | MedicationUsage.identifier | NutritionOrder.identifier | Observation.identifier | Procedure.identifier | RiskAssessment.identifier | Ser'
      +'viceRequest.identifier | SupplyDelivery.identifier | SupplyRequest.identifier | VisionPrescription.identifier', sxpNormal);
  indexes.add('SupplyDelivery', 'patient', '): Patient for whom the item is supplied', sptREFERENCE, ['Patient', 'Practitioner', 'Group', 'Organization', 'BiologicallyDerivedProduct', 'NutritionProduct', 'Device', 'Medication', 'Procedure', 'Substance', 'Location'], 'AllergyIntolerance.patient | CarePlan.subject.where(resolve() is Patient) | CareTeam.subject.where(resolve() is Patient) | ClinicalImpression.subject.where(resolve() is Patient) | Composition.subject.where(resolve() is Patient) | Condition.subject.wh'
      +'ere(resolve() is Patient) | Consent.subject.where(resolve() is Patient) | DetectedIssue.subject | DeviceRequest.subject.where(resolve() is Patient) | DeviceUsage.patient | DiagnosticReport.subject.where(resolve() is Patient) | DocumentManifes'
      +'t.subject.where(resolve() is Patient) | DocumentReference.subject.where(resolve() is Patient) | Encounter.subject.where(resolve() is Patient) | EpisodeOfCare.patient | FamilyMemberHistory.patient | Flag.subject.where(r'+
   'esolve() is Patient) | G'
      +'oal.subject.where(resolve() is Patient) | ImagingStudy.subject.where(resolve() is Patient) | Immunization.patient | List.subject.where(resolve() is Patient) | MedicationAdministration.subject.where(resolve() is Patient) | MedicationDispense.s'
      +'ubject.where(resolve() is Patient) | MedicationRequest.subject.where(resolve() is Patient) | MedicationUsage.subject.where(resolve() is Patient) | NutritionOrder.subject.where(resolve() is Patient) | Observation.subject.where(resolve() is Pat'
      +'ient) | Procedure.subject.where(resolve() is Patient) | RiskAssessment.subject.where(resolve() is Patient) | ServiceRequest.subject.where(resolve() is Patient) | SupplyDelivery.patient | VisionPrescription.patient', sxpNormal);
  indexes.add('SupplyDelivery', 'receiver', 'Who collected the Supply', sptREFERENCE, ['Practitioner', 'PractitionerRole'], 'SupplyDelivery.receiver', sxpNormal);
  indexes.add('SupplyDelivery', 'status', 'in-progress | completed | abandoned | entered-in-error', sptTOKEN, [], 'SupplyDelivery.status', sxpNormal);
  indexes.add('SupplyDelivery', 'supplier', 'Dispenser', sptREFERENCE, ['Practitioner', 'Organization', 'PractitionerRole'], 'SupplyDelivery.supplier', sxpNormal);
  indexes.add('SupplyDelivery', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
  compartments.register('Patient', 'SupplyDelivery', ['patient']);
  compartments.register('Practitioner', 'SupplyDelivery', ['supplier', 'receiver']);
end;
{$ENDIF FHIR_SUPPLYDELIVERY}

{$IFDEF FHIR_SUPPLYREQUEST}
procedure TFHIRIndexBuilderR5.buildIndexesForSupplyRequest(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('SupplyRequest', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('SupplyRequest', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('SupplyRequest', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('SupplyRequest', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('SupplyRequest', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('SupplyRequest', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('SupplyRequest', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('SupplyRequest', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('SupplyRequest', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('SupplyRequest', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('SupplyRequest', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('SupplyRequest', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('SupplyRequest', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('SupplyRequest', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('SupplyRequest', 'category', 'The kind of supply (central, non-stock, etc.)', sptTOKEN, [], 'SupplyRequest.category', sxpNormal);
  indexes.add('SupplyRequest', 'date', '): When the request was made', sptDATE, [], 'AllergyIntolerance.recordedDate | CarePlan.period | ClinicalImpression.date | Composition.date | Consent.dateTime | DiagnosticReport.effective | Encounter.actualPeriod | EpisodeOfCare.period | FamilyMemberHistory.date | Flag.period | (Immunization.oc'
      +'currence as dateTime) | List.date | Observation.effective | Procedure.occurrence | (RiskAssessment.occurrence as dateTime) | SupplyRequest.authoredOn', sxpNormal);
  indexes.add('SupplyRequest', 'identifier', '): Business Identifier for SupplyRequest', sptTOKEN, [], 'AllergyIntolerance.identifier | CarePlan.identifier | CareTeam.identifier | Composition.identifier | Condition.identifier | Consent.identifier | DetectedIssue.identifier | DeviceRequest.identifier | DiagnosticReport.identifier | DocumentManifest.mast'
      +'erIdentifier | DocumentManifest.identifier | DocumentReference.identifier | Encounter.identifier | EpisodeOfCare.identifier | FamilyMemberHistory.identifier | Goal.identifier | ImagingStudy.identifier | Immunization.identifier | List.identifi'
      +'er | MedicationAdministration.identifier | MedicationDispense.identifier | MedicationRequest.identifier | MedicationUsage.identifier | NutritionOrder.identifier | Observation.identifier | Procedure.identifier | RiskAssessment.identifier | Ser'
      +'viceRequest.identifier | SupplyDelivery.identifier | SupplyRequest.identifier | VisionPrescription.identifier', sxpNormal);
  indexes.add('SupplyRequest', 'requester', 'Individual making the request', sptREFERENCE, ['Practitioner', 'Organization', 'CareTeam', 'Device', 'Patient', 'PractitionerRole', 'RelatedPerson'], 'SupplyRequest.requester', sxpNormal);
  indexes.add('SupplyRequest', 'status', 'draft | active | suspended +', sptTOKEN, [], 'SupplyRequest.status', sxpNormal);
  indexes.add('SupplyRequest', 'subject', 'The destination of the supply', sptREFERENCE, ['Organization', 'Patient', 'Location'], 'SupplyRequest.deliverTo', sxpNormal);
  indexes.add('SupplyRequest', 'supplier', 'Who is intended to fulfill the request', sptREFERENCE, ['Organization', 'HealthcareService'], 'SupplyRequest.supplier', sxpNormal);
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
  indexes.add('Task', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('Task', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('Task', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('Task', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('Task', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('Task', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('Task', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('Task', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('Task', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('Task', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('Task', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('Task', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('Task', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('Task', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('Task', 'authored-on', 'Search by creation date', sptDATE, [], 'Task.authoredOn', sxpNormal);
  indexes.add('Task', 'based-on', 'Search by requests this task is based on', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'Task.basedOn', sxpNormal);
  indexes.add('Task', 'business-status', 'Search by business status', sptTOKEN, [], 'Task.businessStatus', sxpNormal);
  indexes.add('Task', 'code', 'Search by task code', sptTOKEN, [], 'Task.code', sxpNormal);
  indexes.add('Task', 'encounter', 'Search by encounter', sptREFERENCE, ['Encounter'], 'Task.encounter', sxpNormal);
  indexes.add('Task', 'focus', 'Search by task focus', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'Task.focus', sxpNormal);
  indexes.add('Task', 'group-identifier', 'Search by group identifier', sptTOKEN, [], 'Task.groupIdentifier', sxpNormal);
  indexes.add('Task', 'identifier', 'Search for a task instance by its business identifier', sptTOKEN, [], 'Task.identifier', sxpNormal);
  indexes.add('Task', 'intent', 'Search by task intent', sptTOKEN, [], 'Task.intent', sxpNormal);
  indexes.add('Task', 'modified', 'Search by last modification date', sptDATE, [], 'Task.lastModified', sxpNormal);
  indexes.add('Task', 'output', 'Search by task output', sptREFERENCE, [], 'Task.output', sxpNormal);
  indexes.add('Task', 'owner', 'Search by task owner', sptREFERENCE, ['Practitioner', 'Organization', 'CareTeam', 'Device', 'Patient', 'HealthcareService', 'PractitionerRole', 'RelatedPerson'], 'Task.owner', sxpNormal);
  indexes.add('Task', 'part-of', 'Search by task this task is part of', sptREFERENCE, ['Task'], 'Task.partOf', sxpNormal);
  indexes.add('Task', 'patient', 'Search by patient', sptREFERENCE, ['Patient'], 'Task.for.where(resolve() is Patient)', sxpNormal);
  indexes.add('Task', 'performer', 'Search by specific requested performer.', sptREFERENCE, [], 'Task.requestedPerformer.reference', sxpNormal);
  indexes.add('Task', 'period', 'Search by period Task is/was underway', sptDATE, [], 'Task.executionPeriod', sxpNormal);
  indexes.add('Task', 'priority', 'Search by task priority', sptTOKEN, [], 'Task.priority', sxpNormal);
  indexes.add('Task', 'requester', 'Search by task requester', sptREFERENCE, ['Practitioner', 'Organization', 'Device', 'Patient', 'PractitionerRole', 'RelatedPerson'], 'Task.requester', sxpNormal);
  indexes.add('Task', 'status', 'Search by task status', sptTOKEN, [], 'Task.status', sxpNormal);
  indexes.add('Task', 'subject', 'Search by subject', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'Task.for', sxpNormal);
  indexes.add('Task', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_TASK}

{$IFDEF FHIR_TERMINOLOGYCAPABILITIES}
procedure TFHIRIndexBuilderR5.buildIndexesForTerminologyCapabilities(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('TerminologyCapabilities', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('TerminologyCapabilities', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('TerminologyCapabilities', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('TerminologyCapabilities', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('TerminologyCapabilities', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('TerminologyCapabilities', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('TerminologyCapabilities', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('TerminologyCapabilities', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('TerminologyCapabilities', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('TerminologyCapabilities', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('TerminologyCapabilities', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('TerminologyCapabilities', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('TerminologyCapabilities', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('TerminologyCapabilities', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('TerminologyCapabilities', 'context', '): A use context assigned to the terminology capabilities', sptTOKEN, [], '(CapabilityStatement.useContext.value as CodeableConcept) | (CodeSystem.useContext.value as CodeableConcept) | (CompartmentDefinition.useContext.value as CodeableConcept) | (ConceptMap.useContext.value as CodeableConcept) | (GraphDefinition.useContex'
      +'t.value as CodeableConcept) | (ImplementationGuide.useContext.value as CodeableConcept) | (MessageDefinition.useContext.value as CodeableConcept) | (NamingSystem.useContext.value as CodeableConcept) | (OperationDefinition.useContext.value as '
      +'CodeableConcept) | (SearchParameter.useContext.value as CodeableConcept) | (StructureDefinition.useContext.value as CodeableConcept) | (StructureMap.useContext.value as CodeableConcept) | (TerminologyCapabilities.useContext.value as CodeableC'
      +'oncept) | (ValueSet.useContext.value as CodeableConcept)', sxpNormal);
  indexes.add('TerminologyCapabilities', 'context-quantity', '): A quantity- or range-valued use context assigned to the terminology capabilities', sptQUANTITY, [], '(CapabilityStatement.useContext.value as Quantity) | (CapabilityStatement.useContext.value as Range) | (CodeSystem.useContext.value as Quantity) | (CodeSystem.useContext.value as Range) | (CompartmentDefinition.useContext.value as Quantity) | (Compar'
      +'tmentDefinition.useContext.value as Range) | (ConceptMap.useContext.value as Quantity) | (ConceptMap.useContext.value as Range) | (GraphDefinition.useContext.value as Quantity) | (GraphDefinition.useContext.value as Range) | (ImplementationGu'
      +'ide.useContext.value as Quantity) | (ImplementationGuide.useContext.value as Range) | (MessageDefinition.useContext.value as Quantity) | (MessageDefinition.useContext.value as Range) | (NamingSystem.useContext.value as Quantity) | (NamingSyst'
      +'em.useContext.value as Range) | (OperationDefinition.useContext.val'+
   'ue as Quantity) | (OperationDefinition.useContext.value as Range) | (SearchParameter.useContext.value as Quantity) | (SearchParameter.useContext.value as Range) | (StructureDe'
      +'finition.useContext.value as Quantity) | (StructureDefinition.useContext.value as Range) | (StructureMap.useContext.value as Quantity) | (StructureMap.useContext.value as Range) | (TerminologyCapabilities.useContext.value as Quantity) | (Term'
      +'inologyCapabilities.useContext.value as Range) | (ValueSet.useContext.value as Quantity) | (ValueSet.useContext.value as Range)', sxpNormal);
  indexes.add('TerminologyCapabilities', 'context-type', '): A type of use context assigned to the terminology capabilities', sptTOKEN, [], 'CapabilityStatement.useContext.code | CodeSystem.useContext.code | CompartmentDefinition.useContext.code | ConceptMap.useContext.code | GraphDefinition.useContext.code | ImplementationGuide.useContext.code | MessageDefinition.useContext.code | Naming'
      +'System.useContext.code | OperationDefinition.useContext.code | SearchParameter.useContext.code | StructureDefinition.useContext.code | StructureMap.useContext.code | TerminologyCapabilities.useContext.code | ValueSet.useContext.code', sxpNormal);
  indexes.add('TerminologyCapabilities', 'context-type-quantity', '): A use context type and quantity- or range-based value assigned to the terminology capabilities', sptCOMPOSITE, [], 'CapabilityStatement.useContext | CodeSystem.useContext | CompartmentDefinition.useContext | ConceptMap.useContext | GraphDefinition.useContext | ImplementationGuide.useContext | MessageDefinition.useContext | NamingSystem.useContext | OperationDefini'
      +'tion.useContext | SearchParameter.useContext | StructureDefinition.useContext | StructureMap.useContext | TerminologyCapabilities.useContext | ValueSet.useContext', sxpNormal);
  indexes.add('TerminologyCapabilities', 'context-type-value', '): A use context type and value assigned to the terminology capabilities', sptCOMPOSITE, [], 'CapabilityStatement.useContext | CodeSystem.useContext | CompartmentDefinition.useContext | ConceptMap.useContext | GraphDefinition.useContext | ImplementationGuide.useContext | MessageDefinition.useContext | NamingSystem.useContext | OperationDefini'
      +'tion.useContext | SearchParameter.useContext | StructureDefinition.useContext | StructureMap.useContext | TerminologyCapabilities.useContext | ValueSet.useContext', sxpNormal);
  indexes.add('TerminologyCapabilities', 'date', '): The terminology capabilities publication date', sptDATE, [], 'CapabilityStatement.date | CodeSystem.date | CompartmentDefinition.date | ConceptMap.date | GraphDefinition.date | ImplementationGuide.date | MessageDefinition.date | NamingSystem.date | OperationDefinition.date | SearchParameter.date | StructureDefi'
      +'nition.date | StructureMap.date | TerminologyCapabilities.date | ValueSet.date', sxpNormal);
  indexes.add('TerminologyCapabilities', 'description', '): The description of the terminology capabilities', sptSTRING, [], 'CapabilityStatement.description | CodeSystem.description | CompartmentDefinition.description | ConceptMap.description | GraphDefinition.description | ImplementationGuide.description | MessageDefinition.description | NamingSystem.description | Operati'
      +'onDefinition.description | SearchParameter.description | StructureDefinition.description | StructureMap.description | TerminologyCapabilities.description | ValueSet.description', sxpNormal);
  indexes.add('TerminologyCapabilities', 'identifier', '): External identifier for the terminology capabilities', sptTOKEN, [], 'CodeSystem.identifier | ConceptMap.identifier | MessageDefinition.identifier | NamingSystem.identifier | StructureDefinition.identifier | StructureMap.identifier | TerminologyCapabilities.identifier | ValueSet.identifier', sxpNormal);
  indexes.add('TerminologyCapabilities', 'jurisdiction', '): Intended jurisdiction for the terminology capabilities', sptTOKEN, [], 'CapabilityStatement.jurisdiction | CodeSystem.jurisdiction | ConceptMap.jurisdiction | GraphDefinition.jurisdiction | ImplementationGuide.jurisdiction | MessageDefinition.jurisdiction | NamingSystem.jurisdiction | OperationDefinition.jurisdiction | S'
      +'earchParameter.jurisdiction | StructureDefinition.jurisdiction | StructureMap.jurisdiction | TerminologyCapabilities.jurisdiction | ValueSet.jurisdiction', sxpNormal);
  indexes.add('TerminologyCapabilities', 'name', '): Computationally friendly name of the terminology capabilities', sptSTRING, [], 'CapabilityStatement.name | CodeSystem.name | CompartmentDefinition.name | ConceptMap.name | GraphDefinition.name | ImplementationGuide.name | MessageDefinition.name | NamingSystem.name | OperationDefinition.name | SearchParameter.name | StructureDefi'
      +'nition.name | StructureMap.name | TerminologyCapabilities.name | ValueSet.name', sxpNormal);
  indexes.add('TerminologyCapabilities', 'publisher', '): Name of the publisher of the terminology capabilities', sptSTRING, [], 'CapabilityStatement.publisher | CodeSystem.publisher | CompartmentDefinition.publisher | ConceptMap.publisher | GraphDefinition.publisher | ImplementationGuide.publisher | MessageDefinition.publisher | NamingSystem.publisher | OperationDefinition.pub'
      +'lisher | SearchParameter.publisher | StructureDefinition.publisher | StructureMap.publisher | TerminologyCapabilities.publisher | ValueSet.publisher', sxpNormal);
  indexes.add('TerminologyCapabilities', 'status', '): The current status of the terminology capabilities', sptTOKEN, [], 'CapabilityStatement.status | CodeSystem.status | CompartmentDefinition.status | ConceptMap.status | GraphDefinition.status | ImplementationGuide.status | MessageDefinition.status | NamingSystem.status | OperationDefinition.status | SearchParameter.st'
      +'atus | StructureDefinition.status | StructureMap.status | TerminologyCapabilities.status | ValueSet.status', sxpNormal);
  indexes.add('TerminologyCapabilities', 'title', '): The human-friendly name of the terminology capabilities', sptSTRING, [], 'CapabilityStatement.title | CodeSystem.title | ConceptMap.title | ImplementationGuide.title | MessageDefinition.title | OperationDefinition.title | StructureDefinition.title | StructureMap.title | TerminologyCapabilities.title | ValueSet.title', sxpNormal);
  indexes.add('TerminologyCapabilities', 'url', '): The uri that identifies the terminology capabilities', sptURI, [], 'CapabilityStatement.url | CodeSystem.url | CompartmentDefinition.url | ConceptMap.url | GraphDefinition.url | ImplementationGuide.url | MessageDefinition.url | NamingSystem.url | OperationDefinition.url | SearchParameter.url | StructureDefinition.url'
      +' | StructureMap.url | TerminologyCapabilities.url | ValueSet.url', sxpNormal);
  indexes.add('TerminologyCapabilities', 'version', '): The business version of the terminology capabilities', sptTOKEN, [], 'CapabilityStatement.version | CodeSystem.version | CompartmentDefinition.version | ConceptMap.version | GraphDefinition.version | ImplementationGuide.version | MessageDefinition.version | NamingSystem.version | OperationDefinition.version | SearchPar'
      +'ameter.version | StructureDefinition.version | StructureMap.version | TerminologyCapabilities.version | ValueSet.version', sxpNormal);
  indexes.add('TerminologyCapabilities', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_TERMINOLOGYCAPABILITIES}

{$IFDEF FHIR_TESTREPORT}
procedure TFHIRIndexBuilderR5.buildIndexesForTestReport(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('TestReport', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('TestReport', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('TestReport', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('TestReport', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('TestReport', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('TestReport', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('TestReport', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('TestReport', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('TestReport', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('TestReport', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('TestReport', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('TestReport', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('TestReport', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('TestReport', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('TestReport', 'identifier', 'An external identifier for the test report', sptTOKEN, [], 'TestReport.identifier', sxpNormal);
  indexes.add('TestReport', 'issued', 'The test report generation date', sptDATE, [], 'TestReport.issued', sxpNormal);
  indexes.add('TestReport', 'participant', 'The reference to a participant in the test execution', sptURI, [], 'TestReport.participant.uri', sxpNormal);
  indexes.add('TestReport', 'result', 'The result disposition of the test execution', sptTOKEN, [], 'TestReport.result', sxpNormal);
  indexes.add('TestReport', 'tester', 'The name of the testing organization', sptSTRING, [], 'TestReport.tester', sxpNormal);
  indexes.add('TestReport', 'testscript', 'The test script executed to produce this report', sptREFERENCE, ['TestScript'], 'TestReport.testScript', sxpNormal);
  indexes.add('TestReport', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_TESTREPORT}

{$IFDEF FHIR_TESTSCRIPT}
procedure TFHIRIndexBuilderR5.buildIndexesForTestScript(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('TestScript', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('TestScript', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('TestScript', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('TestScript', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('TestScript', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('TestScript', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('TestScript', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('TestScript', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('TestScript', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('TestScript', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('TestScript', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('TestScript', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('TestScript', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('TestScript', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('TestScript', 'context', 'A use context assigned to the test script', sptTOKEN, [], '(TestScript.useContext.value as CodeableConcept)', sxpNormal);
  indexes.add('TestScript', 'context-quantity', 'A quantity- or range-valued use context assigned to the test script', sptQUANTITY, [], '(TestScript.useContext.value as Quantity) | (TestScript.useContext.value as Range)', sxpNormal);
  indexes.add('TestScript', 'context-type', 'A type of use context assigned to the test script', sptTOKEN, [], 'TestScript.useContext.code', sxpNormal);
  indexes.add('TestScript', 'context-type-quantity', 'A use context type and quantity- or range-based value assigned to the test script', sptCOMPOSITE, [], 'TestScript.useContext', sxpNormal);
  indexes.add('TestScript', 'context-type-value', 'A use context type and value assigned to the test script', sptCOMPOSITE, [], 'TestScript.useContext', sxpNormal);
  indexes.add('TestScript', 'date', 'The test script publication date', sptDATE, [], 'TestScript.date', sxpNormal);
  indexes.add('TestScript', 'description', 'The description of the test script', sptSTRING, [], 'TestScript.description', sxpNormal);
  indexes.add('TestScript', 'identifier', 'External identifier for the test script', sptTOKEN, [], 'TestScript.identifier', sxpNormal);
  indexes.add('TestScript', 'jurisdiction', 'Intended jurisdiction for the test script', sptTOKEN, [], 'TestScript.jurisdiction', sxpNormal);
  indexes.add('TestScript', 'name', 'Computationally friendly name of the test script', sptSTRING, [], 'TestScript.name', sxpNormal);
  indexes.add('TestScript', 'publisher', 'Name of the publisher of the test script', sptSTRING, [], 'TestScript.publisher', sxpNormal);
  indexes.add('TestScript', 'scope-artifact', 'The artifact under test', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'TestScript.scope.artifact', sxpNormal);
  indexes.add('TestScript', 'scope-artifact-conformance', 'The artifact under test and conformance testing expectation', sptCOMPOSITE, [], 'TestScript.scope', sxpNormal);
  indexes.add('TestScript', 'scope-artifact-phase', 'The artifact under test and phase of testing', sptCOMPOSITE, [], 'TestScript.scope', sxpNormal);
  indexes.add('TestScript', 'status', 'The current status of the test script', sptTOKEN, [], 'TestScript.status', sxpNormal);
  indexes.add('TestScript', 'testscript-capability', 'TestScript required and validated capability', sptSTRING, [], 'TestScript.metadata.capability.description', sxpNormal);
  indexes.add('TestScript', 'title', 'The human-friendly name of the test script', sptSTRING, [], 'TestScript.title', sxpNormal);
  indexes.add('TestScript', 'url', 'The uri that identifies the test script', sptURI, [], 'TestScript.url', sxpNormal);
  indexes.add('TestScript', 'version', 'The business version of the test script', sptTOKEN, [], 'TestScript.version', sxpNormal);
  indexes.add('TestScript', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_TESTSCRIPT}

{$IFDEF FHIR_TRANSPORT}
procedure TFHIRIndexBuilderR5.buildIndexesForTransport(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('Transport', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('Transport', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('Transport', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('Transport', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('Transport', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('Transport', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('Transport', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('Transport', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('Transport', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('Transport', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('Transport', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('Transport', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('Transport', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('Transport', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('Transport', 'identifier', 'External identifier', sptTOKEN, [], 'Transport.identifier', sxpNormal);
  indexes.add('Transport', 'status', 'in-progress | completed | entered-in-error', sptTOKEN, [], 'Transport.status', sxpNormal);
  indexes.add('Transport', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_TRANSPORT}

{$IFDEF FHIR_VALUESET}
procedure TFHIRIndexBuilderR5.buildIndexesForValueSet(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('ValueSet', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('ValueSet', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('ValueSet', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('ValueSet', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('ValueSet', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('ValueSet', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('ValueSet', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('ValueSet', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('ValueSet', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('ValueSet', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('ValueSet', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('ValueSet', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('ValueSet', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('ValueSet', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('ValueSet', 'author', 'Optional Extensions Element', sptSTRING, [], 'CodeSystem.extension(''http://hl7.org/fhir/StructureDefinition/valueset-author'').value', sxpNormal);
  indexes.add('ValueSet', 'code', 'This special parameter searches for codes in the value set. See additional notes on the ValueSet resource', sptTOKEN, [], 'ValueSet.expansion.contains.code | ValueSet.compose.include.concept.code', sxpNormal);
  indexes.add('ValueSet', 'context', '): A use context assigned to the value set', sptTOKEN, [], '(CapabilityStatement.useContext.value as CodeableConcept) | (CodeSystem.useContext.value as CodeableConcept) | (CompartmentDefinition.useContext.value as CodeableConcept) | (ConceptMap.useContext.value as CodeableConcept) | (GraphDefinition.useContex'
      +'t.value as CodeableConcept) | (ImplementationGuide.useContext.value as CodeableConcept) | (MessageDefinition.useContext.value as CodeableConcept) | (NamingSystem.useContext.value as CodeableConcept) | (OperationDefinition.useContext.value as '
      +'CodeableConcept) | (SearchParameter.useContext.value as CodeableConcept) | (StructureDefinition.useContext.value as CodeableConcept) | (StructureMap.useContext.value as CodeableConcept) | (TerminologyCapabilities.useContext.value as CodeableC'
      +'oncept) | (ValueSet.useContext.value as CodeableConcept)', sxpNormal);
  indexes.add('ValueSet', 'context-quantity', '): A quantity- or range-valued use context assigned to the value set', sptQUANTITY, [], '(CapabilityStatement.useContext.value as Quantity) | (CapabilityStatement.useContext.value as Range) | (CodeSystem.useContext.value as Quantity) | (CodeSystem.useContext.value as Range) | (CompartmentDefinition.useContext.value as Quantity) | (Compar'
      +'tmentDefinition.useContext.value as Range) | (ConceptMap.useContext.value as Quantity) | (ConceptMap.useContext.value as Range) | (GraphDefinition.useContext.value as Quantity) | (GraphDefinition.useContext.value as Range) | (ImplementationGu'
      +'ide.useContext.value as Quantity) | (ImplementationGuide.useContext.value as Range) | (MessageDefinition.useContext.value as Quantity) | (MessageDefinition.useContext.value as Range) | (NamingSystem.useContext.value as Quantity) | (NamingSyst'
      +'em.useContext.value as Range) | (OperationDefinition.useContext.value as Quantity) | (OperationDe'+
   'finition.useContext.value as Range) | (SearchParameter.useContext.value as Quantity) | (SearchParameter.useContext.value as Range) | (StructureDe'
      +'finition.useContext.value as Quantity) | (StructureDefinition.useContext.value as Range) | (StructureMap.useContext.value as Quantity) | (StructureMap.useContext.value as Range) | (TerminologyCapabilities.useContext.value as Quantity) | (Term'
      +'inologyCapabilities.useContext.value as Range) | (ValueSet.useContext.value as Quantity) | (ValueSet.useContext.value as Range)', sxpNormal);
  indexes.add('ValueSet', 'context-type', '): A type of use context assigned to the value set', sptTOKEN, [], 'CapabilityStatement.useContext.code | CodeSystem.useContext.code | CompartmentDefinition.useContext.code | ConceptMap.useContext.code | GraphDefinition.useContext.code | ImplementationGuide.useContext.code | MessageDefinition.useContext.code | Naming'
      +'System.useContext.code | OperationDefinition.useContext.code | SearchParameter.useContext.code | StructureDefinition.useContext.code | StructureMap.useContext.code | TerminologyCapabilities.useContext.code | ValueSet.useContext.code', sxpNormal);
  indexes.add('ValueSet', 'context-type-quantity', '): A use context type and quantity- or range-based value assigned to the value set', sptCOMPOSITE, [], 'CapabilityStatement.useContext | CodeSystem.useContext | CompartmentDefinition.useContext | ConceptMap.useContext | GraphDefinition.useContext | ImplementationGuide.useContext | MessageDefinition.useContext | NamingSystem.useContext | OperationDefini'
      +'tion.useContext | SearchParameter.useContext | StructureDefinition.useContext | StructureMap.useContext | TerminologyCapabilities.useContext | ValueSet.useContext', sxpNormal);
  indexes.add('ValueSet', 'context-type-value', '): A use context type and value assigned to the value set', sptCOMPOSITE, [], 'CapabilityStatement.useContext | CodeSystem.useContext | CompartmentDefinition.useContext | ConceptMap.useContext | GraphDefinition.useContext | ImplementationGuide.useContext | MessageDefinition.useContext | NamingSystem.useContext | OperationDefini'
      +'tion.useContext | SearchParameter.useContext | StructureDefinition.useContext | StructureMap.useContext | TerminologyCapabilities.useContext | ValueSet.useContext', sxpNormal);
  indexes.add('ValueSet', 'date', '): The value set publication date', sptDATE, [], 'CapabilityStatement.date | CodeSystem.date | CompartmentDefinition.date | ConceptMap.date | GraphDefinition.date | ImplementationGuide.date | MessageDefinition.date | NamingSystem.date | OperationDefinition.date | SearchParameter.date | StructureDefi'
      +'nition.date | StructureMap.date | TerminologyCapabilities.date | ValueSet.date', sxpNormal);
  indexes.add('ValueSet', 'derived-from', 'A resource that the ValueSet is derived from', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'ValueSet.relatedArtifact.where(type=''derived-from'').resource', sxpNormal);
  indexes.add('ValueSet', 'description', '): The description of the value set', sptSTRING, [], 'CapabilityStatement.description | CodeSystem.description | CompartmentDefinition.description | ConceptMap.description | GraphDefinition.description | ImplementationGuide.description | MessageDefinition.description | NamingSystem.description | Operati'
      +'onDefinition.description | SearchParameter.description | StructureDefinition.description | StructureMap.description | TerminologyCapabilities.description | ValueSet.description', sxpNormal);
  indexes.add('ValueSet', 'effective', '): The time during which the ValueSet is intended to be in use', sptDATE, [], 'CodeSystem.effectivePeriod | ConceptMap.effectivePeriod | NamingSystem.effectivePeriod | ValueSet.effectivePeriod', sxpNormal);
  indexes.add('ValueSet', 'effective', 'Optional Extensions Element', sptDATE, [], 'CodeSystem.extension(''http://hl7.org/fhir/StructureDefinition/valueset-effectiveDate'').value', sxpNormal);
  indexes.add('ValueSet', 'end', 'Optional Extensions Element', sptDATE, [], 'CodeSystem.extension(''http://hl7.org/fhir/StructureDefinition/valueset-expirationDate'').value', sxpNormal);
  indexes.add('ValueSet', 'expansion', 'Identifies the value set expansion (business identifier)', sptURI, [], 'ValueSet.expansion.identifier', sxpNormal);
  indexes.add('ValueSet', 'identifier', '): External identifier for the value set', sptTOKEN, [], 'CodeSystem.identifier | ConceptMap.identifier | MessageDefinition.identifier | NamingSystem.identifier | StructureDefinition.identifier | StructureMap.identifier | TerminologyCapabilities.identifier | ValueSet.identifier', sxpNormal);
  indexes.add('ValueSet', 'jurisdiction', '): Intended jurisdiction for the value set', sptTOKEN, [], 'CapabilityStatement.jurisdiction | CodeSystem.jurisdiction | ConceptMap.jurisdiction | GraphDefinition.jurisdiction | ImplementationGuide.jurisdiction | MessageDefinition.jurisdiction | NamingSystem.jurisdiction | OperationDefinition.jurisdiction | S'
      +'earchParameter.jurisdiction | StructureDefinition.jurisdiction | StructureMap.jurisdiction | TerminologyCapabilities.jurisdiction | ValueSet.jurisdiction', sxpNormal);
  indexes.add('ValueSet', 'keyword', 'Optional Extensions Element', sptSTRING, [], 'CodeSystem.extension(''http://hl7.org/fhir/StructureDefinition/valueset-keyWord'').value', sxpNormal);
  indexes.add('ValueSet', 'name', '): Computationally friendly name of the value set', sptSTRING, [], 'CapabilityStatement.name | CodeSystem.name | CompartmentDefinition.name | ConceptMap.name | GraphDefinition.name | ImplementationGuide.name | MessageDefinition.name | NamingSystem.name | OperationDefinition.name | SearchParameter.name | StructureDefi'
      +'nition.name | StructureMap.name | TerminologyCapabilities.name | ValueSet.name', sxpNormal);
  indexes.add('ValueSet', 'predecessor', 'The predecessor of the ValueSet', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'ValueSet.relatedArtifact.where(type=''predecessor'').resource', sxpNormal);
  indexes.add('ValueSet', 'publisher', '): Name of the publisher of the value set', sptSTRING, [], 'CapabilityStatement.publisher | CodeSystem.publisher | CompartmentDefinition.publisher | ConceptMap.publisher | GraphDefinition.publisher | ImplementationGuide.publisher | MessageDefinition.publisher | NamingSystem.publisher | OperationDefinition.pub'
      +'lisher | SearchParameter.publisher | StructureDefinition.publisher | StructureMap.publisher | TerminologyCapabilities.publisher | ValueSet.publisher', sxpNormal);
  indexes.add('ValueSet', 'reference', 'A code system included or excluded in the value set or an imported value set', sptURI, [], 'ValueSet.compose.include.system', sxpNormal);
  indexes.add('ValueSet', 'status', '): The current status of the value set', sptTOKEN, [], 'CapabilityStatement.status | CodeSystem.status | CompartmentDefinition.status | ConceptMap.status | GraphDefinition.status | ImplementationGuide.status | MessageDefinition.status | NamingSystem.status | OperationDefinition.status | SearchParameter.st'
      +'atus | StructureDefinition.status | StructureMap.status | TerminologyCapabilities.status | ValueSet.status', sxpNormal);
  indexes.add('ValueSet', 'title', '): The human-friendly name of the value set', sptSTRING, [], 'CapabilityStatement.title | CodeSystem.title | ConceptMap.title | ImplementationGuide.title | MessageDefinition.title | OperationDefinition.title | StructureDefinition.title | StructureMap.title | TerminologyCapabilities.title | ValueSet.title', sxpNormal);
  indexes.add('ValueSet', 'topic', 'Topics associated with the ValueSet', sptTOKEN, [], 'ValueSet.topic', sxpNormal);
  indexes.add('ValueSet', 'url', '): The uri that identifies the value set', sptURI, [], 'CapabilityStatement.url | CodeSystem.url | CompartmentDefinition.url | ConceptMap.url | GraphDefinition.url | ImplementationGuide.url | MessageDefinition.url | NamingSystem.url | OperationDefinition.url | SearchParameter.url | StructureDefinition.url'
      +' | StructureMap.url | TerminologyCapabilities.url | ValueSet.url', sxpNormal);
  indexes.add('ValueSet', 'version', '): The business version of the value set', sptTOKEN, [], 'CapabilityStatement.version | CodeSystem.version | CompartmentDefinition.version | ConceptMap.version | GraphDefinition.version | ImplementationGuide.version | MessageDefinition.version | NamingSystem.version | OperationDefinition.version | SearchPar'
      +'ameter.version | StructureDefinition.version | StructureMap.version | TerminologyCapabilities.version | ValueSet.version', sxpNormal);
  indexes.add('ValueSet', 'workflow', 'Optional Extensions Element', sptTOKEN, [], 'CodeSystem.extension(''http://hl7.org/fhir/StructureDefinition/valueset-workflowStatus'').value', sxpNormal);
  indexes.add('ValueSet', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_VALUESET}

{$IFDEF FHIR_VERIFICATIONRESULT}
procedure TFHIRIndexBuilderR5.buildIndexesForVerificationResult(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('VerificationResult', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('VerificationResult', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('VerificationResult', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('VerificationResult', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('VerificationResult', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('VerificationResult', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('VerificationResult', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('VerificationResult', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('VerificationResult', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('VerificationResult', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('VerificationResult', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('VerificationResult', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('VerificationResult', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('VerificationResult', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('VerificationResult', 'target', 'A resource that was validated', sptREFERENCE, ALL_RESOURCE_TYPE_NAMES, 'VerificationResult.target', sxpNormal);
  indexes.add('VerificationResult', '_text', 'Search on the narrative of the resource', sptSTRING, [], '', sxpNormal);
end;
{$ENDIF FHIR_VERIFICATIONRESULT}

{$IFDEF FHIR_VISIONPRESCRIPTION}
procedure TFHIRIndexBuilderR5.buildIndexesForVisionPrescription(Indexes : TFhirIndexList; compartments : TFHIRCompartmentList);
begin
  indexes.add('VisionPrescription', '_content', 'Search on the entire content of the resource', sptNULL, [], '', sxpNormal);
  indexes.add('VisionPrescription', '_filter', 'This is the formal declaration for the _filter parameter, documented at [http://hl7.org/fhir/search_filter.html](http://hl7.org/fhir/search_filter.html)', sptNULL, [], '', sxpNormal);
  indexes.add('VisionPrescription', '_id', 'Logical id of this artifact', sptTOKEN, [], 'Resource.id', sxpNormal);
  indexes.add('VisionPrescription', '_in', 'Allows for the retrieval of resources that are active members of a CareTeam, Group, or List', sptREFERENCE, [], 'Resource.id', sxpNormal);
  indexes.add('VisionPrescription', '_language', 'Language of the resource content', sptTOKEN, [], 'Resource.language', sxpNormal);
  indexes.add('VisionPrescription', '_lastUpdated', 'When the resource version last changed', sptDATE, [], 'Resource.meta.lastUpdated', sxpNormal);
  indexes.add('VisionPrescription', '_list', 'Allows for the retrieval of resources that are referenced by a List resource or by one of the pre-defined functional lists', sptNULL, [], '', sxpNormal);
  indexes.add('VisionPrescription', '_profile', 'Profiles this resource claims to conform to', sptREFERENCE, ['StructureDefinition'], 'Resource.meta.profile', sxpNormal);
  indexes.add('VisionPrescription', '_query', 'A custom search profile that describes a specific defined query operation', sptNULL, [], '', sxpNormal);
  indexes.add('VisionPrescription', '_security', 'Security Labels applied to this resource', sptTOKEN, [], 'Resource.meta.security', sxpNormal);
  indexes.add('VisionPrescription', '_source', 'Identifies where the resource comes from', sptURI, [], 'Resource.meta.source', sxpNormal);
  indexes.add('VisionPrescription', '_tag', 'Tags applied to this resource', sptTOKEN, [], 'Resource.meta.tag', sxpNormal);
  indexes.add('VisionPrescription', '_text', 'Text search against the narrative', sptSTRING, [], '', sxpNormal);
  indexes.add('VisionPrescription', '_type', 'A resource type filter', sptNULL, [], '', sxpNormal);
  indexes.add('VisionPrescription', 'datewritten', 'Return prescriptions written on this date', sptDATE, [], 'VisionPrescription.dateWritten', sxpNormal);
  indexes.add('VisionPrescription', 'encounter', '): Return prescriptions with this encounter identifier', sptREFERENCE, ['Encounter'], 'Composition.encounter | DeviceRequest.encounter | DiagnosticReport.encounter | Flag.encounter | List.encounter | NutritionOrder.encounter | Observation.encounter | Procedure.encounter | RiskAssessment.encounter | ServiceRequest.encounter | VisionPres'
      +'cription.encounter', sxpNormal);
  indexes.add('VisionPrescription', 'identifier', '): Return prescriptions with this external identifier', sptTOKEN, [], 'AllergyIntolerance.identifier | CarePlan.identifier | CareTeam.identifier | Composition.identifier | Condition.identifier | Consent.identifier | DetectedIssue.identifier | DeviceRequest.identifier | DiagnosticReport.identifier | DocumentManifest.mast'
      +'erIdentifier | DocumentManifest.identifier | DocumentReference.identifier | Encounter.identifier | EpisodeOfCare.identifier | FamilyMemberHistory.identifier | Goal.identifier | ImagingStudy.identifier | Immunization.identifier | List.identifi'
      +'er | MedicationAdministration.identifier | MedicationDispense.identifier | MedicationRequest.identifier | MedicationUsage.identifier | NutritionOrder.identifier | Observation.identifier | Procedure.identifier | RiskAssessment.identifier | Ser'
      +'viceRequest.identifier | SupplyDelivery.identifier | SupplyRequest.identifier | VisionPrescription.identifier', sxpNormal);
  indexes.add('VisionPrescription', 'patient', '): The identity of a patient to list dispenses for', sptREFERENCE, ['Patient', 'Practitioner', 'Group', 'Organization', 'BiologicallyDerivedProduct', 'NutritionProduct', 'Device', 'Medication', 'Procedure', 'Substance', 'Location'], 'AllergyIntolerance.patient | CarePlan.subject.where(resolve() is Patient) | CareTeam.subject.where(resolve() is Patient) | ClinicalImpression.subject.where(resolve() is Patient) | Composition.subject.where(resolve() is Patient) | Condition.subject.wh'
      +'ere(resolve() is Patient) | Consent.subject.where(resolve() is Patient) | DetectedIssue.subject | DeviceRequest.subject.where(resolve() is Patient) | DeviceUsage.patient | DiagnosticReport.subject.where(resolve() is Patient) | DocumentManifes'
      +'t.subject.where(resolve() is Patient) | DocumentReference.subject.where(resolve() is Patient) | Encounter.subject.where(resolve() is Patient) | EpisodeOfCare.patient | FamilyMemberHistory.patient | Flag.s'+
   'ubject.where(resolve() is Patient) | G'
      +'oal.subject.where(resolve() is Patient) | ImagingStudy.subject.where(resolve() is Patient) | Immunization.patient | List.subject.where(resolve() is Patient) | MedicationAdministration.subject.where(resolve() is Patient) | MedicationDispense.s'
      +'ubject.where(resolve() is Patient) | MedicationRequest.subject.where(resolve() is Patient) | MedicationUsage.subject.where(resolve() is Patient) | NutritionOrder.subject.where(resolve() is Patient) | Observation.subject.where(resolve() is Pat'
      +'ient) | Procedure.subject.where(resolve() is Patient) | RiskAssessment.subject.where(resolve() is Patient) | ServiceRequest.subject.where(resolve() is Patient) | SupplyDelivery.patient | VisionPrescription.patient', sxpNormal);
  indexes.add('VisionPrescription', 'prescriber', 'Who authorized the vision prescription', sptREFERENCE, ['Practitioner', 'PractitionerRole'], 'VisionPrescription.prescriber', sxpNormal);
  indexes.add('VisionPrescription', 'status', 'The status of the vision prescription', sptTOKEN, [], 'VisionPrescription.status', sxpNormal);
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
  {$IFDEF FHIR_ACTORDEFINITION}
  buildIndexesForActorDefinition(Indexes, compartments);
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
  {$IFDEF FHIR_ARTIFACTASSESSMENT}
  buildIndexesForArtifactAssessment(Indexes, compartments);
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
  {$IFDEF FHIR_CONDITIONDEFINITION}
  buildIndexesForConditionDefinition(Indexes, compartments);
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
  {$IFDEF FHIR_DEVICEDISPENSE}
  buildIndexesForDeviceDispense(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_DEVICEMETRIC}
  buildIndexesForDeviceMetric(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_DEVICEREQUEST}
  buildIndexesForDeviceRequest(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_DEVICEUSAGE}
  buildIndexesForDeviceUsage(Indexes, compartments);
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
  {$IFDEF FHIR_FORMULARYITEM}
  buildIndexesForFormularyItem(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_GENOMICSTUDY}
  buildIndexesForGenomicStudy(Indexes, compartments);
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
  {$IFDEF FHIR_IMAGINGSELECTION}
  buildIndexesForImagingSelection(Indexes, compartments);
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
  {$IFDEF FHIR_INVENTORYREPORT}
  buildIndexesForInventoryReport(Indexes, compartments);
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
  {$IFDEF FHIR_MEDICATIONUSAGE}
  buildIndexesForMedicationUsage(Indexes, compartments);
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
  {$IFDEF FHIR_NUTRITIONINTAKE}
  buildIndexesForNutritionIntake(Indexes, compartments);
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
  {$IFDEF FHIR_PERMISSION}
  buildIndexesForPermission(Indexes, compartments);
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
  {$IFDEF FHIR_REQUESTORCHESTRATION}
  buildIndexesForRequestOrchestration(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_REQUIREMENTS}
  buildIndexesForRequirements(Indexes, compartments);
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
  {$IFDEF FHIR_SUBSTANCENUCLEICACID}
  buildIndexesForSubstanceNucleicAcid(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_SUBSTANCEPOLYMER}
  buildIndexesForSubstancePolymer(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_SUBSTANCEPROTEIN}
  buildIndexesForSubstanceProtein(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_SUBSTANCEREFERENCEINFORMATION}
  buildIndexesForSubstanceReferenceInformation(Indexes, compartments);
  {$ENDIF}
  {$IFDEF FHIR_SUBSTANCESOURCEMATERIAL}
  buildIndexesForSubstanceSourceMaterial(Indexes, compartments);
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
  {$IFDEF FHIR_TRANSPORT}
  buildIndexesForTransport(Indexes, compartments);
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

