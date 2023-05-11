unit fxver_tests;

{
Copyright (c) 2017+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

interface

uses
  SysUtils, Classes,
  fsl_testing,
  fsl_stream, fsl_tests, fsl_http,
  fhir_objects, fhir_parser,
  fhir3_parser, fhir4_parser,
  fxver_convertors;

type
  TVersionConversionTests = Class (TFslTestCase)
  private
    procedure test4to3to4(res : TBytes);
  public
    Procedure SetUp; override;
    procedure TearDown; override;
  published
    Procedure TestPatient_34_simple;
    Procedure TestParameters_34;
    Procedure TestAllergyIntolerance_34;
    Procedure TestAppointment_34;
    Procedure TestAppointmentResponse_34;
    Procedure TestAuditEvent_34;
    Procedure TestBasic_34;
    Procedure TestBinary_34;
    Procedure TestBodyStructure_34;
    Procedure TestBundle_34;
    Procedure TestCapabilityStatement_34;
    Procedure TestCareTeam_34;
    Procedure TestClinicalImpression_34;
    Procedure TestCodeSystem_34;
    Procedure TestCommunication_34;
    Procedure TestCompartmentDefinition_34;
    Procedure TestComposition_34;
    Procedure TestConceptMap_34;
    Procedure TestCondition_34;
    Procedure TestConsent_34;
    Procedure TestDetectedIssue_34;
    Procedure TestDevice_34;
    Procedure TestDeviceMetric_34;
    Procedure TestDeviceUseStatement_34;
    Procedure TestDiagnosticReport_34;
    Procedure TestDocumentReference_34;
    Procedure TestEncounter_34;
    Procedure Testendpoint_34;
    Procedure TestEpisodeOfCare_34;
    Procedure TestFamilyMemberHistory_34;
    Procedure TestFlag_34;
    Procedure TestGoal_34;
    Procedure TestGraphDefinition_34;
    Procedure TestGroup_34;
    Procedure TestHealthcareService_34;
    Procedure TestImmunization_34;
    Procedure TestImplementationGuide_34;
    Procedure TestLinkage_34;
    Procedure TestList_34;
    Procedure TestLocation_34;
    Procedure TestMedicationAdministration_34;
    Procedure TestMedicationDispense_34;
    Procedure TestMedicationRequest_34;
    Procedure TestMedicationStatement_34;
    Procedure TestMessageDefinition_34;
    Procedure TestMessageHeader_34;
    Procedure TestNamingSystem_34;
    Procedure TestObservation_34;
    Procedure TestOperationDefinition_34;
    Procedure TestOperationOutcome_34;
    Procedure TestOrganization_34;
    Procedure TestPatient_34;
    Procedure TestPaymentNotice_34;
    Procedure TestPerson_34;
    Procedure TestPractitioner_34;
    Procedure TestPractitionerRole_34;
    Procedure TestQuestionnaire_34;
    Procedure TestQuestionnaireResponse_34;
    Procedure TestRiskAssessment_34;
    Procedure TestSchedule_34;
    Procedure TestSearchParameter_34;
    Procedure TestSequence_34;
    Procedure TestSlot_34;
    Procedure TestSpecimen_34;
    Procedure TestStructureDefinition_34;
    Procedure TestStructureMap_34;
    Procedure TestSubscription_34;
    Procedure TestSubstance_34;
    Procedure TestSupplyDelivery_34;
    Procedure TestValueSet_34;
  End;

procedure registerTests;

implementation

{ TVersionConversionTests }

procedure TVersionConversionTests.Setup;
begin
  // nothing
end;

procedure TVersionConversionTests.TearDown;
begin
  // nothing
end;

procedure TVersionConversionTests.test4to3to4(res: TBytes);
var
  b : TBytes;
begin
  b := TFhirVersionConvertors.convertResource(res, ffJson, OutputStylePretty, THTTPLanguages.create('en'), fhirVersionRelease4, fhirVersionRelease3);
  b := TFhirVersionConvertors.convertResource(b, ffJson, OutputStylePretty, THTTPLanguages.create('en'), fhirVersionRelease3, fhirVersionRelease4);
  assertTrue(length(b) > 0);
end;

procedure TVersionConversionTests.TestPatient_34_simple;
begin
  test4to3to4(TEncoding.UTF8.GetBytes('{"resourceType" : "Patient", "gender" : "male"}'));
end;

procedure TVersionConversionTests.TestParameters_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'parameters-example.json'])));
end;

procedure TVersionConversionTests.TestAllergyIntolerance_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'allergyintolerance-example.json'])));
end;

procedure TVersionConversionTests.TestAppointment_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'appointment-example.json'])));
end;

procedure TVersionConversionTests.TestAppointmentResponse_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'appointmentresponse-example.json'])));
end;

procedure TVersionConversionTests.TestAuditEvent_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'auditevent-example.json'])));
end;

procedure TVersionConversionTests.TestBasic_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'basic-example.json'])));
end;

procedure TVersionConversionTests.TestBinary_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'binary-example.json'])));
end;

procedure TVersionConversionTests.TestBodyStructure_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'bodystructure-example-fetus.json'])));
end;

procedure TVersionConversionTests.TestBundle_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'bundle-questionnaire.json'])));
end;

procedure TVersionConversionTests.TestCapabilityStatement_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'capabilitystatement-example.json'])));
end;

procedure TVersionConversionTests.TestCareTeam_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'careteam-example.json'])));
end;

procedure TVersionConversionTests.TestClinicalImpression_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'clinicalimpression-example.json'])));
end;

procedure TVersionConversionTests.TestCodeSystem_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'codesystem-example.json'])));
end;

procedure TVersionConversionTests.TestCommunication_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'communication-example.json'])));
end;

procedure TVersionConversionTests.TestCompartmentDefinition_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'compartmentdefinition-example.json'])));
end;

procedure TVersionConversionTests.TestComposition_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'composition-example.json'])));
end;

procedure TVersionConversionTests.TestConceptMap_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'conceptmap-example.json'])));
end;

procedure TVersionConversionTests.TestCondition_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'condition-example.json'])));
end;

procedure TVersionConversionTests.TestConsent_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'consent-example.json'])));
end;

procedure TVersionConversionTests.TestDetectedIssue_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'detectedissue-example.json'])));
end;

procedure TVersionConversionTests.TestDevice_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'device-example.json'])));
end;

procedure TVersionConversionTests.TestDeviceMetric_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'devicemetric-example.json'])));
end;

procedure TVersionConversionTests.TestDeviceUseStatement_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'deviceusestatement-example.json'])));
end;

procedure TVersionConversionTests.TestDiagnosticReport_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'diagnosticreport-example.json'])));
end;

procedure TVersionConversionTests.TestDocumentReference_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'documentreference-example.json'])));
end;

procedure TVersionConversionTests.TestEncounter_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'encounter-example.json'])));
end;

procedure TVersionConversionTests.Testendpoint_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'endpoint-example.json'])));
end;

procedure TVersionConversionTests.TestEpisodeOfCare_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'episodeofcare-example.json'])));
end;

procedure TVersionConversionTests.TestFamilyMemberHistory_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'familymemberhistory-example.json'])));
end;

procedure TVersionConversionTests.TestFlag_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'flag-example.json'])));
end;

procedure TVersionConversionTests.TestGoal_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'goal-example.json'])));
end;

procedure TVersionConversionTests.TestGraphDefinition_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'graphdefinition-example.json'])));
end;

procedure TVersionConversionTests.TestGroup_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'group-example.json'])));
end;

procedure TVersionConversionTests.TestHealthcareService_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'healthcareservice-example.json'])));
end;

procedure TVersionConversionTests.TestImmunization_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'immunization-example.json'])));
end;

procedure TVersionConversionTests.TestImplementationGuide_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'implementationguide-example.json'])));
end;

procedure TVersionConversionTests.TestLinkage_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'linkage-example.json'])));
end;

procedure TVersionConversionTests.TestList_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'list-example.json'])));
end;

procedure TVersionConversionTests.TestLocation_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'location-example.json'])));
end;

procedure TVersionConversionTests.TestMedicationAdministration_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'medicationadministration0301.json'])));
end;

procedure TVersionConversionTests.TestMedicationDispense_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'medicationdispense0301.json'])));
end;

procedure TVersionConversionTests.TestMedicationRequest_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'medicationrequest0301.json'])));
end;

procedure TVersionConversionTests.TestMedicationStatement_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'medicationstatementexample1.json'])));
end;

procedure TVersionConversionTests.TestMessageDefinition_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'messagedefinition-example.json'])));
end;

procedure TVersionConversionTests.TestMessageHeader_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'messageheader-example.json'])));
end;

procedure TVersionConversionTests.TestNamingSystem_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'namingsystem-example.json'])));
end;

procedure TVersionConversionTests.TestObservation_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'observation-example.json'])));
end;

procedure TVersionConversionTests.TestOperationDefinition_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'operationdefinition-example.json'])));
end;

procedure TVersionConversionTests.TestOperationOutcome_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'operationoutcome-example.json'])));
end;

procedure TVersionConversionTests.TestOrganization_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'organization-example.json'])));
end;

procedure TVersionConversionTests.TestPatient_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'patient-example.json'])));
end;

procedure TVersionConversionTests.TestPaymentNotice_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'paymentnotice-example.json'])));
end;

procedure TVersionConversionTests.TestPerson_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'person-example.json'])));
end;

procedure TVersionConversionTests.TestPractitioner_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'practitioner-example.json'])));
end;

procedure TVersionConversionTests.TestPractitionerRole_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'practitionerrole-example.json'])));
end;

procedure TVersionConversionTests.TestQuestionnaire_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'questionnaire-example.json'])));
end;

procedure TVersionConversionTests.TestQuestionnaireResponse_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'questionnaireresponse-example-bluebook.json'])));
end;

procedure TVersionConversionTests.TestRiskAssessment_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'riskassessment-example.json'])));
end;

procedure TVersionConversionTests.TestSchedule_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'schedule-example.json'])));
end;

procedure TVersionConversionTests.TestSearchParameter_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'searchparameter-example.json'])));
end;

procedure TVersionConversionTests.TestSequence_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'molecularsequence-example.json'])));
end;

procedure TVersionConversionTests.TestSlot_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'slot-example.json'])));
end;

procedure TVersionConversionTests.TestSpecimen_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'specimen-example.json'])));
end;

procedure TVersionConversionTests.TestStructureDefinition_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'structuredefinition-example-composition.json'])));
end;

procedure TVersionConversionTests.TestStructureMap_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'structuremap-example.json'])));
end;

procedure TVersionConversionTests.TestSubscription_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'subscription-example.json'])));
end;

procedure TVersionConversionTests.TestSubstance_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'substance-example.json'])));
end;

procedure TVersionConversionTests.TestSupplyDelivery_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'supplydelivery-example.json'])));
end;

procedure TVersionConversionTests.TestValueSet_34;
begin
  test4to3to4(FileToBytes(TestSettings.fhirTestFile(['r4', 'examples', 'valueset-example.json'])));
end;

procedure registerTests;
begin
  RegisterTest('ConversionTests', TVersionConversionTests.Suite);
end;

end.
