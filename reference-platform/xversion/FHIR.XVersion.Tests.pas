unit FHIR.XVersion.Tests;

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

interface

uses
  SysUtils,
  DUnitX.TestFramework,
  FHIR.Support.Stream,
  FHIR.Base.Objects, FHIR.Base.Parser,
  FHIR.R3.Parser, FHIR.R4.Parser,
  FHIR.XVersion.Convertors;

type
  [TextFixture]
  TVersionConversionTests = Class (TObject)
  private
    procedure test4to3to4(res : TBytes);
  public
    [Setup] procedure Setup;
    [TearDown] procedure TearDown;

    [TestCase] Procedure TestPatient_34_simple;
    [TestCase] Procedure TestParameters_34;
    [TestCase] Procedure TestActivityDefinition_34;
    [TestCase] Procedure TestAllergyIntolerance_34;
    [TestCase] Procedure TestAppointment_34;
    [TestCase] Procedure TestAppointmentResponse_34;
    [TestCase] Procedure TestAuditEvent_34;
    [TestCase] Procedure TestBasic_34;
    [TestCase] Procedure TestBinary_34;
    [TestCase] Procedure TestBodyStructure_34;
    [TestCase] Procedure TestBundle_34;
    [TestCase] Procedure TestCapabilityStatement_34;
    [TestCase] Procedure TestCareTeam_34;
    [TestCase] Procedure TestClaim_34;
    [TestCase] Procedure TestClinicalImpression_34;
    [TestCase] Procedure TestCodeSystem_34;
    [TestCase] Procedure TestCommunication_34;
    [TestCase] Procedure TestCompartmentDefinition_34;
    [TestCase] Procedure TestComposition_34;
    [TestCase] Procedure TestConceptMap_34;
    [TestCase] Procedure TestCondition_34;
    [TestCase] Procedure TestConsent_34;
    [TestCase] Procedure TestDetectedIssue_34;
    [TestCase] Procedure TestDevice_34;
    [TestCase] Procedure TestDeviceComponent_34;
    [TestCase] Procedure TestDeviceMetric_34;
    [TestCase] Procedure TestDeviceUseStatement_34;
    [TestCase] Procedure TestDiagnosticReport_34;
    [TestCase] Procedure TestDocumentReference_34;
    [TestCase] Procedure TestEncounter_34;
    [TestCase] Procedure Testendpoint_34;
    [TestCase] Procedure TestEpisodeOfCare_34;
    [TestCase] Procedure TestExpansionProfile_34;
    [TestCase] Procedure TestFamilyMemberHistory_34;
    [TestCase] Procedure TestFlag_34;
    [TestCase] Procedure TestGoal_34;
    [TestCase] Procedure TestGraphDefinition_34;
    [TestCase] Procedure TestGroup_34;
    [TestCase] Procedure TestHealthcareService_34;
    [TestCase] Procedure TestImmunization_34;
    [TestCase] Procedure TestImplementationGuide_34;
    [TestCase] Procedure TestLinkage_34;
    [TestCase] Procedure TestList_34;
    [TestCase] Procedure TestLocation_34;
    [TestCase] Procedure TestMedicationAdministration_34;
    [TestCase] Procedure TestMedicationDispense_34;
    [TestCase] Procedure TestMedicationRequest_34;
    [TestCase] Procedure TestMedicationStatement_34;
    [TestCase] Procedure TestMessageDefinition_34;
    [TestCase] Procedure TestMessageHeader_34;
    [TestCase] Procedure TestNamingSystem_34;
    [TestCase] Procedure TestObservation_34;
    [TestCase] Procedure TestOperationDefinition_34;
    [TestCase] Procedure TestOperationOutcome_34;
    [TestCase] Procedure TestOrganization_34;
    [TestCase] Procedure TestPatient_34;
    [TestCase] Procedure TestPaymentNotice_34;
    [TestCase] Procedure TestPerson_34;
    [TestCase] Procedure TestPractitioner_34;
    [TestCase] Procedure TestPractitionerRole_34;
    [TestCase] Procedure TestQuestionnaire_34;
    [TestCase] Procedure TestQuestionnaireResponse_34;
    [TestCase] Procedure TestRiskAssessment_34;
    [TestCase] Procedure TestSchedule_34;
    [TestCase] Procedure TestSearchParameter_34;
    [TestCase] Procedure TestSequence_34;
    [TestCase] Procedure TestSlot_34;
    [TestCase] Procedure TestSpecimen_34;
    [TestCase] Procedure TestStructureDefinition_34;
    [TestCase] Procedure TestStructureMap_34;
    [TestCase] Procedure TestSubscription_34;
    [TestCase] Procedure TestSubstance_34;
    [TestCase] Procedure TestSupplyDelivery_34;
    [TestCase] Procedure TestValueSet_34;
  End;

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
  b := TFhirVersionConvertors.convertResource(res, ffJson, OutputStylePretty, 'en', fhirVersionRelease4, fhirVersionRelease3);
  b := TFhirVersionConvertors.convertResource(b, ffJson, OutputStylePretty, 'en', fhirVersionRelease3, fhirVersionRelease4);
  Assert.isTrue(length(b) > 0);
end;

procedure TVersionConversionTests.TestPatient_34_simple;
begin
  test4to3to4(TEncoding.UTF8.GetBytes('{"resourceType" : "Patient", "gender" : "male"}'));
end;

procedure TVersionConversionTests.TestParameters_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\parameters-example.json'));
end;

procedure TVersionConversionTests.TestActivityDefinition_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\activitydefinition-example.json'));
end;

procedure TVersionConversionTests.TestAllergyIntolerance_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\allergyintolerance-example.json'));
end;

procedure TVersionConversionTests.TestAppointment_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\appointment-example.json'));
end;

procedure TVersionConversionTests.TestAppointmentResponse_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\appointmentresponse-example.json'));
end;

procedure TVersionConversionTests.TestAuditEvent_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\auditevent-example.json'));
end;

procedure TVersionConversionTests.TestBasic_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\basic-example.json'));
end;

procedure TVersionConversionTests.TestBinary_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\binary-example.json'));
end;

procedure TVersionConversionTests.TestBodyStructure_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\bodystructure-example-fetus.json'));
end;

procedure TVersionConversionTests.TestBundle_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\bundle-questionnaire.json'));
end;

procedure TVersionConversionTests.TestCapabilityStatement_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\capabilitystatement-example.json'));
end;

procedure TVersionConversionTests.TestCareTeam_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\careteam-example.json'));
end;

procedure TVersionConversionTests.TestClaim_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\claim-example.json'));
end;

procedure TVersionConversionTests.TestClinicalImpression_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\clinicalimpression-example.json'));
end;

procedure TVersionConversionTests.TestCodeSystem_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\codesystem-example.json'));
end;

procedure TVersionConversionTests.TestCommunication_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\communication-example.json'));
end;

procedure TVersionConversionTests.TestCompartmentDefinition_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\compartmentdefinition-example.json'));
end;

procedure TVersionConversionTests.TestComposition_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\composition-example.json'));
end;

procedure TVersionConversionTests.TestConceptMap_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\conceptmap-example.json'));
end;

procedure TVersionConversionTests.TestCondition_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\condition-example.json'));
end;

procedure TVersionConversionTests.TestConsent_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\consent-example.json'));
end;

procedure TVersionConversionTests.TestDetectedIssue_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\detectedissue-example.json'));
end;

procedure TVersionConversionTests.TestDevice_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\device-example.json'));
end;

procedure TVersionConversionTests.TestDeviceComponent_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\devicecomponent-example.json'));
end;

procedure TVersionConversionTests.TestDeviceMetric_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\devicemetric-example.json'));
end;

procedure TVersionConversionTests.TestDeviceUseStatement_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\deviceusestatement-example.json'));
end;

procedure TVersionConversionTests.TestDiagnosticReport_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\diagnosticreport-example.json'));
end;

procedure TVersionConversionTests.TestDocumentReference_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\documentreference-example.json'));
end;

procedure TVersionConversionTests.TestEncounter_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\encounter-example.json'));
end;

procedure TVersionConversionTests.Testendpoint_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\endpoint-example.json'));
end;

procedure TVersionConversionTests.TestEpisodeOfCare_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\episodeofcare-example.json'));
end;

procedure TVersionConversionTests.TestExpansionProfile_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\expansionprofile-example.json'));
end;

procedure TVersionConversionTests.TestFamilyMemberHistory_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\familymemberhistory-example.json'));
end;

procedure TVersionConversionTests.TestFlag_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\flag-example.json'));
end;

procedure TVersionConversionTests.TestGoal_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\goal-example.json'));
end;

procedure TVersionConversionTests.TestGraphDefinition_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\graphdefinition-example.json'));
end;

procedure TVersionConversionTests.TestGroup_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\group-example.json'));
end;

procedure TVersionConversionTests.TestHealthcareService_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\healthcareservice-example.json'));
end;

procedure TVersionConversionTests.TestImmunization_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\immunization-example.json'));
end;

procedure TVersionConversionTests.TestImplementationGuide_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\implementationguide-example.json'));
end;

procedure TVersionConversionTests.TestLinkage_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\linkage-example.json'));
end;

procedure TVersionConversionTests.TestList_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\list-example.json'));
end;

procedure TVersionConversionTests.TestLocation_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\location-example.json'));
end;

procedure TVersionConversionTests.TestMedicationAdministration_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\medicationadministration0301.json'));
end;

procedure TVersionConversionTests.TestMedicationDispense_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\medicationdispense0301.json'));
end;

procedure TVersionConversionTests.TestMedicationRequest_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\medicationrequest0301.json'));
end;

procedure TVersionConversionTests.TestMedicationStatement_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\medicationstatementexample1.json'));
end;

procedure TVersionConversionTests.TestMessageDefinition_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\messagedefinition-example.json'));
end;

procedure TVersionConversionTests.TestMessageHeader_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\messageheader-example.json'));
end;

procedure TVersionConversionTests.TestNamingSystem_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\namingsystem-example.json'));
end;

procedure TVersionConversionTests.TestObservation_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\observation-example.json'));
end;

procedure TVersionConversionTests.TestOperationDefinition_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\operationdefinition-example.json'));
end;

procedure TVersionConversionTests.TestOperationOutcome_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\operationoutcome-example.json'));
end;

procedure TVersionConversionTests.TestOrganization_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\organization-example.json'));
end;

procedure TVersionConversionTests.TestPatient_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\patient-example.json'));
end;

procedure TVersionConversionTests.TestPaymentNotice_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\paymentnotice-example.json'));
end;

procedure TVersionConversionTests.TestPerson_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\person-example.json'));
end;

procedure TVersionConversionTests.TestPractitioner_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\practitioner-example.json'));
end;

procedure TVersionConversionTests.TestPractitionerRole_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\practitionerrole-example.json'));
end;

procedure TVersionConversionTests.TestQuestionnaire_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\questionnaire-example.json'));
end;

procedure TVersionConversionTests.TestQuestionnaireResponse_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\questionnaireresponse-example-bluebook.json'));
end;

procedure TVersionConversionTests.TestRiskAssessment_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\riskassessment-example.json'));
end;

procedure TVersionConversionTests.TestSchedule_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\schedule-example.json'));
end;

procedure TVersionConversionTests.TestSearchParameter_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\searchparameter-example.json'));
end;

procedure TVersionConversionTests.TestSequence_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\sequence-example.json'));
end;

procedure TVersionConversionTests.TestSlot_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\slot-example.json'));
end;

procedure TVersionConversionTests.TestSpecimen_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\specimen-example.json'));
end;

procedure TVersionConversionTests.TestStructureDefinition_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\structuredefinition-example.json'));
end;

procedure TVersionConversionTests.TestStructureMap_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\structuremap-example.json'));
end;

procedure TVersionConversionTests.TestSubscription_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\subscription-example.json'));
end;

procedure TVersionConversionTests.TestSubstance_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\substance-example.json'));
end;

procedure TVersionConversionTests.TestSupplyDelivery_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\supplydelivery-example.json'));
end;

procedure TVersionConversionTests.TestValueSet_34;
begin
  test4to3to4(FileToBytes('c:\work\org.hl7.fhir\build\publish\valueset-example.json'));
end;

initialization
  TDUnitX.RegisterTestFixture(TVersionConversionTests);
end.
