unit FHIR.R4.Tests.Parser;


{
Copyright (c) 2001-2013, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 'AS IS' AND
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
  Windows, SysUtils, Classes, System.IOUtils,
  DUnitX.TestFramework,
  FHIR.Support.Stream,
  FHIR.Base.Objects, FHIR.Version.Parser, FHIR.Base.Parser,
  FHIR.R4.Resources, FHIR.R4.ElementModel, FHIR.R4.Context, FHIR.R4.Tests.Worker,
  FHIR.Support.Tests;

type
  FHIRParserTestCaseAttribute = class (FHIRFolderBasedTestCaseAttribute)
  public
    constructor Create;
  end;

  [TextFixture]
  TFHIRParserTests = class (TObject)
  public
    [FHIRParserTestCase] procedure RoundTripTest(Filename: String);
  end;

  [TextFixture]
  TFHIRParserSpecialTests = class (TObject)
  public
    [TestCase] procedure DecimalTest;
  end;
  
  [TextFixture]
  TTurtleResourceTests = Class (TObject)
  Private
    procedure parseResource(filename : String);
  Published
    [TestCase] procedure test_audit_event_example_pixQuery();
    [TestCase] procedure test_audit_event_example_media();
    [TestCase] procedure test_audit_event_example_logout();
    [TestCase] procedure test_audit_event_example_login();
    [TestCase] procedure test_appointmentresponse_example();
    [TestCase] procedure test_appointmentresponse_example_req();
    [TestCase] procedure test_appointment_example2doctors();
    [TestCase] procedure test_appointment_example();
    [TestCase] procedure test_appointment_example_request();
    [TestCase] procedure test_allergyintolerance_medication();
    [TestCase] procedure test_allergyintolerance_fishallergy();
    [TestCase] procedure test_allergyintolerance_example();
    [TestCase] procedure test_account_example();
    [TestCase] procedure test_xds_example();
    [TestCase] procedure test_visionprescription_example();
    [TestCase] procedure test_visionprescription_example_1();
    [TestCase] procedure test_valueset_ucum_common();
    [TestCase] procedure test_valueset_nhin_purposeofuse();
    [TestCase] procedure test_valueset_example();
    [TestCase] procedure test_valueset_example_yesnodontknow();
    [TestCase] procedure test_valueset_example_intensional();
    [TestCase] procedure test_valueset_example_expansion();
    [TestCase] procedure test_valueset_cpt_all();
    [TestCase] procedure test_testscript_example();
    [TestCase] procedure test_testscript_example_rule();
    [TestCase] procedure test_supplydelivery_example();
    [TestCase] procedure test_substance_example();
    [TestCase] procedure test_substance_example_silver_nitrate_product();
    [TestCase] procedure test_substance_example_f203_potassium();
    [TestCase] procedure test_substance_example_f202_staphylococcus();
    [TestCase] procedure test_substance_example_f201_dust();
    [TestCase] procedure test_substance_example_amoxicillin_clavulanate();
    [TestCase] procedure test_subscription_example();
    [TestCase] procedure test_subscription_example_error();
    [TestCase] procedure test_structuremap_example();
    [TestCase] procedure test_structuredefinition_example();
    [TestCase] procedure test_specimen_example();
    [TestCase] procedure test_specimen_example_urine();
    [TestCase] procedure test_specimen_example_isolate();
    [TestCase] procedure test_slot_example();
    [TestCase] procedure test_slot_example_unavailable();
    [TestCase] procedure test_slot_example_tentative();
    [TestCase] procedure test_slot_example_busy();
    [TestCase] procedure test_sequence_example();
    [TestCase] procedure test_searchparameter_example();
    [TestCase] procedure test_searchparameter_example_extension();
    [TestCase] procedure test_schedule_example();
    [TestCase] procedure test_riskassessment_example();
    [TestCase] procedure test_riskassessment_example_prognosis();
    [TestCase] procedure test_riskassessment_example_population();
    [TestCase] procedure test_riskassessment_example_cardiac();
    [TestCase] procedure test_relatedperson_example();
    [TestCase] procedure test_relatedperson_example_peter();
    [TestCase] procedure test_relatedperson_example_f002_ariadne();
    [TestCase] procedure test_relatedperson_example_f001_sarah();
    [TestCase] procedure test_provenance_example();
    [TestCase] procedure test_provenance_example_sig();
    [TestCase] procedure test_processresponse_example();
    [TestCase] procedure test_processrequest_example();
    [TestCase] procedure test_processrequest_example_status();
    [TestCase] procedure test_processrequest_example_reverse();
    [TestCase] procedure test_processrequest_example_reprocess();
    [TestCase] procedure test_processrequest_example_poll_specific();
    [TestCase] procedure test_processrequest_example_poll_payrec();
    [TestCase] procedure test_processrequest_example_poll_inclusive();
    [TestCase] procedure test_processrequest_example_poll_exclusive();
    [TestCase] procedure test_processrequest_example_poll_eob();
    [TestCase] procedure test_procedure_example();
    [TestCase] procedure test_procedure_example_implant();
    [TestCase] procedure test_procedure_example_f201_tpf();
    [TestCase] procedure test_procedure_example_f004_tracheotomy();
    [TestCase] procedure test_procedure_example_f003_abscess();
    [TestCase] procedure test_procedure_example_f002_lung();
    [TestCase] procedure test_procedure_example_f001_heart();
    [TestCase] procedure test_procedure_example_biopsy();
    [TestCase] procedure test_practitionerrole_example();
    [TestCase] procedure test_practitioner_examples_general();
    [TestCase] procedure test_practitioner_example();
    [TestCase] procedure test_practitioner_example_xcda1();
    [TestCase] procedure test_practitioner_example_xcda_author();
    [TestCase] procedure test_practitioner_example_f204_ce();
    [TestCase] procedure test_practitioner_example_f203_jvg();
    [TestCase] procedure test_practitioner_example_f202_lm();
    [TestCase] procedure test_practitioner_example_f201_ab();
    [TestCase] procedure test_practitioner_example_f007_sh();
    [TestCase] procedure test_practitioner_example_f006_rvdb();
    [TestCase] procedure test_practitioner_example_f005_al();
    [TestCase] procedure test_practitioner_example_f004_rb();
    [TestCase] procedure test_practitioner_example_f003_mv();
    [TestCase] procedure test_practitioner_example_f002_pv();
    [TestCase] procedure test_practitioner_example_f001_evdb();
    [TestCase] procedure test_person_provider_directory();
    [TestCase] procedure test_person_patient_portal();
    [TestCase] procedure test_person_grahame();
    [TestCase] procedure test_person_example();
    [TestCase] procedure test_person_example_f002_ariadne();
    [TestCase] procedure test_paymentreconciliation_example();
    [TestCase] procedure test_paymentnotice_example();
    [TestCase] procedure test_patient_glossy_example();
    [TestCase] procedure test_patient_examples_general();
    [TestCase] procedure test_patient_examples_cypress_template();
    [TestCase] procedure test_patient_example();
    [TestCase] procedure test_patient_example_xds();
    [TestCase] procedure test_patient_example_xcda();
    [TestCase] procedure test_patient_example_proband();
    [TestCase] procedure test_patient_example_ihe_pcd();
    [TestCase] procedure test_patient_example_f201_roel();
    [TestCase] procedure test_patient_example_f001_pieter();
    [TestCase] procedure test_patient_example_dicom();
    [TestCase] procedure test_patient_example_d();
    [TestCase] procedure test_patient_example_c();
    [TestCase] procedure test_patient_example_b();
    [TestCase] procedure test_patient_example_animal();
    [TestCase] procedure test_patient_example_a();
    [TestCase] procedure test_parameters_example();
    [TestCase] procedure test_organization_example();
    [TestCase] procedure test_organization_example_lab();
    [TestCase] procedure test_organization_example_insurer();
    [TestCase] procedure test_organization_example_good_health_care();
    [TestCase] procedure test_organization_example_gastro();
    [TestCase] procedure test_organization_example_f203_bumc();
    [TestCase] procedure test_organization_example_f201_aumc();
    [TestCase] procedure test_organization_example_f003_burgers_ENT();
    [TestCase] procedure test_organization_example_f002_burgers_card();
    [TestCase] procedure test_organization_example_f001_burgers();
    [TestCase] procedure test_operationoutcome_example();
    [TestCase] procedure test_operationoutcome_example_validationfail();
    [TestCase] procedure test_operationoutcome_example_searchfail();
    [TestCase] procedure test_operationoutcome_example_exception();
    [TestCase] procedure test_operationoutcome_example_break_the_glass();
    [TestCase] procedure test_operationoutcome_example_allok();
    [TestCase] procedure test_operationdefinition_example();
    [TestCase] procedure test_observation_example();
    [TestCase] procedure test_observation_example_unsat();
    [TestCase] procedure test_observation_example_satO2();
    [TestCase] procedure test_observation_example_sample_data();
    [TestCase] procedure test_observation_example_glasgow();
    [TestCase] procedure test_observation_example_glasgow_qa();
    [TestCase] procedure test_observation_example_genetics_5();
    [TestCase] procedure test_observation_example_genetics_4();
    [TestCase] procedure test_observation_example_genetics_3();
    [TestCase] procedure test_observation_example_genetics_2();
    [TestCase] procedure test_observation_example_genetics_1();
    [TestCase] procedure test_observation_example_f206_staphylococcus();
    [TestCase] procedure test_observation_example_f205_egfr();
    [TestCase] procedure test_observation_example_f204_creatinine();
    [TestCase] procedure test_observation_example_f203_bicarbonate();
    [TestCase] procedure test_observation_example_f202_temperature();
    [TestCase] procedure test_observation_example_f005_hemoglobin();
    [TestCase] procedure test_observation_example_f004_erythrocyte();
    [TestCase] procedure test_observation_example_f003_co2();
    [TestCase] procedure test_observation_example_f002_excess();
    [TestCase] procedure test_observation_example_f001_glucose();
    [TestCase] procedure test_observation_example_bloodpressure();
    [TestCase] procedure test_observation_example_bloodpressure_cancel();
    [TestCase] procedure test_nutritionorder_example_texture_modified();
    [TestCase] procedure test_nutritionorder_example_renaldiet();
    [TestCase] procedure test_nutritionorder_example_pureeddiet();
    [TestCase] procedure test_nutritionorder_example_pureeddiet_simple();
    [TestCase] procedure test_nutritionorder_example_proteinsupplement();
    [TestCase] procedure test_nutritionorder_example_infantenteral();
    [TestCase] procedure test_nutritionorder_example_fiberrestricteddiet();
    [TestCase] procedure test_nutritionorder_example_enteralcontinuous();
    [TestCase] procedure test_nutritionorder_example_enteralbolus();
    [TestCase] procedure test_nutritionorder_example_energysupplement();
    [TestCase] procedure test_nutritionorder_example_diabeticsupplement();
    [TestCase] procedure test_nutritionorder_example_diabeticdiet();
    [TestCase] procedure test_nutritionorder_example_cardiacdiet();
    [TestCase] procedure test_namingsystem_registry();
    [TestCase] procedure test_namingsystem_example();
    [TestCase] procedure test_namingsystem_example_id();
    [TestCase] procedure test_messageheader_example();
    [TestCase] procedure test_message_response_link();
    [TestCase] procedure test_message_request_link();
    [TestCase] procedure test_medicationstatementexample7();
    [TestCase] procedure test_medicationstatementexample6();
    [TestCase] procedure test_medicationstatementexample5();
    [TestCase] procedure test_medicationstatementexample4();
    [TestCase] procedure test_medicationstatementexample2();
    [TestCase] procedure test_medicationstatementexample1();
    [TestCase] procedure test_medicationrequestexample2();
    [TestCase] procedure test_medicationrequestexample1();
    [TestCase] procedure test_medicationexample15();
    [TestCase] procedure test_medicationexample1();
    [TestCase] procedure test_medicationdispenseexample8();
    [TestCase] procedure test_medicationadministrationexample3();
    [TestCase] procedure test_medication_example_f203_paracetamol();
    [TestCase] procedure test_media_example();
    [TestCase] procedure test_media_example_sound();
    [TestCase] procedure test_media_example_dicom();
    [TestCase] procedure test_measurereport_cms146_cat3_example();
    [TestCase] procedure test_measurereport_cms146_cat2_example();
    [TestCase] procedure test_measurereport_cms146_cat1_example();
    [TestCase] procedure test_measure_exclusive_breastfeeding();
    [TestCase] procedure test_location_example();
    [TestCase] procedure test_location_example_ukpharmacy();
    [TestCase] procedure test_location_example_room();
    [TestCase] procedure test_location_example_patients_home();
    [TestCase] procedure test_location_example_hl7hq();
    [TestCase] procedure test_location_example_ambulance();
    [TestCase] procedure test_list_example();
    [TestCase] procedure test_list_example_medlist();
    [TestCase] procedure test_list_example_familyhistory_f201_roel();
    [TestCase] procedure test_list_example_empty();
    [TestCase] procedure test_list_example_allergies();
    [TestCase] procedure test_linkage_example();
    [TestCase] procedure test_library_exclusive_breastfeeding_cqm_logic();
    [TestCase] procedure test_library_exclusive_breastfeeding_cds_logic();
    [TestCase] procedure test_library_example();
    [TestCase] procedure test_library_cms146_example();
    [TestCase] procedure test_implementationguide_example();
    [TestCase] procedure test_immunizationrecommendation_example();
    [TestCase] procedure test_immunization_example();
    [TestCase] procedure test_immunization_example_refused();
    [TestCase] procedure test_imagingstudy_example();
    [TestCase] procedure test_healthcareservice_example();
    [TestCase] procedure test_guidanceresponse_example();
    [TestCase] procedure test_group_example();
    [TestCase] procedure test_group_example_member();
    [TestCase] procedure test_goal_example();
    [TestCase] procedure test_flag_example();
    [TestCase] procedure test_flag_example_encounter();
    [TestCase] procedure test_familymemberhistory_example();
    [TestCase] procedure test_familymemberhistory_example_mother();
    [TestCase] procedure test_explanationofbenefit_example();
    [TestCase] procedure test_episodeofcare_example();
    [TestCase] procedure test_enrollmentresponse_example();
    [TestCase] procedure test_enrollmentrequest_example();
    [TestCase] procedure test_endpoint_example();
    [TestCase] procedure test_encounter_example();
    [TestCase] procedure test_encounter_example_xcda();
    [TestCase] procedure test_encounter_example_home();
    [TestCase] procedure test_encounter_example_f203_20130311();
    [TestCase] procedure test_encounter_example_f202_20130128();
    [TestCase] procedure test_encounter_example_f201_20130404();
    [TestCase] procedure test_encounter_example_f003_abscess();
    [TestCase] procedure test_encounter_example_f002_lung();
    [TestCase] procedure test_encounter_example_f001_heart();
    [TestCase] procedure test_eligibilityresponse_example();
    [TestCase] procedure test_eligibilityrequest_example();
    [TestCase] procedure test_documentreference_example();
    [TestCase] procedure test_documentmanifest_fm_attachment();
    [TestCase] procedure test_document_example_dischargesummary();
    [TestCase] procedure test_diagnosticreport_micro1();
    [TestCase] procedure test_diagnosticreport_hla_genetics_results_example();
    [TestCase] procedure test_diagnosticreport_genetics_comprehensive_bone_marrow_report();
    [TestCase] procedure test_diagnosticreport_examples_general();
    [TestCase] procedure test_diagnosticreport_example_ultrasound();
    [TestCase] procedure test_diagnosticreport_example_lipids();
    [TestCase] procedure test_diagnosticreport_example_ghp();
    [TestCase] procedure test_diagnosticreport_example_f202_bloodculture();
    [TestCase] procedure test_diagnosticreport_example_f201_brainct();
    [TestCase] procedure test_diagnosticreport_example_f001_bloodexam();
    [TestCase] procedure test_diagnosticreport_example_dxa();
    [TestCase] procedure test_deviceusestatement_example();
    [TestCase] procedure test_devicemetric_example();
    [TestCase] procedure test_devicecomponent_example();
    [TestCase] procedure test_devicecomponent_example_prodspec();
    [TestCase] procedure test_device_example();
    [TestCase] procedure test_device_example_udi1();
    [TestCase] procedure test_device_example_software();
    [TestCase] procedure test_device_example_pacemaker();
    [TestCase] procedure test_device_example_ihe_pcd();
    [TestCase] procedure test_device_example_f001_feedingtube();
    [TestCase] procedure test_detectedissue_example();
    [TestCase] procedure test_detectedissue_example_lab();
    [TestCase] procedure test_detectedissue_example_dup();
    [TestCase] procedure test_detectedissue_example_allergy();
    [TestCase] procedure test_coverage_example();
    [TestCase] procedure test_coverage_example_2();
    [TestCase] procedure test_contract_example();
    [TestCase] procedure test_condition_example2();
    [TestCase] procedure test_condition_example();
    [TestCase] procedure test_condition_example_stroke();
    [TestCase] procedure test_condition_example_f205_infection();
    [TestCase] procedure test_condition_example_f204_renal();
    [TestCase] procedure test_condition_example_f203_sepsis();
    [TestCase] procedure test_condition_example_f202_malignancy();
    [TestCase] procedure test_condition_example_f201_fever();
    [TestCase] procedure test_condition_example_f003_abscess();
    [TestCase] procedure test_condition_example_f002_lung();
    [TestCase] procedure test_condition_example_f001_heart();
    [TestCase] procedure test_conceptmap_example();
    [TestCase] procedure test_conceptmap_example_specimen_type();
    [TestCase] procedure test_conceptmap_103();
    [TestCase] procedure test_composition_example();
    [TestCase] procedure test_communicationrequest_example();
    [TestCase] procedure test_communication_example();
    [TestCase] procedure test_codesystem_nhin_purposeofuse();
    [TestCase] procedure test_codesystem_example();
    [TestCase] procedure test_clinicalimpression_example();
    [TestCase] procedure test_claimresponse_example();
    [TestCase] procedure test_claim_example();
    [TestCase] procedure test_claim_example_vision();
    [TestCase] procedure test_claim_example_vision_glasses();
    [TestCase] procedure test_claim_example_professional();
    [TestCase] procedure test_claim_example_pharmacy();
    [TestCase] procedure test_claim_example_oral_orthoplan();
    [TestCase] procedure test_claim_example_oral_identifier();
    [TestCase] procedure test_claim_example_oral_contained();
    [TestCase] procedure test_claim_example_oral_contained_identifier();
    [TestCase] procedure test_claim_example_oral_average();
    [TestCase] procedure test_claim_example_institutional();
    [TestCase] procedure test_careteam_example();
    [TestCase] procedure test_careplan_example();
    [TestCase] procedure test_careplan_example_pregnancy();
    [TestCase] procedure test_careplan_example_integrated();
    [TestCase] procedure test_careplan_example_GPVisit();
    [TestCase] procedure test_careplan_example_f203_sepsis();
    [TestCase] procedure test_careplan_example_f202_malignancy();
    [TestCase] procedure test_careplan_example_f201_renal();
    [TestCase] procedure test_careplan_example_f003_pharynx();
    [TestCase] procedure test_careplan_example_f002_lung();
    [TestCase] procedure test_careplan_example_f001_heart();
    [TestCase] procedure test_bundle_transaction();
    [TestCase] procedure test_bundle_response();
    [TestCase] procedure test_bundle_example();
    [TestCase] procedure test_binary_f006();
    [TestCase] procedure test_binary_example();
    [TestCase] procedure test_basic_example2();
    [TestCase] procedure test_basic_example();
    [TestCase] procedure test_basic_example_narrative();
    [TestCase] procedure test_auditevent_example();
    [TestCase] procedure test_auditevent_example_disclosure();
    [TestCase] procedure test_audit_event_example_vread();
    [TestCase] procedure test_audit_event_example_search();
  End;

  

implementation

uses
  IdGlobalProtocols;

{ TFHIRParserTests }

procedure TFHIRParserSpecialTests.DecimalTest;
var
  src, xml, json, json2 : String;
  obs : TFhirObservation;
begin
  src := 'C:\temp\obs.xml';
  xml := 'C:\temp\xml.xml';
  json := 'C:\temp\json.json';
  json2 := 'C:\temp\json2.json';
  TFile.Copy(FHIR_PUB_FILE('observation-decimal.xml'), src, false);
  obs := TFHIRParsers.ParseFile(nil, ffXml, 'en', src) as TFhirObservation;
  try
    TFHIRParsers.composeFile(nil, ffJson, obs, 'en', json, OutputStylePretty);
  finally
    obs.Free;
  end;
  obs := TFHIRParsers.ParseFile(nil, ffJson, 'en', json) as TFhirObservation;
  try
    TFHIRParsers.composeFile(nil, ffJson, obs, 'en', json2, OutputStylePretty);
    TFHIRParsers.composeFile(nil, ffXml, obs, 'en', xml, OutputStylePretty);
  finally
    obs.Free;
  end;
  Assert.IsTrue(true);
end;

procedure TFHIRParserTests.RoundTripTest(filename: String);
var
  r : TFHIRResource;
  fn, j1, j2, x1, x2 : String;             
  b : boolean;
  msg : String;
  re : TFHIRMMElement;
  ctxt : TFHIRWorkerContext;
begin
//  AllocConsole;
  r := TFHIRParsers.parseFile(nil, ffXml, 'en', filename);
  try
    Assert.IsNotNull(r, 'Resource could not be loaded');
    fn := MakeTempFilename();
    try
      TFHIRParsers.composeFile(nil, ffXml, r, 'en', fn, OutputStylePretty);
      b := CheckXMLIsSame(filename, fn, msg);
      assert.IsTrue(b, msg);
    finally
      DeleteFile(fn);
    end;
    j1 := MakeTempFilename();
    TFHIRParsers.composeFile(nil, ffJson, r, 'en', j1, OutputStylePretty);
  finally
    r.Free;
  end;

  ctxt := TTestingWorkerContext.Use;
  try
    re := TFHIRMMManager.parseFile(ctxt, filename, ffXml);
    try
      Assert.IsNotNull(re, 'Resource could not be loaded');
      fn := MakeTempFilename();
      try
        TFHIRMMManager.composeFile(ctxt, re, fn, ffXml, OutputStylePretty);
        b := CheckXMLIsSame(filename, fn, msg);
        assert.IsTrue(b, msg);
      finally
        DeleteFile(fn);
      end;
      j2 := MakeTempFilename();
      TFHIRMMManager.composeFile(ctxt, re, j2, ffJson, OutputStylePretty);
    finally
      re.Free;
    end;
  finally
    ctxt.free;
  end;

  b := CheckJsonIsSame(j1, j2, msg);
  assert.IsTrue(b, msg);

  // ok, we've produced equivalent JSON by both methods.
  // now, we're going to reverse the process
  r := TFHIRParsers.parseFile(nil, ffJson, 'en', j2); // crossover too
  try
    Assert.IsNotNull(r, 'Resource could not be loaded');
    fn := MakeTempFilename();
    try
      TFHIRParsers.composeFile(nil, ffJson, r, 'en', fn, OutputStyleNormal);
      b := CheckJsonIsSame(j2, fn, msg);
      assert.IsTrue(b, msg);
    finally
      DeleteFile(fn);
    end;
    x1 := MakeTempFilename();
    TFHIRParsers.composeFile(nil, ffXml, r, 'en', x1, OutputStyleNormal);
  finally
    r.Free;
  end;

  ctxt := TTestingWorkerContext.Use;
  try
    re := TFHIRMMManager.parseFile(ctxt, j1, ffJson);
    try
      Assert.IsNotNull(re, 'Resource could not be loaded');
      fn := MakeTempFilename();
      try
        TFHIRMMManager.composeFile(ctxt, re, fn, ffJson, OutputStylePretty);
        b := CheckJsonIsSame(j1, fn, msg);
        assert.IsTrue(b, msg);
      finally
        DeleteFile(fn);
      end;
      x2 := MakeTempFilename();
      TFHIRMMManager.composeFile(ctxt, re, x2, ffXml, OutputStylePretty);
    finally
      re.Free;
    end;
  finally
    ctxt.free;
  end;

  b := CheckXMLIsSame(x1, x2, msg);
  assert.IsTrue(b, msg);

  b := CheckXMLIsSame(filename, x1, msg);
  assert.IsTrue(b, msg);
end;

{ FHIRParserTestCaseAttribute }

constructor FHIRParserTestCaseAttribute.Create;
begin
  inherited Create(FHIR_PUB_FILE('examples'), '.xml', 20);
end;

procedure TTurtleResourceTests.parseResource(filename: String);
var
  i, o{, m} : string;
  p : TFHIRParser;
  c : TFHIRComposer;
  s : TStringStream;
begin
  i := FileToString(FHIR_PUB_FILE(filename), TEncoding.UTF8);
  p := TFHIRTurtleParser.Create(nil, 'en');
  try
    p.source := TStringStream.Create(i, TENcoding.UTF8);
    try
      p.Parse;
      c := TFHIRTurtleComposer.Create(nil, OutputStyleNormal, 'en');
      try
        s := TStringStream.Create;
        try
          c.Compose(s, p.resource);
          o := s.DataString;
//          if not CheckTurtleIsSame(i, o, m) then
//            Assert.Fail(m)
//          else
            Assert.Pass();
        finally
          s.Free;
        end;
      finally
        c.Free;
      end;
    finally
      p.source.Free;
    end;
  finally
    p.free;
  end;
end;

procedure TTurtleResourceTests.test_audit_event_example_pixQuery();
begin
  parseResource('audit-event-example-pixQuery.ttl');
end;

procedure TTurtleResourceTests.test_audit_event_example_media();
begin
  parseResource('audit-event-example-media.ttl');
end;

procedure TTurtleResourceTests.test_audit_event_example_logout();
begin
  parseResource('audit-event-example-logout.ttl');
end;

procedure TTurtleResourceTests.test_audit_event_example_login();
begin
  parseResource('audit-event-example-login.ttl');
end;

procedure TTurtleResourceTests.test_appointmentresponse_example();
begin
  parseResource('appointmentresponse-example.ttl');
end;

procedure TTurtleResourceTests.test_appointmentresponse_example_req();
begin
  parseResource('appointmentresponse-example-req.ttl');
end;

procedure TTurtleResourceTests.test_appointment_example2doctors();
begin
  parseResource('appointment-example2doctors.ttl');
end;

procedure TTurtleResourceTests.test_appointment_example();
begin
  parseResource('appointment-example.ttl');
end;

procedure TTurtleResourceTests.test_appointment_example_request();
begin
  parseResource('appointment-example-request.ttl');
end;

procedure TTurtleResourceTests.test_allergyintolerance_medication();
begin
  parseResource('allergyintolerance-medication.ttl');
end;

procedure TTurtleResourceTests.test_allergyintolerance_fishallergy();
begin
  parseResource('allergyintolerance-fishallergy.ttl');
end;

procedure TTurtleResourceTests.test_allergyintolerance_example();
begin
  parseResource('allergyintolerance-example.ttl');
end;

procedure TTurtleResourceTests.test_account_example();
begin
  parseResource('account-example.ttl');
end;

procedure TTurtleResourceTests.test_xds_example();
begin
  parseResource('xds-example.ttl');
end;

procedure TTurtleResourceTests.test_visionprescription_example();
begin
  parseResource('visionprescription-example.ttl');
end;

procedure TTurtleResourceTests.test_visionprescription_example_1();
begin
  parseResource('visionprescription-example-1.ttl');
end;

procedure TTurtleResourceTests.test_valueset_ucum_common();
begin
  parseResource('valueset-ucum-common.ttl');
end;

procedure TTurtleResourceTests.test_valueset_nhin_purposeofuse();
begin
  parseResource('valueset-nhin-purposeofuse.ttl');
end;

procedure TTurtleResourceTests.test_valueset_example();
begin
  parseResource('valueset-example.ttl');
end;

procedure TTurtleResourceTests.test_valueset_example_yesnodontknow();
begin
  parseResource('valueset-example-yesnodontknow.ttl');
end;

procedure TTurtleResourceTests.test_valueset_example_intensional();
begin
  parseResource('valueset-example-intensional.ttl');
end;

procedure TTurtleResourceTests.test_valueset_example_expansion();
begin
  parseResource('valueset-example-expansion.ttl');
end;

procedure TTurtleResourceTests.test_valueset_cpt_all();
begin
  parseResource('valueset-cpt-all.ttl');
end;

procedure TTurtleResourceTests.test_testscript_example();
begin
  parseResource('testscript-example.ttl');
end;

procedure TTurtleResourceTests.test_testscript_example_rule();
begin
  parseResource('testscript-example-rule.ttl');
end;

procedure TTurtleResourceTests.test_supplydelivery_example();
begin
  parseResource('supplydelivery-example.ttl');
end;

procedure TTurtleResourceTests.test_substance_example();
begin
  parseResource('substance-example.ttl');
end;

procedure TTurtleResourceTests.test_substance_example_silver_nitrate_product();
begin
  parseResource('substance-example-silver-nitrate-product.ttl');
end;

procedure TTurtleResourceTests.test_substance_example_f203_potassium();
begin
  parseResource('substance-example-f203-potassium.ttl');
end;

procedure TTurtleResourceTests.test_substance_example_f202_staphylococcus();
begin
  parseResource('substance-example-f202-staphylococcus.ttl');
end;

procedure TTurtleResourceTests.test_substance_example_f201_dust();
begin
  parseResource('substance-example-f201-dust.ttl');
end;

procedure TTurtleResourceTests.test_substance_example_amoxicillin_clavulanate();
begin
  parseResource('substance-example-amoxicillin-clavulanate.ttl');
end;

procedure TTurtleResourceTests.test_subscription_example();
begin
  parseResource('subscription-example.ttl');
end;

procedure TTurtleResourceTests.test_subscription_example_error();
begin
  parseResource('subscription-example-error.ttl');
end;

procedure TTurtleResourceTests.test_structuremap_example();
begin
  parseResource('structuremap-example.ttl');
end;

procedure TTurtleResourceTests.test_structuredefinition_example();
begin
  parseResource('structuredefinition-example.ttl');
end;

procedure TTurtleResourceTests.test_specimen_example();
begin
  parseResource('specimen-example.ttl');
end;

procedure TTurtleResourceTests.test_specimen_example_urine();
begin
  parseResource('specimen-example-urine.ttl');
end;

procedure TTurtleResourceTests.test_specimen_example_isolate();
begin
  parseResource('specimen-example-isolate.ttl');
end;

procedure TTurtleResourceTests.test_slot_example();
begin
  parseResource('slot-example.ttl');
end;

procedure TTurtleResourceTests.test_slot_example_unavailable();
begin
  parseResource('slot-example-unavailable.ttl');
end;

procedure TTurtleResourceTests.test_slot_example_tentative();
begin
  parseResource('slot-example-tentative.ttl');
end;

procedure TTurtleResourceTests.test_slot_example_busy();
begin
  parseResource('slot-example-busy.ttl');
end;

procedure TTurtleResourceTests.test_sequence_example();
begin
  parseResource('sequence-example.ttl');
end;

procedure TTurtleResourceTests.test_searchparameter_example();
begin
  parseResource('searchparameter-example.ttl');
end;

procedure TTurtleResourceTests.test_searchparameter_example_extension();
begin
  parseResource('searchparameter-example-extension.ttl');
end;

procedure TTurtleResourceTests.test_schedule_example();
begin
  parseResource('schedule-example.ttl');
end;

procedure TTurtleResourceTests.test_riskassessment_example();
begin
  parseResource('riskassessment-example.ttl');
end;

procedure TTurtleResourceTests.test_riskassessment_example_prognosis();
begin
  parseResource('riskassessment-example-prognosis.ttl');
end;

procedure TTurtleResourceTests.test_riskassessment_example_population();
begin
  parseResource('riskassessment-example-population.ttl');
end;

procedure TTurtleResourceTests.test_riskassessment_example_cardiac();
begin
  parseResource('riskassessment-example-cardiac.ttl');
end;

procedure TTurtleResourceTests.test_relatedperson_example();
begin
  parseResource('relatedperson-example.ttl');
end;

procedure TTurtleResourceTests.test_relatedperson_example_peter();
begin
  parseResource('relatedperson-example-peter.ttl');
end;

procedure TTurtleResourceTests.test_relatedperson_example_f002_ariadne();
begin
  parseResource('relatedperson-example-f002-ariadne.ttl');
end;

procedure TTurtleResourceTests.test_relatedperson_example_f001_sarah();
begin
  parseResource('relatedperson-example-f001-sarah.ttl');
end;

procedure TTurtleResourceTests.test_provenance_example();
begin
  parseResource('provenance-example.ttl');
end;

procedure TTurtleResourceTests.test_provenance_example_sig();
begin
  parseResource('provenance-example-sig.ttl');
end;

procedure TTurtleResourceTests.test_processresponse_example();
begin
  parseResource('processresponse-example.ttl');
end;

procedure TTurtleResourceTests.test_processrequest_example();
begin
  parseResource('processrequest-example.ttl');
end;

procedure TTurtleResourceTests.test_processrequest_example_status();
begin
  parseResource('processrequest-example-status.ttl');
end;

procedure TTurtleResourceTests.test_processrequest_example_reverse();
begin
  parseResource('processrequest-example-reverse.ttl');
end;

procedure TTurtleResourceTests.test_processrequest_example_reprocess();
begin
  parseResource('processrequest-example-reprocess.ttl');
end;

procedure TTurtleResourceTests.test_processrequest_example_poll_specific();
begin
  parseResource('processrequest-example-poll-specific.ttl');
end;

procedure TTurtleResourceTests.test_processrequest_example_poll_payrec();
begin
  parseResource('processrequest-example-poll-payrec.ttl');
end;

procedure TTurtleResourceTests.test_processrequest_example_poll_inclusive();
begin
  parseResource('processrequest-example-poll-inclusive.ttl');
end;

procedure TTurtleResourceTests.test_processrequest_example_poll_exclusive();
begin
  parseResource('processrequest-example-poll-exclusive.ttl');
end;

procedure TTurtleResourceTests.test_processrequest_example_poll_eob();
begin
  parseResource('processrequest-example-poll-eob.ttl');
end;

procedure TTurtleResourceTests.test_procedure_example();
begin
  parseResource('procedure-example.ttl');
end;

procedure TTurtleResourceTests.test_procedure_example_implant();
begin
  parseResource('procedure-example-implant.ttl');
end;

procedure TTurtleResourceTests.test_procedure_example_f201_tpf();
begin
  parseResource('procedure-example-f201-tpf.ttl');
end;

procedure TTurtleResourceTests.test_procedure_example_f004_tracheotomy();
begin
  parseResource('procedure-example-f004-tracheotomy.ttl');
end;

procedure TTurtleResourceTests.test_procedure_example_f003_abscess();
begin
  parseResource('procedure-example-f003-abscess.ttl');
end;

procedure TTurtleResourceTests.test_procedure_example_f002_lung();
begin
  parseResource('procedure-example-f002-lung.ttl');
end;

procedure TTurtleResourceTests.test_procedure_example_f001_heart();
begin
  parseResource('procedure-example-f001-heart.ttl');
end;

procedure TTurtleResourceTests.test_procedure_example_biopsy();
begin
  parseResource('procedure-example-biopsy.ttl');
end;

procedure TTurtleResourceTests.test_practitionerrole_example();
begin
  parseResource('practitionerrole-example.ttl');
end;

procedure TTurtleResourceTests.test_practitioner_examples_general();
begin
  parseResource('practitioner-examples-general.ttl');
end;

procedure TTurtleResourceTests.test_practitioner_example();
begin
  parseResource('practitioner-example.ttl');
end;

procedure TTurtleResourceTests.test_practitioner_example_xcda1();
begin
  parseResource('practitioner-example-xcda1.ttl');
end;

procedure TTurtleResourceTests.test_practitioner_example_xcda_author();
begin
  parseResource('practitioner-example-xcda-author.ttl');
end;

procedure TTurtleResourceTests.test_practitioner_example_f204_ce();
begin
  parseResource('practitioner-example-f204-ce.ttl');
end;

procedure TTurtleResourceTests.test_practitioner_example_f203_jvg();
begin
  parseResource('practitioner-example-f203-jvg.ttl');
end;

procedure TTurtleResourceTests.test_practitioner_example_f202_lm();
begin
  parseResource('practitioner-example-f202-lm.ttl');
end;

procedure TTurtleResourceTests.test_practitioner_example_f201_ab();
begin
  parseResource('practitioner-example-f201-ab.ttl');
end;

procedure TTurtleResourceTests.test_practitioner_example_f007_sh();
begin
  parseResource('practitioner-example-f007-sh.ttl');
end;

procedure TTurtleResourceTests.test_practitioner_example_f006_rvdb();
begin
  parseResource('practitioner-example-f006-rvdb.ttl');
end;

procedure TTurtleResourceTests.test_practitioner_example_f005_al();
begin
  parseResource('practitioner-example-f005-al.ttl');
end;

procedure TTurtleResourceTests.test_practitioner_example_f004_rb();
begin
  parseResource('practitioner-example-f004-rb.ttl');
end;

procedure TTurtleResourceTests.test_practitioner_example_f003_mv();
begin
  parseResource('practitioner-example-f003-mv.ttl');
end;

procedure TTurtleResourceTests.test_practitioner_example_f002_pv();
begin
  parseResource('practitioner-example-f002-pv.ttl');
end;

procedure TTurtleResourceTests.test_practitioner_example_f001_evdb();
begin
  parseResource('practitioner-example-f001-evdb.ttl');
end;

procedure TTurtleResourceTests.test_person_provider_directory();
begin
  parseResource('person-provider-directory.ttl');
end;

procedure TTurtleResourceTests.test_person_patient_portal();
begin
  parseResource('person-patient-portal.ttl');
end;

procedure TTurtleResourceTests.test_person_grahame();
begin
  parseResource('person-grahame.ttl');
end;

procedure TTurtleResourceTests.test_person_example();
begin
  parseResource('person-example.ttl');
end;

procedure TTurtleResourceTests.test_person_example_f002_ariadne();
begin
  parseResource('person-example-f002-ariadne.ttl');
end;

procedure TTurtleResourceTests.test_paymentreconciliation_example();
begin
  parseResource('paymentreconciliation-example.ttl');
end;

procedure TTurtleResourceTests.test_paymentnotice_example();
begin
  parseResource('paymentnotice-example.ttl');
end;

procedure TTurtleResourceTests.test_patient_glossy_example();
begin
  parseResource('patient-glossy-example.ttl');
end;

procedure TTurtleResourceTests.test_patient_examples_general();
begin
  parseResource('patient-examples-general.ttl');
end;

procedure TTurtleResourceTests.test_patient_examples_cypress_template();
begin
  parseResource('patient-examples-cypress-template.ttl');
end;

procedure TTurtleResourceTests.test_patient_example();
begin
  parseResource('patient-example.ttl');
end;

procedure TTurtleResourceTests.test_patient_example_xds();
begin
  parseResource('patient-example-xds.ttl');
end;

procedure TTurtleResourceTests.test_patient_example_xcda();
begin
  parseResource('patient-example-xcda.ttl');
end;

procedure TTurtleResourceTests.test_patient_example_proband();
begin
  parseResource('patient-example-proband.ttl');
end;

procedure TTurtleResourceTests.test_patient_example_ihe_pcd();
begin
  parseResource('patient-example-ihe-pcd.ttl');
end;

procedure TTurtleResourceTests.test_patient_example_f201_roel();
begin
  parseResource('patient-example-f201-roel.ttl');
end;

procedure TTurtleResourceTests.test_patient_example_f001_pieter();
begin
  parseResource('patient-example-f001-pieter.ttl');
end;

procedure TTurtleResourceTests.test_patient_example_dicom();
begin
  parseResource('patient-example-dicom.ttl');
end;

procedure TTurtleResourceTests.test_patient_example_d();
begin
  parseResource('patient-example-d.ttl');
end;

procedure TTurtleResourceTests.test_patient_example_c();
begin
  parseResource('patient-example-c.ttl');
end;

procedure TTurtleResourceTests.test_patient_example_b();
begin
  parseResource('patient-example-b.ttl');
end;

procedure TTurtleResourceTests.test_patient_example_animal();
begin
  parseResource('patient-example-animal.ttl');
end;

procedure TTurtleResourceTests.test_patient_example_a();
begin
  parseResource('patient-example-a.ttl');
end;

procedure TTurtleResourceTests.test_parameters_example();
begin
  parseResource('parameters-example.ttl');
end;

procedure TTurtleResourceTests.test_organization_example();
begin
  parseResource('organization-example.ttl');
end;

procedure TTurtleResourceTests.test_organization_example_lab();
begin
  parseResource('organization-example-lab.ttl');
end;

procedure TTurtleResourceTests.test_organization_example_insurer();
begin
  parseResource('organization-example-insurer.ttl');
end;

procedure TTurtleResourceTests.test_organization_example_good_health_care();
begin
  parseResource('organization-example-good-health-care.ttl');
end;

procedure TTurtleResourceTests.test_organization_example_gastro();
begin
  parseResource('organization-example-gastro.ttl');
end;

procedure TTurtleResourceTests.test_organization_example_f203_bumc();
begin
  parseResource('organization-example-f203-bumc.ttl');
end;

procedure TTurtleResourceTests.test_organization_example_f201_aumc();
begin
  parseResource('organization-example-f201-aumc.ttl');
end;

procedure TTurtleResourceTests.test_organization_example_f003_burgers_ENT();
begin
  parseResource('organization-example-f003-burgers-ENT.ttl');
end;

procedure TTurtleResourceTests.test_organization_example_f002_burgers_card();
begin
  parseResource('organization-example-f002-burgers-card.ttl');
end;

procedure TTurtleResourceTests.test_organization_example_f001_burgers();
begin
  parseResource('organization-example-f001-burgers.ttl');
end;

procedure TTurtleResourceTests.test_operationoutcome_example();
begin
  parseResource('operationoutcome-example.ttl');
end;

procedure TTurtleResourceTests.test_operationoutcome_example_validationfail();
begin
  parseResource('operationoutcome-example-validationfail.ttl');
end;

procedure TTurtleResourceTests.test_operationoutcome_example_searchfail();
begin
  parseResource('operationoutcome-example-searchfail.ttl');
end;

procedure TTurtleResourceTests.test_operationoutcome_example_exception();
begin
  parseResource('operationoutcome-example-exception.ttl');
end;

procedure TTurtleResourceTests.test_operationoutcome_example_break_the_glass();
begin
  parseResource('operationoutcome-example-break-the-glass.ttl');
end;

procedure TTurtleResourceTests.test_operationoutcome_example_allok();
begin
  parseResource('operationoutcome-example-allok.ttl');
end;

procedure TTurtleResourceTests.test_operationdefinition_example();
begin
  parseResource('operationdefinition-example.ttl');
end;

procedure TTurtleResourceTests.test_observation_example();
begin
  parseResource('observation-example.ttl');
end;

procedure TTurtleResourceTests.test_observation_example_unsat();
begin
  parseResource('observation-example-unsat.ttl');
end;

procedure TTurtleResourceTests.test_observation_example_satO2();
begin
  parseResource('observation-example-satO2.ttl');
end;

procedure TTurtleResourceTests.test_observation_example_sample_data();
begin
  parseResource('observation-example-sample-data.ttl');
end;

procedure TTurtleResourceTests.test_observation_example_glasgow();
begin
  parseResource('observation-example-glasgow.ttl');
end;

procedure TTurtleResourceTests.test_observation_example_glasgow_qa();
begin
  parseResource('observation-example-glasgow-qa.ttl');
end;

procedure TTurtleResourceTests.test_observation_example_genetics_5();
begin
  parseResource('observation-example-genetics-5.ttl');
end;

procedure TTurtleResourceTests.test_observation_example_genetics_4();
begin
  parseResource('observation-example-genetics-4.ttl');
end;

procedure TTurtleResourceTests.test_observation_example_genetics_3();
begin
  parseResource('observation-example-genetics-3.ttl');
end;

procedure TTurtleResourceTests.test_observation_example_genetics_2();
begin
  parseResource('observation-example-genetics-2.ttl');
end;

procedure TTurtleResourceTests.test_observation_example_genetics_1();
begin
  parseResource('observation-example-genetics-1.ttl');
end;

procedure TTurtleResourceTests.test_observation_example_f206_staphylococcus();
begin
  parseResource('observation-example-f206-staphylococcus.ttl');
end;

procedure TTurtleResourceTests.test_observation_example_f205_egfr();
begin
  parseResource('observation-example-f205-egfr.ttl');
end;

procedure TTurtleResourceTests.test_observation_example_f204_creatinine();
begin
  parseResource('observation-example-f204-creatinine.ttl');
end;

procedure TTurtleResourceTests.test_observation_example_f203_bicarbonate();
begin
  parseResource('observation-example-f203-bicarbonate.ttl');
end;

procedure TTurtleResourceTests.test_observation_example_f202_temperature();
begin
  parseResource('observation-example-f202-temperature.ttl');
end;

procedure TTurtleResourceTests.test_observation_example_f005_hemoglobin();
begin
  parseResource('observation-example-f005-hemoglobin.ttl');
end;

procedure TTurtleResourceTests.test_observation_example_f004_erythrocyte();
begin
  parseResource('observation-example-f004-erythrocyte.ttl');
end;

procedure TTurtleResourceTests.test_observation_example_f003_co2();
begin
  parseResource('observation-example-f003-co2.ttl');
end;

procedure TTurtleResourceTests.test_observation_example_f002_excess();
begin
  parseResource('observation-example-f002-excess.ttl');
end;

procedure TTurtleResourceTests.test_observation_example_f001_glucose();
begin
  parseResource('observation-example-f001-glucose.ttl');
end;

procedure TTurtleResourceTests.test_observation_example_bloodpressure();
begin
  parseResource('observation-example-bloodpressure.ttl');
end;

procedure TTurtleResourceTests.test_observation_example_bloodpressure_cancel();
begin
  parseResource('observation-example-bloodpressure-cancel.ttl');
end;

procedure TTurtleResourceTests.test_nutritionorder_example_texture_modified();
begin
  parseResource('nutritionorder-example-texture-modified.ttl');
end;

procedure TTurtleResourceTests.test_nutritionorder_example_renaldiet();
begin
  parseResource('nutritionorder-example-renaldiet.ttl');
end;

procedure TTurtleResourceTests.test_nutritionorder_example_pureeddiet();
begin
  parseResource('nutritionorder-example-pureeddiet.ttl');
end;

procedure TTurtleResourceTests.test_nutritionorder_example_pureeddiet_simple();
begin
  parseResource('nutritionorder-example-pureeddiet-simple.ttl');
end;

procedure TTurtleResourceTests.test_nutritionorder_example_proteinsupplement();
begin
  parseResource('nutritionorder-example-proteinsupplement.ttl');
end;

procedure TTurtleResourceTests.test_nutritionorder_example_infantenteral();
begin
  parseResource('nutritionorder-example-infantenteral.ttl');
end;

procedure TTurtleResourceTests.test_nutritionorder_example_fiberrestricteddiet();
begin
  parseResource('nutritionorder-example-fiberrestricteddiet.ttl');
end;

procedure TTurtleResourceTests.test_nutritionorder_example_enteralcontinuous();
begin
  parseResource('nutritionorder-example-enteralcontinuous.ttl');
end;

procedure TTurtleResourceTests.test_nutritionorder_example_enteralbolus();
begin
  parseResource('nutritionorder-example-enteralbolus.ttl');
end;

procedure TTurtleResourceTests.test_nutritionorder_example_energysupplement();
begin
  parseResource('nutritionorder-example-energysupplement.ttl');
end;

procedure TTurtleResourceTests.test_nutritionorder_example_diabeticsupplement();
begin
  parseResource('nutritionorder-example-diabeticsupplement.ttl');
end;

procedure TTurtleResourceTests.test_nutritionorder_example_diabeticdiet();
begin
  parseResource('nutritionorder-example-diabeticdiet.ttl');
end;

procedure TTurtleResourceTests.test_nutritionorder_example_cardiacdiet();
begin
  parseResource('nutritionorder-example-cardiacdiet.ttl');
end;

procedure TTurtleResourceTests.test_namingsystem_registry();
begin
  parseResource('namingsystem-registry.ttl');
end;

procedure TTurtleResourceTests.test_namingsystem_example();
begin
  parseResource('namingsystem-example.ttl');
end;

procedure TTurtleResourceTests.test_namingsystem_example_id();
begin
  parseResource('namingsystem-example-id.ttl');
end;

procedure TTurtleResourceTests.test_messageheader_example();
begin
  parseResource('messageheader-example.ttl');
end;

procedure TTurtleResourceTests.test_message_response_link();
begin
  parseResource('message-response-link.ttl');
end;

procedure TTurtleResourceTests.test_message_request_link();
begin
  parseResource('message-request-link.ttl');
end;

procedure TTurtleResourceTests.test_medicationstatementexample7();
begin
  parseResource('medicationstatementexample7.ttl');
end;

procedure TTurtleResourceTests.test_medicationstatementexample6();
begin
  parseResource('medicationstatementexample6.ttl');
end;

procedure TTurtleResourceTests.test_medicationstatementexample5();
begin
  parseResource('medicationstatementexample5.ttl');
end;

procedure TTurtleResourceTests.test_medicationstatementexample4();
begin
  parseResource('medicationstatementexample4.ttl');
end;

procedure TTurtleResourceTests.test_medicationstatementexample2();
begin
  parseResource('medicationstatementexample2.ttl');
end;

procedure TTurtleResourceTests.test_medicationstatementexample1();
begin
  parseResource('medicationstatementexample1.ttl');
end;

procedure TTurtleResourceTests.test_medicationrequestexample2();
begin
  parseResource('medicationrequestexample2.ttl');
end;

procedure TTurtleResourceTests.test_medicationrequestexample1();
begin
  parseResource('medicationrequestexample1.ttl');
end;

procedure TTurtleResourceTests.test_medicationexample15();
begin
  parseResource('medicationexample15.ttl');
end;

procedure TTurtleResourceTests.test_medicationexample1();
begin
  parseResource('medicationexample1.ttl');
end;

procedure TTurtleResourceTests.test_medicationdispenseexample8();
begin
  parseResource('medicationdispenseexample8.ttl');
end;

procedure TTurtleResourceTests.test_medicationadministrationexample3();
begin
  parseResource('medicationadministrationexample3.ttl');
end;

procedure TTurtleResourceTests.test_medication_example_f203_paracetamol();
begin
  parseResource('medicationexample0312.ttl');
end;

procedure TTurtleResourceTests.test_media_example();
begin
  parseResource('media-example.ttl');
end;

procedure TTurtleResourceTests.test_media_example_sound();
begin
  parseResource('media-example-sound.ttl');
end;

procedure TTurtleResourceTests.test_media_example_dicom();
begin
  parseResource('media-example-dicom.ttl');
end;

procedure TTurtleResourceTests.test_measurereport_cms146_cat3_example();
begin
  parseResource('measurereport-cms146-cat3-example.ttl');
end;

procedure TTurtleResourceTests.test_measurereport_cms146_cat2_example();
begin
  parseResource('measurereport-cms146-cat2-example.ttl');
end;

procedure TTurtleResourceTests.test_measurereport_cms146_cat1_example();
begin
  parseResource('measurereport-cms146-cat1-example.ttl');
end;

procedure TTurtleResourceTests.test_measure_exclusive_breastfeeding();
begin
  parseResource('measure-exclusive-breastfeeding.ttl');
end;

procedure TTurtleResourceTests.test_location_example();
begin
  parseResource('location-example.ttl');
end;

procedure TTurtleResourceTests.test_location_example_ukpharmacy();
begin
  parseResource('location-example-ukpharmacy.ttl');
end;

procedure TTurtleResourceTests.test_location_example_room();
begin
  parseResource('location-example-room.ttl');
end;

procedure TTurtleResourceTests.test_location_example_patients_home();
begin
  parseResource('location-example-patients-home.ttl');
end;

procedure TTurtleResourceTests.test_location_example_hl7hq();
begin
  parseResource('location-example-hl7hq.ttl');
end;

procedure TTurtleResourceTests.test_location_example_ambulance();
begin
  parseResource('location-example-ambulance.ttl');
end;

procedure TTurtleResourceTests.test_list_example();
begin
  parseResource('list-example.ttl');
end;

procedure TTurtleResourceTests.test_list_example_medlist();
begin
  parseResource('list-example-medlist.ttl');
end;

procedure TTurtleResourceTests.test_list_example_familyhistory_f201_roel();
begin
  parseResource('list-example-familyhistory-f201-roel.ttl');
end;

procedure TTurtleResourceTests.test_list_example_empty();
begin
  parseResource('list-example-empty.ttl');
end;

procedure TTurtleResourceTests.test_list_example_allergies();
begin
  parseResource('list-example-allergies.ttl');
end;

procedure TTurtleResourceTests.test_linkage_example();
begin
  parseResource('linkage-example.ttl');
end;

procedure TTurtleResourceTests.test_library_exclusive_breastfeeding_cqm_logic();
begin
  parseResource('library-exclusive-breastfeeding-cqm-logic.ttl');
end;

procedure TTurtleResourceTests.test_library_exclusive_breastfeeding_cds_logic();
begin
  parseResource('library-exclusive-breastfeeding-cds-logic.ttl');
end;

procedure TTurtleResourceTests.test_library_example();
begin
  parseResource('library-example.ttl');
end;

procedure TTurtleResourceTests.test_library_cms146_example();
begin
  parseResource('library-cms146-example.ttl');
end;

procedure TTurtleResourceTests.test_implementationguide_example();
begin
  parseResource('implementationguide-example.ttl');
end;

procedure TTurtleResourceTests.test_immunizationrecommendation_example();
begin
  parseResource('immunizationrecommendation-example.ttl');
end;

procedure TTurtleResourceTests.test_immunization_example();
begin
  parseResource('immunization-example.ttl');
end;

procedure TTurtleResourceTests.test_immunization_example_refused();
begin
  parseResource('immunization-example-refused.ttl');
end;

procedure TTurtleResourceTests.test_imagingstudy_example();
begin
  parseResource('imagingstudy-example.ttl');
end;

procedure TTurtleResourceTests.test_healthcareservice_example();
begin
  parseResource('healthcareservice-example.ttl');
end;

procedure TTurtleResourceTests.test_guidanceresponse_example();
begin
  parseResource('guidanceresponse-example.ttl');
end;

procedure TTurtleResourceTests.test_group_example();
begin
  parseResource('group-example.ttl');
end;

procedure TTurtleResourceTests.test_group_example_member();
begin
  parseResource('group-example-member.ttl');
end;

procedure TTurtleResourceTests.test_goal_example();
begin
  parseResource('goal-example.ttl');
end;

procedure TTurtleResourceTests.test_flag_example();
begin
  parseResource('flag-example.ttl');
end;

procedure TTurtleResourceTests.test_flag_example_encounter();
begin
  parseResource('flag-example-encounter.ttl');
end;

procedure TTurtleResourceTests.test_familymemberhistory_example();
begin
  parseResource('familymemberhistory-example.ttl');
end;

procedure TTurtleResourceTests.test_familymemberhistory_example_mother();
begin
  parseResource('familymemberhistory-example-mother.ttl');
end;

procedure TTurtleResourceTests.test_explanationofbenefit_example();
begin
  parseResource('explanationofbenefit-example.ttl');
end;

procedure TTurtleResourceTests.test_episodeofcare_example();
begin
  parseResource('episodeofcare-example.ttl');
end;

procedure TTurtleResourceTests.test_enrollmentresponse_example();
begin
  parseResource('enrollmentresponse-example.ttl');
end;

procedure TTurtleResourceTests.test_enrollmentrequest_example();
begin
  parseResource('enrollmentrequest-example.ttl');
end;

procedure TTurtleResourceTests.test_endpoint_example();
begin
  parseResource('endpoint-example.ttl');
end;

procedure TTurtleResourceTests.test_encounter_example();
begin
  parseResource('encounter-example.ttl');
end;

procedure TTurtleResourceTests.test_encounter_example_xcda();
begin
  parseResource('encounter-example-xcda.ttl');
end;

procedure TTurtleResourceTests.test_encounter_example_home();
begin
  parseResource('encounter-example-home.ttl');
end;

procedure TTurtleResourceTests.test_encounter_example_f203_20130311();
begin
  parseResource('encounter-example-f203-20130311.ttl');
end;

procedure TTurtleResourceTests.test_encounter_example_f202_20130128();
begin
  parseResource('encounter-example-f202-20130128.ttl');
end;

procedure TTurtleResourceTests.test_encounter_example_f201_20130404();
begin
  parseResource('encounter-example-f201-20130404.ttl');
end;

procedure TTurtleResourceTests.test_encounter_example_f003_abscess();
begin
  parseResource('encounter-example-f003-abscess.ttl');
end;

procedure TTurtleResourceTests.test_encounter_example_f002_lung();
begin
  parseResource('encounter-example-f002-lung.ttl');
end;

procedure TTurtleResourceTests.test_encounter_example_f001_heart();
begin
  parseResource('encounter-example-f001-heart.ttl');
end;

procedure TTurtleResourceTests.test_eligibilityresponse_example();
begin
  parseResource('eligibilityresponse-example.ttl');
end;

procedure TTurtleResourceTests.test_eligibilityrequest_example();
begin
  parseResource('eligibilityrequest-example.ttl');
end;

procedure TTurtleResourceTests.test_documentreference_example();
begin
  parseResource('documentreference-example.ttl');
end;

procedure TTurtleResourceTests.test_documentmanifest_fm_attachment();
begin
  parseResource('documentmanifest-fm-attachment.ttl');
end;

procedure TTurtleResourceTests.test_document_example_dischargesummary();
begin
  parseResource('document-example-dischargesummary.ttl');
end;

procedure TTurtleResourceTests.test_diagnosticreport_micro1();
begin
  parseResource('diagnosticreport-micro1.ttl');
end;

procedure TTurtleResourceTests.test_diagnosticreport_hla_genetics_results_example();
begin
  parseResource('diagnosticreport-hla-genetics-results-example.ttl');
end;


procedure TTurtleResourceTests.test_diagnosticreport_genetics_comprehensive_bone_marrow_report();
begin
  parseResource('diagnosticreport-genetics-comprehensive-bone-marrow-report.ttl');
end;

procedure TTurtleResourceTests.test_diagnosticreport_examples_general();
begin
  parseResource('diagnosticreport-examples-general.ttl');
end;

procedure TTurtleResourceTests.test_diagnosticreport_example_ultrasound();
begin
  parseResource('diagnosticreport-example-ultrasound.ttl');
end;

procedure TTurtleResourceTests.test_diagnosticreport_example_lipids();
begin
  parseResource('diagnosticreport-example-lipids.ttl');
end;

procedure TTurtleResourceTests.test_diagnosticreport_example_ghp();
begin
  parseResource('diagnosticreport-example-ghp.ttl');
end;

procedure TTurtleResourceTests.test_diagnosticreport_example_f202_bloodculture();
begin
  parseResource('diagnosticreport-example-f202-bloodculture.ttl');
end;

procedure TTurtleResourceTests.test_diagnosticreport_example_f201_brainct();
begin
  parseResource('diagnosticreport-example-f201-brainct.ttl');
end;

procedure TTurtleResourceTests.test_diagnosticreport_example_f001_bloodexam();
begin
  parseResource('diagnosticreport-example-f001-bloodexam.ttl');
end;

procedure TTurtleResourceTests.test_diagnosticreport_example_dxa();
begin
  parseResource('diagnosticreport-example-dxa.ttl');
end;

procedure TTurtleResourceTests.test_deviceusestatement_example();
begin
  parseResource('deviceusestatement-example.ttl');
end;

procedure TTurtleResourceTests.test_devicemetric_example();
begin
  parseResource('devicemetric-example.ttl');
end;

procedure TTurtleResourceTests.test_devicecomponent_example();
begin
  parseResource('devicecomponent-example.ttl');
end;

procedure TTurtleResourceTests.test_devicecomponent_example_prodspec();
begin
  parseResource('devicecomponent-example-prodspec.ttl');
end;

procedure TTurtleResourceTests.test_device_example();
begin
  parseResource('device-example.ttl');
end;

procedure TTurtleResourceTests.test_device_example_udi1();
begin
  parseResource('device-example-udi1.ttl');
end;

procedure TTurtleResourceTests.test_device_example_software();
begin
  parseResource('device-example-software.ttl');
end;

procedure TTurtleResourceTests.test_device_example_pacemaker();
begin
  parseResource('device-example-pacemaker.ttl');
end;

procedure TTurtleResourceTests.test_device_example_ihe_pcd();
begin
  parseResource('device-example-ihe-pcd.ttl');
end;

procedure TTurtleResourceTests.test_device_example_f001_feedingtube();
begin
  parseResource('device-example-f001-feedingtube.ttl');
end;

procedure TTurtleResourceTests.test_detectedissue_example();
begin
  parseResource('detectedissue-example.ttl');
end;

procedure TTurtleResourceTests.test_detectedissue_example_lab();
begin
  parseResource('detectedissue-example-lab.ttl');
end;

procedure TTurtleResourceTests.test_detectedissue_example_dup();
begin
  parseResource('detectedissue-example-dup.ttl');
end;

procedure TTurtleResourceTests.test_detectedissue_example_allergy();
begin
  parseResource('detectedissue-example-allergy.ttl');
end;

procedure TTurtleResourceTests.test_coverage_example();
begin
  parseResource('coverage-example.ttl');
end;

procedure TTurtleResourceTests.test_coverage_example_2();
begin
  parseResource('coverage-example-2.ttl');
end;

procedure TTurtleResourceTests.test_contract_example();
begin
  parseResource('contract-example.ttl');
end;

procedure TTurtleResourceTests.test_condition_example2();
begin
  parseResource('condition-example2.ttl');
end;

procedure TTurtleResourceTests.test_condition_example();
begin
  parseResource('condition-example.ttl');
end;

procedure TTurtleResourceTests.test_condition_example_stroke();
begin
  parseResource('condition-example-stroke.ttl');
end;

procedure TTurtleResourceTests.test_condition_example_f205_infection();
begin
  parseResource('condition-example-f205-infection.ttl');
end;

procedure TTurtleResourceTests.test_condition_example_f204_renal();
begin
  parseResource('condition-example-f204-renal.ttl');
end;

procedure TTurtleResourceTests.test_condition_example_f203_sepsis();
begin
  parseResource('condition-example-f203-sepsis.ttl');
end;

procedure TTurtleResourceTests.test_condition_example_f202_malignancy();
begin
  parseResource('condition-example-f202-malignancy.ttl');
end;

procedure TTurtleResourceTests.test_condition_example_f201_fever();
begin
  parseResource('condition-example-f201-fever.ttl');
end;

procedure TTurtleResourceTests.test_condition_example_f003_abscess();
begin
  parseResource('condition-example-f003-abscess.ttl');
end;

procedure TTurtleResourceTests.test_condition_example_f002_lung();
begin
  parseResource('condition-example-f002-lung.ttl');
end;

procedure TTurtleResourceTests.test_condition_example_f001_heart();
begin
  parseResource('condition-example-f001-heart.ttl');
end;

procedure TTurtleResourceTests.test_conceptmap_example();
begin
  parseResource('conceptmap-example.ttl');
end;

procedure TTurtleResourceTests.test_conceptmap_example_specimen_type();
begin
  parseResource('conceptmap-example-specimen-type.ttl');
end;

procedure TTurtleResourceTests.test_conceptmap_103();
begin
  parseResource('conceptmap-103.ttl');
end;

procedure TTurtleResourceTests.test_composition_example();
begin
  parseResource('composition-example.ttl');
end;

procedure TTurtleResourceTests.test_communicationrequest_example();
begin
  parseResource('communicationrequest-example.ttl');
end;

procedure TTurtleResourceTests.test_communication_example();
begin
  parseResource('communication-example.ttl');
end;

procedure TTurtleResourceTests.test_codesystem_nhin_purposeofuse();
begin
  parseResource('codesystem-nhin-purposeofuse.ttl');
end;

procedure TTurtleResourceTests.test_codesystem_example();
begin
  parseResource('codesystem-example.ttl');
end;

procedure TTurtleResourceTests.test_clinicalimpression_example();
begin
  parseResource('clinicalimpression-example.ttl');
end;

procedure TTurtleResourceTests.test_claimresponse_example();
begin
  parseResource('claimresponse-example.ttl');
end;

procedure TTurtleResourceTests.test_claim_example();
begin
  parseResource('claim-example.ttl');
end;

procedure TTurtleResourceTests.test_claim_example_vision();
begin
  parseResource('claim-example-vision.ttl');
end;

procedure TTurtleResourceTests.test_claim_example_vision_glasses();
begin
  parseResource('claim-example-vision-glasses.ttl');
end;

procedure TTurtleResourceTests.test_claim_example_professional();
begin
  parseResource('claim-example-professional.ttl');
end;

procedure TTurtleResourceTests.test_claim_example_pharmacy();
begin
  parseResource('claim-example-pharmacy.ttl');
end;

procedure TTurtleResourceTests.test_claim_example_oral_orthoplan();
begin
  parseResource('claim-example-oral-orthoplan.ttl');
end;

procedure TTurtleResourceTests.test_claim_example_oral_identifier();
begin
  parseResource('claim-example-oral-identifier.ttl');
end;

procedure TTurtleResourceTests.test_claim_example_oral_contained();
begin
  parseResource('claim-example-oral-contained.ttl');
end;

procedure TTurtleResourceTests.test_claim_example_oral_contained_identifier();
begin
  parseResource('claim-example-oral-contained-identifier.ttl');
end;

procedure TTurtleResourceTests.test_claim_example_oral_average();
begin
  parseResource('claim-example-oral-average.ttl');
end;

procedure TTurtleResourceTests.test_claim_example_institutional();
begin
  parseResource('claim-example-institutional.ttl');
end;

procedure TTurtleResourceTests.test_careteam_example();
begin
  parseResource('careteam-example.ttl');
end;

procedure TTurtleResourceTests.test_careplan_example();
begin
  parseResource('careplan-example.ttl');
end;

procedure TTurtleResourceTests.test_careplan_example_pregnancy();
begin
  parseResource('careplan-example-pregnancy.ttl');
end;

procedure TTurtleResourceTests.test_careplan_example_integrated();
begin
  parseResource('careplan-example-integrated.ttl');
end;

procedure TTurtleResourceTests.test_careplan_example_GPVisit();
begin
  parseResource('careplan-example-GPVisit.ttl');
end;

procedure TTurtleResourceTests.test_careplan_example_f203_sepsis();
begin
  parseResource('careplan-example-f203-sepsis.ttl');
end;

procedure TTurtleResourceTests.test_careplan_example_f202_malignancy();
begin
  parseResource('careplan-example-f202-malignancy.ttl');
end;

procedure TTurtleResourceTests.test_careplan_example_f201_renal();
begin
  parseResource('careplan-example-f201-renal.ttl');
end;

procedure TTurtleResourceTests.test_careplan_example_f003_pharynx();
begin
  parseResource('careplan-example-f003-pharynx.ttl');
end;

procedure TTurtleResourceTests.test_careplan_example_f002_lung();
begin
  parseResource('careplan-example-f002-lung.ttl');
end;

procedure TTurtleResourceTests.test_careplan_example_f001_heart();
begin
  parseResource('careplan-example-f001-heart.ttl');
end;

procedure TTurtleResourceTests.test_bundle_transaction();
begin
  parseResource('bundle-transaction.ttl');
end;

procedure TTurtleResourceTests.test_bundle_response();
begin
  parseResource('bundle-response.ttl');
end;

procedure TTurtleResourceTests.test_bundle_example();
begin
  parseResource('bundle-example.ttl');
end;

procedure TTurtleResourceTests.test_binary_f006();
begin
  parseResource('binary-f006.ttl');
end;

procedure TTurtleResourceTests.test_binary_example();
begin
  parseResource('binary-example.ttl');
end;

procedure TTurtleResourceTests.test_basic_example2();
begin
  parseResource('basic-example2.ttl');
end;

procedure TTurtleResourceTests.test_basic_example();
begin
  parseResource('basic-example.ttl');
end;

procedure TTurtleResourceTests.test_basic_example_narrative();
begin
  parseResource('basic-example-narrative.ttl');
end;

procedure TTurtleResourceTests.test_auditevent_example();
begin
  parseResource('auditevent-example.ttl');
end;

procedure TTurtleResourceTests.test_auditevent_example_disclosure();
begin
  parseResource('auditevent-example-disclosure.ttl');
end;

procedure TTurtleResourceTests.test_audit_event_example_vread();
begin
  parseResource('audit-event-example-vread.ttl');
end;

procedure TTurtleResourceTests.test_audit_event_example_search();
begin
  parseResource('audit-event-example-search.ttl');
end;

initialization
  TDUnitX.RegisterTestFixture(TFHIRParserTests);
  TDUnitX.RegisterTestFixture(TFHIRParserSpecialTests);
  TDUnitX.RegisterTestFixture(TTurtleResourceTests);
end.
