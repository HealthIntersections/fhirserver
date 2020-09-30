package org.fhir.delphi.misc;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.hl7.fhir.utilities.TextFile;

public class ResourcesSplitter {

  public static void main(String[] args) throws IOException {
    new ResourcesSplitter().execute();
  }

  public void execute() throws IOException {
    process("Admin", new String[] {"CatalogEntry", "Device", "DeviceDefinition", "DeviceMetric", "Encounter", "Endpoint", "Group", "HealthcareService", "Location", "Organization", "OrganizationAffiliation", "Patient", "Person", "Practitioner", "PractitionerRole", "RelatedPerson", "Schedule", "Slot"});
    process("Clinical", new String[] {"AdverseEvent", "AllergyIntolerance", "Appointment", "AppointmentResponse", "Basic", "BiologicallyDerivedProduct", "BodyStructure", "CarePlan", "CareTeam", "ClinicalImpression", "ClinicalUseIssue", "Communication", "CommunicationRequest", "Composition", "Condition", "DetectedIssue", "DeviceRequest", "DeviceUseStatement", "DiagnosticReport", "DocumentManifest", "DocumentReference", "EpisodeOfCare", "FamilyMemberHistory", "Flag", "Goal", "ImagingStudy", "Immunization", "ImmunizationEvaluation", "ImmunizationRecommendation", "MedicationAdministration", "MedicationDispense", "MedicationRequest", "MedicationUsage", "MolecularSequence", "NutritionIntake", "NutritionOrder", "Observation", "Procedure", "RiskAssessment", "ServiceRequest", "Specimen", "SupplyDelivery", "SupplyRequest", "VisionPrescription"});
//    process("Financial", new String[] {"Account", "ChargeItem", "ChargeItemDefinition", "Claim", "ClaimResponse", "Coverage", "CoverageEligibilityRequest", "CoverageEligibilityResponse", "EnrollmentRequest", "EnrollmentResponse", "ExplanationOfBenefit", "InsurancePlan", "Invoice", "PaymentNotice", "PaymentReconciliation", "Citation"});
//    process("Medications", new String[] {"AdministrableProductDefinition", "Ingredient", "ManufacturedItemDefinition", "Medication", "MedicationKnowledge", "MedicinalProductDefinition", "NutritionProduct", "PackagedProductDefinition", "RegulatedAuthorization", "Substance", "SubstanceDefinition", "SubstanceNucleicAcid", "SubstancePolymer", "SubstanceProtein", "SubstanceReferenceInformation", "SubstanceSourceMaterial"});
    process("Other", new String[] {"AuditEvent", "Binary", "Bundle", "Consent", "Contract", "Evidence", "EvidenceReport", "EvidenceVariable", "GuidanceResponse", "Linkage", "List", "MeasureReport", "MessageHeader", "OperationOutcome", "Parameters", "Permission", "Provenance", "QuestionnaireResponse", "ResearchStudy", "ResearchSubject", "Subscription", "SubscriptionStatus", "SubscriptionTopic", "Task", "TestReport", "VerificationResult"});
    process("Canonical", new String[] {"ActivityDefinition", "CapabilityStatement", "CapabilityStatement2", "CodeSystem", "CompartmentDefinition", "ConceptMap", "ConditionDefinition", "EventDefinition", "ExampleScenario", "GraphDefinition", "ImplementationGuide", "Library", "Measure", "MessageDefinition", "NamingSystem", "ObservationDefinition", "OperationDefinition", "PlanDefinition", "Questionnaire", "RequestGroup", "SearchParameter", "SpecimenDefinition", "StructureDefinition", "StructureMap", "TerminologyCapabilities", "TestScript", "ValueSet"});

  }

  private void process(String name, String[] resources) throws IOException {
    List<String> lines = loadFile("C:\\work\\fhirserver\\library\\R3\\FHIR.R3.Resources."+name+".pas"); 
    List<String> typelines = new ArrayList<>();
    
    boolean processing = true;
    boolean hasAlias = false;
    String start = null;

    for (String s : lines) {
      if (s.startsWith("{$IFDEF FHIR_")) {
        start = s;
      } else if (s.startsWith("{$ENDIF FHIR_")) {
        if (!typelines.isEmpty()) {
          System.out.println(start);
          for (String t : typelines) {
            System.out.println(t);            
          }
          System.out.println(s);
        }
        typelines.clear();
        start = null;
      } else if (s.endsWith("= class;") && start != null) {
        String tn = s.substring(0, s.indexOf("=")).trim();
        typelines.add("  "+tn+" = FHIR.R5.Resources."+name+"."+tn+";");      
      }
    }
//    for (int i = lines.size()-1; i >= 0; i--) {
//      String s = lines.get(i);
//      if (s.startsWith("{$ENDIF FHIR_")) {
//        String rn = s.substring(13);
//        rn = rn.substring(0, rn.indexOf("}"));
//        processing = hasResource(resources, rn);
//        if (!processing) {
//          lines.remove(i);
//        } else {
//          typelines.add(0, s);
//          hasAlias = false;
//        }
//      } else if (s.startsWith("{$IFDEF FHIR_")) {
//        if (!processing) {
//          lines.remove(i);
//        } else {
//          if (hasAlias) {
//            typelines.add(0, s);
//          } else {
//            typelines.remove(0);            
//          }
//        }
//        processing = true;
//      } else if (!processing) {
//        lines.remove(i);
//      } else {
//        if (s.endsWith("= class;") ) {
//          String tn = s.substring(0, s.indexOf("=")).trim();
//          typelines.add(0, "  "+tn+" = FHIR.R5.Resources."+name+"."+tn);
//          hasAlias = true;
//        }
//      }
//    }
//    StringBuilder b = new StringBuilder();
//    for (String line : lines) {
//      b.append(line);
//      b.append("\r\n");
//    }
//    TextFile.stringToFile(b.toString(), "C:\\work\\fhirserver\\library\\R5\\FHIR.R5.Resources."+name+".pas");
//    for (String line : typelines) {
//      System.out.println(line);
//    }
  }
  
  private boolean hasResource(String[] resources, String rn) {
    for (String s : resources) {
      if (s.equalsIgnoreCase(rn)) {
        return true;
      }
    }
    return false;
  }

  private List<String> loadFile(String filename) throws IOException {
    BufferedReader bufReader = new BufferedReader(new FileReader(filename)); 
    ArrayList<String> listOfLines = new ArrayList<>(); 
    String line = bufReader.readLine(); 
    while (line != null) { 
      listOfLines.add(line); 
      line = bufReader.readLine(); 
    } 
    bufReader.close();

    return listOfLines;
  }

}
