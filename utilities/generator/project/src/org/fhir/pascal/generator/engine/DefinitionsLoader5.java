package org.fhir.pascal.generator.engine;

import java.io.IOException;

import org.hl7.fhir.exceptions.FHIRFormatError;
import org.hl7.fhir.r5.formats.JsonParser;
import org.hl7.fhir.r5.model.Bundle;
import org.hl7.fhir.r5.model.Bundle.BundleEntryComponent;
import org.hl7.fhir.r5.model.CapabilityStatement;
import org.hl7.fhir.r5.model.CodeSystem;
import org.hl7.fhir.r5.model.CompartmentDefinition;
import org.hl7.fhir.r5.model.ConceptMap;
import org.hl7.fhir.r5.model.ElementDefinition;
import org.hl7.fhir.r5.model.OperationDefinition;
import org.hl7.fhir.r5.model.Resource;
import org.hl7.fhir.r5.model.SearchParameter;
import org.hl7.fhir.r5.model.StructureDefinition;
import org.hl7.fhir.r5.model.UrlType;
import org.hl7.fhir.r5.model.ValueSet;
import org.hl7.fhir.r5.utils.ToolingExtensions;
import org.hl7.fhir.utilities.npm.NpmPackage;

public class DefinitionsLoader5 {

  public static Definitions load(NpmPackage npm) throws IOException {
    Definitions res = new Definitions();
    
    for (String t : npm.listResources("CodeSystem")) {
      res.getCodeSystems().see((CodeSystem) load(npm, t), null);
    }
    for (String t : npm.listResources("ValueSet")) {
      res.getValuesets().see((ValueSet) load(npm, t), null);
    }
    for (String t : npm.listResources("ConceptMap")) {
      res.getConceptMaps().see((ConceptMap) load(npm, t), null);
    }
    for (String t : npm.listResources("CapabilityStatement")) {
      res.getStatements().see((CapabilityStatement) load(npm, t), null);
    }
    for (String t : npm.listResources("StructureDefinition")) {
      res.getStructures().see(fixSD((StructureDefinition) load(npm, t)), null);
    }
    for (String t : npm.listResources("OperationDefinition")) {
      res.getOperations().see((OperationDefinition) load(npm, t), null);
    }
    for (String t : npm.listResources("SearchParameter")) {
      res.getSearchParams().see((SearchParameter) load(npm, t), null);
    }
    for (String t : npm.listResources("CompartmentDefinition")) {
      res.getCompartments().see((CompartmentDefinition) load(npm, t), null);
    }
    for (String t : npm.listResources("Bundle")) {
      loadBundle(npm, t, res);
    }
    return res;
  }

  private static StructureDefinition fixSD(StructureDefinition sd) {
    for (ElementDefinition ed : sd.getSnapshot().getElement()) {
      if ("Element.id".equals(ed.getPath()) || "Element.id".equals(ed.getBase().getPath())) {
        ToolingExtensions.setUriExtension(ed.getTypeFirstRep(), 
            "http://hl7.org/fhir/StructureDefinition/structuredefinition-fhir-type", 
            "string");
      }
    }
    for (ElementDefinition ed : sd.getDifferential().getElement()) {
      if ("Element.id".equals(ed.getPath()) || "Element.id".equals(ed.getBase().getPath())) {
        ToolingExtensions.setUriExtension(ed.getTypeFirstRep(), 
            "http://hl7.org/fhir/StructureDefinition/structuredefinition-fhir-type", 
            "string");
      }
    }
    return sd;
  }

  public static void loadBundle(NpmPackage npm, String t, Definitions def) throws FHIRFormatError, IOException {
	  try {
		  Bundle bundle = (Bundle) new JsonParser().parse(npm.loadResource(t));
		  for (BundleEntryComponent be : bundle.getEntry()) {
			  if (be.hasResource()) {
				  Resource res = be.getResource();
				  if (res instanceof SearchParameter) {
					  def.getSearchParams().see((SearchParameter) res, null);				
				  }
			  }
		  }
	  } catch (Exception e) {
         System.out.print("Unable to parse "+t);         
	  }
  }
  
  public static Resource load(NpmPackage npm, String t) {
    try {
      return new JsonParser().parse(npm.loadResource(t));
    } catch (Exception e) {
      throw new Error("Error reading "+t+": "+e.getMessage(), e);
    }
  }
}
