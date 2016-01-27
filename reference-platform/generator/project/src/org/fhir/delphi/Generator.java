package org.fhir.delphi;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.hl7.fhir.dstu2.formats.XmlParser;
import org.hl7.fhir.dstu2.model.Bundle;
import org.hl7.fhir.dstu2.model.Bundle.BundleEntryComponent;
import org.hl7.fhir.dstu2.model.Conformance;
import org.hl7.fhir.dstu2.model.DateTimeType;
import org.hl7.fhir.dstu2.model.OperationDefinition;
import org.hl7.fhir.dstu2.utils.ToolingExtensions;
import org.hl7.fhir.dstu2.model.Resource;
import org.hl7.fhir.dstu2.model.SearchParameter;
import org.hl7.fhir.dstu2.model.StructureDefinition;
import org.hl7.fhir.exceptions.FHIRFormatError;
import org.hl7.fhir.utilities.TextFile;
import org.hl7.fhir.utilities.Utilities;

public class Generator {

  public static void main(String[] args) throws Exception {
    Generator self = new Generator();
    System.out.println("Generate pascal code in "+args[1]+" from "+args[0]);
    Definitions definitions = new DefinitionsLoader2().loadDefinitions(args[0]);
    TextFile.stringToFile(new DefinitionDumper().dumpDefinitions(definitions), "c:\\temp\\pascal.txt");
    new DelphiGenerator(args[1]).generate(definitions, definitions.getVersion(), definitions.getGenDate());
    System.out.println("Done");
  }

}
