package org.fhir.delphi;

import org.hl7.fhir.utilities.TextFile;

public class Generator {

  public static void main(String[] args) throws Exception {
    System.out.println("Generate pascal code in "+args[1]+" from "+args[0]);
    Definitions definitions = new DefinitionsLoader2().loadDefinitions(args[0]);
    TextFile.stringToFile(new DefinitionDumper().dumpDefinitions(definitions), "c:\\temp\\pascal.txt");
    new DelphiGenerator(args[1]).generate(definitions, definitions.getVersion(), definitions.getGenDate());
    System.out.println("Done");
  }

}
