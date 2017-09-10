package org.fhir.delphi;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

import org.hl7.fhir.convertors.VersionConvertor_10_40;
import org.hl7.fhir.dstu2.model.Enumerations.SearchParamType;
import org.hl7.fhir.dstu2.model.SearchParameter.XPathUsageType;
import org.hl7.fhir.utilities.TextFile;

public class Generator {

  public static void main(String[] args) throws Exception {
    System.out.println("Generate pascal code for dstu"+args[2]+" in "+args[1]+" from "+args[0]);
    Definitions definitions;
    if (args[2].equals("4"))
      definitions = new DefinitionsLoader4().loadDefinitions(args[0]);
    else if (args[2].equals("3"))
      definitions = new DefinitionsLoader3().loadDefinitions(args[0]);
    else if (args[2].equals("2"))
      definitions = new DefinitionsLoader2().loadDefinitions(args[0]);
    else
      throw new Error("Unsupported version "+args[2]);
    TextFile.stringToFile(new DefinitionDumper().dumpDefinitions(definitions), "c:\\temp\\pascal.txt");
    addExtensionSearch(definitions, "dna-variant", "http://hl7.org/fhir/StructureDefinition/observation-geneticsDNASequenceVariantName", SearchParamType.TOKEN, "Observation");
    addExtensionSearch(definitions, "gene-dnavariant", "http://hl7.org/fhir/StructureDefinition/observation-geneticsDNAVariantId", SearchParamType.TOKEN, "Observation");
    addExtensionSearch(definitions, "gene-identifier", "http://hl7.org/fhir/StructureDefinition/observation-geneticsGene", SearchParamType.TOKEN, "Observation");
    new DelphiGenerator(args[1]).generate(definitions, definitions.getVersion(), definitions.getGenDate(), Integer.parseInt(args[2]));
    System.out.println("Done");
  }

  private static void addExtensionSearch(Definitions def, String code, String extUrl, SearchParamType type, String rt) throws Exception {
    SearchParameterDefn spd = new SearchParameterDefn();
    spd.setCode(code);
    spd.setExpression(rt+".extension('"+extUrl+"').value");
    spd.setWorks(true);
    spd.setDescription("search for extension "+extUrl);
    spd.setType(type);
    spd.setxPathUsage(XPathUsageType.NORMAL);
    def.getResourceByName(rt).getSearchParams().put(spd.getCode(), spd);
  }

}
